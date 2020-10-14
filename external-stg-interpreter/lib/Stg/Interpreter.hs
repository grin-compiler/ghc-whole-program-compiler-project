{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter where

import Debug.Trace
import Control.Monad.State

import Data.Char
import Data.List (partition, concatMap)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.Vector (Vector)
import qualified Data.Vector as V

import Stg.Syntax
import Stg.Program

{-
  Q: what is the operational semantic of StgApp
  A: check slowCall :: CmmExpr -> [StgArg] -> FCode ReturnKind semantics
  done - read STG eval apply paper

  TODO:
    - refresh all STG uniques to be globally unique
    - collect free variables for closures

  EVAL:
    - setup top level env (TopEnv) ; add top level bindings to it
    - call main
-}

newtype Id = Id {unId :: Binder}

instance Eq Id where
  (Id a) == (Id b) = binderUniqueName a == binderUniqueName b -- FIXME: make this fast

instance Ord Id where
  compare (Id a) (Id b) = compare (binderUniqueName a) (binderUniqueName b) -- FIXME: make this fast

instance Show Id where
  show (Id a) = show $ binderUniqueName a

type StgRhsClosure = Rhs  -- NOTE: must be StgRhsClosure only!

data HeapObject
  = Con
    { hoCon         :: DataCon
    , hoConArgs     :: [Atom]
    }
  | Closure
    { hoName        :: Id
    , hoCloBody     :: StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [Atom]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }
  | Blackhole HeapObject
  deriving (Show, Eq, Ord)

data StackContinuation
  = CaseOf  Id AltType [Alt]  -- pattern match on the result
  | Update  Addr                  -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply   [Atom]                -- apply args on the result heap object
  deriving (Show, Eq, Ord)

data Atom     -- Q: should atom fit into a cpu register?
  = HeapPtr   Addr
  | Literal   Lit             -- Q: shopuld we allow string literals, or should string lits be modeled as StringPtr?
  | StringPtr Int ByteString  -- HINT: StgTopStringLit ; maybe include its origin? ; the PrimRep is AddrRep
  | RtsPrim   -- ??? or is this a heap object?
  | Void
  | StablePointer Atom -- TODO: need proper implementation
  | MVar          Int
  | MutableArray  Int
  | MutVar        Int
  deriving (Show, Eq, Ord)

type ReturnValue = [Atom]

type Addr   = Int
type Heap   = IntMap HeapObject
type Env    = Map Id Atom   -- NOTE: must contain only the defined local variables
type Stack  = [(Env, StackContinuation)]

{-
  Q: do we want homogeneous or heterogeneous Heap ; e.g. single intmap with mixed things or multiple intmaps/vector with multiple address spaces
-}

data StgState
  = StgState
  { ssHeap      :: !Heap
  , ssEnv       :: !Env
--  , ssStack     :: [(Env, StackContinuation)] -- TODO: use reified stack instead of the host language stack
  , ssEvalStack :: [Id]
  , ssNextAddr  :: !Int
  , ssMVars         :: IntMap (Maybe Atom)
  , ssMutableArrays :: IntMap (Vector Atom)
  , ssMutVars       :: IntMap Atom
  }
  deriving (Show, Eq, Ord)

emptyStgState :: StgState
emptyStgState = StgState
  { ssHeap      = mempty
  , ssEnv       = mempty
  , ssEvalStack = []
  , ssNextAddr  = 0
  , ssMVars         = mempty
  , ssMutableArrays = mempty
  , ssMutVars       = mempty
  }

type M = State StgState

freshHeapAddress :: M Addr
freshHeapAddress = state $ \s@StgState{..} -> (ssNextAddr, s {ssNextAddr = succ ssNextAddr})

allocAndStore :: HeapObject -> M Addr
allocAndStore o = do
  a <- freshHeapAddress
  store a o
  pure a

store :: Addr -> HeapObject -> M ()
store a o = modify' $ \s -> s {ssHeap = IntMap.insert a o (ssHeap s)}

addEnv :: Binder -> Atom -> M ()
addEnv b a = modify' $ \s -> s {ssEnv = Map.insert (Id b) a (ssEnv s)}

stgErrorM :: String -> M a
stgErrorM msg = do
  es <- gets ssEvalStack
  error $ unlines ["eval stack:", show es, msg]

lookupEnv :: Binder -> M Atom
lookupEnv b
 | binderId b == BinderId (Unique '0' 21) -- void#
 = pure Void
 | binderId b == BinderId (Unique '0' 15) -- realWorld#
 = pure Void
 | otherwise
 = do
  env <- gets ssEnv
  case Map.lookup (Id b) env of
    Nothing -> stgErrorM $ "unknown variable: " ++ show b
    Just a  -> pure a

evalArg :: Arg -> M Atom
evalArg = \case
  StgLitArg l -> pure $ Literal l
  StgVarArg b -> lookupEnv b

update :: Atom -> [Atom] -> M [Atom]
update (HeapPtr dst) result@[src] = do
  o <- readHeap src
  store dst o
  pure result

builtinStgEval :: Atom -> M [Atom]
builtinStgEval a = do
  o <- readHeap a
  case o of
    Con{}       -> pure [a]
    Blackhole t -> stgErrorM $ "blackhole ; loop in evaluation of : " ++ show t
    Closure{..}
      | hoCloMissing /= 0
      -> pure [a]

      | otherwise
      -> do
        let StgRhsClosure uf params e = hoCloBody
            HeapPtr l = a
            argsM = zipWithM_ addEnv params hoCloArgs
        -- TODO: env or free var handling
        case uf of
          ReEntrant -> do
            -- closure may be entered multiple times, but should not be updated or blackholed.
            withEnv hoName hoEnv (argsM >> evalExpr e)
          Updatable -> do
            -- closure should be updated after evaluation (and may be blackholed during evaluation).
            store l (Blackhole o)
            result <- withEnv hoName hoEnv (argsM >> evalExpr e)
            update a result
          SingleEntry -> do
            -- closure will only be entered once, and so need not be updated but may safely be blackholed.
            store l (Blackhole o)
            withEnv hoName hoEnv (argsM >> evalExpr e)

withEnv :: Id -> Env -> M a -> M a
withEnv i env m = do
  save <- gets ssEnv
  saveES <- gets ssEvalStack
  modify' $ \s -> s {ssEnv = env, ssEvalStack = [i]}
  res <- trace ("eval " ++ show i ++ " " ++ show (binderModule $ unId i)) $ m
  modify' $ \s -> s {ssEnv = save, ssEvalStack = saveES}
  pure res

builtinStgApply :: Atom -> [Atom] -> M [Atom]
builtinStgApply a args = do
  let argCount      = length args
      HeapPtr addr  = a
  o <- readHeap a
  case o of
    Con{}       -> stgErrorM $ "unexpexted con at apply: " ++ show o
    Blackhole t -> stgErrorM $ "blackhole ; loop in application of : " ++ show t
    Closure{..}
      -- under saturation
      | hoCloMissing - argCount > 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        pure [HeapPtr newAp]

      -- over saturation
      | hoCloMissing - argCount < 0
      -> do
        let (satArgs, remArgs) = splitAt hoCloMissing args
        builtinStgApply a satArgs >>= \case
          [f] -> builtinStgApply f remArgs
          r   -> stgErrorM $ "invalid over-application result: " ++ show r

      -- saturation
      | hoCloMissing - argCount == 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        builtinStgEval (HeapPtr newAp)

readHeap :: Atom -> M HeapObject
readHeap (HeapPtr l) = do
  h <- gets ssHeap
  case IntMap.lookup l h of
    Nothing -> stgErrorM $ "unknown heap address: " ++ show l
    Just o  -> pure o

readHeapCon :: Atom -> M HeapObject
readHeapCon a = readHeap a >>= \o -> case o of
    Con{} -> pure o
    _     -> stgErrorM $ "expected con but got: " ++ show o

evalExpr :: Expr -> M [Atom]
evalExpr = \case
  StgTick _ e       -> evalExpr e
  StgLit l          -> pure [Literal l] -- FIXME: LitString?? is it addr??
  StgConApp dc l _
    -- HINT: make and return unboxed tuple
    | UnboxedTupleCon{} <- dcRep dc
    -> mapM evalArg l   -- Q: is this only for unboxed tuple? could datacon be heap allocated?

    -- HINT: create boxed datacon on the heap
    | otherwise
    -> do
      args <- mapM evalArg l
      loc <- allocAndStore (Con dc args)
      pure [HeapPtr loc]

  StgLet b e -> do
    declareBinding b
    evalExpr e

  StgLetNoEscape b e -> do -- TODO: do not allocate closure on heap, instead put into env (stack) allocated closure ; model stack allocated heap objects
    declareBinding b
    evalExpr e

  -- var
  StgApp i [] _t _ -> case binderType i of

    SingleValue LiftedRep -> do
      -- HINT: must be HeapPtr ; read heap ; check if Con or Closure ; eval if Closure ; return HeapPtr if Con
      v <- lookupEnv i
      builtinStgEval v

    SingleValue _ -> do
      -- HINT: unlifted and literals ; Unlifted must be a HeapPtr that points to a Con
      v <- lookupEnv i
      pure [v]

    r -> stgErrorM $ "unsupported var rep: " ++ show r -- unboxed: is it possible??

  -- fun app
  --  Q: should app always be lifted/unlifted?
  --  Q: what does unlifted app mean? (i.e. no Ap node, but saturated calls to known functions only?)
  StgApp i l _t _ -> case binderType i of
    SingleValue _ -> do
      args <- mapM evalArg l
      v <- lookupEnv i
      builtinStgApply v args

    r -> stgErrorM $ "unsupported app rep: " ++ show r -- unboxed: invalid

  StgCase e resultId aty alts -> do
    result <- evalExpr e
    case aty of
      AlgAlt tc -> do
        let [v] = result
        addEnv resultId v -- HINT: bind the result
        con <- readHeapCon v
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstCon con $ al ++ [d]
          _ -> matchFirstCon con alts

      PrimAlt r -> do
        let [lit] = result
        addEnv resultId lit -- HINT: bind the result
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstLit lit $ al ++ [d]
          _ -> matchFirstLit lit alts

      MultiValAlt _n -> do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let [Alt{..}] = alts
        zipWithM_ addEnv altBinders result
        evalExpr altRHS

      PolyAlt -> do
        let [v] = result
        addEnv resultId v -- HINT: bind the result
        let [Alt{..}] = alts
        zipWithM_ addEnv altBinders result
        evalExpr altRHS

  StgOpApp (StgPrimOp op) l _t _tc -> do
    args <- mapM evalArg l
    case (op, args) of
      ("catch#", [f, h, w]) -> builtinStgApply f [w]
      ("myThreadId#", [w]) -> pure [RtsPrim] -- State# RealWorld -> (# State# RealWorld, ThreadId# #)
      ("mkWeakNoFinalizer#", [_o, _b, w]) -> pure [RtsPrim] -- o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
      ("getMaskingState#", [w]) -> pure [Literal $ LitNumber LitNumInt 1] -- State# RealWorld -> (# State# RealWorld, Int# #)
      ("noDuplicate#", [s]) -> pure [s] --  State# s -> State# s

      -- Mutable Array
      ("newArray#", [Literal (LitNumber LitNumInt i), a, s]) -> do
        -- Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
        let v = V.replicate (fromIntegral i) a
        state (\s@StgState{..} -> let next = IntMap.size ssMutableArrays in ([MutableArray next], s {ssMutableArrays = IntMap.insert next v ssMutableArrays}))

      ("readArray#", [MutableArray a, Literal (LitNumber LitNumInt i), s]) -> do
        -- MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
        v <- lookupMutableArray a
        pure [v V.! (fromIntegral i)]

      ("writeArray#", [MutableArray m, Literal (LitNumber LitNumInt i), a, s]) -> do
        -- MutableArray# s a -> Int# -> a -> State# s -> State# s
        v <- lookupMutableArray m
        modify' $ \s@StgState{..} -> s {ssMutableArrays = IntMap.insert m (v V.// [(fromIntegral i, a)]) ssMutableArrays}
        pure [s]

      -- MutVar
      ("newMutVar#", [a, s]) -> do
        -- a -> State# s -> (# State# s, MutVar# s a #)
        state (\s@StgState{..} -> let next = IntMap.size ssMutVars in ([MutVar next], s {ssMutVars = IntMap.insert next a ssMutVars}))

      ("readMutVar#", [MutVar m, s]) -> do
        -- MutVar# s a -> State# s -> (# State# s, a #)
        a <- lookupMutVar m
        pure [a]

      -- MVar
      ("newMVar#", [s]) -> do
        -- State# s -> (# State# s, MVar# s a #)
        state (\s@StgState{..} -> let next = IntMap.size ssMVars in ([MVar next], s {ssMVars = IntMap.insert next Nothing ssMVars}))
      ("putMVar#", [MVar m, a, s]) -> do
        -- MVar# s a -> a -> State# s -> State# s
        lookupMVar m >>= \case
          Just{}  -> stgErrorM $ "TODO: blocking putMVar# for mvar " ++ show m
          Nothing -> modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m (Just a) ssMVars}
        pure [s]
      ("takeMVar#", [MVar m, s]) -> do
        -- MVar# s a -> State# s -> (# State# s, a #)
        lookupMVar m >>= \case
          Nothing -> stgErrorM $ "TODO: blocking takeMVar# for mvar " ++ show m
          Just a  -> state $ \s@StgState{..} -> ([a], s {ssMVars = IntMap.insert m Nothing ssMVars})

      ("eqAddr#", [a, b]) -> pure [Literal $ LitNumber LitNumInt 0] -- Addr# -> Addr# -> Int#

      -- TODO: implement stable pointers properly
      ("makeStablePtr#", [a, s]) -> pure [StablePointer a] -- a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
      ("deRefStablePtr#", [StablePointer a, s]) -> pure [a] -- TODO: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)

      ("maskUninterruptible#", [a]) -> pure [a] -- TODO : (State# RealWorld -> (# State# RealWorld, a #)) -> (State# RealWorld -> (# State# RealWorld, a #))
      -- HACK:
      ("maskUninterruptible#", [a, b]) -> builtinStgApply a [b]

      ("<=#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (if a <= b then 1 else 0))] -- Int# -> Int# -> Int#
      (">=#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (if a >= b then 1 else 0))] -- Int# -> Int# -> Int#

      ("+#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (a + b))] -- Int# -> Int# -> Int#
      ("*#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (a * b))] -- Int# -> Int# -> Int#

      -- "uncheckedIShiftL#" args: [Literal (LitNumber LitNumInt 0),Literal (LitNumber LitNumInt 2)]
      ("uncheckedIShiftL#", [a, b]) -> do
        -- Int# -> Int# -> Int#
        pure [a] -- TODO

      ("newPinnedByteArray#", [Literal (LitNumber LitNumInt size), s]) -> do
        -- Int# -> State# s -> (# State# s, MutableByteArray# s #)
        pure [RtsPrim]

      ("newAlignedPinnedByteArray#", [Literal (LitNumber LitNumInt size), Literal (LitNumber LitNumInt align), s]) -> do
        -- Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
        pure [RtsPrim]

      ("unsafeFreezeByteArray#", [a, s]) -> do
        -- MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
        pure [RtsPrim]

      ("byteArrayContents#", [a]) -> do
        -- ByteArray# -> Addr#
        pure [Literal LitNullAddr]

      ("readAddrOffAddr#", [addr, Literal (LitNumber LitNumInt offset), s]) -> do
        -- Addr# -> Int# -> State# s -> (# State# s, Addr# #)
        pure [Literal LitNullAddr]

      ("readInt8OffAddr#", [addr, Literal (LitNumber LitNumInt offset), s]) -> do
        -- Addr# -> Int# -> State# s -> (# State# s, Int# #)
        pure [Literal (LitNumber LitNumInt 0)]

      ("indexCharOffAddr#", [StringPtr base str, Literal (LitNumber LitNumInt offset)]) -> do
        -- Addr# -> Int# -> Char#
        pure [Literal $ LitChar $ BS8.index str (base + fromIntegral offset)]

      ("plusAddr#", [StringPtr base str, Literal (LitNumber LitNumInt offset)]) -> do
        -- Addr# -> Int# -> Addr#
        pure [StringPtr (base + fromIntegral offset) str]

      ("plusAddr#", [a, Literal (LitNumber LitNumInt offset)]) -> do
        -- Addr# -> Int# -> Addr#
        pure [a]


      ("ord#", [Literal (LitChar c)]) -> do
        -- Char# -> Int#
        pure [Literal (LitNumber LitNumInt . fromIntegral $ ord c)]

      ("narrow8Int#", [i]) -> do
        -- Int# -> Int#
        pure [i] -- TODO

      -- "writeInt8OffAddr#" args: [Literal LitNullAddr,Literal (LitNumber LitNumInt 0),Literal (LitNumber LitNumInt 85),Void]
      ("writeInt8OffAddr#", [a, b, c, s]) -> do
        -- Addr# -> Int# -> Int# -> State# s -> State# s
        pure [] -- TODO

      ("touch#", [o, s]) -> do
        -- o -> State# RealWorld -> State# RealWorld
        pure []

      -- "writeWideCharOffAddr#" args: [Literal LitNullAddr,Literal (LitNumber LitNumInt 0),Literal (LitChar 'a'),Void]
      ("writeWideCharOffAddr#", [a, b, c, s]) -> do
        -- Addr# -> Int# -> Char# -> State# s -> State# s
        pure [] -- TODO

      _ -> stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args

  StgOpApp fop@(StgFCallOp (ForeignCall{..})) l t _tc -> do
    args <- mapM evalArg l
    case foreignCTarget of
      StaticTarget _ "localeEncoding" _ _ -> pure [Literal LitNullAddr]
      StaticTarget _ "getProgArgv" _ _ -> pure []
      StaticTarget _ "hs_free_stable_ptr" _ _ -> pure []
      StaticTarget _ "rts_setMainThread" _ _ -> pure []
      StaticTarget _ "getOrSetGHCConcSignalSignalHandlerStore" _ _ -> pure [head args] -- WTF!
      StaticTarget _ "stg_sig_install" _ _ -> pure [Literal (LitNumber LitNumInt 0)]
      StaticTarget _ "hs_iconv_open" _ _ -> pure [Literal (LitNumber LitNumInt 0)]
      _ -> stgErrorM $ "unsupported StgFCallOp: " ++ show fop ++ " :: " ++ show t ++ " args: " ++ show args

  StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t

lookupMutVar :: Int -> M Atom
lookupMutVar m = do
  IntMap.lookup m <$> gets ssMutVars >>= \case
    Nothing -> stgErrorM $ "unknown MutVar: " ++ show m
    Just a  -> pure a

lookupMVar :: Int -> M (Maybe Atom)
lookupMVar m = do
  IntMap.lookup m <$> gets ssMVars >>= \case
    Nothing -> stgErrorM $ "unknown MVar: " ++ show m
    Just a  -> pure a

lookupMutableArray :: Int -> M (Vector Atom)
lookupMutableArray m = do
  IntMap.lookup m <$> gets ssMutableArrays >>= \case
    Nothing -> stgErrorM $ "unknown MutableArrays: " ++ show m
    Just a  -> pure a

{-
          (State# RealWorld -> (# State# RealWorld, a #) )
       -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
       -> State# RealWorld
       -> (# State# RealWorld, a #)
-}

{-
  | StgOpApp    StgOp         -- Primitive op or foreign call
                [Arg' idOcc]  -- Saturated.
                Type          -- result type
                (Maybe tcOcc) -- result type name (required for tagToEnum wrapper generator)


data StgOp
  = StgPrimOp     !Name
  | StgPrimCallOp !PrimCall
  | StgFCallOp    !ForeignCall

data PrimCall = PrimCall !BS8.ByteString !UnitId

data ForeignCall
  = ForeignCall
  { foreignCTarget  :: !CCallTarget
  , foreignCConv    :: !CCallConv
  , foreignCSafety  :: !Safety
  }

data CCallTarget
  = StaticTarget !SourceText !BS8.ByteString !(Maybe UnitId) !Bool
  | DynamicTarget

data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv

-}

matchFirstLit :: Atom -> [Alt] -> M [Atom]
matchFirstLit (Literal lit) alts = case head $ [a | a@Alt{..} <- alts, matchLit lit altCon] ++ (error $ "no lit match" ++ show (lit, map altCon alts)) of
  Alt{..} -> do
    evalExpr altRHS
matchFirstLit a ([Alt AltDefault _ rhs]) = evalExpr rhs
matchFirstLit l alts = error $ "no lit match" ++ show (l, map altCon alts)

matchLit :: Lit -> AltCon -> Bool
matchLit a = \case
  AltDataCon{}  -> False
  AltLit l      -> a == l
  AltDefault    -> True

matchFirstCon :: HeapObject -> [Alt] -> M [Atom]
matchFirstCon (Con dc args) alts = case head [a | a@Alt{..} <- alts, matchCon dc altCon] of
  Alt{..} -> do
    zipWithM_ addEnv altBinders args
    evalExpr altRHS

matchCon :: DataCon -> AltCon -> Bool
matchCon a = \case
  AltDataCon dc -> dataConUniqueName a == dataConUniqueName dc
  AltLit{}      -> False
  AltDefault    -> True

declareBinding :: Binding -> M ()
declareBinding = \case
  StgNonRec i rhs -> do
    a <- freshHeapAddress
    addEnv i (HeapPtr a)
    storeRhs i a rhs
  StgRec l -> do
    ls <- forM l $ \(i, _) -> do
      a <- freshHeapAddress
      addEnv i (HeapPtr a)
      pure a
    forM_ (zip ls l) $ \(a, (i, rhs)) -> do
      storeRhs i a rhs

storeRhs :: Binder -> Addr -> Rhs -> M ()
storeRhs i a = \case
  StgRhsCon dc l -> do
    args <- mapM evalArg l
    store a (Con dc args)

  cl@(StgRhsClosure _ l _) -> do
    env <- gets ssEnv -- TODO: pruning
    store a (Closure (Id i) cl env [] (length l))

-----------------------

declareTopBindings :: [Module] -> M ()
declareTopBindings mods = do
  let (strings, closures) = partition isStringLit $ (concatMap moduleTopBindings) mods
      isStringLit = \case
        StgTopStringLit{} -> True
        _                 -> False
  -- bind string lits
  forM_ strings $ \(StgTopStringLit b str) -> do
    addEnv b (StringPtr 0 (str <> "\0")) -- FIXME: StringPtr or Lit???)

  -- bind closures
  let bindings = concatMap getBindings closures
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) -> [(i, rhs)]
        StgTopLifted (StgRec l) -> l
  addrList <- forM bindings $ \(i, _) -> do
    a <- freshHeapAddress
    addEnv i (HeapPtr a)
    pure a

  forM_ (zip addrList bindings) $ \(a, (i, rhs)) -> do
    storeRhs i a rhs

test :: IO ()
test = do
  mods <- getFullpakModules "minigame.fullpak"
  let run = do
        declareTopBindings mods
        rootMain <- unId . head . Map.keys <$> gets ssEnv
        evalExpr (StgApp rootMain [StgLitArg LitNullAddr] (SingleValue VoidRep) mempty)

  let s@StgState{..} = execState run emptyStgState

  putStrLn $ unlines $ [BS8.unpack $ binderUniqueName b | Id b <- Map.keys ssEnv]
  print ssNextAddr
  print $ head $ Map.toList ssEnv
  -- TODO: handle :Main.main properly ; currenlty it is in conflict with Main.main
