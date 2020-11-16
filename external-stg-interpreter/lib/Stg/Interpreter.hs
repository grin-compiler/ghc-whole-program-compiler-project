{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Stg.Interpreter where

import GHC.Stack
import qualified GHC.Exts as Exts
import Foreign.Ptr

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Control.Exception
import qualified Data.Primitive.ByteArray as BA

import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import System.Posix.DynamicLinker

import Stg.Syntax
import Stg.Program

import Stg.Interpreter.Base
import Stg.Interpreter.FFI
import Stg.Interpreter.Rts
import Stg.Interpreter.Debug

import qualified Stg.Interpreter.PrimOp.Addr          as PrimAddr
import qualified Stg.Interpreter.PrimOp.Array         as PrimArray
import qualified Stg.Interpreter.PrimOp.ByteArray     as PrimByteArray
import qualified Stg.Interpreter.PrimOp.Char          as PrimChar
import qualified Stg.Interpreter.PrimOp.Concurrency   as PrimConcurrency
import qualified Stg.Interpreter.PrimOp.Exceptions    as PrimExceptions
import qualified Stg.Interpreter.PrimOp.Float         as PrimFloat
import qualified Stg.Interpreter.PrimOp.Double        as PrimDouble
import qualified Stg.Interpreter.PrimOp.Word          as PrimWord
import qualified Stg.Interpreter.PrimOp.Word8         as PrimWord8
import qualified Stg.Interpreter.PrimOp.Word16        as PrimWord16
import qualified Stg.Interpreter.PrimOp.Int           as PrimInt
import qualified Stg.Interpreter.PrimOp.Int8          as PrimInt8
import qualified Stg.Interpreter.PrimOp.Int16         as PrimInt16
import qualified Stg.Interpreter.PrimOp.MutVar        as PrimMutVar
import qualified Stg.Interpreter.PrimOp.MVar          as PrimMVar
import qualified Stg.Interpreter.PrimOp.Narrowings    as PrimNarrowings
import qualified Stg.Interpreter.PrimOp.StablePointer as PrimStablePointer
import qualified Stg.Interpreter.PrimOp.WeakPointer   as PrimWeakPointer
import qualified Stg.Interpreter.PrimOp.TagToEnum     as PrimTagToEnum
import qualified Stg.Interpreter.PrimOp.Unsafe        as PrimUnsafe

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

{-
data LitNumType
  = LitNumInt     -- ^ @Int#@ - according to target machine
  | LitNumInt64   -- ^ @Int64#@ - exactly 64 bits
  | LitNumWord    -- ^ @Word#@ - according to target machine
  | LitNumWord64  -- ^ @Word64#@ - exactly 64 bits
  deriving (Eq, Ord, Generic, Show)

data Lit
  = LitChar     !Char
  | LitString   !BS.ByteString
  | LitNumber   !LitNumType !Integer
-}

evalLiteral :: HasCallStack => Lit -> M Atom
evalLiteral = \case
  LitString str -> getCStringConstantPtrAtom str
  LitFloat f    -> pure . FloatAtom $ realToFrac f
  LitDouble d   -> pure . DoubleAtom $ realToFrac d
  LitNullAddr   -> pure $ PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n     -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> pure . WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> pure . WordAtom $ fromIntegral n
  l -> pure $ Literal l

evalArg :: HasCallStack => Arg -> M Atom
evalArg = \case
  StgLitArg l -> evalLiteral l
  StgVarArg b -> lookupEnv b

builtinStgEval :: HasCallStack => Atom -> M [Atom]
builtinStgEval a@HeapPtr{} = do
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
        markExecuted l
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
--builtinStgEval x = pure [x] -- FIXME: this is a debug hack!! remove it!!!!!
builtinStgEval a = stgErrorM $ "expected a thunk, got: " ++ show a

builtinStgApply :: HasCallStack => Atom -> [Atom] -> M [Atom]
builtinStgApply a@HeapPtr{} args = do
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
          [f] -> do
            builtinStgApply f remArgs
          r   -> stgErrorM $ "invalid over-application result: " ++ show r

      -- saturation
      | hoCloMissing - argCount == 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        builtinStgEval (HeapPtr newAp)
builtinStgApply a _ = stgErrorM $ "expected a closure, got: " ++ show a

heapObjectKind :: HeapObject -> String
heapObjectKind = \case
  Con{}       -> "Con"
  Closure{}   -> "Closure"
  Blackhole{} -> "Blackhole"

peekResult :: [Atom] -> M String
peekResult [hp@HeapPtr{}] = do
  o <- readHeap hp
  case o of
    Con dc args -> pure $ "Con: " ++ show (dataConUniqueName dc) ++ " " ++ show args
    Closure{..} -> pure $ "Closure missing: " ++ show hoCloMissing ++ " args: " ++ show hoCloArgs
    Blackhole{} -> pure "Blackhole"
peekResult r = pure $ show r

assertWHNF :: HasCallStack => [Atom] -> AltType -> Binder -> Expr -> M ()
assertWHNF [hp@HeapPtr{}] aty res e  = do
  o <- readHeap hp
  case o of
    Con dc args -> pure ()
    Closure{..}
      | hoCloMissing == 0
      , aty /= MultiValAlt 1
      -> do
          liftIO $ do
            putStrLn "Thunk"
            putStrLn ""
            print aty
            putStrLn ""
            print res
            putStrLn ""
            print e
            putStrLn ""
          error "Thunk"
      | otherwise         -> pure ()
    Blackhole{} -> error "Blackhole"
assertWHNF _ _ _ _ = pure ()


evalExpr :: HasCallStack => Expr -> M [Atom]
evalExpr = \case
  StgTick _ e       -> evalExpr e
  StgLit l          -> pure <$> evalLiteral l
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
      --liftIO $ putStrLn $ "force: " ++ show (Id i) ++ " " ++ show v
      result <- builtinStgEval v
      --pResult <- peekResult result
      --liftIO $ putStrLn $ "force-result: " ++ show (Id i) ++ " = " ++ show result ++ " " ++ pResult
      pure result

    SingleValue UnliftedRep -> error "SingleValue UnliftedRep"

    xxx@(SingleValue _) -> do
      -- HINT: unlifted and literals ; Unlifted must be a HeapPtr that points to a Con
      v <- lookupEnv i
      case v of
        HeapPtr{} -> error $ "xxx: " ++ show xxx
        _ -> pure [v]
      --pure [v]
      --builtinStgEval v

    UnboxedTuple [LiftedRep] -> do
      v <- lookupEnv i
      {-
      case binderDetails i of
        JoinId 0 -> do
      -}
      case (show $ Id i) `elem` [ "base_GHC.Foreign.$j_s4kA"
                                , "gloss-1.13.1.1-9h1aMufeUtm5ZHz3o9mip4_Graphics.Gloss.Internals.Interface.Backend.GLUT.$j_ssmU"
                                ] of
        True -> do
          result <- builtinStgEval v
          --pResult <- peekResult result
          --liftIO $ putStrLn $ "force-result2: " ++ show (Id i) ++ " = " ++ show result ++ " " ++ pResult
          pure result
        _ -> do
          error $ show $ "UnboxedTuple [LiftedRep]: " ++ show (Id i) ++ " " ++ show i
      --- base_GHC.Foreign.$j_s4kA
      --pure [v]
{-
    UnboxedTuple []
      | binderId i == BinderId (Unique '0' 124) -- wired in coercion token
      -> do
        --v <- lookupEnv i
        --result <- builtinStgEval v
        --pResult <- peekResult result
        --liftIO $ putStrLn $ "force-result2: " ++ show (Id i) ++ " = " ++ show result ++ " " ++ pResult
        --pure result
        pure []
-}
    r -> stgErrorM $ "unsupported var rep: " ++ show r ++ " " ++ show i -- unboxed: is it possible??

  -- fun app
  --  Q: should app always be lifted/unlifted?
  --  Q: what does unlifted app mean? (i.e. no Ap node, but saturated calls to known functions only?)
  StgApp i l _t _ -> case binderType i of
    SingleValue _ -> do
      args <- mapM evalArg l
      v <- lookupEnv i
      --liftIO $ putStrLn $ "call: " ++ show (Id i) ++ " " ++ show args 
      result <- builtinStgApply v args
      --pResult <- peekResult result
      --liftIO $ putStrLn $ "call-result: " ++ show (Id i) ++ " " ++ show args ++ " = " ++ show result ++ " " ++ pResult
      pure result

    r -> stgErrorM $ "unsupported app rep: " ++ show r -- unboxed: invalid

  StgCase e resultId aty alts -> do
    result <- evalExpr e
    assertWHNF result aty resultId e
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

  StgOpApp (StgPrimOp op) l t tc -> do
    args <- mapM evalArg l
    evalStack <- gets ssEvalStack
    --liftIO $ putStrLn $ show (head evalStack) ++ " " ++ show op ++ " " ++ show args ++ " = ..."
    result <- evalPrimOp op args t tc
    --liftIO $ putStrLn $ show (head evalStack) ++ " " ++ show op ++ " " ++ show args ++ " = " ++ show result
    markPrimOp op
    pure result

  StgOpApp (StgFCallOp foreignCall) l t tc -> do
    args <- mapM evalArg l
    result <- evalFCallOp builtinStgApply foreignCall args t tc
    evalStack <- gets ssEvalStack
    --liftIO $ putStrLn $ show (head evalStack) ++ " " ++ show foreignCall ++ " " ++ show args ++ " = " ++ show result
    markFFI foreignCall
    pure result

  StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t


matchFirstLit :: HasCallStack => Atom -> [Alt] -> M [Atom]
matchFirstLit a ([Alt AltDefault _ rhs]) = evalExpr rhs
matchFirstLit atom alts = case head $ [a | a@Alt{..} <- alts, matchLit atom altCon] ++ (error $ "no lit match" ++ show (atom, map altCon alts)) of
  Alt{..} -> do
    evalExpr altRHS
matchFirstLit l alts = error $ "no lit match" ++ show (l, map altCon alts)

matchLit :: HasCallStack => Atom -> AltCon -> Bool
matchLit a = \case
  AltDataCon{}  -> False
  AltLit l      -> a == convertAltLit l
  AltDefault    -> True

convertAltLit :: Lit -> Atom
convertAltLit = \case
  LitFloat f                -> FloatAtom $ realToFrac f
  LitDouble d               -> DoubleAtom $ realToFrac d
  LitNullAddr               -> PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n     -> IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> WordAtom $ fromIntegral n
  l -> Literal l

matchFirstCon :: HasCallStack => HeapObject -> [Alt] -> M [Atom]
matchFirstCon (Con dc args) alts = case head $ [a | a@Alt{..} <- alts, matchCon dc altCon] ++ error "no matching alts" of
  Alt{..} -> do
    zipWithM_ addEnv altBinders args
    evalExpr altRHS

matchCon :: HasCallStack => DataCon -> AltCon -> Bool
matchCon a = \case
  AltDataCon dc -> dataConUniqueName a == dataConUniqueName dc
  AltLit{}      -> False
  AltDefault    -> True

declareBinding :: HasCallStack => Binding -> M ()
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

storeRhs :: HasCallStack => Binder -> Addr -> Rhs -> M ()
storeRhs i a = \case
  StgRhsCon dc l -> do
    args <- mapM evalArg l
    store a (Con dc args)

  cl@(StgRhsClosure _ l _) -> do
    env <- gets ssEnv -- TODO: pruning
    store a (Closure (Id i) cl env [] (length l))

-----------------------

declareTopBindings :: HasCallStack => [Module] -> M ()
declareTopBindings mods = do
  let (strings, closures) = partition isStringLit $ (concatMap moduleTopBindings) mods
      isStringLit = \case
        StgTopStringLit{} -> True
        _                 -> False
  -- bind string lits
  forM_ strings $ \(StgTopStringLit b str) -> do
    strPtr <- getCStringConstantPtrAtom str
    addEnv b strPtr

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


test :: HasCallStack => IO ()
test = do
  mods <- getFullpakModules "minigame.fullpak"
  --mods <- getFullpakModules "tsumupto.fullpak"
  --mods <- getFullpakModules "words.fullpak"
  let run = do
        declareTopBindings mods
        initRtsSupport mods
        env <- gets ssEnv
        liftIO $ putStrLn $ "top Id count: " ++ show (Map.size env)
        let rootMain = unId . head $ [i | i <- Map.keys env, show i == "main_:Main.main"]
        limit <- gets ssNextAddr
        modify' $ \s@StgState{..} -> s {ssAddressAfterInit = limit}
        evalExpr (StgApp rootMain [StgLitArg LitNullAddr] (SingleValue VoidRep) mempty)
        showDebug builtinStgApply

  stateStore <- newEmptyMVar
  dl <- dlopen "./libHSbase-4.14.0.0.cbits.so" [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]
  flip catch (\e -> do {dlclose dl; throw (e :: SomeException)}) $ do
    s@StgState{..} <- execStateT run (emptyStgState (PrintableMVar stateStore) dl)
    dlclose dl

    --putStrLn $ unlines $ [BS8.unpack $ binderUniqueName b | Id b <- Map.keys ssEnv]
    --print ssNextAddr
    --print $ head $ Map.toList ssEnv
    -- TODO: handle :Main.main properly ; currenlty it is in conflict with Main.main

---------------------- primops

evalPrimOp :: HasCallStack => Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp =
  PrimAddr.evalPrimOp $
  PrimArray.evalPrimOp $
  PrimByteArray.evalPrimOp $
  PrimChar.evalPrimOp $
  PrimConcurrency.evalPrimOp $
  PrimExceptions.evalPrimOp builtinStgApply $
  PrimFloat.evalPrimOp $
  PrimDouble.evalPrimOp $
  PrimInt16.evalPrimOp $
  PrimInt8.evalPrimOp $
  PrimInt.evalPrimOp $
  PrimMutVar.evalPrimOp builtinStgApply $
  PrimMVar.evalPrimOp $
  PrimNarrowings.evalPrimOp $
  PrimStablePointer.evalPrimOp $
  PrimWeakPointer.evalPrimOp $
  PrimWord16.evalPrimOp $
  PrimWord8.evalPrimOp $
  PrimWord.evalPrimOp $
  PrimTagToEnum.evalPrimOp builtinStgEval $
  PrimUnsafe.evalPrimOp $
  unsupported where
    unsupported op args _t _tc = stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args
