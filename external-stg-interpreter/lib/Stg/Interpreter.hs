{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter where

import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Stg.Syntax

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

type StgRhsClosure = Rhs  -- NOTE: must be StgRhsClosure only!

data HeapObject
  = Con
    { hoCon         :: DataCon
    , hoConArgs     :: [Atom]
    }
  | Closure
    { hoCloBody     :: StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [Atom]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }
  | Blackhole HeapObject
  deriving (Show, Eq, Ord)

data StackContinuation
  = CaseOf  Binder AltType [Alt]  -- pattern match on the result
  | Update  Addr                  -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply   [Atom]                -- apply args on the result heap object
  deriving (Show, Eq, Ord)

data Atom     -- Q: should atom fit into a cpu register?
  = HeapPtr   Addr
  | Literal   Lit             -- Q: shopuld we allow string literals, or should string lits be modeled as StringPtr?
  | StringPtr Int ByteString  -- HINT: StgTopStringLit ; maybe include its origin? ; the PrimRep is AddrRep
  | RtsPrim   -- ??? or is this a heap object?
  deriving (Show, Eq, Ord)

type ReturnValue = [Atom]

type Addr   = Int
type Heap   = IntMap HeapObject
type Env    = Map Binder Atom   -- NOTE: must contain only the defined local variables
type Stack  = [(Env, StackContinuation)]

{-
  Q: do we want homogeneous or heterogeneous Heap ; e.g. single intmap with mixed things or multiple intmaps/vector with multiple address spaces
-}

data StgState
  = StgState
  { ssHeap      :: !Heap
  , ssEnv       :: !Env
--  , ssStack     :: [(Env, StackContinuation)] -- TODO: use reified stack instead of the host language stack
  , ssNextAddr  :: !Int
  }
  deriving (Show, Eq, Ord)

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
addEnv b a =   modify' $ \s -> s {ssEnv = Map.insert b a (ssEnv s)}

lookupEnv :: Binder -> M Atom
lookupEnv b = do
  env <- gets ssEnv
  case Map.lookup b env of
    Nothing -> error $ "unknown variable: " ++ show b
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
    Blackhole t -> error $ "blackhole ; loop in evaluation of : " ++ show t
    Closure{..}
      | hoCloMissing /= 0
      -> pure [a]

      | otherwise
      -> do
        let StgRhsClosure uf params e = hoCloBody
            HeapPtr l = a
        zipWithM_ addEnv params hoCloArgs
        -- TODO: env or free var handling
        case uf of
          ReEntrant -> do
            -- closure may be entered multiple times, but should not be updated or blackholed.
            withEnv hoEnv (evalExpr e)
          Updatable -> do
            -- closure should be updated after evaluation (and may be blackholed during evaluation).
            store l (Blackhole o)
            result <- withEnv hoEnv (evalExpr e)
            update a result
          SingleEntry -> do
            -- closure will only be entered once, and so need not be updated but may safely be blackholed.
            store l (Blackhole o)
            withEnv hoEnv (evalExpr e)

withEnv :: Env -> M a -> M a
withEnv env m = do
  save <- gets ssEnv
  modify' $ \s -> s {ssEnv = env}
  res <- m
  modify' $ \s -> s {ssEnv = save}
  pure res

builtinStgApply :: Atom -> [Atom] -> M [Atom]
builtinStgApply a args = do
  let argCount      = length args
      HeapPtr addr  = a
  o <- readHeap a
  case o of
    Con{}       -> error $ "unexpexted con at apply: " ++ show o
    Blackhole t -> error $ "blackhole ; loop in application of : " ++ show t
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
          r   -> error $ "invalid over-application result: " ++ show r

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
    Nothing -> error $ "unknown heap address: " ++ show l
    Just o  -> pure o

readHeapCon :: Atom -> M HeapObject
readHeapCon a = readHeap a >>= \o -> case o of
    Con{} -> pure o
    _     -> error $ "expected con but got: " ++ show o

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

    r -> error $ "unsupported var rep: " ++ show r -- unboxed: is it possible??

  -- fun app
  --  Q: should app always be lifted/unlifted?
  --  Q: what does unlifted app mean? (i.e. no Ap node, but saturated calls to known functions only?)
  StgApp i l _t _ -> case binderType i of
    SingleValue _ -> do
      args <- mapM evalArg l
      v <- lookupEnv i
      builtinStgApply v args

    r -> error $ "unsupported app rep: " ++ show r -- unboxed: invalid

  StgCase e resultId aty alts -> do
    result <- evalExpr e
    case aty of
      AlgAlt tc -> do
        let [v] = result
        addEnv resultId v -- HINT: bind the result
        con <- readHeapCon v
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstCon con $ alts ++ [d]
          _ -> matchFirstCon con alts

      PrimAlt r -> do
        let [lit] = result
        addEnv resultId lit -- HINT: bind the result
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstLit lit $ alts ++ [d]
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

  StgOpApp op _args _t _tc -> error $ "unsupported StgOp: " ++ show op
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
matchFirstLit (Literal lit) alts = case head [a | a@Alt{..} <- alts, matchLit lit altCon] of
  Alt{..} -> do
    evalExpr altRHS

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
  AltDataCon dc -> a == dc
  AltLit{}      -> False
  AltDefault    -> True

declareBinding :: Binding -> M ()
declareBinding = \case
  StgNonRec i rhs -> do
    a <- freshHeapAddress
    addEnv i (HeapPtr a)
    storeRhs a rhs
  StgRec l -> do
    ls <- forM l $ \(i, _) -> do
      a <- freshHeapAddress
      addEnv i (HeapPtr a)
      pure a
    forM_ (zip ls l) $ \(a, (_i, rhs)) -> do
      storeRhs a rhs

storeRhs :: Addr -> Rhs -> M ()
storeRhs a = \case
  StgRhsCon dc l -> do
    args <- mapM evalArg l
    store a (Con dc args)

  cl@(StgRhsClosure _ l _) -> do
    env <- gets ssEnv -- TODO: pruning
    store a (Closure cl env [] (length l))
