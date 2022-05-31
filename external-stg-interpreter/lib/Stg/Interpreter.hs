{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Stg.Interpreter where

import GHC.Stack
import qualified GHC.Exts as Exts
import Foreign.Ptr

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Control.Monad.State.Strict
import Control.Exception
import qualified Data.Primitive.ByteArray as BA

import Data.Maybe
import Data.List (partition, isSuffixOf)
import Data.Set (Set)
import Data.Map (Map)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as StrictMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import System.Posix.DynamicLinker

import System.FilePath
import System.IO
import System.Directory

import Stg.Syntax
import Stg.Program
import Stg.JSON
import Stg.Analysis.LiveVariable

import Stg.Interpreter.Base
import Stg.Interpreter.PrimCall
import Stg.Interpreter.FFI
import Stg.Interpreter.Rts
import Stg.Interpreter.Debug
import qualified Stg.Interpreter.ThreadScheduler as Scheduler
import qualified Stg.Interpreter.Debugger        as Debugger
import qualified Stg.Interpreter.Debugger.Region as Debugger
import qualified Stg.Interpreter.GC as GC

import qualified Stg.Interpreter.PrimOp.Addr          as PrimAddr
import qualified Stg.Interpreter.PrimOp.Array         as PrimArray
import qualified Stg.Interpreter.PrimOp.SmallArray    as PrimSmallArray
import qualified Stg.Interpreter.PrimOp.ArrayArray    as PrimArrayArray
import qualified Stg.Interpreter.PrimOp.ByteArray     as PrimByteArray
import qualified Stg.Interpreter.PrimOp.Char          as PrimChar
import qualified Stg.Interpreter.PrimOp.Concurrency   as PrimConcurrency
import qualified Stg.Interpreter.PrimOp.DelayWait     as PrimDelayWait
import qualified Stg.Interpreter.PrimOp.Parallelism   as PrimParallelism
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
import qualified Stg.Interpreter.PrimOp.Prefetch      as PrimPrefetch
import qualified Stg.Interpreter.PrimOp.StablePointer as PrimStablePointer
import qualified Stg.Interpreter.PrimOp.WeakPointer   as PrimWeakPointer
import qualified Stg.Interpreter.PrimOp.TagToEnum     as PrimTagToEnum
import qualified Stg.Interpreter.PrimOp.Unsafe        as PrimUnsafe
import qualified Stg.Interpreter.PrimOp.MiscEtc       as PrimMiscEtc
import qualified Stg.Interpreter.PrimOp.Idris         as PrimIdris

import Text.PrettyPrint.ANSI.Leijen (plain, pretty)

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
  LitLabel name spec  -> getFFILabelPtrAtom name spec
  LitString str       -> getCStringConstantPtrAtom str
  LitFloat f    -> pure . FloatAtom $ realToFrac f
  LitDouble d   -> pure . DoubleAtom $ realToFrac d
  LitNullAddr   -> pure $ PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n     -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> pure . WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> pure . WordAtom $ fromIntegral n
  l -> pure $ Literal l

evalArg :: HasCallStack => Env -> Arg -> M Atom
evalArg localEnv = \case
  StgLitArg l -> evalLiteral l
  StgVarArg b -> lookupEnv localEnv b

{-
  TODO: support these
  | ApStack                   -- HINT: needed for the async exceptions
    { hoResult      :: [Atom]
    , hoStack       :: [StackContinuation]
    }
-}

stackPushRestoreProgramPoint :: Int -> M ()
stackPushRestoreProgramPoint argCount = do
  progPoint <- gets ssCurrentProgramPoint
  currentClosure <- gets ssCurrentClosure
  stackPush $ DebugFrame (RestoreProgramPoint currentClosure $ PP_Apply argCount progPoint)

buildCallGraph :: StaticOrigin -> Id -> M ()
buildCallGraph so hoName = do
  progPoint <- gets ssCurrentProgramPoint
  addInterClosureCallGraphEdge so progPoint $ PP_Closure hoName
  setProgramPoint $ PP_Closure hoName
  -- connect call sites to parent closure
  currentClosure <- gets ssCurrentClosure
  case progPoint of
    PP_Global -> pure ()
    _ -> case currentClosure of
      Just cloId  -> addIntraClosureCallGraphEdge (PP_Closure cloId) so progPoint
      _           -> pure ()
  -- write whole program path entry
  gets ssTracingState >>= \case
    NoTracing     -> pure ()
    DoTracing{..} -> liftIO $ hPutStrLn thWholeProgramPath $ maybe "<global>" show currentClosure ++ "\t" ++ show progPoint ++ "\t" ++ show hoName

builtinStgEval :: HasCallStack => StaticOrigin -> Atom -> M [Atom]
builtinStgEval so a@HeapPtr{} = do
  o <- readHeap a
  case o of
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}       -> pure [a]
    BlackHole t -> stgErrorM $ "blackhole ; loop in evaluation of : " ++ show t
    Closure{..}
      | hoCloMissing /= 0
      -> pure [a]

      | otherwise
      -> do

        let StgRhsClosure _ uf params e = hoCloBody
            HeapPtr l = a
            extendedEnv = addManyBindersToEnv SO_CloArg params hoCloArgs hoEnv

        modify' $ \s@StgState{..} -> s {ssClosureCallCounter = succ ssClosureCallCounter}
        markExecuted l
        markExecutedId hoName

        -- build call graph
        buildCallGraph so hoName

        modify' $ \s -> s {ssCurrentClosure = Just hoName, ssCurrentClosureEnv = extendedEnv, ssCurrentClosureAddr = l}
        -- check breakpoints and region entering
        let closureName = binderUniqueName $ unId hoName
        markClosure closureName -- HINT: this list can be deleted by a debugger command, so this is not the same as `markExecutedId`
        Debugger.checkBreakpoint extendedEnv closureName
        Debugger.checkRegion closureName
        GC.checkGC [a] -- HINT: add local env as GC root

        -- TODO: env or free var handling
        case uf of
          ReEntrant -> do
            -- closure may be entered multiple times, but should not be updated or blackholed.
            evalExpr extendedEnv e
          Updatable -> do
            -- closure should be updated after evaluation (and may be blackholed during evaluation).
            stackPush (Update l)
            store l (BlackHole o)
            evalExpr extendedEnv e
          SingleEntry -> do
            -- closure will only be entered once, and so need not be updated but may safely be blackholed.
            store l (BlackHole o)
            evalExpr extendedEnv e
    _ -> stgErrorM $ "expected heap object: " ++ show o
builtinStgEval so a = stgErrorM $ "expected a thunk, got: " ++ show a ++ ", static-origin: " ++ show so

builtinStgApply :: HasCallStack => StaticOrigin -> Atom -> [Atom] -> M [Atom]
builtinStgApply so a [] = builtinStgEval so a
builtinStgApply so a@HeapPtr{} args = do
  let argCount      = length args
      HeapPtr addr  = a
  o <- readHeap a
  case o of
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}             -> stgErrorM $ "unexpexted con at apply: "-- ++ show o
    BlackHole t       -> stgErrorM $ "blackhole ; loop in application of : " ++ show t
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
        stackPush (Apply remArgs)
        stackPushRestoreProgramPoint $ length remArgs -- HINT: for call-graph builder ; use the current closure as call origin
        builtinStgApply so a satArgs

      -- saturation
      | hoCloMissing - argCount == 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ args, hoCloMissing = hoCloMissing - argCount})
        builtinStgEval so (HeapPtr newAp)

builtinStgApply so a args = stgErrorM $ "builtinStgApply - expected a closure (ptr), got: " ++
  show a ++ ", args: " ++ show args ++ ", static-origin: " ++ show so

{-
heapObjectKind :: HeapObject -> String
heapObjectKind = \case
  Con{}             -> "Con"
  Closure{}         -> "Closure"
  BlackHole{}       -> "BlackHole"
  RaiseException{}  -> "RaiseException"
  ApStack{}         -> "ApStack"

peekResult :: [Atom] -> M String
peekResult [hp@HeapPtr{}] = do
  o <- readHeap hp
  case o of
    Con dc args -> pure $ "Con: " ++ show (dcUniqueName dc) ++ " " ++ show args
    Closure{..} -> pure $ "Closure missing: " ++ show hoCloMissing ++ " args: " ++ show hoCloArgs
    BlackHole{} -> pure "BlackHole"
peekResult r = pure $ show r
-}
assertWHNF :: HasCallStack => [Atom] -> AltType -> Binder -> M ()
assertWHNF [hp@HeapPtr{}] aty res = do
  o <- readHeap hp
  case o of
    Con _ dc args -> pure ()
    Closure{..}
      | hoCloMissing == 0
      , aty /= MultiValAlt 1
      -> do
          liftIO $ do
            putStrLn "Thunk"
            putStrLn ""
            putStrLn $ describeHeapObject o
            putStrLn ""
            print aty
            putStrLn ""
            print res
            putStrLn ""
          stgErrorM "Thunk"
      | otherwise         -> pure ()
    BlackHole{} -> stgErrorM "BlackHole"
assertWHNF _ _ _ = pure ()

assertWHNFAtom :: HasCallStack => String -> Atom -> M ()
assertWHNFAtom opkind hp@HeapPtr{} = do
  o <- readHeap hp
  case o of
    Con _ dc args
      -> pure ()
    Closure{..}
      | hoCloMissing == 0
          -> stgErrorM $ "assertWHNFAtom: Thunk " ++ opkind
      | otherwise
          -> pure ()
    BlackHole{}
      -> stgErrorM $ "assertWHNFAtom: BlackHole " ++ opkind
assertWHNFAtom _ _ = pure ()


{-
  machine state:
    stack
    local env (local bindings only = non-top-level)
    global env (top-level bindings only) (read-only)
    heap

  NOTES:
    closure execution always clears the local env (Apply stack constructor)
    case continuation restores the saved local env (Case stack constructor)
    the update stack constructor does not touch the local environment

  IDEA:
    model the local environment as a parameter of evalExpr
-}

{-
evalOnMainThread  :: M [Atom] -> M [Atom] -- setup_action_with_initial_result -> result
evalOnNewThread   :: M [Atom] -> M [Atom] -- setup_action_with_initial_result -> result

data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished  -- RTS name: ThreadComplete
  | ThreadDied      -- RTS name: ThreadKilled
-}

killAllThreads :: M ()
killAllThreads = do
  pure () -- TODO

evalOnMainThread :: M [Atom] -> M [Atom]
evalOnMainThread = evalOnThread True

evalOnNewThread :: M [Atom] -> M [Atom]
evalOnNewThread = evalOnThread False

evalOnThread :: Bool -> M [Atom] -> M [Atom]
evalOnThread isMainThread setupAction = do
  -- create main thread
  (tid, ts) <- createThread
  scheduleToTheEnd tid
  switchToThread tid

  stackPush $ RunScheduler SR_ThreadFinished
  result0 <- setupAction
  let loop resultIn = do
        resultOut <- evalStackMachine resultIn
        ThreadState{..} <- getThreadState tid
        case isThreadLive tsStatus of
          True  -> loop resultOut -- HINT: the new scheduling is ready
          False -> do
            when isMainThread killAllThreads
            pure tsCurrentResult
  loop result0

evalStackMachine :: [Atom] -> M [Atom]
evalStackMachine result = do
  tid <- gets ssCurrentThreadId
  stackPop >>= \case
    Nothing         -> pure result
    Just stackCont  -> evalStackContinuation result stackCont >>= evalStackMachine

evalStackContinuation :: [Atom] -> StackContinuation -> M [Atom]
evalStackContinuation result = \case
  Apply args
    | [fun@HeapPtr{}] <- result
    -> builtinStgApply SO_ClosureResult fun args

  Update dstAddr
    | [src@HeapPtr{}] <- result
    -> do
      o <- readHeap src
      store dstAddr o
      pure result

  -- HINT: STG IR uses 'case' expressions to chain instructions with strict evaluation
  CaseOf curClosureAddr curClosure localEnv resultBinder altType alts -> do
    modify' $ \s -> s
      { ssCurrentClosure     = Just curClosure
      , ssCurrentClosureAddr = curClosureAddr
      , ssCurrentClosureEnv  = localEnv
      }
    assertWHNF result altType resultBinder
    let resultId = (Id resultBinder)
    case altType of
      AlgAlt tc -> do
        let v = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder v localEnv
        con <- readHeapCon v
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstCon resultId extendedEnv con $ al ++ [d]
          _ -> matchFirstCon resultId extendedEnv con alts

      PrimAlt _r -> do
        let lit = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder lit localEnv
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstLit resultId extendedEnv lit $ al ++ [d]
          _ -> matchFirstLit resultId extendedEnv lit alts

      MultiValAlt _n -> do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let [Alt{..}] = alts
            extendedEnv = addManyBindersToEnv SO_Scrut altBinders result localEnv

        setProgramPoint $ PP_Alt resultId altCon
        evalExpr extendedEnv altRHS

      PolyAlt -> do
        let [Alt{..}]   = alts
            [v]         = result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder v $                 -- HINT: bind the result
                          addManyBindersToEnv SO_AltArg altBinders result localEnv  -- HINT: bind alt params

        setProgramPoint $ PP_Alt resultId altCon
        evalExpr extendedEnv altRHS

  RestoreExMask b i -> do
    -- TODO
    pure result

  Catch h b i -> do
    -- TODO: is anything to do??
    pure result

  RunScheduler sr -> Scheduler.runScheduler result sr

  -- HINT: dataToTag# has an eval call in the middle, that's why we need this continuation, it is the post-returning part of the op implementation
  DataToTagOp -> PrimTagToEnum.dataToTagOp result

  DebugFrame df -> evalDebugFrame result df

  x -> error $ "unsupported continuation: " ++ show x ++ ", result: " ++ show result

debugExpr :: Env -> Expr -> M ()
debugExpr env expr = pure () -- lift $ print (env, expr)

evalDebugFrame :: [Atom] -> DebugFrame -> M [Atom]
evalDebugFrame result = \case
  RestoreProgramPoint currentClosure progPoint -> do
    modify' $ \s -> s {ssCurrentClosure = currentClosure}
    setProgramPoint progPoint
    pure result

  x -> error $ "unsupported debug-frame: " ++ show x ++ ", result: " ++ show result

evalExpr :: HasCallStack => Env -> Expr -> M [Atom]
evalExpr localEnv expr = debugExpr localEnv expr >> case expr of
  StgTick _ e       -> evalExpr localEnv e
  StgLit l          -> pure <$> evalLiteral l
  StgConApp dc l _
    -- HINT: make and return unboxed tuple
    | UnboxedTupleCon{} <- dcRep dc
    -> mapM (evalArg localEnv) l   -- Q: is this only for unboxed tuple? could datacon be heap allocated?

    -- HINT: create boxed datacon on the heap
    | otherwise
    -> do
      args <- mapM (evalArg localEnv) l
      loc <- allocAndStore (Con False dc args)
      pure [HeapPtr loc]

  StgLet b e -> do
    extendedEnv <- declareBinding False localEnv b
    evalExpr extendedEnv e

  StgLetNoEscape b e -> do -- TODO: do not allocate closure on heap, instead put into env (stack) allocated closure ; model stack allocated heap objects
    extendedEnv <- declareBinding True localEnv b
    evalExpr extendedEnv e

  -- var (join id)
  StgApp i [] _t _
    | JoinId 0 <- binderDetails i
    -> do
      -- HINT: join id-s are always closures, needs eval
      -- NOTE: join id's type tells the closure return value representation
      (so, v) <- lookupEnvSO localEnv i
      builtinStgEval so v

    | JoinId x <- binderDetails i
    -> stgErrorM $ "join-id var arity error, expected 0, got: " ++ show x ++ " id: " ++ show i

  -- var (non join id)
  StgApp i [] _t _ -> case binderType i of

    SingleValue LiftedRep -> do
      -- HINT: must be HeapPtr ; read heap ; check if Con or Closure ; eval if Closure ; return HeapPtr if Con
      (so, v) <- lookupEnvSO localEnv i
      builtinStgEval so v

    SingleValue _ -> do
      v <- lookupEnv localEnv i
      pure [v]

    UnboxedTuple []
      | binderId i == BinderId (Unique '0' 124) -- wired in coercion token
      -> do
        pure []

    r -> stgErrorM $ "unsupported var rep: " ++ show r ++ " " ++ show i -- unboxed: is it possible??

  -- fun app
  --  Q: should app always be lifted/unlifted?
  --  Q: what does unlifted app mean? (i.e. no Ap node, but saturated calls to known functions only?)
  --  A: the join id type is for the return value representation and not for the id representation, so it can be unlifted.
  StgApp i l _t _
    | JoinId _ <- binderDetails i
    -> do
      args <- mapM (evalArg localEnv) l
      (so, v) <- lookupEnvSO localEnv i
      builtinStgApply so v args

  {- non-join id -}
  StgApp i l _t _ -> case binderType i of
    SingleValue LiftedRep -> do
      args <- mapM (evalArg localEnv) l
      (so, v) <- lookupEnvSO localEnv i
      builtinStgApply so v args

    r -> stgErrorM $ "unsupported app rep: " ++ show r -- unboxed: invalid

  StgCase e scrutineeResult altType alts -> do
    let breakpointName = binderUniqueName scrutineeResult
    Debugger.checkBreakpoint localEnv breakpointName
    Debugger.checkRegion breakpointName
    Just curClosure <- gets ssCurrentClosure
    curClosureAddr <- gets ssCurrentClosureAddr
    stackPush (CaseOf curClosureAddr curClosure localEnv scrutineeResult altType alts)
    setProgramPoint . PP_Scrutinee $ Id scrutineeResult
    evalExpr localEnv e

  StgOpApp (StgPrimOp op) l t tc -> do
    Debugger.checkBreakpoint localEnv op
    Debugger.checkRegion op
    markPrimOp op
    args <- mapM (evalArg localEnv) l
    tid <- gets ssCurrentThreadId
    evalPrimOp op args t tc

  StgOpApp (StgFCallOp foreignCall) l t tc -> do
    -- check foreign target region and breakpoint
    case foreignCTarget foreignCall of
      StaticTarget _ targetName _ _ -> do
        Debugger.checkBreakpoint localEnv targetName
        Debugger.checkRegion targetName
      _ -> pure ()

    markFFI foreignCall
    args <- mapM (evalArg localEnv) l
    evalFCallOp evalOnNewThread foreignCall args t tc
{-
  StgOpApp (StgPrimCallOp (PrimCall "stg_getThreadAllocationCounterzh" _)) _args t _tc -> do
    i <- gets ssNextHeapAddr
    pure [IntAtom (-i)]
-}
  StgOpApp (StgPrimCallOp primCall) l t tc -> do
    markPrimCall primCall
    args <- mapM (evalArg localEnv) l
    evalPrimCallOp primCall args t tc

  StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t


matchFirstLit :: HasCallStack => Id -> Env -> Atom -> [Alt] -> M [Atom]
matchFirstLit resultId localEnv a [Alt AltDefault _ rhs] = do
  setProgramPoint $ PP_Alt resultId AltDefault
  evalExpr localEnv rhs
matchFirstLit resultId localEnv atom alts = case head $ [a | a@Alt{..} <- alts, matchLit atom altCon] ++ (error $ "no lit match" ++ show (atom, map altCon alts)) of
  Alt{..} -> do
    setProgramPoint $ PP_Alt resultId altCon
    evalExpr localEnv altRHS
matchFirstLit resultId localEnv l alts = error $ "no lit match" ++ show (resultId, localEnv, l, map altCon alts)

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

matchFirstCon :: HasCallStack => Id -> Env -> HeapObject -> [Alt] -> M [Atom]
matchFirstCon resultId localEnv (Con _ dc args) alts = case [a | a@Alt{..} <- alts, matchCon dc altCon] of
  []  -> stgErrorM $ "no matching alts for: " ++ show (resultId, DC dc, "ALTS:\n", map altCon alts)
  Alt{..} : _ -> do
    let extendedEnv = addManyBindersToEnv SO_AltArg altBinders args localEnv
    setProgramPoint $ PP_Alt resultId altCon
    evalExpr extendedEnv altRHS

matchCon :: HasCallStack => DataCon -> AltCon -> Bool
matchCon a = \case
  AltDataCon dc -> dcUNameHash a == dcUNameHash dc && dcUniqueName a == dcUniqueName dc
  AltLit{}      -> False
  AltDefault    -> True

declareBinding :: HasCallStack => Bool -> Env -> Binding -> M Env
declareBinding isLetNoEscape localEnv = \case
  StgNonRec b rhs -> do
    addr <- freshHeapAddress
    storeRhs isLetNoEscape localEnv b addr rhs
    when isLetNoEscape $ do
      markLNE [addr]
    pure $ addBinderToEnv SO_Let b (HeapPtr addr) localEnv

  StgRec l -> do
    (ls, newEnvItems) <- fmap unzip . forM l $ \(b, _) -> do
      addr <- freshHeapAddress
      pure (addr, (b, (HeapPtr addr)))
    let extendedEnv = addZippedBindersToEnv SO_Let newEnvItems localEnv
    forM_ (zip ls l) $ \(addr, (b, rhs)) -> do
      storeRhs isLetNoEscape extendedEnv b addr rhs
    when isLetNoEscape $ do
      markLNE ls
    pure extendedEnv

storeRhs :: HasCallStack => Bool -> Env -> Binder -> Addr -> Rhs -> M ()
storeRhs isLetNoEscape localEnv i addr = \case
  StgRhsCon dc l -> do
    args <- mapM (evalArg localEnv) l
    store addr (Con isLetNoEscape dc args)

  cl@(StgRhsClosure freeVars _ paramNames _) -> do
    let liveSet   = Set.fromList $ map Id freeVars
        prunedEnv = Map.restrictKeys localEnv liveSet -- HINT: do pruning to keep only the live/later referred variables
    store addr (Closure isLetNoEscape (Id i) cl prunedEnv [] (length paramNames))

-----------------------

declareTopBindings :: HasCallStack => [Module] -> M ()
declareTopBindings mods = do
  let (strings, closures) = partition isStringLit $ (concatMap moduleTopBindings) mods
      isStringLit = \case
        StgTopStringLit{} -> True
        _                 -> False
  -- bind string lits
  stringEnv <- forM strings $ \(StgTopStringLit b str) -> do
    strPtr <- getCStringConstantPtrAtom str
    pure (Id b, (SO_TopLevel, strPtr))

  -- bind closures
  let bindings = concatMap getBindings closures
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) -> [(i, rhs)]
        StgTopLifted (StgRec l) -> l

  (closureEnv, rhsList) <- fmap unzip . forM bindings $ \(b, rhs) -> do
    addr <- freshHeapAddress
    pure ((Id b, (SO_TopLevel, HeapPtr addr)), (b, addr, rhs))

  -- set the top level binder env
  modify' $ \s@StgState{..} -> s {ssStaticGlobalEnv = Map.fromList $ stringEnv ++ closureEnv}

  -- HINT: top level closures does not capture local variables
  forM_ rhsList $ \(b, addr, rhs) -> storeRhs False mempty b addr rhs

data Context = Context {
    baseFullPaks :: [FilePath]
  , libBasePath  :: FilePath
  }

loadAndRunProgram :: HasCallStack => Context -> Bool -> String -> [String] -> DebuggerChan -> DebugState -> Bool -> Bool -> IO ()
loadAndRunProgram ctx switchCWD fullpak_name progArgs dbgChan dbgState tracing showstg = do

  -- baseMods <- getFullpakModules "./data/ghc-rts-base.fullpak"
  -- idrisHaskellInterfaceMods <- getFullpakModules "./data/idris-haskell-interface.fullpak"
  mods0 <- traverse getFullpakModules (baseFullPaks ctx)

  mods1 <- case takeExtension fullpak_name of
    ".fullpak" -> do
        mods <- getFullpakModules fullpak_name
        when showstg $ do
          putStrLn "Printing fullpak modules."
          print $ plain $ pretty mods
        pure mods
    ".json" -> do
      -- TODO: Add CLI arg to refer fullpaks to be loaded.
      mods <- getJSONModules fullpak_name
      when showstg $ do
        putStrLn "Printing JSON defined modules."
        print $ plain $ pretty mods
      pure mods -- $ concat [baseMods, idrisHaskellInterfaceMods, jsonMods]
    ext | isSuffixOf "_ghc_stgapp" ext  -> getGhcStgAppModules fullpak_name
    _                                   -> error "unknown input file format"
  let mods = concat $ mods0 ++ [mods1]
  runProgram ctx switchCWD fullpak_name mods progArgs dbgChan dbgState tracing

runProgram
  :: HasCallStack
  => Context -> Bool -> String -> [Module] -> [String] -> DebuggerChan -> DebugState -> Bool
  -> IO ()
runProgram ctx switchCWD progFilePath mods0 progArgs dbgChan dbgState tracing = do
  let mods      = map annotateWithLiveVariables $ extStgRtsSupportModule : mods0 -- NOTE: add RTS support module
      progName  = dropExtension progFilePath

  currentDir <- liftIO getCurrentDirectory
  stgappDir <- makeAbsolute $ takeDirectory progFilePath
  --putStrLn $ "progName: " ++ show progName ++ " progArgs: " ++ show progArgs
  let run = do
        when switchCWD $ liftIO $ setCurrentDirectory stgappDir
        declareTopBindings mods
        initRtsSupport progName progArgs mods
        env <- gets ssStaticGlobalEnv
        let rootMain = unId $ head $ [i | i <- Map.keys env, show i == "main_:Main.main"]
        limit <- gets ssNextHeapAddr
        modify' $ \s@StgState{..} -> s {ssHeapStartAddress = limit}
        modify' $ \s@StgState{..} -> s {ssStgErrorAction = Printable $ Debugger.processCommandsUntilExit}

        -- TODO: check how it is done in the native RTS: call hs_main
        mainAtom <- lookupEnv mempty rootMain

        evalOnMainThread $ do
          stackPush $ Apply [Void]
          pure [mainAtom]

        {-
        Capability *cap = rts_lock();
        rts_evalLazyIO(&cap, main_closure, NULL);
        rts_unlock(cap);
        -}
        flushStdHandles
        --showDebug evalOnNewThread
        -- TODO: do everything that 'hs_exit_' does

        exportCallGraph

        -- HINT: start debugger REPL in debug mode
        when (dbgState == DbgStepByStep) $ do
          Debugger.processCommandsUntilExit

  let (dbgCmdO, _) = getDebuggerChan dbgChan
  nextDbgCmd <- NextDebugCommand <$> Unagi.tryReadChan dbgCmdO

  tracingState <- case tracing of
    False -> pure NoTracing
    True  -> do
      let tracePath = ".extstg-trace" </> takeFileName progName
      createDirectoryIfMissing True tracePath
      DoTracing <$> openFile (tracePath </> "originDB.tsv") WriteMode
                <*> openFile (progName ++ ".whole-program-path.tsv") WriteMode

  stateStore <- PrintableMVar <$> newEmptyMVar
  (gcThreadId, gcIn', gcOut') <- GC.init
  let gcIn  = PrintableMVar gcIn'
      gcOut = PrintableMVar gcOut'
  dl <- dlopen (libBasePath ctx) [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]
  let freeResources = do
        dlclose dl
        killThread gcThreadId
        case tracingState of
          DoTracing h wpp -> do
            hClose wpp
            hClose h
          _ -> pure ()
  flip catch (\e -> do {freeResources; throw (e :: SomeException)}) $ do
    s@StgState{..} <- execStateT run (emptyStgState stateStore dl dbgChan nextDbgCmd dbgState tracingState gcIn gcOut)
    when switchCWD $ setCurrentDirectory currentDir
    freeResources

    putStrLn $ "ssHeapStartAddress: " ++ show ssHeapStartAddress
    putStrLn $ "ssTotalLNECount: " ++ show ssTotalLNECount
    putStrLn $ "ssClosureCallCounter: " ++ show ssClosureCallCounter
    putStrLn $ "executed closure id count: " ++ show (Set.size ssExecutedClosureIds)
    putStrLn $ "call graph size: " ++ show (StrictMap.size . cgInterClosureCallGraph $ ssCallGraph)
    --putStrLn $ unlines $ [BS8.unpack $ binderUniqueName b | Id b <- Map.keys ssEnv]
    --print ssNextHeapAddr
    --print $ head $ Map.toList ssEnv
    -- TODO: handle :Main.main properly ; currenlty it is in conflict with Main.main

-------------------------

-------------------------

flushStdHandles :: M ()
flushStdHandles = do
  Rts{..} <- gets ssRtsSupport
  evalOnNewThread $ do
    stackPush $ Apply [] -- HINT: force IO monad result to WHNF
    stackPush $ Apply [Void]
    pure [rtsTopHandlerFlushStdHandles]
{-
  (tid, ts) <- createThread
  insertThread tid ts
  scheduleToTheEnd tid
  switchToThread tid

  -- force result to WHNF
  --stackPush $ Apply []
  resultLazy <- builtinStackMachineApply rtsTopHandlerFlushStdHandles [Void]
  case resultLazy of
    []            -> pure resultLazy
    [valueThunk]  -> builtinStgEval valueThunk -- pure resultLazy -- builtinStackMachineApply valueThunk []
-}
  pure ()



{-
// Flush stdout and stderr.  We do this during shutdown so that it
// happens even when the RTS is being used as a library, without a
// main (#5594)
static void flushStdHandles(void)
{
    Capability *cap;
    cap = rts_lock();
    rts_evalIO(&cap, flushStdHandles_closure, NULL);
    rts_unlock(cap);
}

/*
 * rts_evalIO() evaluates a value of the form (IO a), forcing the action's
 * result to WHNF before returning.
 */
void rts_evalIO (/* inout */ Capability **cap,
                 /* in    */ HaskellObj p,
                 /* out */   HaskellObj *ret)
{
    StgTSO* tso;

    tso = createStrictIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

StgTSO *
createStrictIOThread(Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread(cap, stack_size);
  pushClosure(t, (W_)&stg_forceIO_info);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}
-}

{-
/*
 * Like rts_evalIO(), but doesn't force the action's result.
 */
void rts_evalLazyIO (/* inout */ Capability **cap,
                     /* in    */ HaskellObj p,
                     /* out */   HaskellObj *ret)
{
    StgTSO *tso;

    tso = createIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

StgTSO *
createIOThread (Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread (cap, stack_size);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

void
scheduleWaitThread (StgTSO* tso, /*[out]*/HaskellObj* ret, Capability **pcap)
{
    Task *task;
    DEBUG_ONLY( StgThreadID id );
    Capability *cap;

    cap = *pcap;

    // We already created/initialised the Task
    task = cap->running_task;

    // This TSO is now a bound thread; make the Task and TSO
    // point to each other.
    tso->bound = task->incall;
    tso->cap = cap;

    task->incall->tso = tso;
    task->incall->ret = ret;
    task->incall->rstat = NoStatus;

    appendToRunQueue(cap,tso);

    DEBUG_ONLY( id = tso->id );
    debugTrace(DEBUG_sched, "new bound thread (%lu)", (unsigned long)id);

    cap = schedule(cap,task);

    ASSERT(task->incall->rstat != NoStatus);
    ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);

    debugTrace(DEBUG_sched, "bound thread (%lu) finished", (unsigned long)id);
    *pcap = cap;
}
-}
---------------------- primops

evalPrimOp :: HasCallStack => Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp =
  PrimAddr.evalPrimOp $
  PrimArray.evalPrimOp $
  PrimSmallArray.evalPrimOp $
  PrimArrayArray.evalPrimOp $
  PrimByteArray.evalPrimOp $
  PrimChar.evalPrimOp $
  PrimConcurrency.evalPrimOp $
  PrimDelayWait.evalPrimOp $
  PrimParallelism.evalPrimOp $
  PrimExceptions.evalPrimOp $
  PrimFloat.evalPrimOp $
  PrimDouble.evalPrimOp $
  PrimInt16.evalPrimOp $
  PrimInt8.evalPrimOp $
  PrimInt.evalPrimOp $
  PrimMutVar.evalPrimOp $
  PrimMVar.evalPrimOp $
  PrimNarrowings.evalPrimOp $
  PrimPrefetch.evalPrimOp $
  PrimStablePointer.evalPrimOp $
  PrimWeakPointer.evalPrimOp $
  PrimWord16.evalPrimOp $
  PrimWord8.evalPrimOp $
  PrimWord.evalPrimOp $
  PrimTagToEnum.evalPrimOp $
  PrimUnsafe.evalPrimOp $
  PrimMiscEtc.evalPrimOp $
  PrimIdris.evalPrimOp $
  unsupported where
    unsupported op args _t _tc = stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args
