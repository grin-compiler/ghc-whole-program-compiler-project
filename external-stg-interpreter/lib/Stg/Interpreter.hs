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
import qualified Data.Map.Strict as StrictMap
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import System.Posix.DynamicLinker
import Codec.Archive.Zip

import System.FilePath
import System.IO
import System.Directory
import System.Exit

import Stg.Syntax
import Stg.Program
import Stg.JSON
import Stg.Analysis.LiveVariable
import Stg.Foreign.Linker

import Stg.Interpreter.Base
import Stg.Interpreter.PrimCall
import Stg.Interpreter.FFI
import Stg.Interpreter.Rts
import qualified Stg.Interpreter.ThreadScheduler as Scheduler

import qualified Stg.Interpreter.PrimOp.Addr          as PrimAddr
--import qualified Stg.Interpreter.PrimOp.Array         as PrimArray
--import qualified Stg.Interpreter.PrimOp.SmallArray    as PrimSmallArray
--import qualified Stg.Interpreter.PrimOp.ArrayArray    as PrimArrayArray
import qualified Stg.Interpreter.PrimOp.ByteArray     as PrimByteArray
import qualified Stg.Interpreter.PrimOp.Char          as PrimChar
--import qualified Stg.Interpreter.PrimOp.Concurrency   as PrimConcurrency
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
--import qualified Stg.Interpreter.PrimOp.MutVar        as PrimMutVar
--import qualified Stg.Interpreter.PrimOp.MVar          as PrimMVar
import qualified Stg.Interpreter.PrimOp.Narrowings    as PrimNarrowings
import qualified Stg.Interpreter.PrimOp.Prefetch      as PrimPrefetch
import qualified Stg.Interpreter.PrimOp.StablePointer as PrimStablePointer
import qualified Stg.Interpreter.PrimOp.WeakPointer   as PrimWeakPointer
import qualified Stg.Interpreter.PrimOp.TagToEnum     as PrimTagToEnum
import qualified Stg.Interpreter.PrimOp.Unsafe        as PrimUnsafe
import qualified Stg.Interpreter.PrimOp.MiscEtc       as PrimMiscEtc

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

evalLiteral :: HasCallStack => Lit -> M AtomAddr
evalLiteral = \case
  LitLabel name spec  -> getFFILabelPtrAtom name spec
  LitString str       -> getCStringConstantPtrAtom str
  LitFloat f    -> storeNewAtom . FloatAtom $ realToFrac f
  LitDouble d   -> storeNewAtom . DoubleAtom $ realToFrac d
  LitNullAddr   -> storeNewAtom $ PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n     -> storeNewAtom . IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> storeNewAtom . IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> storeNewAtom . WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> storeNewAtom . WordAtom $ fromIntegral n
  l -> storeNewAtom $ Literal l

evalArg :: HasCallStack => Env -> Arg -> M AtomAddr
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


builtinStgEval :: HasCallStack => StaticOrigin -> AtomAddr -> M [AtomAddr]
builtinStgEval so atomAddr = do
  atom <- getAtom atomAddr >>= \case
    v@HeapPtr{} -> pure v
    v           -> stgErrorM $ "expected a thunk, got: " ++ show v ++ ", static-origin: " ++ show so
  o <- readHeap atom
  case o of
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}             -> pure [atomAddr]
    {-
    -- TODO: check how the cmm stg machine handles this case
    BlackHole t -> do
                    Rts{..} <- gets ssRtsSupport
                    liftIO $ do
                      hPutStrLn stderr $ takeBaseName rtsProgName ++ ": <<loop>>"
                      exitWith ExitSuccess
                    stgErrorM $ "blackhole ; loop in evaluation of : " ++ show t
    -}
    Closure{..}
      | hoCloMissing /= 0
      -> pure [atomAddr]

      | otherwise
      -> do

        let StgRhsClosure _ uf params e = hoCloBody
            HeapPtr l = atom
            extendedEnv = addManyBindersToEnv SO_CloArg params hoCloArgs hoEnv

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

builtinStgApply :: HasCallStack => StaticOrigin -> AtomAddr -> [AtomAddr] -> M [AtomAddr]
builtinStgApply so a [] = builtinStgEval so a
--builtinStgApply so a@HeapPtr{} args = do
builtinStgApply so atomAddr argsAddr = do
  atom <- getAtom atomAddr >>= \case
    v@HeapPtr{} -> pure v
    v -> do
      args <- getAtoms argsAddr
      stgErrorM $ "builtinStgApply - expected a closure (ptr), got: " ++ show v ++ ", args: " ++ show args ++ ", static-origin: " ++ show so

  let argCount      = length argsAddr
      HeapPtr addr  = atom
  o <- readHeap atom
  case o of
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}             -> stgErrorM $ "unexpexted con at apply: "-- ++ show o
    BlackHole t       -> stgErrorM $ "blackhole ; loop in application of : " ++ show t
    Closure{..}
      -- under saturation
      | hoCloMissing - argCount > 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ argsAddr, hoCloMissing = hoCloMissing - argCount})
        allocAtoms [HeapPtr newAp]

      -- over saturation
      | hoCloMissing - argCount < 0
      -> do
        let (satArgs, remArgs) = splitAt hoCloMissing argsAddr
        stackPush (Apply remArgs)
        builtinStgApply so atomAddr satArgs

      -- saturation
      | hoCloMissing - argCount == 0
      -> do
        newAp <- freshHeapAddress
        store newAp (o {hoCloArgs = hoCloArgs ++ argsAddr, hoCloMissing = hoCloMissing - argCount})
        hpAddr <- storeNewAtom (HeapPtr newAp)
        builtinStgEval so hpAddr

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
            print aty
            putStrLn ""
            print res
            putStrLn ""
          error "Thunk"
      | otherwise         -> pure ()
    BlackHole{} -> error "BlackHole"
    RaiseException{} -> pure ()
    _ -> error $ "assertWHNF: " ++ show o
assertWHNF _ _ _ = pure ()

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

evalOnMainThread :: M [AtomAddr] -> M [AtomAddr]
evalOnMainThread = evalOnThread True

evalOnNewThread :: M [AtomAddr] -> M [AtomAddr]
evalOnNewThread = evalOnThread False

evalOnThread :: Bool -> M [AtomAddr] -> M [AtomAddr]
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

evalStackMachine :: [AtomAddr] -> M [AtomAddr]
evalStackMachine result = do
  stackPop >>= \case
    Nothing         -> pure result
    Just stackCont  -> evalStackContinuation result stackCont >>= evalStackMachine

evalStackContinuation :: [AtomAddr] -> StackContinuation -> M [AtomAddr]
evalStackContinuation resultAddr stackCont = do
 result <- getAtoms resultAddr
 case stackCont of
  Apply args
    | [fun@HeapPtr{}] <- result
    , [funAddr] <- resultAddr
    -> builtinStgApply SO_ClosureResult funAddr args

  Update dstAddr
    | [src@HeapPtr{}] <- result
    -> do
      o <- readHeap src
      store dstAddr o
      pure resultAddr

  -- HINT: STG IR uses 'case' expressions to chain instructions with strict evaluation
  CaseOf localEnv resultBinder altType alts -> do
    assertWHNF result altType resultBinder
    let resultId = (Id resultBinder)
    case altType of
      AlgAlt tc -> do
        let vAddr = case resultAddr of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            v = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder vAddr localEnv
        con <- readHeapCon v
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstCon resultId extendedEnv con $ al ++ [d]
          _ -> matchFirstCon resultId extendedEnv con alts

      PrimAlt _r -> do
        let litAddr = case resultAddr of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            lit = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder litAddr localEnv
        case alts of
          d@(Alt AltDefault _ _) : al -> matchFirstLit resultId extendedEnv lit $ al ++ [d]
          _ -> matchFirstLit resultId extendedEnv lit alts

      MultiValAlt _n -> do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let [Alt{..}] = alts
            extendedEnv = addManyBindersToEnv SO_Scrut altBinders resultAddr localEnv

        evalExpr extendedEnv altRHS

      PolyAlt -> do
        let [Alt{..}]   = alts
            [v]         = resultAddr
            extendedEnv = addBinderToEnv SO_Scrut resultBinder v $                 -- HINT: bind the result
                          addManyBindersToEnv SO_AltArg altBinders resultAddr localEnv  -- HINT: bind alt params

        evalExpr extendedEnv altRHS

  RestoreExMask b i -> do
    -- TODO
    pure resultAddr

  Catch h b i -> do
    -- TODO: is anything to do??
    pure resultAddr

  RunScheduler sr -> Scheduler.runScheduler resultAddr sr

  -- HINT: dataToTag# has an eval call in the middle, that's why we need this continuation, it is the post-returning part of the op implementation
  DataToTagOp -> PrimTagToEnum.dataToTagOp result

  x -> error $ "unsupported continuation: " ++ show x ++ ", result: " ++ show result

evalExpr :: HasCallStack => Env -> Expr -> M [AtomAddr]
evalExpr localEnv = \case
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
      allocAtoms [HeapPtr loc]

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
      | binderUniqueName i == "ghc-prim_GHC.Prim.coercionToken#" -- wired in coercion token ; FIXME: handle wired-in names with a better design
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
    stackPush (CaseOf localEnv scrutineeResult altType alts)
    evalExpr localEnv e

  StgOpApp (StgPrimOp op) l t tc -> do
    --Debugger.checkBreakpoint op
    --Debugger.checkRegion op
    args <- mapM (evalArg localEnv) l
    tid <- gets ssCurrentThreadId
    evalPrimOp op args t tc

  StgOpApp (StgFCallOp foreignCall) l t tc -> do
    args <- case foreignCTarget foreignCall of
      StaticTarget _ "createAdjustor" _ _
        | [arg0, arg1, arg2, arg3, arg4, arg5] <- l
        -> do
            -- HINT: do not resolve the unused label pointer that comes from the stub code
            mapM (evalArg localEnv) [arg0, arg1, StgLitArg LitNullAddr, arg3, arg4, arg5]
      _ -> mapM (evalArg localEnv) l
    evalFCallOp evalOnNewThread foreignCall args t tc

  StgOpApp (StgPrimCallOp primCall) l t tc -> do
    args <- mapM (evalArg localEnv) l
    evalPrimCallOp primCall args t tc

  StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t


matchFirstLit :: HasCallStack => Id -> Env -> Atom -> [Alt] -> M [AtomAddr]
matchFirstLit resultId localEnv a [Alt AltDefault _ rhs] = do
  evalExpr localEnv rhs
matchFirstLit resultId localEnv atom alts = case head $ [a | a@Alt{..} <- alts, matchLit atom altCon] ++ (error $ "no lit match" ++ show (resultId, atom, map altCon alts)) of
  Alt{..} -> do
    evalExpr localEnv altRHS

matchLit :: HasCallStack => Atom -> AltCon -> Bool
matchLit a = \case
  AltDataCon{}  -> False
  AltLit l      -> a == convertAltLit l
  AltDefault    -> True

convertAltLit :: Lit -> Atom
convertAltLit lit = case lit of
  LitFloat f                -> FloatAtom $ realToFrac f
  LitDouble d               -> DoubleAtom $ realToFrac d
  LitNullAddr               -> PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n     -> IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> WordAtom $ fromIntegral n
  LitLabel{}                -> error $ "invalid alt pattern: " ++ show lit
  LitString{}               -> error $ "invalid alt pattern: " ++ show lit
  l -> Literal l

matchFirstCon :: HasCallStack => Id -> Env -> HeapObject -> [Alt] -> M [AtomAddr]
matchFirstCon resultId localEnv (Con _ dc args) alts = case [a | a@Alt{..} <- alts, matchCon dc altCon] of
  []  -> stgErrorM $ "no matching alts for: " ++ show resultId
  Alt{..} : _ -> do
    let extendedEnv = addManyBindersToEnv SO_AltArg altBinders args localEnv
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
    atomAddr <- storeNewAtom (HeapPtr addr)
    pure $ addBinderToEnv SO_Let b atomAddr localEnv

  StgRec l -> do
    (ls, newEnvItems) <- fmap unzip . forM l $ \(b, _) -> do
      addr <- freshHeapAddress
      atomAddr <- storeNewAtom (HeapPtr addr)
      pure (addr, (b, atomAddr))
    let extendedEnv = addZippedBindersToEnv SO_Let newEnvItems localEnv
    forM_ (zip ls l) $ \(addr, (b, rhs)) -> do
      storeRhs isLetNoEscape extendedEnv b addr rhs
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
    atomAddr <- storeNewAtom $ HeapPtr addr
    pure ((Id b, (SO_TopLevel, atomAddr)), (b, addr, rhs))

  -- set the top level binder env
  modify' $ \s@StgState{..} -> s {ssStaticGlobalEnv = Map.fromList $ stringEnv ++ closureEnv}

  -- HINT: top level closures does not capture local variables
  forM_ rhsList $ \(b, addr, rhs) -> storeRhs False mempty b addr rhs

loadAndRunProgram :: HasCallStack => Bool -> String -> [String] -> IO ()
loadAndRunProgram switchCWD fullpak_name progArgs = do

  mods0 <- case takeExtension fullpak_name of
    ".fullpak"                          -> getFullpakModules fullpak_name
    ".json"                             -> getJSONModules fullpak_name
    ext | isSuffixOf "_ghc_stgapp" ext  -> getGhcStgAppModules fullpak_name
    _                                   -> error "unknown input file format"
  runProgram switchCWD fullpak_name mods0 progArgs

runProgram :: HasCallStack => Bool -> String -> [Module] -> [String] -> IO ()
runProgram switchCWD progFilePath mods0 progArgs = do
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
        let rootMain = unId $ case [i | i <- Map.keys env, show i == "main_:Main.main"] of
              [mainId]  -> mainId
              []        -> error "main_:Main.main not found"
              _         -> error "multiple main_:Main.main have found"
        limit <- gets ssNextHeapAddr
        --modify' $ \s@StgState{..} -> s {ssHeapStartAddress = limit}
        --modify' $ \s@StgState{..} -> s {ssStgErrorAction = Printable $ Debugger.processCommandsUntilExit}

        -- TODO: check how it is done in the native RTS: call hs_main
        mainAtom <- lookupEnv mempty rootMain

        evalOnMainThread $ do
          voidAddr <- storeNewAtom Void
          stackPush $ Apply [voidAddr]
          pure [mainAtom]

        {-
        Capability *cap = rts_lock();
        rts_evalLazyIO(&cap, main_closure, NULL);
        rts_unlock(cap);
        -}
        flushStdHandles
        --showDebug evalOnNewThread
        -- TODO: do everything that 'hs_exit_' does

        --exportCallGraph
        {-
        -- HINT: start debugger REPL in debug mode
        when (dbgState == DbgStepByStep) $ do
          Debugger.processCommandsUntilExit
        -}


  stateStore <- PrintableMVar <$> newEmptyMVar

  dl <- loadCbitsSO False progFilePath
  let freeResources = do
        dlclose dl
        --killThread gcThreadId
  flip catch (\e -> do {freeResources; throw (e :: SomeException)}) $ do
    s@StgState{..} <- execStateT run (emptyStgState stateStore dl)
    when switchCWD $ setCurrentDirectory currentDir
    freeResources

    -- TODO: handle :Main.main properly ; currenlty it is in conflict with Main.main

loadCbitsSO :: Bool -> FilePath -> IO DL
loadCbitsSO isQuiet progFilePath = do
  workDir <- getExtStgWorkDirectory progFilePath
  createDirectoryIfMissing True workDir
  let soName = workDir </> "cbits.so"
  doesFileExist soName >>= \case
    True  -> pure ()
    False -> case takeExtension progFilePath of
      ".fullpak" -> do
        unless isQuiet $ putStrLn "unpacking cbits.so"
        withArchive progFilePath $ do
          s <- mkEntrySelector "cbits/cbits.so"
          saveEntry s soName
      _ -> do
        unless isQuiet $ putStrLn "linking cbits.so"
        linkForeignCbitsSharedLib progFilePath
  dlopen soName [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]
  --dlmopen LM_ID_BASE soName [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]
  --dlmopen LM_ID_NEWLM "./libHSbase-4.14.0.0.cbits.so" [RTLD_NOW, RTLD_LOCAL]

-------------------------

-------------------------

flushStdHandles :: M ()
flushStdHandles = do
  Rts{..} <- gets ssRtsSupport
  evalOnNewThread $ do
    stackPush $ Apply [] -- HINT: force IO monad result to WHNF
    voidAddr <- storeNewAtom Void
    stackPush $ Apply [voidAddr]
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

evalPrimOp :: HasCallStack => Name -> [AtomAddr] -> Type -> Maybe TyCon -> M [AtomAddr]
evalPrimOp =
  PrimAddr.evalPrimOp $
--  PrimArray.evalPrimOp $
--  PrimSmallArray.evalPrimOp $
--  PrimArrayArray.evalPrimOp $
  PrimByteArray.evalPrimOp $
  PrimChar.evalPrimOp $
--  PrimConcurrency.evalPrimOp $
  PrimDelayWait.evalPrimOp $
  PrimParallelism.evalPrimOp $
  PrimExceptions.evalPrimOp $
  PrimFloat.evalPrimOp $
  PrimDouble.evalPrimOp $
  PrimInt16.evalPrimOp $
  PrimInt8.evalPrimOp $
  PrimInt.evalPrimOp $
--  PrimMutVar.evalPrimOp $
--  PrimMVar.evalPrimOp $
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
  unsupported where
    unsupported op args _t _tc = stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args
