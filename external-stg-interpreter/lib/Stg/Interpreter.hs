
module Stg.Interpreter where

import           Codec.Archive.Zip                      (mkEntrySelector, saveEntry, withArchive)

import           Control.Applicative                    (Applicative (..), (<$>))
import           Control.Concurrent                     (killThread, newEmptyMVar)
import           Control.Exception                      (SomeException, handle, throw)
import           Control.Monad                          (Functor (..), Monad (..), forM, forM_, mapM, mapM_, unless,
                                                         void, when)
import           Control.Monad.State.Strict             (MonadIO (..), StateT, execStateT, gets, modify')

import           Data.Bool                              (Bool (..), not, otherwise, (&&), (||))
import           Data.Eq                                (Eq (..))
import           Data.Function                          (id, ($), (.))
import           Data.Int                               (Int)
import qualified Data.IntMap                            as IntMap
import qualified Data.IntSet                            as IntSet
import           Data.List                              (concatMap, elem, isSuffixOf, length, null, partition, reverse,
                                                         splitAt, unzip, zip, (++))
import qualified Data.Map                               as Map
import qualified Data.Map.Strict                        as StrictMap
import           Data.Maybe                             (Maybe (..), maybe)
import           Data.Monoid                            (Monoid (..))
import           Data.Ord                               (Ord (..))
import qualified Data.Set                               as Set
import           Data.String                            (String)
import           Data.Time.Clock                        (getCurrentTime)
import qualified Data.Yaml                              as Y

import           Foreign.Ptr                            (nullPtr)

import           GHC.Err                                (error, undefined)
import           GHC.Num                                (Num (..))
import           GHC.Real                               (fromIntegral, realToFrac)
import           GHC.Stack                              (HasCallStack)

import           Prelude                                (Enum (..))

import           Stg.Analysis.LiveVariable              (annotateWithLiveVariables)
import           Stg.Foreign.Linker                     (getExtStgWorkDirectory, linkForeignCbitsSharedLib)
import           Stg.Interpreter.Base                   (Addr, Atom (..), BlockReason (..), Breakpoint (..),
                                                         CallGraph (..), CutShow (..), DebugFrame (..), DebugSettings,
                                                         DebugState (..), DebuggerChan, Env, HeapObject (..), M,
                                                         Printable (..), PrintableMVar (..), ProgramPoint (..),
                                                         PtrOrigin (..), Rts (..), ScheduleReason (..),
                                                         StackContinuation (..), StaticOrigin (..), StgState (..),
                                                         ThreadState (..), ThreadStatus (..), TracingState (..),
                                                         addBinderToEnv, addInterClosureCallGraphEdge,
                                                         addIntraClosureCallGraphEdge, addManyBindersToEnv,
                                                         addZippedBindersToEnv, allocAndStore, createThread,
                                                         debugPrintAtom, debugPrintHeapObject, emptyStgState,
                                                         envToAtoms, freshHeapAddress, getCStringConstantPtrAtom,
                                                         getCurrentThreadState, getThreadState, isThreadLive, lookupEnv,
                                                         lookupEnvSO, markClosure, markExecuted, markExecutedId,
                                                         markFFI, markLNE, markPrimCall, markPrimOp, mylog, promptM,
                                                         readHeap, readHeapCon, reportThreads, scheduleToTheEnd,
                                                         setProgramPoint, showStackCont, stackPop, stackPush, stgErrorM,
                                                         store, switchToThread, traceLog, updateThreadState,
                                                         wakeupBlackHoleQueueThreads)
import           Stg.Interpreter.Debug                  (exportCallGraph)
import qualified Stg.Interpreter.Debugger               as Debugger
import qualified Stg.Interpreter.Debugger.Region        as Debugger
import           Stg.Interpreter.FFI                    (buildCWrapperHsTypeMap, evalFCallOp, getFFILabelPtrAtom)
import qualified Stg.Interpreter.GC                     as GC
import           Stg.Interpreter.PrimCall               (evalPrimCallOp)
import qualified Stg.Interpreter.PrimOp.Addr            as PrimAddr
import qualified Stg.Interpreter.PrimOp.Array           as PrimArray
import qualified Stg.Interpreter.PrimOp.ArrayArray      as PrimArrayArray
import qualified Stg.Interpreter.PrimOp.ByteArray       as PrimByteArray
import qualified Stg.Interpreter.PrimOp.Char            as PrimChar
import qualified Stg.Interpreter.PrimOp.Concurrency     as PrimConcurrency
import qualified Stg.Interpreter.PrimOp.DelayWait       as PrimDelayWait
import qualified Stg.Interpreter.PrimOp.Double          as PrimDouble
import qualified Stg.Interpreter.PrimOp.Exceptions      as PrimExceptions
import qualified Stg.Interpreter.PrimOp.Float           as PrimFloat
import qualified Stg.Interpreter.PrimOp.InfoTableOrigin as PrimInfoTableOrigin
import qualified Stg.Interpreter.PrimOp.Int             as PrimInt
import qualified Stg.Interpreter.PrimOp.Int16           as PrimInt16
import qualified Stg.Interpreter.PrimOp.Int32           as PrimInt32
import qualified Stg.Interpreter.PrimOp.Int64           as PrimInt64
import qualified Stg.Interpreter.PrimOp.Int8            as PrimInt8
import qualified Stg.Interpreter.PrimOp.MiscEtc         as PrimMiscEtc
import qualified Stg.Interpreter.PrimOp.MutVar          as PrimMutVar
import qualified Stg.Interpreter.PrimOp.MVar            as PrimMVar
import qualified Stg.Interpreter.PrimOp.Narrowings      as PrimNarrowings
import qualified Stg.Interpreter.PrimOp.ObjectLifetime  as PrimObjectLifetime
import qualified Stg.Interpreter.PrimOp.Parallelism     as PrimParallelism
import qualified Stg.Interpreter.PrimOp.Prefetch        as PrimPrefetch
import qualified Stg.Interpreter.PrimOp.SmallArray      as PrimSmallArray
import qualified Stg.Interpreter.PrimOp.StablePointer   as PrimStablePointer
import qualified Stg.Interpreter.PrimOp.STM             as PrimSTM
import qualified Stg.Interpreter.PrimOp.TagToEnum       as PrimTagToEnum
import qualified Stg.Interpreter.PrimOp.Unsafe          as PrimUnsafe
import qualified Stg.Interpreter.PrimOp.WeakPointer     as PrimWeakPointer
import qualified Stg.Interpreter.PrimOp.Word            as PrimWord
import qualified Stg.Interpreter.PrimOp.Word16          as PrimWord16
import qualified Stg.Interpreter.PrimOp.Word32          as PrimWord32
import qualified Stg.Interpreter.PrimOp.Word64          as PrimWord64
import qualified Stg.Interpreter.PrimOp.Word8           as PrimWord8
import           Stg.Interpreter.Rts                    (extStgRtsSupportModule, initRtsSupport)
import qualified Stg.Interpreter.ThreadScheduler        as Scheduler
import           Stg.IO                                 (readModpakS)
import           Stg.IRLocation                         (StgPoint (..), binderToStgId)
import           Stg.Program                            (GhcStgApp (..), getFullpakModules, getGhcStgAppModules,
                                                         getJSONModules, readGhcStgApp)
import           Stg.Syntax                             (Alt, Alt' (..), AltCon, AltCon' (..), AltType, AltType' (..),
                                                         Arg, Arg' (..), Binder (..), Binding, Binding' (..),
                                                         CCallTarget (..), DC (..), DataCon (..), DataConRep (..), Expr,
                                                         Expr' (..), ForeignCall (..), Id (..), IdDetails (..),
                                                         Lit (..), LitNumType (..), Module, Module' (..), Name,
                                                         PrimRep (..), Rhs, Rhs' (..), StgOp (..), TopBinding' (..),
                                                         TyCon, Type (..), UpdateFlag (..))

import           System.Directory                       (createDirectoryIfMissing, doesFileExist, getCurrentDirectory,
                                                         makeAbsolute, setCurrentDirectory)
import           System.FilePath                        (FilePath, dropExtension, takeDirectory, takeExtension,
                                                         takeFileName, (</>))
import           System.IO                              (BufferMode (..), IO, IOMode (..), hClose, hSetBuffering,
                                                         openFile, print, putStrLn)
import           System.Posix.DynamicLinker             (DL, RTLDFlags (..), dlclose, dlopen)

import           Text.Show                              (Show (..))


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
  LitNumber LitNumInt8 n    -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumInt16 n   -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumInt32 n   -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n   -> pure . IntAtom $ fromIntegral n
  LitNumber LitNumWord n    -> pure . WordAtom $ fromIntegral n
  LitNumber LitNumWord8 n   -> pure . WordAtom $ fromIntegral n
  LitNumber LitNumWord16 n  -> pure . WordAtom $ fromIntegral n
  LitNumber LitNumWord32 n  -> pure . WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n  -> pure . WordAtom $ fromIntegral n
  c@LitChar{}               -> pure $ Literal c
  LitRubbish{} -> pure Rubbish
  -- l -> error $ "unsupported: " ++ show l

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
  addInterClosureCallGraphEdge so progPoint . PP_StgPoint . SP_RhsClosureExpr . binderToStgId $ unId hoName
  setProgramPoint $ PP_StgPoint . SP_RhsClosureExpr . binderToStgId $ unId hoName
  -- connect call sites to parent closure
  currentClosure <- gets ssCurrentClosure
  case progPoint of
    PP_Global -> pure ()
    _ -> case currentClosure of
      Just cloId  -> addIntraClosureCallGraphEdge (PP_StgPoint . SP_RhsClosureExpr . binderToStgId $ unId cloId) so progPoint
      _           -> pure ()
  -- write whole program path entry
  fuel <- gets ssDebugFuel
  stepCounter <- gets ssStepCounter

  ts <- getCurrentThreadState
  unless (tsLabel ts == Just "resource-pool: reaper") $ do
    traceLog $ maybe "<global>" show currentClosure ++ "\t" ++ show progPoint ++ "\t" ++ show hoName ++ "\t" ++ show fuel ++ "\t" ++ show stepCounter

builtinStgEval :: HasCallStack => StaticOrigin -> Atom -> M [Atom]
builtinStgEval so a@HeapPtr{} = do
  o <- readHeap a
  Debugger.checkBreakpoint [a] $ BkpCustom "eval"
  case o of
    ApStack{..} -> do
      tid <- gets ssCurrentThreadId
      let HeapPtr l = a
      -- HINT: prevent duplicate computation
      store l BlackHole
        { hoBHOwnerThreadId = tid
        , hoBHOriginalThunk = o
        , hoBHWaitQueue     = []
        }
      stackPush (Update l)      -- HINT: ensure sharing, ApStack is always created from Update frame
      mapM_ stackPush (reverse hoStack)
      pure hoResult
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}       -> pure [a]
    BlackHole{..} -> do
      let HeapPtr addr = a
      tid <- gets ssCurrentThreadId
      ts <- getThreadState tid
      updateThreadState tid (ts {tsStatus = ThreadBlocked (BlockedOnBlackHole addr)})
      store addr o {hoBHWaitQueue = tid : hoBHWaitQueue}
      stackPush (Apply []) -- retry evaluation next time also
      stackPush $ RunScheduler SR_ThreadBlocked
      pure [a]

    Closure{..}
      | hoCloMissing /= 0
      -> pure [a]

      | otherwise
      -> do
        {-
        when ("estgiObserve" == binderName (unId hoName) || "$westgiObserve" == binderName (unId hoName)) $ do
          modify' $ \s -> s {ssPrimOpTrace = True}
          traceLog "full-trace-on"
          argStrs <- mapM debugPrintAtom hoCloArgs
          tid <- gets ssCurrentThreadId
          liftIO $ do
            BS8.putStrLn (binderName $ unId hoName)
            BS8.putStrLn . BS8.pack $ "tid: " ++ show tid ++ " estgiObserve " ++ show hoCloArgs
            BS8.putStrLn . BS8.pack $ unlines argStrs
            hFlush stdout
        -}
        let (uf, params, e) = case getCutShowItem hoCloBody of
                  StgRhsClosure _ uf' params' e' -> (uf', params', e')
                  StgRhsCon _ _                  -> error "missed StgRhsClosure"
        let HeapPtr l = a
            extendedEnv = addManyBindersToEnv SO_CloArg params hoCloArgs hoEnv
        unless (length params == length hoCloArgs) $ do
          stgErrorM $ "builtinStgEval - Closure - length mismatch: " ++ show (params, hoCloArgs)

        modify' $ \s@StgState{..} -> s {ssClosureCallCounter = succ ssClosureCallCounter}
        markExecuted l
        markExecutedId hoName

        -- build call graph
        buildCallGraph so hoName

        modify' $ \s -> s {ssCurrentClosure = Just hoName, ssCurrentClosureEnv = extendedEnv, ssCurrentClosureAddr = l}
        -- check breakpoints and region entering
        let closureName = binderUniqueName $ unId hoName
        markClosure closureName -- HINT: this list can be deleted by a debugger command, so this is not the same as `markExecutedId`
        Debugger.checkBreakpoint [a] . BkpStgPoint . SP_RhsClosureExpr . binderToStgId . unId $ hoName
        Debugger.checkRegion closureName
        GC.checkGC [a] -- HINT: add local env as GC root

        -- TODO: env or free var handling
        case uf of
          ReEntrant -> do
            -- closure may be entered multiple times, but should not be updated or blackholed.
            evalExpr extendedEnv e
          Updatable -> do
            -- closure should be updated after evaluation (and may be blackholed during evaluation).
            -- Q: what is eager and lazy blackholing?
            --  read: http://mainisusuallyafunction.blogspot.com/2011/10/thunks-and-lazy-blackholes-introduction.html
            --  read: https://www.microsoft.com/en-us/research/wp-content/uploads/2005/09/2005-haskell.pdf
            stackPush (Update l)
            tid <- gets ssCurrentThreadId
            store l BlackHole
              { hoBHOwnerThreadId = tid
              , hoBHOriginalThunk = o
              , hoBHWaitQueue     = []
              }
            evalExpr extendedEnv e
          SingleEntry -> do
            _tid <- gets ssCurrentThreadId
            -- TODO: investigate how does single-entry blackholing cause problem (estgi does not have racy memops as it is mentioned in GHC Note below)
            -- no backholing, see: GHC Note [Black-holing non-updatable thunks]
            -- closure will only be entered once, and so need not be updated but may safely be blackholed.
            --stackPush (Update l) -- FIX??? Q: what will remove the backhole if there is no update? Q: is the value linear?
            --store l (BlackHole o) -- Q: is this a bug?
            evalExpr extendedEnv e
    -- _ -> stgErrorM $ "expected evaluable heap object, got: " ++ show a ++ " heap-object: " ++ show o ++ " static-origin: " ++ show so
builtinStgEval so a = stgErrorM $ "expected a thunk, got: " ++ show a ++ ", static-origin: " ++ show so

builtinStgApply :: HasCallStack => StaticOrigin -> Atom -> [Atom] -> M [Atom]
builtinStgApply so a [] = builtinStgEval so a
builtinStgApply so a@HeapPtr{} args = do
  let argCount      = length args
      HeapPtr addr  = a
  o <- readHeap a
  case o of
    ApStack{..} -> do
      let HeapPtr l = a
      tid <- gets ssCurrentThreadId
      -- HINT: prevent duplicate computation
      store l BlackHole
        { hoBHOwnerThreadId = tid
        , hoBHOriginalThunk = o
        , hoBHWaitQueue     = []
        }
      stackPush (Apply args)
      stackPush (Update l)      -- HINT: ensure sharing, ApStack is always created from Update frame
      mapM_ stackPush (reverse hoStack)
      pure hoResult
    RaiseException ex -> PrimExceptions.raiseEx ex
    Con{}             -> stgErrorM $ "unexpected con at apply: " ++ show o ++ ", args: " ++ show args ++ ", static-origin: " ++ show so
    BlackHole{..} -> do
      tid <- gets ssCurrentThreadId
      ts <- getThreadState tid
      updateThreadState tid (ts {tsStatus = ThreadBlocked (BlockedOnBlackHole addr)})
      store addr o {hoBHWaitQueue = tid : hoBHWaitQueue}
      stackPush (Apply args) -- retry evaluation next time also
      stackPush $ RunScheduler SR_ThreadBlocked
      pure [a]

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
    _ -> stgErrorM $ "builtinStgApply - expected closure, got: " ++ show o ++ ", args: " ++ show args ++ ", static-origin: " ++ show so

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
    Con _ _dc _args -> pure ()
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
  mylog "[estgi] - killAllThreads"
  -- TODO: check if there are running threads
  tsList <- gets $ IntMap.toList . ssThreads
  let runnableThreads = [tid | (tid, ts) <- tsList, tsStatus ts == ThreadRunning]
  isQuiet <- gets ssIsQuiet
  unless isQuiet $ when (runnableThreads /= []) $ do
    reportThreads
    error "killing all running threads" -- TODO

evalOnMainThread :: M [Atom] -> M [Atom]
evalOnMainThread = evalOnThread True

evalOnNewThread :: M [Atom] -> M [Atom]
evalOnNewThread = evalOnThread False

evalOnThread :: HasCallStack => Bool -> M [Atom] -> M [Atom]
evalOnThread isMainThread setupAction = do
  -- create main thread
  (tid, _ts) <- createThread
  scheduleToTheEnd tid
  switchToThread tid

  stackPush $ RunScheduler $ if isMainThread then SR_ThreadFinishedMain else SR_ThreadFinished
  result0 <- setupAction
  --liftIO $ putStrLn $ "evalOnThread result0 = " ++ show result0
  --Debugger.reportState
  let loop :: HasCallStack => [Atom] -> StateT StgState IO [Atom]
      loop resultIn = do
        resultOut <- evalStackMachine resultIn
        ThreadState{..} <- getThreadState tid
        if isThreadLive tsStatus then
          loop resultOut -- HINT: the new scheduling is ready
        else do
            when isMainThread $ do
              mylog "[estgi] - main hs thread finished"
              killAllThreads
            pure tsCurrentResult
  loop result0


contextSwitchTimer :: M ()
contextSwitchTimer = do
  t0 <- gets ssThreadStepBudget
  t1 <- if t0 > 0 then pure (pred t0) else do
    stackPush $ RunScheduler SR_ThreadYield
    pure 500 -- NOTE: step budget
    --pure 1 -- NOTE: step budget
  modify' $ \s -> s {ssThreadStepBudget = t1}

evalStackMachine :: [Atom] -> M [Atom]
evalStackMachine result = do
  --debugAsyncExceptions
  stackPop >>= \case
    Nothing         -> pure result
    Just stackCont  -> do
      -- promptM $ do
      --   putStrLn $ "  input result = " ++ show result
      --   putStrLn $ "  stack-cont   = " ++ showStackCont stackCont

      -- _doTrace <- gets ssPrimOpTrace
      do -- when doTrace $ do
        resultStr <- mapM debugPrintAtom result
        traceLog $ showStackCont stackCont ++ " current-result: " ++ show resultStr

      Debugger.checkBreakpoint result $ BkpCustom "stack"
      nextResult <- evalStackContinuation result stackCont
      case stackCont of
        RunScheduler{} -> pure ()
        _              -> contextSwitchTimer
      evalStackMachine nextResult

peekAtom :: HasCallStack => Atom -> M String
peekAtom a = case a of
  HeapPtr{} -> debugPrintHeapObject <$> readHeap a
  _         -> pure $ show a

peekAtoms :: [Atom] -> StateT StgState IO [String]
peekAtoms = mapM peekAtom

evalStackContinuation :: HasCallStack => [Atom] -> StackContinuation -> M [Atom]
evalStackContinuation result = \case
  Apply args
    | [fun@HeapPtr{}] <- result
    -> do
      _argsS <- peekAtoms args
      _resultS <- peekAtoms result
      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show args ++ " to " ++ show result
      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show argsS ++ " to " ++ show resultS

      out <- builtinStgApply SO_ClosureResult fun args
      _outS <- peekAtoms out

      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show args ++ " to " ++ show result ++ " output-result: " ++ show out
      --liftIO $ putStrLn $ "evalStackContinuation Apply args: " ++ show argsS ++ " to " ++ show resultS ++ " output-result: " ++ show outS
      pure out

  Update dstAddr
    | [src@HeapPtr{}] <- result
    -> do
      wakeupBlackHoleQueueThreads dstAddr
      o <- readHeap src
      store dstAddr o
      dynamicHeapStartAddr <- gets ssDynamicHeapStart
      when (dstAddr < dynamicHeapStartAddr) $ do
        modify' $ \s@StgState{..} -> s {ssCAFSet = IntSet.insert dstAddr ssCAFSet}
      pure result

  -- HINT: STG IR uses 'case' expressions to chain instructions with strict evaluation
  CaseOf curClosureAddr curClosure localEnv (Id resultBinder) (CutShow altType) alts -> do
    modify' $ \s -> s {ssCurrentClosure = Just curClosure, ssCurrentClosureAddr = curClosureAddr}
    assertWHNF result altType resultBinder
    let resultId = Id resultBinder
        alt = case getCutShowItem alts of
          []      -> undefined
          (a : _) -> a
        v = case result of
          [l] -> l
          _   -> error $ "expected a single value: " ++ show result
    case altType of
      AlgAlt _tc -> do
        let extendedEnv = addBinderToEnv SO_Scrut resultBinder v localEnv
        con <- readHeapCon v
        matchFirstCon resultId extendedEnv con $ getCutShowItem alts

      PrimAlt _r -> do
        let lit = case result of
              [l] -> l
              _   -> error $ "expected a single value: " ++ show result
            extendedEnv = addBinderToEnv SO_Scrut resultBinder lit localEnv
        matchFirstLit resultId extendedEnv lit $ getCutShowItem alts

      MultiValAlt n -> do -- unboxed tuple
        -- NOTE: result binder is not assigned
        let Alt{..} = alt
        when (n /= length altBinders) $ do
          stgErrorM $ "evalStackContinuation - MultiValAlt n broken assumption 2: " ++ show (n, altBinders, result)
        let extendedEnv = if n == 1 && null altBinders
                            then addManyBindersToEnv SO_Scrut [resultBinder] result localEnv
                            else addManyBindersToEnv SO_Scrut altBinders result localEnv
        --unless (length altBinders == length result) $ do
        --  stgErrorM $ "evalStackContinuation - MultiValAlt - length mismatch: " ++ show (n, altBinders, result)

        setProgramPoint . PP_StgPoint $ SP_AltExpr (binderToStgId resultBinder) 0
        evalExpr extendedEnv altRHS

      PolyAlt -> do
        let Alt{..} = alt
            extendedEnv = addBinderToEnv SO_Scrut resultBinder v localEnv             -- HINT: bind the result
                          --addManyBindersToEnv SO_AltArg altBinders result localEnv  -- HINT: bind alt params
        {-
        unless (length altBinders == length result) $ do
          stgErrorM $ "evalStackContinuation - PolyAlt - length mismatch: " ++ show (altBinders, result)
        -}
        setProgramPoint . PP_StgPoint $ SP_AltExpr (binderToStgId resultBinder) 0
        evalExpr extendedEnv altRHS

  (RestoreExMask _oldMask blockAsyncEx isInterruptible) -> do
    tid <- gets ssCurrentThreadId
    ts <- getCurrentThreadState
    updateThreadState tid $ ts {tsBlockExceptions = blockAsyncEx, tsInterruptible = isInterruptible}
    case tsBlockedExceptions ts of
      (thowingTid, exception) : waitingTids
        | not blockAsyncEx
        -> do
          -- try wake up thread
          throwingTS <- getThreadState thowingTid
          when (tsStatus throwingTS == ThreadBlocked (BlockedOnThrowAsyncEx tid)) $ do
            updateThreadState thowingTid throwingTS {tsStatus = ThreadRunning}
          -- raise exception
          ts' <- getCurrentThreadState
          updateThreadState tid ts' {tsBlockedExceptions = waitingTids}
          PrimConcurrency.raiseAsyncEx result tid exception
      _ -> pure ()
    pure result

  Catch _h b i -> do
    -- TODO: is anything to do??
    -- assert if current mask is the same as the one in stack frame
    ThreadState{..} <- getCurrentThreadState
    when (tsBlockExceptions /= b || tsInterruptible /= i) $ do
      error $ "Catch frame assertion failure - ex mask mismatch, expected: " ++ show (b, i) ++ " got: " ++ show (tsBlockExceptions, tsInterruptible)
    pure result

  Atomically stmAction -> PrimSTM.commitOrRestart stmAction result

  CatchSTM{} -> PrimSTM.mergeNestedOrRestart result -- Q: check how this is implemented in the native RTS

  CatchRetry{} -> PrimSTM.mergeNestedOrRestart result

  RunScheduler sr -> do
    --liftIO $ print (RunScheduler sr)
    Scheduler.runScheduler result sr

  -- HINT: dataToTag# has an eval call in the middle, that's why we need this continuation, it is the post-returning part of the op implementation
  DataToTagOp -> PrimTagToEnum.dataToTagOp result

  AtomicallyOp stmAction -> do
    promptM $ putStrLn "[ AtomicallyOp ]"
    PrimSTM.atomicallyOp stmAction

  RaiseOp ex -> do
    ctid <- gets ssCurrentThreadId
    mylog $ "ctid: " ++ show ctid ++ " " ++ show (RaiseOp ex)
    PrimExceptions.raiseEx ex

  KeepAlive{} -> do
    pure result

  DebugFrame df -> evalDebugFrame result df

  x -> error $ "unsupported continuation: " ++ show x ++ ", result: " ++ show result

evalDebugFrame :: HasCallStack => [Atom] -> DebugFrame -> M [Atom]
evalDebugFrame result = \case
  RestoreProgramPoint currentClosure progPoint -> do
    modify' $ \s -> s {ssCurrentClosure = currentClosure}
    setProgramPoint progPoint
    pure result
  DisablePrimOpTrace -> do
    modify' $ \s -> s {ssPrimOpTrace = False}
    traceLog "full-trace-off"
    pure result

  -- x -> error $ "unsupported debug-frame: " ++ show x ++ ", result: " ++ show result

evalExpr :: HasCallStack => Env -> Expr -> M [Atom]
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
      loc <- allocAndStore (Con False (DC dc) args)
      pure [HeapPtr loc]

  StgLet b e -> do
    extendedEnv <- declareBinding False localEnv b
    evalExpr extendedEnv e

  StgLetNoEscape b e -> do -- TODO: do not allocate closure on heap, instead put into env (stack) allocated closure ; model stack allocated heap objects
    extendedEnv <- declareBinding True localEnv b
    evalExpr extendedEnv e

  -- var (join id)
  StgApp i []
    | JoinId 0 _ <- binderDetails i
    -> do
      -- HINT: join id-s are always closures, needs eval
      -- NOTE: join id's type tells the closure return value representation
      (so, v) <- lookupEnvSO localEnv i
      builtinStgEval so v

    | JoinId x _ <- binderDetails i
    -> stgErrorM $ "join-id var arity error, expected 0, got: " ++ show x ++ " id: " ++ show i

  -- var (non join id)
  StgApp i [] -> case binderType i of

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
  StgApp i l
    | JoinId _ _ <- binderDetails i
    -> do
      args <- mapM (evalArg localEnv) l
      (so, v) <- lookupEnvSO localEnv i
      builtinStgApply so v args

  {- non-join id -}
  StgApp i l -> case binderType i of
    SingleValue LiftedRep -> do
      args <- mapM (evalArg localEnv) l
      (so, v) <- lookupEnvSO localEnv i
      builtinStgApply so v args

    r -> stgErrorM $ "unsupported app rep: " ++ show r -- unboxed: invalid

  StgCase e scrutineeResult altType alts -> do
    Just curClosure <- gets ssCurrentClosure
    curClosureAddr <- gets ssCurrentClosureAddr
    stackPush (CaseOf curClosureAddr curClosure localEnv (Id scrutineeResult) (CutShow altType) $ CutShow alts)
    setProgramPoint . PP_StgPoint . SP_CaseScrutineeExpr $ binderToStgId scrutineeResult
    evalExpr localEnv e

  StgOpApp (StgPrimOp op) l t tc -> do
    Debugger.checkBreakpoint (envToAtoms localEnv) $ BkpPrimOp op
    Debugger.checkRegion op
    markPrimOp op
    args <- mapM (evalArg localEnv) l
    _tid <- gets ssCurrentThreadId
    result <- evalPrimOp op args t tc
    doTrace <- gets ssPrimOpTrace
    when doTrace $ traceLog $ show (op, args)
    pure result

  StgOpApp (StgFCallOp foreignCall) l t tc -> do
    -- check foreign target region and breakpoint
    case foreignCTarget foreignCall of
      StaticTarget _ targetName _ _ -> do
        Debugger.checkBreakpoint (envToAtoms localEnv) $ BkpFFISymbol targetName
        Debugger.checkRegion targetName
      _ -> pure ()

    markFFI foreignCall
    args <- case foreignCTarget foreignCall of
      StaticTarget _ "createAdjustor" _ _
        -- void* createAdjustor (int cconv, StgStablePtr hptr, StgFunPtr wptr, char *typeString);
        | [arg0_cconv, arg1_hptr, StgLitArg arg2_wptr, arg3_typeString, arg4_void] <- l
        -> do
            -- HINT:
            --  do not resolve the wrapper function label
            --  the label name is used for FFI type signature lookup
            [arg0_cconvAtom, arg1_hptrAtom, arg3_typeStringAtom, arg4_voidAtom] <- mapM (evalArg localEnv) [arg0_cconv, arg1_hptr, arg3_typeString, arg4_void]
            pure [arg0_cconvAtom, arg1_hptrAtom, Literal arg2_wptr, arg3_typeStringAtom, arg4_voidAtom]
      StaticTarget _ "createAdjustor" _ _
        -> do
            liftIO $ do
              putStrLn "illegal createAdjustor call:"
              putStrLn $ "  foreignCall: " ++ show foreignCall
              putStrLn $ "  type:        " ++ show t
              putStrLn   "  args:"
              forM_ l $ \a -> do
                putStrLn $ "    " ++ show a
            stgErrorM "illegal createAdjustor call"
      _ -> mapM (evalArg localEnv) l
    --mylog $ show ("executing", foreignCall, args)
    evalFCallOp evalOnNewThread foreignCall args t tc
    --mylog $ show (foreignCall, args, result)

  StgOpApp (StgPrimCallOp primCall) l t tc -> do
    markPrimCall primCall
    args <- mapM (evalArg localEnv) l
    evalPrimCallOp primCall args t tc
    --liftIO $ print (primCall, args, result)

  -- StgOpApp op _args t _tc -> stgErrorM $ "unsupported StgOp: " ++ show op ++ " :: " ++ show t


matchFirstLit :: HasCallStack => Id -> Env -> Atom -> [Alt] -> M [Atom]
matchFirstLit resultId localEnv _a [Alt AltDefault _ rhs] = do
  setProgramPoint . PP_StgPoint $ SP_AltExpr (binderToStgId $ unId resultId) 0
  evalExpr localEnv rhs
matchFirstLit resultId localEnv atom alts
  | indexedAlts <- zip [0..] alts
  , indexedAltsWithDefault <- case indexedAlts of
      d@(_, Alt AltDefault _ _) : xs -> xs ++ [d]
      xs                             -> xs
  = case [a | a@(_idx, Alt{..}) <- indexedAltsWithDefault, matchLit atom altCon] of
      [] -> error $ "no lit match" ++ show (resultId, atom, fmap altCon alts)
      ((idx, Alt{..}) : _) -> do
        setProgramPoint . PP_StgPoint $ SP_AltExpr (binderToStgId $ unId resultId) idx
        evalExpr localEnv altRHS

matchLit :: HasCallStack => Atom -> AltCon -> Bool
matchLit a = \case
  AltDataCon{}  -> False
  AltLit l      -> a == convertAltLit l
  AltDefault    -> True

convertAltLit :: Lit -> Atom
convertAltLit lit = case lit of
  LitFloat f               -> FloatAtom $ realToFrac f
  LitDouble d              -> DoubleAtom $ realToFrac d
  LitNullAddr              -> PtrAtom RawPtr nullPtr
  LitNumber LitNumInt n    -> IntAtom $ fromIntegral n
  LitNumber LitNumInt8 n   -> IntAtom $ fromIntegral n
  LitNumber LitNumInt16 n  -> IntAtom $ fromIntegral n
  LitNumber LitNumInt32 n  -> IntAtom $ fromIntegral n
  LitNumber LitNumInt64 n  -> IntAtom $ fromIntegral n
  LitNumber LitNumWord n   -> WordAtom $ fromIntegral n
  LitNumber LitNumWord8 n  -> WordAtom $ fromIntegral n
  LitNumber LitNumWord16 n -> WordAtom $ fromIntegral n
  LitNumber LitNumWord32 n -> WordAtom $ fromIntegral n
  LitNumber LitNumWord64 n -> WordAtom $ fromIntegral n
  LitLabel{}               -> error $ "invalid alt pattern: " ++ show lit
  LitString{}              -> error $ "invalid alt pattern: " ++ show lit
  c@LitChar{}              -> Literal c
  l                        -> error $ "unsupported: " ++ show l

matchFirstCon :: HasCallStack => Id -> Env -> HeapObject -> [Alt] -> M [Atom]
matchFirstCon resultId localEnv heap alts
  | indexedAlts <- zip [0..] alts
  , indexedAltsWithDefault <- case indexedAlts of
      d@(_, Alt AltDefault _ _) : xs -> xs ++ [d]
      xs                             -> xs
  = case heap of
      (Con _ (DC dc) args) -> do
        case [a | a@(_idx, Alt{..}) <- indexedAltsWithDefault, matchCon dc altCon] of
          []  -> stgErrorM $ "no matching alts for: " ++ show resultId
          (idx, Alt{..}) : _ -> do
            let extendedEnv = case altCon of
                                AltDataCon{} -> addManyBindersToEnv SO_AltArg altBinders args localEnv
                                _            -> localEnv
            --unless (length altBinders == length args) $ do
            --  stgErrorM $ "matchFirstCon length mismatch: " ++ show (DC dc, altBinders, args, resultId)
            setProgramPoint . PP_StgPoint $ SP_AltExpr (binderToStgId $ unId resultId) idx
            evalExpr extendedEnv altRHS
      Closure {}       -> error "unsupported"
      BlackHole {}     -> error "unsupported"
      ApStack _ _      -> error "unsupported"
      RaiseException _ -> error "unsupported"

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
      pure (addr, (b, HeapPtr addr))
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
    store addr (Con isLetNoEscape (DC dc) args)

  cl@(StgRhsClosure freeVars _ paramNames _) -> do
    let liveSet   = Set.fromList $ fmap Id freeVars
        prunedEnv = Map.restrictKeys localEnv liveSet -- HINT: do pruning to keep only the live/later referred variables
    store addr (Closure isLetNoEscape (Id i) (CutShow cl) prunedEnv [] (length paramNames))

-----------------------

declareTopBindings :: HasCallStack => [Module] -> M ()
declareTopBindings mods = do
  let (strings, closures) = partition isStringLit $ concatMap moduleTopBindings mods
      isStringLit = \case
        StgTopStringLit{} -> True
        _                 -> False
  -- bind string lits
  stringEnv <- forM strings $ \case
    (StgTopStringLit b str) -> do
      strPtr <- getCStringConstantPtrAtom str
      pure (Id b, (SO_TopLevel, strPtr))
    StgTopLifted _ -> error "unsupported"

  -- bind closures
  let bindings = concatMap getBindings closures
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) -> [(i, rhs)]
        StgTopLifted (StgRec l) -> l
        StgTopStringLit _ _ -> error "unsupported"

  (closureEnv, rhsList) <- fmap unzip . forM bindings $ \(b, rhs) -> do
    addr <- freshHeapAddress
    pure ((Id b, (SO_TopLevel, HeapPtr addr)), (b, addr, rhs))

  -- set the top level binder env
  modify' $ \s -> s {ssStaticGlobalEnv = Map.fromList $ stringEnv ++ closureEnv}

  -- HINT: top level closures does not capture local variables
  forM_ rhsList $ \(b, addr, rhs) -> storeRhs False mempty b addr rhs

usesMultiThreadedRts :: FilePath -> IO Bool
usesMultiThreadedRts fullpak_name = do
  case takeExtension fullpak_name of
    ".fullpak"                          -> do
                                            bs <- readModpakS fullpak_name "app.ghc_stgapp" id
                                            GhcStgApp{..} <- Y.decodeThrow bs
                                            pure $ "WayThreaded" `elem` appWays
    ".json"                             -> error "TODO: read rts concurrency mode from json"
    ext | "_ghc_stgapp" `isSuffixOf` ext  -> do
                                            GhcStgApp{..} <- readGhcStgApp fullpak_name
                                            pure $ "WayThreaded" `elem` appWays
    _                                   -> error "unknown input file format"

loadAndRunProgram :: HasCallStack => Bool -> Bool -> String -> [String] -> DebuggerChan -> DebugState -> Bool -> DebugSettings -> IO ()
loadAndRunProgram isQuiet switchCWD fullpak_name progArgs dbgChan dbgState tracing debugSettings = do

  mods0 <- case takeExtension fullpak_name of
    ".fullpak"                           -> getFullpakModules fullpak_name
    ".json"                              -> getJSONModules fullpak_name
    ext | "_ghc_stgapp" `isSuffixOf` ext -> getGhcStgAppModules fullpak_name
    _                                    -> error "unknown input file format"
  runProgram isQuiet switchCWD fullpak_name mods0 progArgs dbgChan dbgState tracing debugSettings

runProgram :: HasCallStack => Bool -> Bool -> String -> [Module] -> [String] -> DebuggerChan -> DebugState -> Bool -> DebugSettings -> IO ()
runProgram isQuiet switchCWD progFilePath mods0 progArgs dbgChan dbgState tracing debugSettings = do
  let mods     = fmap annotateWithLiveVariables $ extStgRtsSupportModule : mods0 -- NOTE: add RTS support module
      progName = dropExtension progFilePath

  usesMultiThreadedRts progFilePath >>= \case
    True  -> error "TODO: implement concurrent FFI semantics"
    False -> pure ()

  currentDir <- liftIO getCurrentDirectory
  stgappDir <- makeAbsolute $ takeDirectory progFilePath

  let run ::HasCallStack => StateT StgState IO ()
      run = do
        when switchCWD $ liftIO $ setCurrentDirectory stgappDir
        declareTopBindings mods
        buildCWrapperHsTypeMap mods
        initRtsSupport progName progArgs mods
        env <- gets ssStaticGlobalEnv
        let rootMain = unId $ case [i | i <- Map.keys env, show i == "main_:Main.main"] of
              [mainId] -> mainId
              []       -> error "main_:Main.main not found"
              _        -> error "multiple main_:Main.main have found"
        limit <- gets ssNextHeapAddr
        modify' $ \s -> s {ssDynamicHeapStart = limit}
        modify' $ \s -> s {ssStgErrorAction = Printable Debugger.processCommandsUntilExit}

        -- TODO: check how it is done in the native RTS: call hs_main
        mainAtom <- lookupEnv mempty rootMain

        _ <- evalOnMainThread $ do
          stackPush $ Apply [Void]
          pure [mainAtom]

        {-
        Capability *cap = rts_lock();
        rts_evalLazyIO(&cap, main_closure, NULL);
        rts_unlock(cap);
        -}
        mylog " *** flushStdHandles"
        flushStdHandles
        --showDebug evalOnNewThread
        -- TODO: do everything that 'hs_exit_' does

        exportCallGraph

        Debugger.checkBreakpoint [] $ BkpCustom "program finished"
        -- HINT: start debugger REPL in debug mode
        when (dbgState == DbgStepByStep) $ do
          Debugger.processCommandsUntilExit

  tracingState <-
    if tracing then
      pure NoTracing
    else do
      let tracePath = ".extstg-trace" </> takeFileName progName
      createDirectoryIfMissing True tracePath
      fd <- openFile (progName ++ ".whole-program-path.tsv") WriteMode
      hSetBuffering fd LineBuffering
      DoTracing <$> openFile (tracePath </> "originDB.tsv") WriteMode
                <*> pure fd

  stateStore <- PrintableMVar <$> newEmptyMVar
  (gcThreadId, gcIn', gcOut') <- GC.init
  let gcIn    = PrintableMVar gcIn'
      gcOut   = PrintableMVar gcOut'
  dl <- loadCbitsSO isQuiet progFilePath
  let freeResources = do
        dlclose dl
        killThread gcThreadId
        case tracingState of
          DoTracing h wpp -> do
            hClose wpp
            hClose h
          _ -> pure ()
  handle (\e -> do {freeResources; throw (e :: SomeException)}) $ do
    now <- getCurrentTime
    StgState{..} <- execStateT run (emptyStgState now isQuiet stateStore dl dbgChan dbgState tracingState debugSettings gcIn gcOut)
    when switchCWD $ setCurrentDirectory currentDir
    freeResources

    unless isQuiet $ do
      putStrLn $ "ssDynamicHeapStart: " ++ show ssDynamicHeapStart
      putStrLn $ "ssTotalLNECount: " ++ show ssTotalLNECount
      putStrLn $ "ssClosureCallCounter: " ++ show ssClosureCallCounter
      putStrLn $ "executed closure id count: " ++ show (Set.size ssExecutedClosureIds)
      putStrLn $ "call graph size: " ++ show (StrictMap.size . cgInterClosureCallGraph $ ssCallGraph)
    --putStrLn $ unlines $ [BS8.unpack $ binderUniqueName b | Id b <- Map.keys ssEnv]
    --print ssNextHeapAddr
    --print $ head $ Map.toList ssEnv
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
  void $ evalOnMainThread $ do
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
  PrimInt64.evalPrimOp $
  PrimInt32.evalPrimOp $
  PrimInt16.evalPrimOp $
  PrimInt8.evalPrimOp $
  PrimInt.evalPrimOp $
  PrimMutVar.evalPrimOp $
  PrimMVar.evalPrimOp $
  PrimNarrowings.evalPrimOp $
  PrimPrefetch.evalPrimOp $
  PrimStablePointer.evalPrimOp $
  PrimSTM.evalPrimOp $
  PrimWeakPointer.evalPrimOp $
  PrimWord64.evalPrimOp $
  PrimWord32.evalPrimOp $
  PrimWord16.evalPrimOp $
  PrimWord8.evalPrimOp $
  PrimWord.evalPrimOp $
  PrimTagToEnum.evalPrimOp $
  PrimUnsafe.evalPrimOp $
  PrimMiscEtc.evalPrimOp $
  PrimObjectLifetime.evalPrimOp $
  PrimInfoTableOrigin.evalPrimOp
  unsupported where
    unsupported op args _t _tc = stgErrorM $ "unsupported StgPrimOp: " ++ show op ++ " args: " ++ show args
