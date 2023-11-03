{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.ThreadScheduler where

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Time.Clock
import System.IO

import Text.Pretty.Simple (pShowNoColor)
import qualified Data.Text.Lazy.IO as Text
import Data.List

import Stg.Interpreter.Base
import Stg.Interpreter.IOManager
import qualified Stg.Interpreter.Debugger as Debugger
import qualified Stg.Interpreter.PrimOp.Concurrency as PrimConcurrency
import qualified Stg.Interpreter.GC as GC
runScheduler :: [Atom] -> ScheduleReason -> M [Atom]
runScheduler result sr = do
  --debugAsyncExceptions
  tid <- gets ssCurrentThreadId
  threads <- gets ssThreads
  promptM $ do
    putStrLn $ " * scheduler: " ++ show sr ++ " thread: " ++ show tid ++ " result: " ++ show result
    --Text.putStrLn $ pShowNoColor threads
  case sr of
    SR_ThreadFinished -> do
      -- set thread status to finished
      ts <- getThreadState tid
      updateThreadState tid ts {tsStatus = ThreadFinished}
      mylog $ show tid ++ " ** SR_ThreadFinished"
      yield result

    SR_ThreadFinishedMain -> do
      -- set thread status to finished
      ts <- getThreadState tid
      updateThreadState tid ts {tsStatus = ThreadFinished, tsCurrentResult = result}
      mylog $ show tid ++ " ** SR_ThreadFinishedMain"
      pure result

    SR_ThreadFinishedFFICallback -> do
      -- set thread status to finished
      ts <- getThreadState tid
      updateThreadState tid ts {tsStatus = ThreadFinished, tsStack = [], tsCurrentResult = result}
      mylog $ show tid ++ " ** SR_ThreadFinishedFFICallback"
      pure result

    SR_ThreadBlocked  -> yield result

    SR_ThreadYield    -> yield result

yield result = do
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  -- save result
  updateThreadState tid ts {tsCurrentResult = result}

  wakeUpSleepingThreads

  -- lookup next thread
  nextTid <- getNextRunnableThread

  -- switchToThread
  switchToThread nextTid

  -- return threads current result
  nextTS <- getThreadState nextTid

  -- TODO/IDEA/IMPROVEMENT:
  --    store this value in the thread state, but only for the suspended states
  --    the running threads should not store the old "current result" that would prevent garbage collection
  -- HINT: clear value to allow garbage collection
  updateThreadState nextTid nextTS {tsCurrentResult = []}


  threads <- gets ssThreads
  let status = sortOn (\(_, a, _, _, _) -> a) [(i, tsStatus t, tsBlockExceptions t, tsInterruptible t, maybe "" id (tsLabel t)) | (i, t) <- IntMap.toList threads]
  traceLog $ show status

  when False $ do
    now <- liftIO $ getCurrentTime
    --putStrLn $ show now ++ " * scheduler next runnable thread: " ++ show nextTid ++ " " ++ show [(i, tsStatus t) | (i, t) <- IntMap.toList threads, tsStatus t == ThreadRunning]
    liftIO $ putStrLn $ show now ++ " * scheduler next runnable thread: " ++ show nextTid ++ " " ++ show [(i, tsStatus t, tsLabel t) | (i, t) <- IntMap.toList threads]
    forM_ [(i, tsStatus t) | (i, t) <- IntMap.toList threads] $ \(i, status) -> case status of
      ThreadBlocked (BlockedOnMVar mid _) -> do
        md <- lookupMVar mid
        liftIO $ putStrLn $ "mvarId: " ++ show mid ++ " " ++ show md
      _ -> pure ()

  promptM_ $ liftIO $ do
    putStrLn $ " * scheduler next runnable thread: " ++ show nextTid
    --Text.putStrLn $ pShowNoColor threads


  -- validate ex mask state
  when (tsInterruptible nextTS && tsBlockExceptions nextTS == False) $ do
    reportThreads
    error $ "invalid ex mask: " ++ show (nextTid, tsBlockExceptions nextTS, tsInterruptible nextTS)

  -- TODO: rethink, validate, reimplement this correctly, check how it is done in native
  -- try to raise async exceptions from the queue if possible
  if (tsBlockExceptions nextTS == False) --  || (tsInterruptible nextTS && interruptible (tsStatus nextTS)))
    then case tsBlockedExceptions nextTS of
      []          -> pure $ tsCurrentResult nextTS
      (thowingTid, exception) : waitingTids -> do

        -- try wake up thread
        throwingTS <- getThreadState thowingTid
        when (tsStatus throwingTS == ThreadBlocked (BlockedOnThrowAsyncEx nextTid)) $ do
          updateThreadState thowingTid throwingTS {tsStatus = ThreadRunning}
        -- raise exception
        updateThreadState nextTid nextTS {tsBlockedExceptions = waitingTids}
        PrimConcurrency.raiseAsyncEx (tsCurrentResult nextTS) nextTid exception
        pure []
        -- IMPLEMENT THIS AND HOPE THAT IT WILL FIX THE ISSUE
        --error $ " * scheduler tsBlockedExceptions: " ++ show waitingTids
    else
      pure $ tsCurrentResult nextTS

interruptible :: ThreadStatus -> Bool
interruptible = \case
  ThreadFinished  -> False
  ThreadDied      -> False
  ThreadRunning   -> False
  ThreadBlocked r -> case r of
    BlockedOnMVar{}         -> True
    BlockedOnMVarRead{}     -> True
    BlockedOnThrowAsyncEx{} -> True
    BlockedOnSTM{}          -> True
    BlockedOnRead{}         -> True
    BlockedOnWrite{}        -> True
    BlockedOnDelay{}        -> True
    BlockedOnBlackHole{}    -> False
    BlockedOnForeignCall{}  -> False

{-
interruptible(StgTSO *t)
{
  switch (t->why_blocked) {
  case BlockedOnMVar:
  case BlockedOnSTM:
  case BlockedOnMVarRead:
  case BlockedOnMsgThrowTo:
  case BlockedOnRead:
  case BlockedOnWrite:
#if defined(mingw32_HOST_OS)
  case BlockedOnDoProc:
#endif
  case BlockedOnDelay:
    return 1;
  // NB. Threaded blocked on foreign calls (BlockedOnCCall) are
  // *not* interruptible.  We can't send these threads an exception.
  default:
    return 0;
  }
}
-}
{-

data BlockReason
  = BlockedOnMVar         Int (Maybe Atom) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int       -- mvar id
  | BlockedOnBlackHole
  | BlockedOnThrowAsyncEx Int Atom  -- target thread id, exception
  | BlockedOnSTM
  | BlockedOnForeignCall            -- RTS name: BlockedOnCCall
  | BlockedOnRead         Int       -- file descriptor
  | BlockedOnWrite        Int       -- file descriptor
  | BlockedOnDelay        UTCTime   -- target time to wake up thread
  deriving (Eq, Ord, Show)

data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished  -- RTS name: ThreadComplete
  | ThreadDied      -- RTS name: ThreadKilled

  , tsBlockedExceptions :: [Int] -- ids of the threads waitng to send an async exception
  , tsBlockExceptions   :: !Bool  -- block async exceptions
  , tsInterruptible     :: !Bool  -- interruptible blocking of async exception
-}
tryRaiseBlockedException :: [Atom] -> M [Atom]
tryRaiseBlockedException result = do
  pure []

getNextRunnableThread :: M Int
getNextRunnableThread = do
  -- HINT: drop current thread id
  tidQueue <- gets $ drop 1 . ssScheduledThreadIds
  modify' $ \s@StgState{..} -> s {ssScheduledThreadIds = tidQueue}
  case tidQueue of
    [] -> head <$> calculateNewSchedule
    tid : _ -> do
      ts <- getThreadState tid
      if tsStatus ts == ThreadRunning
        then pure tid
        else getNextRunnableThread -- HINT: try next

calculateNewSchedule :: M [Int]
calculateNewSchedule = do
  wakeUpSleepingThreads
  -- calculate the new scheduling
  tsList <- gets $ IntMap.toList . ssThreads
  promptM_ $ liftIO $ putStrLn $ "[calculateNewSchedule] - thread status list: " ++ show [(tid, tsStatus ts) | (tid, ts) <- tsList]

  let runnableThreads = [tid | (tid, ts) <- tsList, tsStatus ts == ThreadRunning]
  case runnableThreads of
    [] -> waitAndScheduleBlockedThreads
    newQueue -> do
      -- save the new scheduling
      modify' $ \s -> s {ssScheduledThreadIds = newQueue}
      promptM_ $ liftIO $ putStrLn $ "[calculateNewSchedule] - new scheduling: " ++ show newQueue
      pure newQueue

wakeUpSleepingThreads :: M ()
wakeUpSleepingThreads = do
  now <- liftIO getCurrentTime
  tsList <- gets $ IntMap.toList . ssThreads
  forM_ tsList $ \(tid, ts) -> case tsStatus ts of
    ThreadBlocked (BlockedOnDelay wakeupTime)
      | wakeupTime <= now -> do
          --liftIO $ putStrLn $ "wake up: " ++ show tid
          updateThreadState tid ts {tsStatus = ThreadRunning}
    ThreadBlocked (BlockedOnThrowAsyncEx targetTid) -> do
      targetTS <- getThreadState targetTid
      when (tsStatus targetTS `elem` [ThreadFinished, ThreadDied]) $ do
          updateThreadState tid ts {tsStatus = ThreadRunning}

    -- hack
    {-
    ThreadBlocked BlockedOnSTM
      -> do
          --liftIO $ putStrLn $ "wake up: " ++ show tid
          updateThreadState tid ts {tsStatus = ThreadRunning}
    -}
    _ -> pure ()

waitAndScheduleBlockedThreads :: M [Int]
waitAndScheduleBlockedThreads = do
  tsList <- gets $ IntMap.toList . ssThreads
  let blockedThreads = [(tid, ts) | (tid, ts) <- tsList, isBlocked (tsStatus ts)]
      isBlocked = \case
        ThreadBlocked{} -> True
        _ -> False
  promptM_ $ liftIO $ do
    putStrLn $ "[waitAndScheduleBlockedThreads] - scheduler no runnable threads"
    putStrLn "blocked threads"
    forM_ blockedThreads $ \(tid, ts) -> do
      putStrLn $ "tid: " ++ show tid ++ " status: " ++ show (tsStatus ts)

  if null blockedThreads
    then do
      error "TODO: scheduler has no runnable thread to schedule" -- nothing to run, what to do??
    else do
      handleBlockedDelayWait
      stopIfThereIsNoRunnableThread
      calculateNewSchedule

stopIfThereIsNoRunnableThread :: M ()
stopIfThereIsNoRunnableThread = do
  let checkOutOfRunnables :: M Bool
      checkOutOfRunnables = do
        -- check if there is anything to run
        tsList <- gets $ IntMap.toList . ssThreads
        let runnableThreads = [tid | (tid, ts) <- tsList, tsStatus ts == ThreadRunning]
            sleepingThreads = [tid | (tid, ts) <- tsList, canWakeUp $ tsStatus ts]
            canWakeUp = \case
              ThreadBlocked BlockedOnDelay{}  -> True
              ThreadBlocked BlockedOnRead{}   -> True
              ThreadBlocked BlockedOnWrite{}  -> True
              _ -> False
        pure (null runnableThreads && null sleepingThreads)
  noRunnable0 <- checkOutOfRunnables
  when noRunnable0 $ do
    GC.runGC [] -- will run deadlock detection
    noRunnable1 <- checkOutOfRunnables
    when noRunnable1 $ do
      --reportThreads
      tsList <- gets $ IntMap.toList . ssThreads
      liftIO $ do
        putStrLn $ "[stopIfThereIsNoRunnableThread] No runnable threads, STOP!"
        putStrLn $ "[stopIfThereIsNoRunnableThread] - all thread status list: "
        forM_ tsList $ \(tid, ts) -> do
          putStrLn $ show (tid, tsStatus ts, tsBlockExceptions ts, tsInterruptible ts, tsBlockedExceptions ts, tsLabel ts)
      dumpStgState
      modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
      Debugger.checkBreakpoint [] $ BkpCustom "thread-scheduler"

{-
  IDEA:
    chain of handlers for different blocked cases
      - delay/wait
      - other
-}

{-
  TODO:
    - handle when all threads are finished
    - handle when all threads are blocked, i.e. sleeping
    - handle when all threads are blocked indefinitely (MVar cycle)

  evalStackMachine :: [Atom] -> M [Atom]
    wrong API, the return value migh not belong to the initially executed thread


  TODO:
    - introduce eval API to the interpreter similar to GHC RTS eval API

  = BlockedOnMVar         Int (Maybe Atom) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int       -- mvar id
  | BlockedOnBlackHole
  | BlockedOnThrowAsyncEx Int Atom  -- target thread id, exception
  | BlockedOnSTM
  | BlockedOnForeignCall            -- RTS name: BlockedOnCCall
  | BlockedOnRead         Int       -- file descriptor
  | BlockedOnWrite        Int       -- file descriptor
  | BlockedOnDelay        UTCTime   -- target time to wake up thread
-}
