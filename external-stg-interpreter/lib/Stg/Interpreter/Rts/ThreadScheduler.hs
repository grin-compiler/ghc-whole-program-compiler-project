{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Rts.ThreadScheduler where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock

import Stg.Interpreter.Base
import Stg.Interpreter.Rts.IOManager

runScheduler :: M sig m => [AtomAddr] -> ScheduleReason -> m [AtomAddr]
runScheduler result sr = do
  tid <- gets ssCurrentThreadId
  --liftIO $ putStrLn $ " * scheduler: " ++ show sr ++ " thread: " ++ show tid
  case sr of
    SR_ThreadFinished -> do
      -- set thread status to finished
      ts <- getThreadState tid
      updateThreadState tid ts {tsStatus = ThreadFinished}
      yield result -- HINT: will save the result
{-
  NOTE: disable temporarly to make sure only independent threads are allowed

    SR_ThreadBlocked  -> yield result

    SR_ThreadYield    -> yield result
-}

yield :: M sig m => [AtomAddr] -> m [AtomAddr]
yield result = do
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  -- save result
  updateThreadState tid ts {tsCurrentResult = result}

  wakeUpSleepingThreads

  -- lookup next thread
  nextTid <- getNextRunnableThread
  --liftIO $ putStrLn $ " * scheduler next runnable thread: " ++ show nextTid

  -- switchToThread
  switchToThread nextTid

  -- return threads current result
  nextTS <- getThreadState nextTid

  -- TODO/IDEA/IMPROVEMENT:
  --    store this value in the thread state, but only for the suspended states
  --    the running threads should not store the old "current result" that would prevent garbage collection
  -- HINT: clear value to allow garbage collection
  updateThreadState nextTid nextTS {tsCurrentResult = []}

  pure $ tsCurrentResult nextTS

getNextRunnableThread :: M sig m => m ThreadAddr
getNextRunnableThread = do
  -- HINT: drop current thread id
  tidQueue <- gets $ drop 1 . ssScheduledThreadIds
  modify $ \s@StgState{..} -> s {ssScheduledThreadIds = tidQueue}
  case tidQueue of
    [] -> head <$> calculateNewSchedule
    tid : _ -> do
      ts <- getThreadState tid
      if tsStatus ts == ThreadRunning
        then pure tid
        else getNextRunnableThread -- HINT: try next

calculateNewSchedule :: M sig m => m [ThreadAddr]
calculateNewSchedule = do
  wakeUpSleepingThreads
  -- calculate the new scheduling
  tsList <- gets $ Map.toList . ssThreads
  let runnableThreads = [tid | (tid, ts) <- tsList, tsStatus ts == ThreadRunning]
  case runnableThreads of
    [] -> waitAndScheduleBlockedThreads
    newQueue -> do
      -- save the new scheduling
      modify $ \s -> s {ssScheduledThreadIds = newQueue}
      --liftIO $ putStrLn $ "new scheduling: " ++ show newQueue
      pure newQueue

wakeUpSleepingThreads :: M sig m => m ()
wakeUpSleepingThreads = do
  now <- sendIO getCurrentTime
  tsList <- gets $ Map.toList . ssThreads
  forM_ tsList $ \(tid, ts) -> case tsStatus ts of
    ThreadBlocked (BlockedOnDelay wakeupTime)
      | wakeupTime >= now -> updateThreadState tid ts {tsStatus = ThreadRunning}
    _ -> pure ()

waitAndScheduleBlockedThreads :: M sig m => m [ThreadAddr]
waitAndScheduleBlockedThreads = do
  tsList <- gets $ Map.toList . ssThreads
  let blockedThreads = [(tid, ts) | (tid, ts) <- tsList, isBlocked (tsStatus ts)]
      isBlocked = \case
        ThreadBlocked{} -> True
        _ -> False
  {-
  liftIO $ do
    putStrLn $ " * scheduler no runnable threads"
    putStrLn "blocked threads"
    forM_ blockedThreads $ \(tid, ts) -> do
      putStrLn $ "tid: " ++ show tid ++ " status: " ++ show (tsStatus ts)
  -}
  if null blockedThreads
    then do
      -- error "TODO: scheduler has n runnable thread to schedule" -- nothing to run, what to do??
      modify $ \s -> s {ssScheduledThreadIds = []}
      -- HACK:
      pure $ map fst tsList
    else do
      handleBlockedDelayWait
      calculateNewSchedule

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
