{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Concurrency where

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IntMap
import Foreign.Ptr

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = IntAtom i

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- fork# :: (State# RealWorld -> (# State# RealWorld, t0 #)) -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "fork#", [ioAction, _s]) -> do
    promptM $ print (op, args)
    currentTS <- getCurrentThreadState

    (newTId, newTS) <- createThread
    updateThreadState newTId $ newTS
      { tsCurrentResult   = [ioAction]
      , tsStack           = [Apply [Void], RunScheduler SR_ThreadFinished]

      -- NOTE: start blocked if the current thread is blocked
      , tsBlockExceptions = tsBlockExceptions currentTS
      , tsInterruptible   = tsInterruptible currentTS
      }
    --liftIO $ putStrLn $ "set mask - " ++ show newTId ++ " fork# b:" ++ show (tsBlockExceptions currentTS) ++ " i:" ++ show (tsInterruptible currentTS)
    mylog $ "fork# new-tid: " ++ show newTId

    scheduleToTheEnd newTId

    -- NOTE: context switch soon, but not immediately: we don't want every forkIO to force a context-switch.
    requestContextSwitch  -- TODO: push continuation reschedule, reason request context switch

    pure [ThreadId newTId]

  -- forkOn# :: Int# -> (State# RealWorld -> (# State# RealWorld, t0 #)) -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "forkOn#", [IntV capabilityNo, ioAction, _s]) -> do
    mylog "forkOn#"
    promptM $ print (op, args)
    currentTS <- getCurrentThreadState

    (newTId, newTS) <- createThread
    updateThreadState newTId $ newTS
      { tsCurrentResult   = [ioAction]
      , tsStack           = [Apply [Void], RunScheduler SR_ThreadFinished]

      -- NOTE: start blocked if the current thread is blocked
      , tsBlockExceptions = tsBlockExceptions currentTS
      , tsInterruptible   = tsInterruptible currentTS

      -- NOTE: capability related
      , tsLocked          = True          -- HINT: do not move this thread across capabilities
      , tsCapability      = capabilityNo
      }
    --liftIO $ putStrLn $ "set mask - " ++ show newTId ++ " forkOn# b:" ++ show (tsBlockExceptions currentTS) ++ " i:" ++ show (tsInterruptible currentTS)

    scheduleToTheEnd newTId

    -- NOTE: context switch soon, but not immediately: we don't want every forkIO to force a context-switch.
    requestContextSwitch  -- TODO: push continuation reschedule, reason request context switch

    pure [ThreadId newTId]

  -- killThread# :: ThreadId# -> a -> State# RealWorld -> State# RealWorld
  ( "killThread#", [ThreadId tidTarget, exception, _s]) -> do
    tid <- gets ssCurrentThreadId
    mylog $ "killThread# current-tid: " ++ show tid ++ " target-tid: " ++ show tidTarget
    case tid == tidTarget of
      True -> do
        mylog "killMyself"
        -- killMyself
        {-
          the thread might survive
            Q: how?
            A: a catch frame can save the thread so that it will handle the exception
        -}
        removeFromQueues tidTarget
        let myResult = [] -- HINT: this is the result of the killThread# primop
        raiseAsyncEx myResult tidTarget exception

        -- TODO: remove this below, problem: raiseAsyncEx may kill the thread ; model kill thread as return to scheduler operation with a descriptive reason
        -- return the result that the raise async ex has calculated
        tsCurrentResult <$> getCurrentThreadState

      False -> do
        -- kill other thread
        targetTS <- getThreadState tidTarget
        mylog $ "kill other thread " ++ show tidTarget ++ " " ++ show (tsStatus targetTS, tsBlockExceptions targetTS, tsInterruptible targetTS)

        let blockIfNotInterruptible_raiseOtherwise
              | tsBlockExceptions targetTS
              , not (tsInterruptible targetTS)  = block
              | otherwise                       = raise

            blockIfBlocked_raiseOtherwise
              | tsBlockExceptions targetTS  = block
              | otherwise                   = raise

            block = do
              mylog "block"
              -- add our thread id and exception to target's blocked excpetions queue
              updateThreadState tidTarget (targetTS {tsBlockedExceptions = (tid, exception) : tsBlockedExceptions targetTS})
              -- block our thread
              myTS <- getCurrentThreadState
              updateThreadState tid (myTS {tsStatus = ThreadBlocked $ BlockedOnThrowAsyncEx tidTarget})

              when False $ do
                reportThread tidTarget
                error $ "BlockedOnThrowAsyncEx, targetTS.tsStatus = " ++ show (tsStatus targetTS) ++ " targetTS.mask: " ++ show (tsBlockExceptions targetTS, tsInterruptible targetTS)

              --liftIO $ putStrLn $ " * killThread#, blocked tid: " ++ show tid
              -- push reschedule continuation, reason: block
              stackPush $ RunScheduler SR_ThreadBlocked
              mylog "block - end"
              --reportThread tid
              --reportThread tidTarget
              pure []

            raise = do
              mylog "raise"
              removeFromQueues tidTarget
              raiseAsyncEx (tsCurrentResult targetTS) tidTarget exception
              mylog "raise - end"
              pure []

        case tsStatus targetTS of
          ThreadFinished  -> pure []  -- NOTE: nothing to do
          ThreadDied      -> pure []  -- NOTE: nothing to do
          ThreadRunning   -> blockIfBlocked_raiseOtherwise
          ThreadBlocked blockReason -> case blockReason of

            BlockedOnForeignCall{}  -> block
            BlockedOnBlackHole{}    -> blockIfBlocked_raiseOtherwise
            BlockedOnThrowAsyncEx{} -> blockIfNotInterruptible_raiseOtherwise
            BlockedOnSTM{}          -> blockIfNotInterruptible_raiseOtherwise
            BlockedOnMVar{}         -> blockIfNotInterruptible_raiseOtherwise
            BlockedOnMVarRead{}     -> blockIfNotInterruptible_raiseOtherwise
            BlockedOnRead{}         -> blockIfNotInterruptible_raiseOtherwise
            BlockedOnWrite{}        -> blockIfNotInterruptible_raiseOtherwise
            BlockedOnDelay{}        -> blockIfNotInterruptible_raiseOtherwise

  -- yield# :: State# RealWorld -> State# RealWorld
  ( "yield#", [_s]) -> do
    stackPush $ RunScheduler SR_ThreadYield
    pure []

  -- myThreadId# :: State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "myThreadId#", [_s]) -> do
    tid <- gets ssCurrentThreadId
    pure [ThreadId tid]

  -- labelThread# :: ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
  ( "labelThread#", [ThreadId tid, PtrAtom _ p, _s]) -> do
    threadLabel <- liftIO . BS8.packCString $ castPtr p
    let setLabel ts@ThreadState{..} = ts {tsLabel = Just threadLabel}
    modify' $ \s@StgState{..} -> s {ssThreads = IntMap.adjust setLabel tid ssThreads}
    pure []

  -- isCurrentThreadBound# :: State# RealWorld -> (# State# RealWorld, Int# #)
  ( "isCurrentThreadBound#", [_s]) -> do
    ThreadState{..} <- getCurrentThreadState
    pure [IntV $ if tsBound then 1 else 0]

  -- noDuplicate# :: State# s -> State# s
  ( "noDuplicate#", [_s]) -> do
    -- NOTE: the stg interpreter is not parallel, so this is a no-op
    pure []

  -- threadStatus# :: ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)
  ( "threadStatus#", [ThreadId tid, _s]) -> do
    ThreadState{..} <- getThreadState tid
    -- HINT:  includes/rts/Constants.h
    --        base:GHC.Conc.Sync.threadStatus
    let statusCode = case tsStatus of
          ThreadRunning   -> 0
          ThreadFinished  -> 16
          ThreadDied      -> 17
          ThreadBlocked r -> case r of
            BlockedOnMVar{}         -> 1
            BlockedOnMVarRead{}     -> 14
            BlockedOnBlackHole{}    -> 2
            BlockedOnSTM{}          -> 6
            BlockedOnForeignCall    -> 10
            BlockedOnRead{}         -> 3
            BlockedOnWrite{}        -> 4
            BlockedOnDelay{}        -> 5
            BlockedOnThrowAsyncEx{} -> 12

    pure [IntV statusCode, IntV tsCapability, IntV $ if tsLocked then 1 else 0]

  _ -> fallback op args t tc


raiseAsyncEx :: [Atom] -> Int -> Atom -> M ()
raiseAsyncEx lastResult targetTid exception = do
  let unwindStack result stackPiece = \case
        -- no Catch stack frame is found, kill thread
        [] -> do
          ts <- getThreadState targetTid
          updateThreadState targetTid (ts {tsStack = [RunScheduler SR_ThreadYield], tsStatus = ThreadDied})

        -- the thread continues with the excaption handler, also wakes up the thread if necessary
        exStack@(Catch _ bEx iEx : _) -> do
          ts <- getThreadState targetTid
          updateThreadState targetTid $ ts
            { tsCurrentResult   = []
            , tsStack           = RaiseOp exception : exStack
            , tsStatus          = ThreadRunning -- HINT: whatever blocked this thread now that operation got cancelled by the async exception
            -- NOTE: Ensure that async exceptions are blocked now, so we don't get a surprise exception before we get around to executing the handler.
            , tsBlockExceptions = True
            , tsInterruptible   = if bEx then iEx else True
            }
          --liftIO $ putStrLn $ "set mask - " ++ show targetTid ++ " raiseAsyncEx b:True i:" ++ show (if bEx then iEx else True)

        -- replace Update with ApStack
        Update addr : stackTail -> do
          when (result == []) $ error "internal error - result should be a [HeapPtr], but it's value is []"
          let apStack = ApStack
                { hoResult  = result
                , hoStack   = reverse stackPiece
                }
          wakeupBlackHoleQueueThreads addr
          store addr apStack
          let newResult = [HeapPtr addr]
          ctid <- gets ssCurrentThreadId
          mylog $ "raiseAsyncEx - Update " ++ show addr ++ " current-tid: " ++ show ctid ++ " target-tid: " ++ show targetTid
          unwindStack newResult [Apply []] stackTail

        Atomically stmAction : stackTail -> do
          ts <- getThreadState targetTid
          -- extra validation (optional)
          when (tsTLogStack ts /= []) $ error "internal error: non-empty tsTLogStack without tsActiveTLog"
          case tsActiveTLog ts of
            Nothing -> do
              liftIO $ putStrLn $ "internal error, tsActiveTLog == Nothing for tid: " ++ show targetTid
              reportThread targetTid
              error "internal error"
            _ -> pure ()
          let Just tlog = tsActiveTLog ts
          -- HINT: abort transaction, do not need to unsubscribe, because it was already done in killThread# before it called raiseAsyncEx
          updateThreadState targetTid $ ts {tsActiveTLog = Nothing}
          unwindStack result (AtomicallyOp stmAction : stackPiece) stackTail

        stackHead@(CatchSTM{}) : stackTail -> do
          -- FIXME: IMO this is smeantically incorrect, but this is how it's done in the native RTS
          --  this stack frame should not persist in ApStack only AtomicallyOp, it will restart the STM transaction anyway
          ts <- getThreadState targetTid
          case tsActiveTLog ts of
            Nothing -> do
              liftIO $ putStrLn $ "internal error, tsActiveTLog == Nothing for tid: " ++ show targetTid
              reportThread targetTid
              error "internal error"
            _ -> pure ()
          let Just tlog = tsActiveTLog ts
              tlogStackTop : tlogStackTail = tsTLogStack ts
          -- HINT: abort transaction, do not need to unsubscribe, because it was already done in killThread# before it called raiseAsyncEx
          mylog $ show targetTid ++ " ** raiseAsyncEx - CatchSTM"
          updateThreadState targetTid $ ts
            { tsActiveTLog  = Just tlogStackTop
            , tsTLogStack   = tlogStackTail
            }
          unwindStack result (stackHead : stackPiece) stackTail

        stackHead@(CatchRetry{}) : stackTail -> do
          -- FIXME: IMO this is smeantically incorrect, but this is how it's done in the native RTS
          --  this stack frame should not persist in ApStack only AtomicallyOp, it will restart the STM transaction anyway
          -- HINT: abort transaction, do not need to unsubscribe, because it was already done in killThread# before it called raiseAsyncEx
          ts <- getThreadState targetTid
          case tsActiveTLog ts of
            Nothing -> do
              liftIO $ putStrLn $ "internal error, tsActiveTLog == Nothing for tid: " ++ show targetTid
              reportThread targetTid
              error "internal error"
            _ -> pure ()
          let Just tlog = tsActiveTLog ts
          updateThreadState targetTid $ ts { tsTLogStack = tail $ tsTLogStack ts }
          unwindStack result (stackHead : stackPiece) stackTail

        -- collect stack frames for ApStack
        stackHead : stackTail -> do
          unwindStack result (stackHead : stackPiece) stackTail

  ts <- getThreadState targetTid
  unwindStack lastResult [] (tsStack ts)

removeFromQueues :: Int -> M ()
removeFromQueues tid = do
  ThreadState{..} <- getThreadState tid
  -- Q: what about the async exception queue?
  case tsStatus of
    ThreadRunning                           -> pure ()
    ThreadBlocked (BlockedOnMVar m _)       -> removeFromMVarQueue tid m
    ThreadBlocked (BlockedOnMVarRead m)     -> removeFromMVarQueue tid m
    ThreadBlocked (BlockedOnSTM tlog)       -> unsubscribeTVarWaitQueues tid tlog
    ThreadBlocked BlockedOnDelay{}          -> pure () -- HINT: no queue for delays
    ThreadBlocked BlockedOnRead{}           -> pure () -- HINT: no queue for file read
    ThreadBlocked BlockedOnWrite{}          -> pure () -- HINT: no queue for file write
    ThreadBlocked BlockedOnThrowAsyncEx{}   -> pure () -- Q: what to do?
    ThreadBlocked (BlockedOnBlackHole addr) -> removeFromBlackHoleQueue tid addr
    _ -> error $ "TODO: removeFromQueues " ++ show tsStatus

removeFromMVarQueue :: Int -> Int -> M ()
removeFromMVarQueue tid m = do
  let filterFun mvd@MVarDescriptor{..} = mvd {mvdQueue = filter (tid /=) mvdQueue}
  modify' $ \s@StgState{..} -> s {ssMVars = IntMap.adjust filterFun m ssMVars}

removeFromBlackHoleQueue :: Int -> Int -> M ()
removeFromBlackHoleQueue tid addr = do
  readHeap (HeapPtr addr) >>= \case
    bh@BlackHole{..}  -> modify' $ \s@StgState{..} -> s { ssHeap = IntMap.insert addr (bh {hoBHWaitQueue = filter (tid /=) hoBHWaitQueue}) ssHeap }
    x                 -> error $ "internal error - expected BlackHole, got: " ++ show x
