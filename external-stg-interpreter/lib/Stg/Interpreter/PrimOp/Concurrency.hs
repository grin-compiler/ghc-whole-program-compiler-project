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

    scheduleToTheEnd newTId

    -- NOTE: context switch soon, but not immediately: we don't want every forkIO to force a context-switch.
    requestContextSwitch  -- TODO: push continuation reschedule, reason request context switch

    pure [ThreadId newTId]

  -- forkOn# :: Int# -> (State# RealWorld -> (# State# RealWorld, t0 #)) -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "forkOn#", [IntV capabilityNo, ioAction, _s]) -> do
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

    scheduleToTheEnd newTId

    -- NOTE: context switch soon, but not immediately: we don't want every forkIO to force a context-switch.
    requestContextSwitch  -- TODO: push continuation reschedule, reason request context switch

    pure [ThreadId newTId]

  -- killThread# :: ThreadId# -> a -> State# RealWorld -> State# RealWorld
  ( "killThread#", [ThreadId tidTarget, exception, _s]) -> do
    tid <- gets ssCurrentThreadId
    case tid == tidTarget of
      True -> do
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

        let blockIfNotInterruptible_raiseOtherwise
              | tsBlockExceptions targetTS
              , not (tsInterruptible targetTS)  = block
              | otherwise                       = raise

            blockIfBlocked_raiseOtherwise
              | tsBlockExceptions targetTS  = block
              | otherwise                   = raise

            block = do
              -- add our thread id and exception to target's blocked excpetions queue
              updateThreadState tidTarget (targetTS {tsBlockedExceptions = tid : tsBlockedExceptions targetTS})
              -- block our thread
              myTS <- getCurrentThreadState
              updateThreadState tid (myTS {tsStatus = ThreadBlocked $ BlockedOnThrowAsyncEx tidTarget exception})
              --liftIO $ putStrLn $ " * killThread#, blocked tid: " ++ show tid
              -- push reschedule continuation, reason: block
              stackPush $ RunScheduler SR_ThreadBlocked
              pure []

            raise = do
              removeFromQueues tidTarget
              raiseAsyncEx (tsCurrentResult targetTS) tidTarget exception
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
            BlockedOnBlackHole      -> 2
            BlockedOnSTM            -> 6
            BlockedOnForeignCall    -> 10
            BlockedOnRead{}         -> 3
            BlockedOnWrite{}        -> 4
            BlockedOnDelay{}        -> 5
            BlockedOnThrowAsyncEx{} -> 12

    pure [IntV statusCode, IntV tsCapability, IntV $ if tsLocked then 1 else 0]

  _ -> fallback op args t tc


raiseAsyncEx :: [Atom] -> Int -> Atom -> M ()
raiseAsyncEx lastResult tid exception = do
  let unwindStack result stackPiece = \case
        -- no Catch stack frame is found, kill thread
        [] -> do
          ts <- getThreadState tid
          updateThreadState tid (ts {tsStack = [], tsStatus = ThreadDied})
          -- TODO: reschedule continuation??
          --stackPush $ RunScheduler SR_ThreadBlocked

        -- the thread continues with the excaption handler, also wakes up the thread if necessary
        exStack@(Catch exHandler bEx iEx : _) -> do
          ts <- getThreadState tid
          updateThreadState tid $ ts
            { tsCurrentResult   = [exception]
            , tsStack           = Apply [] : exStack -- TODO: restore the catch frames exception mask, sync exceptions do it, and according the async ex pape it should be done here also
            , tsStatus          = ThreadRunning -- HINT: whatever blocked this thread now that operation got cancelled by the async exception
            -- NOTE: Ensure that async exceptions are blocked now, so we don't get a surprise exception before we get around to executing the handler.
            , tsBlockExceptions = True
            , tsInterruptible   = iEx
            }

        -- replace Update with ApStack
        Update addr : stackTail -> do
          let apStack = ApStack
                { hoResult  = result
                , hoStack   = reverse stackPiece
                }
          store addr apStack
          let newResult = [HeapPtr addr]
          unwindStack newResult [Apply []] stackTail

        Atomically stmAction : stackTail -> do
          ts <- getCurrentThreadState
          tid <- gets ssCurrentThreadId
          -- extra validation (optional)
          when (tsTLogStack ts /= []) $ error "internal error: non-empty tsTLogStack without tsActiveTLog"
          let Just tlog = tsActiveTLog ts
          -- abandon transaction
          updateThreadState tid $ ts {tsActiveTLog = Nothing}
          unsubscribeTVarWaitQueues tid tlog
          unwindStack result (AtomicallyOp stmAction : stackPiece) stackTail

        stackHead@(CatchSTM{}) : stackTail -> do
          -- FIXME: IMO this is smeantically incorrect, but this is how it's done in the native RTS
          --  this stack frame should not persist in ApStack only AtomicallyOp, it will restart the STM transaction anyway
          ts <- getCurrentThreadState
          tid <- gets ssCurrentThreadId
          let Just tlog = tsActiveTLog ts
              tlogStackTop : tlogStackTail = tsTLogStack ts
          -- HINT: abort transaction
          unsubscribeTVarWaitQueues tid tlog
          updateThreadState tid $ ts
            { tsActiveTLog  = Just tlogStackTop
            , tsTLogStack   = tlogStackTail
            }
          unwindStack result (stackHead : stackPiece) stackTail

        stackHead@(CatchRetry{}) : stackTail -> do
          -- FIXME: IMO this is smeantically incorrect, but this is how it's done in the native RTS
          --  this stack frame should not persist in ApStack only AtomicallyOp, it will restart the STM transaction anyway
          -- HINT: abort transaction
          ts <- getCurrentThreadState
          tid <- gets ssCurrentThreadId
          let Just tlog = tsActiveTLog ts
          unsubscribeTVarWaitQueues tid tlog
          updateThreadState tid $ ts { tsTLogStack = tail $ tsTLogStack ts }
          unwindStack result (stackHead : stackPiece) stackTail

        -- collect stack frames for ApStack
        stackHead : stackTail -> do
          unwindStack result (stackHead : stackPiece) stackTail

  ts <- getThreadState tid
  unwindStack lastResult [] (tsStack ts)

removeFromQueues :: Int -> M ()
removeFromQueues tid = do
  ThreadState{..} <- getThreadState tid
  case tsStatus of
    ThreadBlocked (BlockedOnMVar m _)   -> removeFromMVarQueue tid m
    ThreadBlocked (BlockedOnMVarRead m) -> removeFromMVarQueue tid m
    _                                   -> pure ()

removeFromMVarQueue :: Int -> Int -> M ()
removeFromMVarQueue tid m = do
  let filterFun mvd@MVarDescriptor{..} = mvd {mvdQueue = filter (tid /=) mvdQueue}
  modify' $ \s@StgState{..} -> s {ssMVars = IntMap.adjust filterFun m ssMVars}
