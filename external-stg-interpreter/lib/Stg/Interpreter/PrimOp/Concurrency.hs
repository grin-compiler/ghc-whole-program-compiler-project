{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Concurrency where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import Foreign.Ptr

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = IntAtom i

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args, argsAddr) of

  -- fork# :: a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "fork#", [_ioAction, _s], [ioAction, _]) -> do
    currentTS <- getCurrentThreadState

    (newTId, newTS) <- createThread
    voidAddr <- storeNewAtom Void
    stackTop <- mkStack Nothing [Apply [voidAddr], RunScheduler SR_ThreadFinished]
    updateThreadState newTId $ newTS
      { tsCurrentResult   = [ioAction]
      , tsStackTop        = stackTop

      -- NOTE: start blocked if the current thread is blocked
      , tsBlockExceptions = tsBlockExceptions currentTS
      , tsInterruptible   = tsInterruptible currentTS
      }

    scheduleToTheEnd newTId

    -- NOTE: context switch soon, but not immediately: we don't want every forkIO to force a context-switch.
    requestContextSwitch  -- TODO: push continuation reschedule, reason request context switch

    allocAtoms [ThreadId newTId]

  -- forkOn# :: Int# -> a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "forkOn#", [IntV capabilityNo, _ioAction, _s], [_, ioAction, _]) -> do
    currentTS <- getCurrentThreadState

    (newTId, newTS) <- createThread
    voidAddr <- storeNewAtom Void
    stackTop <- mkStack Nothing [Apply [voidAddr], RunScheduler SR_ThreadFinished]
    updateThreadState newTId $ newTS
      { tsCurrentResult   = [ioAction]
      , tsStackTop        = stackTop

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

    allocAtoms [ThreadId newTId]

  -- killThread# :: ThreadId# -> a -> State# RealWorld -> State# RealWorld
  ( "killThread#", [ThreadId tidTarget, _exception, _s], [_, exception, _]) -> do
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
  ( "yield#", [_s], _) -> do
    stackPush $ RunScheduler SR_ThreadYield
    pure []

  -- myThreadId# :: State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "myThreadId#", [_s], _) -> do
    tid <- gets ssCurrentThreadId
    allocAtoms [ThreadId tid]

  -- labelThread# :: ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
  ( "labelThread#", [ThreadId tid, PtrAtom _ p, _s], _) -> do
    threadLabel <- sendIO . BS8.packCString $ castPtr p
    let setLabel ts = ts {tsLabel = Just threadLabel}
    modify $ \s@StgState{..} -> s {ssThreads = Map.adjust setLabel tid ssThreads}
    pure []

  -- isCurrentThreadBound# :: State# RealWorld -> (# State# RealWorld, Int# #)
  ( "isCurrentThreadBound#", [_s], _) -> do
    ThreadState{..} <- getCurrentThreadState
    allocAtoms [IntV $ if tsBound then 1 else 0]

  -- noDuplicate# :: State# s -> State# s
  ( "noDuplicate#", [_s], _) -> do
    -- NOTE: the stg interpreter is not parallel, so this is a no-op
    pure []

  -- threadStatus# :: ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)
  ( "threadStatus#", [ThreadId tid, _s], _) -> do
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

    allocAtoms [IntV statusCode, IntV tsCapability, IntV $ if tsLocked then 1 else 0]

  _ -> fallback op argsAddr t tc


raiseAsyncEx :: M sig m => [AtomAddr] -> ThreadAddr -> AtomAddr -> m ()
raiseAsyncEx lastResult tid exception = do
  ts@ThreadState{..} <- getThreadState tid
  let unwindStack :: M sig m =>  [AtomAddr] -> [StackContinuation] -> Maybe StackAddr -> m ()
      unwindStack result stackPiece = \case
        -- no Catch stack frame is found, kill thread
        Nothing -> do
          updateThreadState tid (ts {tsStackTop = Nothing, tsStatus = ThreadDied})
          -- TODO: reschedule continuation??
          --stackPush $ RunScheduler SR_ThreadBlocked
        currentFrameAddr@(Just stackAddr) -> do
          (stackCont, prevStackAddr) <- getStackFrame stackAddr
          case stackCont of
            -- the thread continues with the excaption handler, also wakes up the thread if necessary
            Catch exHandler bEx iEx -> do
              stackTop <- mkStack currentFrameAddr [Apply []]
              updateThreadState tid $ ts
                { tsCurrentResult   = [exception]
                , tsStackTop        = stackTop -- TODO: restore the catch frames exception mask, sync exceptions do it, and according the async ex pape it should be done here also
                , tsStatus          = ThreadRunning -- HINT: whatever blocked this thread now that operation got cancelled by the async exception
                -- NOTE: Ensure that async exceptions are blocked now, so we don't get a surprise exception before we get around to executing the handler.
                , tsBlockExceptions = True
                , tsInterruptible   = iEx
                }

            -- replace Update with ApStack
            Update addr -> do
              stackSegment <- mkStack Nothing stackPiece
              let apStack = ApStack
                    { hoResult        = result
                    , hoStackPieceTop = stackSegment
                    }
              store addr apStack
              newResult <- allocAtoms [HeapPtr addr]
              unwindStack newResult [] prevStackAddr

            -- collect stack frames for ApStack
            _ -> unwindStack result (stackCont : stackPiece) prevStackAddr

  unwindStack lastResult [] tsStackTop

removeFromQueues :: M sig m => ThreadAddr -> m ()
removeFromQueues tid = do
  ThreadState{..} <- getThreadState tid
  case tsStatus of
    ThreadBlocked (BlockedOnMVar m _)   -> removeFromMVarQueue tid m
    ThreadBlocked (BlockedOnMVarRead m) -> removeFromMVarQueue tid m
    _                                   -> pure ()

removeFromMVarQueue :: M sig m => ThreadAddr -> MVarAddr -> m ()
removeFromMVarQueue tid m = do
  let filterFun mvd@MVarDescriptor{..} = mvd {mvdQueue = filter (tid /=) mvdQueue}
  modify $ \s@StgState{..} -> s {ssMVars = Map.adjust filterFun m ssMVars}
