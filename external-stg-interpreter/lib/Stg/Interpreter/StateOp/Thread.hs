{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.StateOp.Thread where

import qualified Data.IntMap as IntMap
import GHC.Stack

import Stg.Interpreter.BaseState
import Stg.Interpreter.StateOp.Allocator
import Stg.Interpreter.StateOp.Stack

-- thread operations

createThread :: M sig m => m (Int, ThreadState)
createThread = do
  let ts = ThreadState
        { tsCurrentResult     = []
        , tsStackTop          = Nothing
        , tsStatus            = ThreadRunning
        , tsBlockedExceptions = []
        , tsBlockExceptions   = False
        , tsInterruptible     = False
        , tsBound             = False
        , tsLocked            = False
        , tsCapability        = 0 -- TODO: implement capability handling
        , tsLabel             = Nothing
        }
  threadId <- freshThreadId
  threads <- gets ssThreads
  modify $ \s -> s {ssThreads = IntMap.insert threadId ts threads}
  pure (threadId, ts)

updateThreadState :: M sig m => Int -> ThreadState -> m ()
updateThreadState tid ts = do
  modify $ \s@StgState{..} -> s {ssThreads = IntMap.insert tid ts ssThreads}

getThreadState :: (HasCallStack, M sig m) => Int -> m ThreadState
getThreadState tid = do
  IntMap.lookup tid <$> gets ssThreads >>= \case
    Nothing -> stgErrorM $ "unknown ThreadState: " ++ show tid
    Just a  -> pure a

getCurrentThreadState :: M sig m => m ThreadState
getCurrentThreadState = do
  tid <- gets ssCurrentThreadId
  getThreadState tid

switchToThread :: M sig m => Int -> m () -- TODO: check what code uses this
switchToThread tid = do
  modify $ \s -> s {ssCurrentThreadId = tid}
{-
  used by:
    FFI.hs:   ffiCallbackBridge
    Base.hs:  liftIOAndBorrowStgState
-}
{-
insertThread :: Int -> ThreadState -> M ()
insertThread = updateThreadState
-}
-- NOTE: only fork# and forkOn# uses requestContextSwitch
requestContextSwitch :: M sig m => m ()
requestContextSwitch = do
  -- NOTE: the semantics does not require immediate yielding, some latency is allowed
  --        for simplicity we yield immediately
  stackPush $ RunScheduler SR_ThreadYield
{-
  used by:
    fork#   - yield
    forkOn# - yield
-}

scheduleToTheEnd :: M sig m => Int -> m ()
scheduleToTheEnd tid = do
  modify $ \s -> s {ssScheduledThreadIds = ssScheduledThreadIds s ++ [tid]}

{-
  used by:
    takeMVar#   - block
    putMVar#    - block
    readMVar#   - block
    delay#      - block
    waitRead#   - block
    waitWrite#  - block
    yield#      - yield

    fork#       - yield (adds the new thread)
    forkOn#     - yield (adds the new thread)
-}

{-
  scheduler operations:
    block
    yield
    finished

  Q: how to add a newly created thread?
     manually or via return-to-scheduler op?

TODO:
  distinct immediate reschedule and relaxed (soonish) context switch
NOTE:
  on the native stg machine the closure/basic block entry point allocates memory, so it is a safe point for context switch
  in the native code the contect switch happens at safe points
-}
