{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.StateOp.Stack where

import qualified Data.IntMap as IntMap

import GHC.Stack
import Data.Foldable
import Stg.Interpreter.BaseState
import Stg.Interpreter.StateOp.Allocator

-- stack operations

getStackFrame :: M sig m => Addr -> m (StackContinuation, Maybe Addr)
getStackFrame stackAddr = do
  gets (IntMap.lookup stackAddr . ssStack) >>= \case
    Nothing     -> stgErrorM $ "missing stack frame at address: " ++ show stackAddr
    Just frame  -> pure frame

stackPush :: M sig m => StackContinuation -> m ()
stackPush sc = do
  a <- freshStackAddress
  cts <- getCurrentThreadState0
  let pushFun ts@ThreadState{..} = ts {tsStackTop = Just a}
      stackFrame = (sc, tsStackTop cts)
  modify $ \s@StgState{..} -> s
    { ssThreads = IntMap.adjust pushFun ssCurrentThreadId ssThreads
    , ssStack = IntMap.insert a stackFrame ssStack
    }

stackPop :: M sig m => m (Maybe StackContinuation)
stackPop = do
  cts <- getCurrentThreadState0
  case tsStackTop cts of
    Nothing -> pure Nothing
    Just oldStackTop -> do
      (sc, newStackTop) <- getStackFrame oldStackTop
      let popFun ts@ThreadState{..} = ts {tsStackTop = newStackTop}
      modify $ \s@StgState{..} -> s
        { ssThreads = IntMap.adjust popFun ssCurrentThreadId ssThreads
        }
      pure $ Just sc

-- HINT: frame list order: [stack-bottom..stack-top]
mkStack :: M sig m => Maybe Addr -> [StackContinuation] -> m (Maybe Addr)
mkStack prevFrameAddr frames = do
  let pushFrame :: M sig m => Maybe Addr -> StackContinuation -> m (Maybe Addr)
      pushFrame stackTop sc = do
        a <- freshStackAddress
        let stackFrame = (sc, stackTop)
        modify $ \s@StgState{..} -> s {ssStack = IntMap.insert a stackFrame ssStack}
        pure $ Just a
  foldlM pushFrame prevFrameAddr frames

-- HINT: result stack frame list order: [stack-bottom..stack-top]
getStackSegment :: M sig m => [StackContinuation] -> Maybe StackAddr -> Maybe StackAddr -> m [StackContinuation]
getStackSegment frames start endExclusive | start == endExclusive = pure frames
getStackSegment frames (Just addr) endExclusive = do
    (stackCont, prevStackAddr) <- getStackFrame addr
    getStackSegment (stackCont : frames) prevStackAddr endExclusive
getStackSegment _ start@Nothing end = error $ "invalid stack segment range, start: " ++ show start ++ " end: " ++ show end

----------------
-- TODO: remove this and fix code duplication

getThreadState0 :: (HasCallStack, M sig m) => Int -> m ThreadState
getThreadState0 tid = do
  IntMap.lookup tid <$> gets ssThreads >>= \case
    Nothing -> stgErrorM $ "unknown ThreadState: " ++ show tid
    Just a  -> pure a

getCurrentThreadState0 :: M sig m => m ThreadState
getCurrentThreadState0 = do
  tid <- gets ssCurrentThreadId
  getThreadState0 tid
