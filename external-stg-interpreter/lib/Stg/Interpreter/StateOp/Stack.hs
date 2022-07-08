{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.StateOp.Stack where

import qualified Data.Map as Map

import GHC.Stack
import Data.Foldable
import Stg.Interpreter.BaseState
import Stg.Interpreter.StateOp.Allocator

-- stack operations

getStackFrame :: M sig m => StackAddr -> m (StackContinuation, Maybe StackAddr)
getStackFrame stackAddr = do
  gets (Map.lookup stackAddr . ssStack) >>= \case
    Nothing     -> stgErrorM $ "missing stack frame at address: " ++ show stackAddr
    Just frame  -> pure frame

stackPush :: M sig m => StackContinuation -> m ()
stackPush sc = do
  a <- freshStackAddress
  cts <- getCurrentThreadState0
  let pushFun ts = ts {tsStackTop = Just a}
      stackFrame = (sc, tsStackTop cts)
  modify $ \s@StgState{..} -> s
    { ssThreads = Map.adjust pushFun ssCurrentThreadId ssThreads
    , ssStack = Map.insert a stackFrame ssStack
    }

stackPop :: M sig m => m (Maybe StackContinuation)
stackPop = do
  cts <- getCurrentThreadState0
  case tsStackTop cts of
    Nothing -> pure Nothing
    Just oldStackTop -> do
      (sc, newStackTop) <- getStackFrame oldStackTop
      let popFun ts = ts {tsStackTop = newStackTop}
      modify $ \s@StgState{..} -> s
        { ssThreads = Map.adjust popFun ssCurrentThreadId ssThreads
        }
      pure $ Just sc

-- HINT: frame list order: [stack-bottom..stack-top]
mkStack :: M sig m => Maybe StackAddr -> [StackContinuation] -> m (Maybe StackAddr)
mkStack prevFrameAddr frames = do
  let pushFrame :: M sig m => Maybe StackAddr -> StackContinuation -> m (Maybe StackAddr)
      pushFrame stackTop sc = do
        a <- freshStackAddress
        let stackFrame = (sc, stackTop)
        modify $ \s@StgState{..} -> s {ssStack = Map.insert a stackFrame ssStack}
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

getThreadState0 :: (HasCallStack, M sig m) => ThreadAddr -> m ThreadState
getThreadState0 tid = do
  Map.lookup tid <$> gets ssThreads >>= \case
    Nothing -> stgErrorM $ "unknown ThreadState: " ++ show tid
    Just a  -> pure a

getCurrentThreadState0 :: M sig m => m ThreadState
getCurrentThreadState0 = do
  tid <- gets ssCurrentThreadId
  getThreadState0 tid
