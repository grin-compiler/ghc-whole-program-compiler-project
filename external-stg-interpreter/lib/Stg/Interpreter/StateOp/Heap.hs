{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.StateOp.Heap where

import qualified Data.Map as Map

import GHC.Stack
import Stg.Interpreter.BaseState
import Stg.Interpreter.StateOp.Allocator

-- heap operations

allocAndStore :: (HasCallStack, M sig m) => HeapObject -> m HeapAddr
allocAndStore o = do
  a <- freshHeapAddress
  store a o
  pure a

store :: (HasCallStack, M sig m) => HeapAddr -> HeapObject -> m ()
store a o = do
  modify $ \s@StgState{..} -> s { ssHeap = Map.insert a o ssHeap }

  {-
  gets ssTracingState >>= \case
    NoTracing   -> pure ()
    DoTracing h -> do
      origin <- gets ssCurrentClosureAddr
      sendIO $ hPutStrLn h $ show a ++ "\t" ++ show origin
  -}

{-
  conclusion:
    write origins to file to save memory
    use binary format to save space

  TODO: implement GC
          - simple (full heap traversal)
          - generational
-}

readHeap :: (HasCallStack, M sig m) => Atom -> m HeapObject
readHeap (HeapPtr l) = do
  h <- gets ssHeap
  case Map.lookup l h of
    Nothing -> stgErrorM $ "unknown heap address: " ++ show l
    Just o  -> pure o
readHeap v = error $ "readHeap: could not read heap object: " ++ show v

readHeapCon :: (HasCallStack, M sig m) => Atom -> m HeapObject
readHeapCon a = readHeap a >>= \o -> case o of
    Con{} -> pure o
    _     -> stgErrorM $ "expected con but got: "-- ++ show o

readHeapClosure :: (HasCallStack, M sig m) => Atom -> m HeapObject
readHeapClosure a = readHeap a >>= \o -> case o of
    Closure{} -> pure o
    _ -> stgErrorM $ "expected closure but got: "-- ++ show o
