{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.ErrorReport where

import qualified Data.Map as Map
import GHC.Stack

import Control.Effect.State
import Control.Effect.Lift
import Control.Monad.Loops

import Stg.Syntax
import Stg.Interpreter.Monad
import Stg.Interpreter.State.Stack
import Stg.Interpreter.State.Thread
import Stg.Interpreter.State.StgState
import Stg.Interpreter.State.Allocator

stgErrorM :: M sig m => String -> m a
stgErrorM msg = do
  tid <- gets ssCurrentThreadId
  sendIO $ putStrLn $ " * stgErrorM: " ++ show msg
  sendIO $ putStrLn $ "current thread id: " ++ show tid
  reportThread tid
  --action <- unPrintable <$> gets ssStgErrorAction
  --action
  error "stgErrorM"

reportThreads :: M sig m => m ()
reportThreads = do
  threadIds <- Map.keys <$> gets ssThreads
  sendIO $ putStrLn $ "thread Ids: " ++ show threadIds
  mapM_ reportThread threadIds

reportThread :: M sig m => ThreadAddr -> m ()
reportThread tid = do
  endTS <- getThreadStateErr tid
  tsStack <- getThreadStack tid
  sendIO $ reportThreadIO tid endTS tsStack

getThreadStack :: M sig m => ThreadAddr -> m [StackContinuation]
getThreadStack tid = do
  ThreadState{..} <- getThreadStateErr tid
  flip unfoldrM tsStackTop $ \case
    Nothing       -> pure Nothing
    Just stackTop -> Just <$> getStackFrameErr stackTop

reportThreadIO :: ThreadAddr -> ThreadState -> [StackContinuation] -> IO ()
reportThreadIO tid endTS tsStack = do
    putStrLn ""
    putStrLn $ show ("tid", tid, "tsStatus", tsStatus endTS)
    putStrLn "stack:"
    putStrLn $ unlines $ map show $ zip [0..] $ map showStackCont tsStack
    putStrLn ""

showStackCont :: StackContinuation -> String
showStackCont = \case
  CaseOf _ b _ _ -> "CaseOf, result var: " ++ show (Id b)
  c -> show c

-- TODO: remove this code duplication below

getThreadStateErr :: (HasCallStack, M sig m) => ThreadAddr -> m ThreadState
getThreadStateErr tid = do
  Map.lookup tid <$> gets ssThreads >>= \case
    Nothing -> stgErrorM $ "unknown ThreadState: " ++ show tid
    Just a  -> pure a

getStackFrameErr :: M sig m => StackAddr -> m (StackContinuation, Maybe StackAddr)
getStackFrameErr stackAddr = do
  gets (Map.lookup stackAddr . ssStack) >>= \case
    Nothing     -> stgErrorM $ "missing stack frame at address: " ++ show stackAddr
    Just frame  -> pure frame
