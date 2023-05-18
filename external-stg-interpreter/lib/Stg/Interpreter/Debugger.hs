{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger where

import GHC.Stack
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Control.Concurrent.MVar

import Stg.Interpreter.Base
import Stg.Syntax

import Stg.Interpreter.Debugger.Internal

getNextDebugCommand :: M DebugCommand
getNextDebugCommand = do
  DebuggerChan{..} <- gets ssDebuggerChan
  liftIO $ takeMVar dbgSyncRequest

tryNextDebugCommand :: M (Maybe DebugCommand)
tryNextDebugCommand = do
  DebuggerChan{..} <- gets ssDebuggerChan
  liftIO (tryTakeMVar dbgSyncRequest)

runDebugCommand :: HasCallStack => DebugCommand -> M ()
runDebugCommand cmd = do
  liftIO $ putStrLn $ "runDebugCommand: " ++ show cmd
  DebuggerChan{..} <- gets ssDebuggerChan
  case cmd of
    CmdCurrentClosure -> do
      currentClosure <- gets ssCurrentClosure
      currentClosureAddr <- gets ssCurrentClosureAddr
      closureEnv <- gets ssCurrentClosureEnv
      liftIO $ putMVar dbgSyncResponse $ DbgOutCurrentClosure currentClosure currentClosureAddr closureEnv

    CmdClearClosureList -> do
      modify' $ \s@StgState{..} -> s {ssEvaluatedClosures = Set.empty}

    CmdListClosures -> do
      closures <- gets ssEvaluatedClosures
      liftIO $ putMVar dbgSyncResponse $ DbgOutClosureList $ Set.toList closures

    CmdAddBreakpoint n i -> do
      modify' $ \s@StgState{..} -> s {ssBreakpoints = Map.insert n i ssBreakpoints}

    CmdRemoveBreakpoint n -> do
      modify' $ \s@StgState{..} -> s {ssBreakpoints = Map.delete n ssBreakpoints}

    CmdStep -> pure ()

    CmdContinue -> do
      modify' $ \s@StgState{..} -> s {ssDebugState = DbgRunProgram}

    CmdPeekHeap addr -> do
      heap <- gets ssHeap
      when (IntMap.member addr heap) $ do
        ho <- readHeap $ HeapPtr addr
        liftIO $ putMVar dbgSyncResponse $ DbgOutHeapObject addr ho

    CmdStop -> do
      modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}

    CmdInternal cmd -> do
      runInternalCommand cmd

isDebugExitCommand :: DebugCommand -> Bool
isDebugExitCommand = \case
  CmdStep     -> True
  CmdContinue -> True
  _           -> False

processCommandsNonBlocking :: M Bool
processCommandsNonBlocking = do
  tryNextDebugCommand >>= \case
    Nothing -> pure False
    Just cmd -> do
      runDebugCommand cmd
      if isDebugExitCommand cmd
        then pure True
        else processCommandsNonBlocking

processCommandsUntilExit :: M ()
processCommandsUntilExit = do
  cmd <- getNextDebugCommand
  runDebugCommand cmd
  if isDebugExitCommand cmd
    then pure ()
    else processCommandsUntilExit

hasFuel :: M Bool
hasFuel = do
  fuel <- gets ssDebugFuel
  modify' $ \s@StgState{..} -> s {ssDebugFuel = fmap pred ssDebugFuel, ssStepCounter = succ ssStepCounter}
  pure $ maybe True (> 0) fuel

checkBreakpoint :: Name -> M ()
checkBreakpoint breakpointName = do
  dbgState <- gets ssDebugState
  exit <- processCommandsNonBlocking
  shouldStep <- hasFuel
  case dbgState of
    DbgStepByStep -> do
      reportStateAsync
      unless exit processCommandsUntilExit
    DbgRunProgram -> do
      unless shouldStep $ modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
      bkMap <- gets ssBreakpoints
      case Map.lookup breakpointName bkMap of
        Nothing -> pure ()
        Just i
          | i > 0 -> do
              -- HINT: the breakpoint can postpone triggering for the requested time
              modify' $ \s@StgState{..} -> s {ssBreakpoints = Map.adjust pred breakpointName ssBreakpoints}

          | otherwise -> do
              -- HINT: trigger breakpoint
              liftIO $ putStrLn $ "hit breakpoint: " ++ show breakpointName
              reportStateAsync
              modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
              unless exit processCommandsUntilExit
