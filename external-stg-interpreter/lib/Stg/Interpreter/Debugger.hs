{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger where

import Control.Monad.State
import qualified Data.Set as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi

import Stg.Interpreter.Base
import Stg.Syntax

runDebugCommand :: DebugCommand -> M ()
runDebugCommand cmd = do
  liftIO $ putStrLn $ "runDebugCommand: " ++ show cmd
  (_, dbgOut) <- getDebuggerChan <$> gets ssDebuggerChan
  case cmd of
    CmdCurrentClosure -> do
      Id currentClosure <- gets ssCurrentClosure
      liftIO $ Unagi.writeChan dbgOut $ DbgOutCurrentClosure $ binderUniqueName currentClosure

    CmdClearClosureList -> do
      modify' $ \s@StgState{..} -> s {ssEvaluatedClosures = Set.empty}

    CmdListClosures -> do
      closures <- gets ssEvaluatedClosures
      liftIO $ Unagi.writeChan dbgOut $ DbgOutClosureList $ Set.toList closures

    CmdAddBreakpoint n -> do
      modify' $ \s@StgState{..} -> s {ssBreakpoints = setInsert n ssBreakpoints}

    CmdRemoveBreakpoint n -> do
      modify' $ \s@StgState{..} -> s {ssBreakpoints = Set.delete n ssBreakpoints}
    _ -> do
      liftIO $ putStrLn $ "ignore: " ++ show cmd

queryNextDebugCommand :: M ()
queryNextDebugCommand = do
  (dbgCmd, _dbgOut) <- getDebuggerChan <$> gets ssDebuggerChan
  nextCmd <- liftIO $ Unagi.tryReadChan dbgCmd
  modify' $ \s@StgState{..} -> s {ssNextDebugCommand = NextDebugCommand nextCmd}

runIncomingDebugCommands :: M ()
runIncomingDebugCommands = do
  (nextCmd, _) <- getNextDebugCommand <$> gets ssNextDebugCommand
  liftIO (Unagi.tryRead nextCmd) >>= \case
    Nothing -> pure ()
    Just c  -> do
      runDebugCommand c
      queryNextDebugCommand
      runIncomingDebugCommands

runDebugCommandsBlocking :: M ()
runDebugCommandsBlocking = do
  (_, nextCmd) <- getNextDebugCommand <$> gets ssNextDebugCommand
  queryNextDebugCommand
  liftIO nextCmd >>= \case
    CmdStep -> do
      runIncomingDebugCommands

    CmdContinue -> do
      modify' $ \s@StgState{..} -> s {ssDebugState = DbgRunProgram}
      runIncomingDebugCommands

    cmd -> do
      runDebugCommand cmd
      runDebugCommandsBlocking

checkBreakpoint :: Id -> M ()
checkBreakpoint (Id b) = do
  let closureName = binderUniqueName b
  markClosure closureName

  runIncomingDebugCommands

  gets ssDebugState >>= \case
    DbgStepByStep -> do
      reportState
      runDebugCommandsBlocking
    DbgRunProgram -> do
      bkSet <- gets ssBreakpoints
      when (Set.member closureName bkSet) $ do
        reportState
        modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
        runDebugCommandsBlocking

reportState = do
  tid <- gets ssCurrentThreadId
  currentClosureName <- gets ssCurrentClosure
  reportThread tid
  liftIO $ do
    putStrLn $ " * breakpoint, thread id: " ++ show tid ++ ", current closure: " ++ show currentClosureName
