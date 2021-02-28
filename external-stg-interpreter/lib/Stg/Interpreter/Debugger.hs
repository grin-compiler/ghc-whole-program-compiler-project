{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi

import Stg.Interpreter.Base
import Stg.Syntax

fetchNextDebugCommand :: M ()
fetchNextDebugCommand = do
  (dbgCmd, _dbgOut) <- getDebuggerChan <$> gets ssDebuggerChan
  nextCmd <- liftIO $ Unagi.tryReadChan dbgCmd
  modify' $ \s@StgState{..} -> s {ssNextDebugCommand = NextDebugCommand nextCmd}

getNextDebugCommand :: M DebugCommand
getNextDebugCommand = do
  NextDebugCommand (_, nextCmd) <- gets ssNextDebugCommand
  fetchNextDebugCommand
  liftIO nextCmd

tryNextDebugCommand :: M (Maybe DebugCommand)
tryNextDebugCommand = do
  NextDebugCommand (nextCmd, _) <- gets ssNextDebugCommand
  liftIO (Unagi.tryRead nextCmd) >>= \case
    Nothing -> pure Nothing
    c@Just{} -> do
      fetchNextDebugCommand
      pure c

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

    CmdStep -> pure ()

    CmdContinue -> do
      modify' $ \s@StgState{..} -> s {ssDebugState = DbgRunProgram}

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

checkBreakpoint :: Id -> M ()
checkBreakpoint (Id b) = do
  let closureName = binderUniqueName b
  markClosure closureName

  dbgState <- gets ssDebugState

  exit <- processCommandsNonBlocking

  case dbgState of
    DbgStepByStep -> do
      reportState
      unless exit processCommandsUntilExit
    DbgRunProgram -> do
      bkSet <- gets ssBreakpoints
      when (Set.member closureName bkSet) $ do
        reportState
        modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
        unless exit processCommandsUntilExit

reportState :: M ()
reportState = do
  tid <- gets ssCurrentThreadId
  currentClosureName <- gets ssCurrentClosure
  reportThread tid
  liftIO $ do
    putStrLn $ " * breakpoint, thread id: " ++ show tid ++ ", current closure: " ++ show currentClosureName
