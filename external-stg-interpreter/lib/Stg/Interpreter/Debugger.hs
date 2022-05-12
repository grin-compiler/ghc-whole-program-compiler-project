{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger where

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import Text.Printf

import Stg.Interpreter.Base
import Stg.Syntax

import Stg.Interpreter.Debugger.Internal

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
      currentClosure <- gets ssCurrentClosure
      currentClosureAddr <- gets ssCurrentClosureAddr
      closureEnv <- gets ssCurrentClosureEnv
      liftIO $ Unagi.writeChan dbgOut $ DbgOutCurrentClosure currentClosure currentClosureAddr closureEnv

    CmdClearClosureList -> do
      modify' $ \s@StgState{..} -> s {ssEvaluatedClosures = Set.empty}

    CmdListClosures -> do
      closures <- gets ssEvaluatedClosures
      liftIO $ Unagi.writeChan dbgOut $ DbgOutClosureList $ Set.toList closures

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
        liftIO $ Unagi.writeChan dbgOut $ DbgOutHeapObject addr ho

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

printEnv :: Env -> IO ()
printEnv env = do
  let unBinderId (BinderId u) = u
      l = maximum . map (\(Id Binder{..}) -> sum [BS8.length $ getModuleName binderModule, 2, BS8.length binderName, 1, length $ show $ unBinderId binderId]) $ Map.keys env

      showItem (n@(Id Binder{..}), v) = printf ("  %-" ++ show l ++ "s  =  %s") (mod ++ "  " ++ name) (show v)
        where
          BinderId u  = binderId
          name        = BS8.unpack binderName ++ ('_' : show u)
          mod         = BS8.unpack $ getModuleName binderModule
      str = List.sort $ map showItem $ Map.toList env
  putStrLn $ unlines str

checkBreakpoint :: Env -> Name -> M ()
checkBreakpoint localEnv breakpointName = do
  dbgState <- gets ssDebugState
  exit <- processCommandsNonBlocking
  case dbgState of
    DbgStepByStep -> do
      reportState
      unless exit processCommandsUntilExit
    DbgRunProgram -> do
      bkMap <- gets ssBreakpoints
      case Map.lookup breakpointName bkMap of
        Nothing -> pure ()
        Just i
          | i > 0 -> do
              -- HINT: the breakpoint can postpone triggering for the requested time
              modify' $ \s@StgState{..} -> s {ssBreakpoints = Map.adjust pred breakpointName ssBreakpoints}

          | otherwise -> do
              -- HINT: trigger breakpoint
              liftIO $ putStrLn $ "Active breakpoint:" ++ BS8.unpack breakpointName
              liftIO $ printEnv localEnv
              reportState
              modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
              unless exit processCommandsUntilExit
