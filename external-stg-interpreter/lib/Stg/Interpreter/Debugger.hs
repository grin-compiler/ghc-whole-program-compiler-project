
module Stg.Interpreter.Debugger where

import           Control.Applicative                   (Applicative (..))
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import           Control.Concurrent.MVar               (putMVar, takeMVar, tryTakeMVar)
import           Control.Monad                         (Functor (..), Monad (..), unless)
import           Control.Monad.State                   (MonadIO (..), gets, modify')

import           Data.Bool                             (Bool (..), otherwise)
import           Data.Enum                             (Enum (..))
import           Data.Function                         (($))
import qualified Data.IntMap                           as IntMap
import           Data.List                             ((++))
import qualified Data.Map                              as Map
import           Data.Maybe                            (Maybe (..), maybe)
import           Data.Ord                              (Ord (..))
import qualified Data.Set                              as Set

import           GHC.Stack                             (HasCallStack)

import           Stg.Interpreter.Base                  (Atom (..), Breakpoint, DebugCommand (..), DebugEvent (..),
                                                        DebugOutput (..), DebugState (..), DebuggerChan (..), M,
                                                        StgState (..), readHeap)
import           Stg.Interpreter.Debugger.Internal     (runInternalCommand)

import           System.IO                             (putStrLn)

import           Text.Show                             (Show (show))


sendDebugEvent :: DebugEvent -> M ()
sendDebugEvent dbgEvent = do
  DebuggerChan{..} <- gets ssDebuggerChan
  liftIO $ Unagi.writeChan dbgAsyncEventIn dbgEvent

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
      modify' $ \s -> s {ssEvaluatedClosures = Set.empty}
      liftIO $ putMVar dbgSyncResponse DbgOut

    CmdListClosures -> do
      closures <- gets ssEvaluatedClosures
      liftIO $ putMVar dbgSyncResponse $ DbgOutClosureList $ Set.toList closures

    CmdAddBreakpoint n i -> do
      modify' $ \s@StgState{..} -> s {ssBreakpoints = Map.insert n i ssBreakpoints}
      liftIO $ putMVar dbgSyncResponse DbgOut

    CmdRemoveBreakpoint n -> do
      modify' $ \s@StgState{..} -> s {ssBreakpoints = Map.delete n ssBreakpoints}
      liftIO $ putMVar dbgSyncResponse DbgOut

    CmdStep -> liftIO $ putMVar dbgSyncResponse DbgOut

    CmdContinue -> do
      modify' $ \s -> s {ssDebugState = DbgRunProgram}
      liftIO $ putMVar dbgSyncResponse DbgOut

    CmdPeekHeap addr -> do
      heap <- gets ssHeap
      if IntMap.member addr heap then do
        ho <- readHeap $ HeapPtr addr
        liftIO $ putMVar dbgSyncResponse $ DbgOutHeapObject addr ho
      else
        liftIO $ putMVar dbgSyncResponse DbgOut

    CmdStop -> do
      modify' $ \s -> s {ssDebugState = DbgStepByStep}
      liftIO $ putMVar dbgSyncResponse DbgOut

    CmdInternal cmd' -> do
      runInternalCommand cmd'

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
  unless (isDebugExitCommand cmd) processCommandsUntilExit

hasFuel :: M Bool
hasFuel = do
  fuel <- gets ssDebugFuel
  modify' $ \s@StgState{..} -> s {ssDebugFuel = fmap pred ssDebugFuel, ssStepCounter = succ ssStepCounter}
  pure $ maybe True (> 0) fuel

checkBreakpoint :: [Atom] -> Breakpoint -> M ()
checkBreakpoint localEnv breakpoint = do
  modify' $ \s -> s {ssLocalEnv = localEnv}
  dbgState <- gets ssDebugState
  exit <- processCommandsNonBlocking
  shouldStep <- hasFuel
  case dbgState of
    DbgStepByStep -> do
      sendDebugEvent DbgEventStopped
      unless exit processCommandsUntilExit
    DbgRunProgram -> do
      unless shouldStep $ modify' $ \s -> s {ssDebugState = DbgStepByStep}
      bkMap <- gets ssBreakpoints
      case Map.lookup breakpoint bkMap of
        Nothing -> pure ()
        Just i
          | i > 0 -> do
              -- HINT: the breakpoint can postpone triggering for the requested time
              modify' $ \s@StgState{..} -> s {ssBreakpoints = Map.adjust pred breakpoint ssBreakpoints}

          | otherwise -> do
              -- HINT: trigger breakpoint
              liftIO $ putStrLn $ "hit breakpoint: " ++ show breakpoint
              Just _currentClosure <- gets ssCurrentClosure
              sendDebugEvent $ DbgEventHitBreakpoint breakpoint
              modify' $ \s -> s {ssDebugState = DbgStepByStep}
              unless exit processCommandsUntilExit
