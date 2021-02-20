{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.DelayWait where

import Control.Monad.State
import Data.Time.Clock
import Data.Fixed

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = IntAtom i

{-
  NOTE:
    these primops are only used by programs that are linked with the non-concurrent RTS
    in the multithreded RTS mode they are not used / invalid (in the GHC implementation)
    this is an ugly design, needs to be fixed in the future!
-}

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- delay# :: Int# -> State# s -> State# s
  ( "delay#", [IntV usDelay, _s]) -> do
    -- safety check
    ts@ThreadState{..} <- getCurrentThreadState
    unless (tsStatus == ThreadRunning) $
      error $ "expected running thread status, but got: " ++ show tsStatus

    -- calculate target time
    t0 <- liftIO getCurrentTime
    let delayTime   = secondsToNominalDiffTime $ (fromIntegral usDelay :: Pico) / 1000000
        targetTime  = addUTCTime delayTime t0

    -- set blocked reason
    tid <- gets ssCurrentThreadId
    updateThreadState tid (ts {tsStatus = ThreadBlocked $ BlockedOnDelay targetTime})
    --liftIO $ putStrLn $ show tid ++ "  (blocked) delay# " ++ show args

    -- reschedule threads
    stackPush $ RunScheduler SR_ThreadBlocked
    pure []

  -- waitRead# :: Int# -> State# s -> State# s
  ( "waitRead#", [IntV fd, _s]) -> do
    -- safety check
    ts@ThreadState{..} <- getCurrentThreadState
    unless (tsStatus == ThreadRunning) $
      error $ "expected running thread status, but got: " ++ show tsStatus

    -- set blocked reason
    tid <- gets ssCurrentThreadId
    updateThreadState tid (ts {tsStatus = ThreadBlocked $ BlockedOnRead fd})
    --liftIO $ putStrLn $ show tid ++ "  (blocked) waitRead# " ++ show args

    -- reschedule threads
    stackPush $ RunScheduler SR_ThreadBlocked
    pure []

  -- waitWrite# :: Int# -> State# s -> State# s
  ( "waitWrite#", [IntV fd, _s]) -> do
    -- safety check
    ts@ThreadState{..} <- getCurrentThreadState
    unless (tsStatus == ThreadRunning) $
      error $ "expected running thread status, but got: " ++ show tsStatus

    -- set blocked reason
    tid <- gets ssCurrentThreadId
    updateThreadState tid (ts {tsStatus = ThreadBlocked $ BlockedOnWrite fd})
    --liftIO $ putStrLn $ show tid ++ "  (blocked) waitWrite# " ++ show args

    -- reschedule threads
    stackPush $ RunScheduler SR_ThreadBlocked
    pure []

  _ -> fallback op args t tc
