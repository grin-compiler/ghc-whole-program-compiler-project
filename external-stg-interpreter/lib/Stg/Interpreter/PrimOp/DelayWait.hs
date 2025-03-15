module Stg.Interpreter.PrimOp.DelayWait where

import           Control.Applicative  (Applicative (..))
import           Control.Monad        (unless)
import           Control.Monad.State  (MonadIO (..), gets)

import           Data.Eq              (Eq (..))
import           Data.Fixed           (Pico)
import           Data.Function        (($))
import           Data.Int             (Int)
import           Data.List            ((++))
import           Data.Maybe           (Maybe)
import           Data.Time.Clock      (addUTCTime, getCurrentTime, secondsToNominalDiffTime)

import           GHC.Err              (error)
import           GHC.Real             (Fractional (..), fromIntegral)

import           Stg.Interpreter.Base (Atom (..), BlockReason (..), M, PrimOpEval, ScheduleReason (..),
                                       StackContinuation (..), StgState (..), ThreadState (..), ThreadStatus (..),
                                       getCurrentThreadState, stackPush, updateThreadState)
import           Stg.Syntax           (Name, TyCon, Type)

import           Text.Show            (Show (..))

pattern IntV :: Int -> Atom
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
