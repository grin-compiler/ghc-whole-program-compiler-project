{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Interpreter.GC.DeadlockAnalysis where

import Control.Monad
import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap

import Stg.Interpreter.Base
import qualified Stg.Interpreter.PrimOp.Concurrency as PrimConcurrency

validateGCThreadResult :: RefSet -> IntSet -> M ()
validateGCThreadResult RefSet{..} deadlockedThreadIds = do

  let assertLiveThread tid = when (IntSet.notMember tid rsThreads) $ do
        reportThread tid
        error $ "internal error - thread should be live: " ++ show tid

      assertLiveOrDeadlocked tid = when (IntSet.notMember tid rsThreads && IntSet.notMember tid deadlockedThreadIds) $ do
        reportThread tid
        error $ "internal error - thread should be live or deadlocked: " ++ show tid

  stgState <- get
  forM_ (IntMap.toList $ ssThreads stgState) $ \(tid, ts) -> case tsStatus ts of
    ThreadFinished  -> pure ()
    ThreadDied      -> pure ()
    ThreadRunning   -> assertLiveThread tid
    ThreadBlocked r -> case r of
      BlockedOnMVar{}         -> assertLiveOrDeadlocked tid
      BlockedOnMVarRead{}     -> assertLiveOrDeadlocked tid
      BlockedOnBlackHole{}    -> error "not implemented yet"
      BlockedOnThrowAsyncEx{} -> error "TODO: what is this case? figure it out"
      BlockedOnSTM{}          -> assertLiveOrDeadlocked tid
      BlockedOnForeignCall{}  -> error "not implemented yet"
      BlockedOnRead{}         -> assertLiveThread tid
      BlockedOnWrite{}        -> assertLiveThread tid
      BlockedOnDelay{}        -> assertLiveThread tid
  pure ()

-- the analysis is done in datalog, this code just uses the analysis result

handleDeadlockedThreads :: IntSet -> M ()
handleDeadlockedThreads deadlockedThreadIds = do
  Rts{..} <- gets ssRtsSupport
  let raiseEx targetTid exception = do
        PrimConcurrency.removeFromQueues targetTid
        PrimConcurrency.raiseAsyncEx [] targetTid exception
  forM_ (IntSet.toList deadlockedThreadIds) $ \tid -> do
    ts <- getThreadState tid
    case tsStatus ts of
      ThreadBlocked r -> case r of
        BlockedOnMVar{}         -> raiseEx tid rtsBlockedIndefinitelyOnMVar
        BlockedOnMVarRead{}     -> raiseEx tid rtsBlockedIndefinitelyOnMVar
        BlockedOnBlackHole{}    -> error "not implemented yet"
        BlockedOnThrowAsyncEx{} -> error "TODO: what is this case? figure it out"
        BlockedOnSTM{}          -> raiseEx tid rtsBlockedIndefinitelyOnSTM
        BlockedOnForeignCall{}  -> error "not implemented yet"
        s -> error $ "internal error - invalid thread state: " ++ show s
      s -> error $ "internal error - invalid thread state: " ++ show s
