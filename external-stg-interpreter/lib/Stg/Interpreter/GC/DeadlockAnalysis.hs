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
      BlockedOnBlackHole{}    -> assertLiveOrDeadlocked tid
      BlockedOnThrowAsyncEx{} -> assertLiveOrDeadlocked tid
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
        targetTS <- getThreadState targetTid
        PrimConcurrency.raiseAsyncEx (tsCurrentResult targetTS) targetTid exception
  tsMap <- gets ssThreads
  forM_ (reverse $ IntSet.toList deadlockedThreadIds) $ \tid -> do
    ts <- getThreadState tid
    case tsStatus ts of
      ThreadRunning
        -- HINT: during async excepion stack unwind, Update frames can wake up threads that were blocking on blackholes
        | Just originalTS <- IntMap.lookup tid tsMap
        , ThreadBlocked BlockedOnBlackHole{} <- tsStatus originalTS
        -> pure ()
      ThreadBlocked r -> case r of
        BlockedOnMVar{}         -> raiseEx tid rtsBlockedIndefinitelyOnMVar
        BlockedOnMVarRead{}     -> raiseEx tid rtsBlockedIndefinitelyOnMVar
        BlockedOnBlackHole{}    -> raiseEx tid rtsNonTermination
        BlockedOnThrowAsyncEx{} -> pure () -- HINT: it might be blocked on other deadlocked thread
        BlockedOnSTM{}          -> raiseEx tid rtsBlockedIndefinitelyOnSTM
        BlockedOnForeignCall{}  -> error "not implemented yet"
        s -> error $ "internal error - invalid thread state: " ++ show s
      s -> error $ "internal error - invalid thread state: " ++ show s
