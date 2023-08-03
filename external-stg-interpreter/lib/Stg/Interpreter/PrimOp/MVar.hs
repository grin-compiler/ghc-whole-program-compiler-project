{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.MVar where

import Control.Monad.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

handleTakeMVar_ValueFullCase :: Int -> MVarDescriptor -> M ()
handleTakeMVar_ValueFullCase m mvd@MVarDescriptor{..} = do
  case mvdQueue of
    [] -> do
      -- HINT: the queue is empty so there is nothing to do, just mark the mvar empty
      modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m (mvd {mvdValue = Nothing}) ssMVars }

    tid : tidTail -> do
      -- HINT: every blocked thread in the queue waits for an empty mvar to write their value in it
      -- NOTE: finished and dead threads are not present in the waiting queue
      -- wake up thread
      ts <- getThreadState tid
      case tsStatus ts of
        ThreadBlocked (BlockedOnMVar _ (Just v)) -> do
          updateThreadState tid (ts {tsStatus = ThreadRunning})
          let newValue = mvd {mvdValue = Just v, mvdQueue = tidTail}
          modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m newValue ssMVars }
        _ -> error $ "internal error - invalid thread status: " ++ show (tsStatus ts)

handlePutMVar_ValueEmptyCase :: Int -> MVarDescriptor -> Atom -> M ()
handlePutMVar_ValueEmptyCase m mvd@MVarDescriptor{..} v = do
  -- HINT: first handle the blocked readMVar case, it does not consume the value
  --       BlockedOnMVarRead are always at the beginning of the queue, process all of them
  let processReads [] = pure []
      processReads tids@(tid : tidTail) = do
        ts@ThreadState{..} <- getThreadState tid
        case tsStatus of
          ThreadBlocked (BlockedOnMVarRead _) -> do
            updateThreadState tid (ts {tsStatus = ThreadRunning, tsCurrentResult = [v]})
            --liftIO $ putStrLn $ " * (handlePutMVar_ValueEmptyCase, processReads) mvar unblock, unblocked tid: " ++ show tid
            processReads tidTail

          _ -> pure tids

  -- HINT: every blocked thread in the queue waits for an incoming value to read
  waitQueue <- processReads mvdQueue
  case waitQueue of
    [] -> do
      -- HINT: the queue is empty so there is nothing to do, just store the value in mvar
      let newValue = MVarDescriptor {mvdValue = Just v, mvdQueue = []}
      modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m newValue ssMVars }

    tid : tidTail -> do
      -- HINT: every blocked thread in the queue waits for an incoming value to take
      -- NOTE: finished and dead threads are not present in the waiting queue
      -- wake up thread and pass the new vale to the thread as a result of the blocked takeMVar
      ts <- getThreadState tid
      case tsStatus ts of
        ThreadBlocked (BlockedOnMVar i Nothing) | i == m
          -> updateThreadState tid (ts {tsStatus = ThreadRunning, tsCurrentResult = [v]})
        _ -> error $ "internal error - invalid thread status, expected 'ThreadBlocked (BlockedOnMVar " ++ show m ++ " Nothing)', got: " ++ show (tsStatus ts)
      -- Q: what if the thread was killed by now?
      -- A: killed threads are always removed from waiting queues

      --liftIO $ putStrLn $ " * (handlePutMVar_ValueEmptyCase) mvar unblock, unblocked tid: " ++ show tid

      -- update wait queue
      let newValue = mvd {mvdQueue = tidTail}
      modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m newValue ssMVars }

appendMVarQueue :: Int -> Int -> M ()
appendMVarQueue m tid = do
  let appendFun mvd = mvd {mvdQueue = mvdQueue mvd ++ [tid]}
  modify' $ \s@StgState{..} -> s {ssMVars = IntMap.adjust appendFun m ssMVars}

reportOp :: Name -> [Atom] -> M ()
reportOp op args = do
  {-
  tid <- gets ssCurrentThreadId
  liftIO $ do
    putStrLn $ show tid ++ "  " ++ show op ++ " " ++ show args
  -}
  pure ()

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newMVar# :: State# s -> (# State# s, MVar# s a #)
  ( "newMVar#", [_s]) -> do
    reportOp op args
    state (\s@StgState{..} ->
      let next  = ssNextMVar
          value = MVarDescriptor {mvdValue = Nothing, mvdQueue = []}
      in ([MVar next], s {ssMVars = IntMap.insert next value ssMVars, ssNextMVar = succ next}))

  -- takeMVar# :: MVar# s a -> State# s -> (# State# s, a #)
  ( "takeMVar#", [MVar m, _s]) -> do
    reportOp op args
    mvd@MVarDescriptor{..} <- lookupMVar m
    --liftIO $ putStrLn $ "mvdValue: " ++ show mvdValue
    case mvdValue of
      Nothing -> do
        -- block current thread on this MVar
        -- set blocked reason
        tid <- gets ssCurrentThreadId
        ts <- getCurrentThreadState
        updateThreadState tid (ts {tsStatus = ThreadBlocked $ BlockedOnMVar m Nothing})
        --liftIO $ putStrLn $ " * mvar block, blocked tid: " ++ show tid

        -- add to mvar's waiting queue
        appendMVarQueue m tid

        -- reschedule threads
        stackPush $ RunScheduler SR_ThreadBlocked
        pure [] -- NOTE: the real return value will be calculated when the tread is unblocked

      Just a -> do
        handleTakeMVar_ValueFullCase m mvd
        pure [a]

  -- tryTakeMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
  ( "tryTakeMVar#", [MVar m, _s]) -> do
    reportOp op args
    mvd@MVarDescriptor{..} <- lookupMVar m
    --liftIO $ putStrLn $ "mvdValue: " ++ show mvdValue
    case mvdValue of
      Nothing -> do
        pure [IntV 0, LiftedUndefined]
      Just a -> do
        handleTakeMVar_ValueFullCase m mvd
        pure [IntV 1, a]

  -- putMVar# :: MVar# s a -> a -> State# s -> State# s
  ( "putMVar#", [MVar m, a, _s]) -> do
    reportOp op args
    mvd@MVarDescriptor{..} <- lookupMVar m
    --liftIO $ putStrLn $ "mvdValue: " ++ show mvdValue
    case mvdValue of
      Just{} -> do
        -- block current thread on this MVar
        -- set blocked reason
        tid <- gets ssCurrentThreadId
        ts <- getCurrentThreadState
        updateThreadState tid (ts {tsStatus = ThreadBlocked $ BlockedOnMVar m (Just a)})
        --liftIO $ putStrLn $ " * mvar block, blocked tid: " ++ show tid

        -- add to mvar's waiting queue
        appendMVarQueue m tid

        -- reschedule threads
        stackPush $ RunScheduler SR_ThreadBlocked
        pure []

      Nothing -> do
        handlePutMVar_ValueEmptyCase m mvd a
        pure []

  -- tryPutMVar# :: MVar# s a -> a -> State# s -> (# State# s, Int# #)
  ( "tryPutMVar#", [MVar m, a, _s]) -> do
    reportOp op args
    mvd@MVarDescriptor{..} <- lookupMVar m
    --liftIO $ putStrLn $ "mvdValue: " ++ show mvdValue
    case mvdValue of
      Nothing -> do
        handlePutMVar_ValueEmptyCase m mvd a
        pure [IntV 1]
      Just _  -> do
        pure [IntV 0]

  -- readMVar# :: MVar# s a -> State# s -> (# State# s, a #)
  ( "readMVar#", [MVar m, _s]) -> do
    reportOp op args
    mvd@MVarDescriptor{..} <- lookupMVar m
    --liftIO $ putStrLn $ "mvdValue: " ++ show mvdValue
    case mvdValue of
      Nothing -> do
        -- block current thread on this MVar
        -- set blocked reason
        tid <- gets ssCurrentThreadId
        ts <- getCurrentThreadState
        updateThreadState tid (ts {tsStatus = ThreadBlocked $ BlockedOnMVarRead m})
        --liftIO $ putStrLn $ " * mvar block, blocked tid: " ++ show tid

        -- add to mvar's waiting queue
        appendMVarQueue m tid

        -- reschedule threads
        stackPush $ RunScheduler SR_ThreadBlocked
        pure [] -- NOTE: the real return value will be calculated when the tread is unblocked

      Just a -> pure [a]

  -- tryReadMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
  ( "tryReadMVar#", [MVar m, _s]) -> do
    reportOp op args
    MVarDescriptor{..} <- lookupMVar m
    --liftIO $ putStrLn $ "mvdValue: " ++ show mvdValue
    case mvdValue of
      Nothing -> pure [IntV 0, LiftedUndefined]
      Just a  -> pure [IntV 1, a]

  -- isEmptyMVar# :: MVar# s a -> State# s -> (# State# s, Int# #)
  ( "isEmptyMVar#", [MVar m, _s]) -> do
    reportOp op args
    MVarDescriptor{..} <- lookupMVar m
    --liftIO $ putStrLn $ "mvdValue: " ++ show mvdValue
    case mvdValue of
      Nothing -> pure [IntV 1]
      Just _  -> pure [IntV 0]

  -- OBSOLETE from GHC 9.4
  -- sameMVar# :: MVar# s a -> MVar# s a -> Int#
  ( "sameMVar#", [MVar a, MVar b]) -> do
    reportOp op args
    pure [IntV $ if a == b then 1 else 0]

  _ -> fallback op args t tc
