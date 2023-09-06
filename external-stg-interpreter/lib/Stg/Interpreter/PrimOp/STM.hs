{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.STM where

import GHC.Stack
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Text.Pretty.Simple (pShowNoColor)
import qualified Data.Text.Lazy.IO as Text
import Data.Maybe

import Stg.Syntax
import Stg.Interpreter.Base
import qualified Stg.Interpreter.PrimOp.Concurrency as PrimConcurrency

{-
  STM design notes
  + per thread transation log

  control flow ops
    done + atomic
      - push atomicFrame
    + retry
      - validate
      - create wait queue
      - block itself
    + catchRetry (alias: orElse)
    + atomicFrame
      - validate
      - commit
        - wake up threads in tvars wait queues
      - re-run

  value ops
    done + read
    done + write

  exceptions
    + catchSTM            - commit
    + reaching atomically - abort, keep tvar allocations

TODO:
  - read paper from 6.1 transaction logs

Q: is there a new tlog entry for each tvar operation or is it one entry per tvar?
A: 

Q: what is the difference between STM and SQL transactions?
    is it the value sampling?
      SQL: sample variables atomically
      STM: sample variables on the go from consistent global state

-}


evalPrimOp :: HasCallStack => PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- DONE
  -- atomically# :: (State# RealWorld -> (# State# RealWorld, a #) )
  --             ->  State# RealWorld -> (# State# RealWorld, a #)
  ( "atomically#", [stmAction, Void]) -> do
    promptM $ do
      print (op, args)
    atomicallyOp stmAction

  -- DONE
  -- retry# :: State# RealWorld -> (# State# RealWorld, a #)
  ( "retry#", [Void]) -> do
    promptM $ do
      print (op, args)
    retrySTM

  -- DONE
  -- catchRetry# :: (State# RealWorld -> (# State# RealWorld, a #) )
  --             -> (State# RealWorld -> (# State# RealWorld, a #) )
  --             -> (State# RealWorld -> (# State# RealWorld, a #) )
  ( "catchRetry#", [firstStmAction, altStmAction, Void]) -> do
    promptM $ do
      print (op, args)
    tid <- gets ssCurrentThreadId
    ts <- getThreadState tid
    let Just tlog = tsActiveTLog ts
    -- push tlog, start fresh active tlog for the nested transaction
    updateThreadState tid $ ts {tsActiveTLog = Just mempty, tsTLogStack = tlog : tsTLogStack ts}
    stackPush $ CatchRetry firstStmAction altStmAction False mempty
    stackPush $ Apply [Void]
    pure [firstStmAction]

  -- DONE
  -- catchSTM# :: (State# RealWorld -> (# State# RealWorld, a #) )
  --           -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
  --           -> (State# RealWorld -> (# State# RealWorld, a #) )
  ( "catchSTM#", [f, h, w]) -> do
    promptM $ do
      print (op, args)
    tid <- gets ssCurrentThreadId
    ts <- getThreadState tid
    let Just tlog = tsActiveTLog ts
    -- push tlog, start fresh active tlog for the nested transaction
    updateThreadState tid $ ts {tsActiveTLog = Just mempty, tsTLogStack = tlog : tsTLogStack ts}
    stackPush $ CatchSTM f h
    stackPush $ Apply [w]
    pure [f]

  -- DONE
  -- newTVar# :: a -> State# s -> (# State# s, TVar# s a #)
  ( "newTVar#", [a, _s]) -> do
    tVars <- gets ssTVars
    next <- gets ssNextTVar
    let value = TVarDescriptor {tvdValue = a, tvdQueue = mempty}
    modify' $ \s -> s {ssTVars = IntMap.insert next value tVars, ssNextTVar = succ next}
    promptM $ print (op, args, TVar next)
    pure [TVar next]

  -- DONE
  -- readTVar# :: TVar# s a -> State# s -> (# State# s, a #)
  ( "readTVar#", [TVar t, _s]) -> do
    promptM $ print (op, args)
    -- read from TLog
    tid <- gets ssCurrentThreadId
    ts <- getThreadState tid
    let Just tlogEntryMap = tsActiveTLog ts
        tlogStack         = tsTLogStack ts
    case IntMap.lookup t tlogEntryMap of
      Nothing -> do
        -- HINT: first access
        entry@TLogEntry{..} <- getTLogEntry tlogStack t
        let extendedTLog = IntMap.insert t entry tlogEntryMap
        updateThreadState tid $ ts {tsActiveTLog = Just extendedTLog}
        pure [tleCurrentLocalValue]
      Just TLogEntry{..} -> do
        pure [tleCurrentLocalValue]

  -- DONE
  -- readTVarIO# :: TVar# s a -> State# s -> (# State# s, a #)
  ( "readTVarIO#", [TVar t, _s]) -> do
    promptM $ print (op, args)
    a <- tvdValue <$> lookupTVar t
    pure [a]

  -- DONE
  -- writeTVar# :: TVar# s a -> a -> State# s -> State# s
  ( "writeTVar#", [TVar t, value, _s]) -> do
    promptM $ print (op, args)
    -- write to TLog
    tid <- gets ssCurrentThreadId
    ts <- getThreadState tid
    let Just tlogEntryMap = tsActiveTLog ts
        tlogStack         = tsTLogStack ts
    case IntMap.lookup t tlogEntryMap of
      Nothing -> do
        -- HINT: first access
        entry <- getTLogEntry tlogStack t
        let newEntry      = entry {tleCurrentLocalValue = value}
            extendedTLog  = IntMap.insert t newEntry tlogEntryMap
        updateThreadState tid $ ts {tsActiveTLog = Just extendedTLog}
        pure []
      Just oldEntry -> do
        let newEntry    = oldEntry {tleCurrentLocalValue = value}
        let updatedTLog = IntMap.insert t newEntry tlogEntryMap
        updateThreadState tid $ ts {tsActiveTLog = Just updatedTLog}
        pure []

  -- OBSOLETE from GHC 9.4
  -- sameTVar# :: TVar# s a -> TVar# s a -> Int#

  _ -> fallback op args t tc

atomicallyOp :: HasCallStack => Atom -> M [Atom]
atomicallyOp stmAction = do
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  case tsActiveTLog ts of
    Just{} -> do
      -- HINT: there is an active TLog, nested atomically operations are not allowed, throw exception
      Rts{..} <- gets ssRtsSupport
      stackPush $ RaiseOp rtsNestedAtomically
      pure []
    Nothing -> do
      -- extra validation (optional)
      when (tsTLogStack ts /= []) $ error "internal error: non-empty tsTLogStack without tsActiveTLog"
      -- create TLog
      updateThreadState tid $ ts {tsActiveTLog = Just mempty, tsTLogStack = []}
      stackPush $ Atomically stmAction
      stackPush $ Apply [Void]
      pure [stmAction]

getTLogEntry :: [TLog] -> Int -> M TLogEntry
getTLogEntry [] tvarId = do
  -- HINT: first access, read from the global state
  globalValue <- tvdValue <$> lookupTVar tvarId
  pure TLogEntry
    { tleObservedGlobalValue  = globalValue
    , tleCurrentLocalValue    = globalValue
    }
getTLogEntry (tlog : tlogStack) tvarId = case IntMap.lookup tvarId tlog of
  Nothing     -> getTLogEntry tlogStack tvarId
  Just entry  -> pure entry

-- read: stg_catch_stm_frame
mergeNestedOrRestart :: HasCallStack => [Atom] -> M [Atom]
mergeNestedOrRestart result = do
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  let Just tlog = tsActiveTLog ts
      tlogStack@(tlogStackTop : tlogStackTail) = tsTLogStack ts
  -- validate every tlog
  allValid <- and <$> mapM validateTLog (tlog : tlogStack)
  case allValid of
    False -> do
      -- drop current transaction
      updateThreadState tid $ ts
        { tsActiveTLog  = Just tlogStackTop
        , tsTLogStack   = tlogStackTail
        }
      -- restart the whole transaction
      restartSTMFromAtomicallyFrame
    True  -> do
      -- merge nested
      let mergedTLog = IntMap.unionWith (\a _ -> a) tlog tlogStackTop
      updateThreadState tid $ ts
        { tsActiveTLog  = Just mergedTLog
        , tsTLogStack   = tlogStackTail
        }
      pure result

restartSTMFromAtomicallyFrame :: HasCallStack => M [Atom]
restartSTMFromAtomicallyFrame = unwindStack where
  unwindStack = do
    stackPop >>= \case
      Nothing -> do
        -- the stack is empty, kill the thread
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        updateThreadState tid (ts {tsStatus = ThreadDied})
        promptM $ putStrLn "[STM] restartSTMFromAtomicallyFrame - unwindStack - ThreadDied"
        pure []

      Just (Atomically stmAction) -> do
        restartTransaction stmAction

      Just CatchSTM{} -> do
        -- HINT: pop tlog stack for some extra stg state consistency and validation
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        updateThreadState tid $ ts {tsTLogStack = tail $ tsTLogStack ts}
        unwindStack

      Just CatchRetry{} -> do
        -- HINT: pop tlog stack for some extra stg state consistency and validation
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        updateThreadState tid $ ts {tsTLogStack = tail $ tsTLogStack ts}
        unwindStack

      _ -> unwindStack -- HINT: discard stack frames

{-
data BlockReason
  = BlockedOnMVar         Int (Maybe Atom) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int       -- mvar id
  | BlockedOnBlackHole
  | BlockedOnThrowAsyncEx Int Atom  -- target thread id, exception
  | BlockedOnSTM
  | BlockedOnForeignCall            -- RTS name: BlockedOnCCall
  | BlockedOnRead         Int       -- file descriptor
  | BlockedOnWrite        Int       -- file descriptor
  | BlockedOnDelay        UTCTime   -- target time to wake up thread
  deriving (Eq, Ord, Show)

data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
-}

{-
  TODO:
    - unwind stack, look for atomically frame
    - subscribe for TVar wait queues
    - suspend thread
-}
retrySTM :: HasCallStack => M [Atom]
retrySTM = unwindStack where
  unwindStack = do
    stackPop >>= \case
      Nothing -> do
        error "internal error - hit the stack bottom in retrySTM, no Atomically or CatchRetry frame found"

      Just CatchSTM{} -> do
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        -- HINT: pop tlog stack, merge the old stack top to the active tlog (it is needed for TVar subscription on STM suspend)
        updateThreadState tid $ ts
          { tsTLogStack = tail $ tsTLogStack ts
          , tsActiveTLog = Just $ IntMap.unionsWith (\a _ -> a) $ maybeToList (tsActiveTLog ts) ++ [head $ tsTLogStack ts]
          }
        unwindStack

      -- Q: what about CatchRetry alternative code and tlog stack popping?
      -- A: the tlog stack is popped, the stack top becomes the new active tlog merged with the old active tlog and the tlog stack top
      --    this is needed to subscribe all TVars that was used so far when stm transaction gets suspended
      Just (CatchRetry _firstStmAction _altStmAction True firstTLog) -> do
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        updateThreadState tid $ ts
          { tsTLogStack   = tail $ tsTLogStack ts
          , tsActiveTLog  = Just $ IntMap.unionsWith (\a _ -> a) $ maybeToList (tsActiveTLog ts) ++ [head $ tsTLogStack ts, firstTLog]
          }
        unwindStack

      Just (CatchRetry firstStmAction altStmAction False _) -> do
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        -- abort current stm branch and run alternative stm action
        updateThreadState tid $ ts {tsActiveTLog = Just mempty}
        -- HINT: save the current active tlog, it is needed for TVar subscriptions on STM suspend
        stackPush $ CatchRetry firstStmAction altStmAction True (fromJust $ tsActiveTLog ts)
        stackPush $ Apply [Void]
        pure [altStmAction]

      Just (Atomically stmAction) -> do
        tid <- gets ssCurrentThreadId
        ts <- getThreadState tid
        let Just tlog = tsActiveTLog ts
        -- extra validation (optional)
        when (tsTLogStack ts /= []) $ error "internal error: non-empty tsTLogStack without tsActiveTLog"

        -- HINT: tlog validation tells if the the control flow was valid or not
        isValid <- validateTLog tlog

        promptM $ do
          putStrLn $ "[STM] tid: " ++ show tid ++ " tlog: " ++ show tlog
          putStrLn $ "[STM] validateTLog: " ++ show isValid

        if (not isValid)
          then do
            restartTransaction stmAction
          else do
            promptM $ putStrLn $ "[STM] retry, block thread, tid: " ++ show tid
            tid <- gets ssCurrentThreadId
            ts <- getThreadState tid
            -- subscribe to wait queues
            let Just tlog = tsActiveTLog ts
            subscribeTVarWaitQueues tid tlog -- HINT: GC deadlock detection will cover empty tlog and dead TVar caused deadlocks
            -- suspend thread
            updateThreadState tid (ts {tsStatus = ThreadBlocked (BlockedOnSTM tlog), tsActiveTLog = Just mempty})
            -- Q: who will update the tsTLog after the wake up?
            stackPush $ Atomically stmAction
            stackPush $ Apply [Void]
            stackPush $ RunScheduler SR_ThreadBlocked
            pure [stmAction]

      _ -> unwindStack -- HINT: discard stack frames

{-
data TVarDescriptor
  = TVarDescriptor
  { tvdValue  :: Atom
  , tvdQueue  :: IntSet -- thread id, STM wake up queue
  }
-}

-- Q: where to put unwait?
commitOrRestart :: HasCallStack => Atom -> [Atom] -> M [Atom]
commitOrRestart stmAction result = do
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  let Just tlog = tsActiveTLog ts
  -- extra validation (optional)
  when (tsTLogStack ts /= []) $ error "internal error: non-empty tsTLogStack without tsActiveTLog"
  -- validate
  isValid <- validateTLog tlog
  if isValid
    then do
      promptM $ putStrLn $ "[STM] commit, tid: " ++ show tid ++ ", result = " ++ show result
      -- commit
      updateThreadState tid $ ts {tsActiveTLog = Nothing}
      -- merge to global tvar state
      forM_ (IntMap.toList tlog) $ \(tvar, TLogEntry{..}) -> do
        old <- lookupTVar tvar
        let newValue = TVarDescriptor
              { tvdValue  = tleCurrentLocalValue
              , tvdQueue  = mempty
              }
        modify' $ \s@StgState{..} -> s {ssTVars = IntMap.insert tvar newValue ssTVars}
        --------------------------------------------------
        -- wake up threads from tvars wait queues
        --------------------------------------------------
        forM_ (IntSet.toList $ tvdQueue old) $ \waitingTid -> do
          promptM $ putStrLn $ "[STM commitOrRestart / commit case] - wake up thread: " ++ show waitingTid ++ ", tid: " ++ show tid
          waitingTS <- getThreadState waitingTid
          -- Q: what if the thread was killed by now?
          -- A: killed threads are always removed from waiting queues
          case tsStatus waitingTS of
            ThreadBlocked (BlockedOnSTM waitingTLog) -> do
              unsubscribeTVarWaitQueues waitingTid waitingTLog
              updateThreadState waitingTid (waitingTS {tsStatus = ThreadRunning})
            _ -> error $ "internal error - invalid thread status: " ++ show (tsStatus waitingTS)
          -- Q: should we check the value also?
      pure result
    else do
      -- restart
      restartTransaction stmAction

restartTransaction :: HasCallStack => Atom -> M [Atom]
restartTransaction stmAction = do
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  -- extra validation (optional)
  when (tsTLogStack ts /= []) $ do
    reportThread tid
    error $ "internal error: non-empty tsTLogStack: " ++ show (tsTLogStack ts) ++ ", tid: " ++ show tid
  promptM $ putStrLn "[STM] restartTransaction"
  updateThreadState tid $ ts {tsActiveTLog = Just mempty}
  stackPush $ Atomically stmAction
  stackPush $ Apply [Void]
  pure [stmAction]

{-
------------------------------------------------------------------------
section "STM-accessible Mutable Variables"
------------------------------------------------------------------------

primtype TVar# s a

primop  AtomicallyOp "atomically#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> State# RealWorld -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

-- NB: retry#'s strictness information specifies it to diverge.
-- This lets the compiler perform some extra simplifications, since retry#
-- will technically never return.
--
-- This allows the simplifier to replace things like:
--   case retry# s1
--     (# s2, a #) -> e
-- with:
--   retry# s1
-- where 'e' would be unreachable anyway.  See #8091.
primop  RetryOp "retry#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  CatchRetryOp "catchRetry#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with
   strictness  = { \ _arity -> mkClosedStrictSig [ lazyApply1Dmd
                                                 , lazyApply1Dmd
                                                 , topDmd ] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  CatchSTMOp "catchSTM#" GenPrimOp
      (State# RealWorld -> (# State# RealWorld, a #) )
   -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
   -> (State# RealWorld -> (# State# RealWorld, a #) )
   with
   strictness  = { \ _arity -> mkClosedStrictSig [ lazyApply1Dmd
                                                 , lazyApply2Dmd
                                                 , topDmd ] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  NewTVarOp "newTVar#" GenPrimOp
       a
    -> State# s -> (# State# s, TVar# s a #)
   {Create a new {\tt TVar\#} holding a specified initial value.}
   with
   out_of_line  = True
   has_side_effects = True

primop  ReadTVarOp "readTVar#" GenPrimOp
       TVar# s a
    -> State# s -> (# State# s, a #)
   {Read contents of {\tt TVar\#}.  Result is not yet evaluated.}
   with
   out_of_line  = True
   has_side_effects = True

primop ReadTVarIOOp "readTVarIO#" GenPrimOp
       TVar# s a
    -> State# s -> (# State# s, a #)
   {Read contents of {\tt TVar\#} outside an STM transaction}
   with
   out_of_line      = True
   has_side_effects = True

primop  WriteTVarOp "writeTVar#" GenPrimOp
       TVar# s a
    -> a
    -> State# s -> State# s
   {Write contents of {\tt TVar\#}.}
   with
   out_of_line      = True
   has_side_effects = True

primop  SameTVarOp "sameTVar#" GenPrimOp
   TVar# s a -> TVar# s a -> Int#
-}