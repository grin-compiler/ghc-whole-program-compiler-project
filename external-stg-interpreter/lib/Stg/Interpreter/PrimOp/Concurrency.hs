{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Concurrency where

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap as IntMap
import Foreign.Ptr

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = IntAtom i

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- fork# :: a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "fork#", [ioAction, _s]) -> do
    currentTS <- getCurrentThreadState

    (newTId, newTS) <- createThread
    updateThreadState newTId $ newTS
      { tsCurrentResult   = [ioAction]
      , tsStack           = [Apply [Void]]

      -- NOTE: start blocked if the current thread is blocked
      , tsBlockExceptions = tsBlockExceptions currentTS
      , tsInterruptible   = tsInterruptible currentTS
      }

    scheduleToTheEnd newTId
    requestContextSwitch

    pure [ThreadId newTId]

  -- forkOn# :: Int# -> a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "forkOn#", [IntV capabilityNo, ioAction, _s]) -> do
    currentTS <- getCurrentThreadState

    (newTId, newTS) <- createThread
    updateThreadState newTId $ newTS
      { tsCurrentResult   = [ioAction]
      , tsStack           = [Apply [Void]]

      -- NOTE: start blocked if the current thread is blocked
      , tsBlockExceptions = tsBlockExceptions currentTS
      , tsInterruptible   = tsInterruptible currentTS

      -- NOTE: capability related
      , tsLocked          = True          -- HINT: do not move this thread across capabilities
      , tsCapability      = capabilityNo
      }

    scheduleToTheEnd newTId
    requestContextSwitch

    pure [ThreadId newTId]

  -- killThread# :: ThreadId# -> a -> State# RealWorld -> State# RealWorld
  -- TODO

  -- yield# :: State# RealWorld -> State# RealWorld
  ( "yield#", [_s]) -> do
    currentTId <- gets ssCurrentThreadId
    scheduleToTheEnd currentTId
    scheduleThreads
    pure []

  -- myThreadId# :: State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "myThreadId#", [_s]) -> do
    tid <- gets ssCurrentThreadId
    pure [ThreadId tid]

  -- labelThread# :: ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
  ( "labelThread#", [ThreadId tid, PtrAtom _ p, _s]) -> do
    threadLabel <- liftIO . BS8.packCString $ castPtr p
    let setLabel ts@ThreadState{..} = ts {tsLabel = Just threadLabel}
    modify' $ \s@StgState{..} -> s {ssThreads = IntMap.adjust setLabel tid ssThreads}
    pure []

  -- isCurrentThreadBound# :: State# RealWorld -> (# State# RealWorld, Int# #)
  ( "isCurrentThreadBound#", [_s]) -> do
    ThreadState{..} <- getCurrentThreadState
    pure [IntV $ if tsBound then 1 else 0]

  -- noDuplicate# :: State# s -> State# s
  ( "noDuplicate#", [_s]) -> do
    -- NOTE: the stg interpreter is not concurrent, so this is a no-op
    pure []

  -- threadStatus# :: ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)
  ( "threadStatus#", [ThreadId tid, _s]) -> do
    ThreadState{..} <- lookupThreadState tid
    -- HINT:  includes/rts/Constants.h
    --        base:GHC.Conc.Sync.threadStatus
    let statusCode = case tsStatus of
          ThreadRunning   -> 0
          ThreadFinished  -> 16
          ThreadDied      -> 17
          ThreadBlocked r -> case r of
            BlockedOnMVar{}       -> 1
            BlockedOnMVarRead{}   -> 14
            BlockedOnBlackHole    -> 2
            BlockedOnSTM          -> 6
            BlockedOnForeignCall  -> 10
            BlockedOnRead{}       -> 3
            BlockedOnWrite{}      -> 4
            BlockedOnDelay{}      -> 5
            BlockedOnThrowAsyncException -> 12

    pure [IntV statusCode, IntV tsCapability, IntV $ if tsLocked then 1 else 0]

  _ -> fallback op args t tc

{-
primop  KillThreadOp "killThread#"  GenPrimOp
   ThreadId# -> a -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True
-}

{-
  ThreadState
    - stack
    - bound
    - status stuff
      - why_blocked
      - what_next
-}
