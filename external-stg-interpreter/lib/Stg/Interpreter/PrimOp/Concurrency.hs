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

  ("noDuplicate#", [_s]) -> pure [] --  State# s -> State# s
  -------------------------

  -- fork# :: a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ( "fork#", [ioAction, _s]) -> do
    currentTId <- gets ssCurrentThreadId
    currentTS <- lookupThreadState currentTId

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
    currentTId <- gets ssCurrentThreadId
    currentTS <- lookupThreadState currentTId

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
    tid <- gets ssCurrentThreadId
    ThreadState{..} <- lookupThreadState tid
    pure [IntV $ if tsBound then 1 else 0]

  -- noDuplicate# :: State# s -> State# s
  -- TODO

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
            BlockedOnMVar         -> 1
            BlockedOnBlackHole    -> 2
            BlockedOnException    -> 12
            BlockedOnSTM          -> 6
            BlockedOnForeignCall  -> 10
            BlockedOnOther        -> 3 -- TODO: do we need this? BlockedOnRead 3 ; BlockedOnWrite 4 ; BlockedOnDelay 5

    pure [IntV statusCode, IntV tsCapability, IntV $ if tsLocked then 1 else 0]

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Concurrency primitives"
------------------------------------------------------------------------

primtype State# s
        { {\tt State\#} is the primitive, unlifted type of states.  It has
        one type parameter, thus {\tt State\# RealWorld}, or {\tt State\# s},
        where s is a type variable. The only purpose of the type parameter
        is to keep different state threads separate.  It is represented by
        nothing at all. }

primtype RealWorld
        { {\tt RealWorld} is deeply magical.  It is {\it primitive}, but it is not
        {\it unlifted} (hence {\tt ptrArg}).  We never manipulate values of type
        {\tt RealWorld}; it's only used in the type system, to parameterise {\tt State\#}. }

primtype ThreadId#
        {(In a non-concurrent implementation, this can be a singleton
        type, whose (unique) value is returned by {\tt myThreadId\#}.  The
        other operations can be omitted.)}

primop  ForkOp "fork#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True
   out_of_line      = True

primop  ForkOnOp "forkOn#" GenPrimOp
   Int# -> a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True
   out_of_line      = True

primop  KillThreadOp "killThread#"  GenPrimOp
   ThreadId# -> a -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True

primop  YieldOp "yield#" GenPrimOp
   State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True

primop  MyThreadIdOp "myThreadId#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, ThreadId# #)
   with
   has_side_effects = True

primop LabelThreadOp "labelThread#" GenPrimOp
   ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
   with
   has_side_effects = True
   out_of_line      = True

primop  IsCurrentThreadBoundOp "isCurrentThreadBound#" GenPrimOp
   State# RealWorld -> (# State# RealWorld, Int# #)
   with
   out_of_line = True
   has_side_effects = True

primop  NoDuplicateOp "noDuplicate#" GenPrimOp
   State# s -> State# s
   with
   out_of_line = True
   has_side_effects = True

primop  ThreadStatusOp "threadStatus#" GenPrimOp
   ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)
   with
   out_of_line = True
   has_side_effects = True
-}

{-
  ThreadState
    - stack
    - bound
    - status stuff
      - why_blocked
      - what_next
-}