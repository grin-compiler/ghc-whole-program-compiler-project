{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Exceptions where

import Control.Monad.State

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = IntAtom i

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  {-
    catch# :: (State# RealWorld -> (# State# RealWorld, a #) )
           -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
           -> State# RealWorld
           -> (# State# RealWorld, a #)
  -}
  ( "catch#", [f, h, w]) -> do
    -- get async exception masking state
    ThreadState{..} <- getCurrentThreadState

    stackPush $ Catch h tsBlockExceptions tsInterruptible
    stackPush $ Apply [w]
    pure [f]

  -- raise# :: b -> o
  ( "raise#", [ex]) -> do
    -- for debug only
    --liftIO $ putStrLn $ show (evalStack) ++ " " ++ show op ++ " " ++ show args ++ " = ..."

    raiseEx ex -- implementation

  -- raiseDivZero# :: Void# -> o
  ( "raiseDivZero#", [_s]) -> do
    Rts{..} <- gets ssRtsSupport
    raiseEx rtsDivZeroException

  -- raiseUnderflow# :: Void# -> o
  ( "raiseUnderflow#", [_s]) -> do
    Rts{..} <- gets ssRtsSupport
    raiseEx rtsUnderflowException

  -- raiseOverflow# :: Void# -> o
  ( "raiseOverflow#", [_s]) -> do
    Rts{..} <- gets ssRtsSupport
    raiseEx rtsOverflowException

  -- raiseIO# :: a -> State# RealWorld -> (# State# RealWorld, b #)
  ( "raiseIO#", [ex, s]) -> do
    -- for debug only
    --liftIO $ putStrLn $ show (evalStack) ++ " " ++ show op ++ " " ++ show args ++ " = ..."

    raiseEx ex -- implementation

  -- maskAsyncExceptions# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)
  ( "maskAsyncExceptions#", [f, w]) -> do

    -- get async exception masking state
    ts@ThreadState{..} <- getCurrentThreadState
    tid <- gets ssCurrentThreadId

    ------------------------ debug
    liftIO $ print (tid, op, args)
    unless (null tsBlockedExceptions) $ do
      reportThreads
    ------------------------ debug

    -- set new masking state
    unless (tsBlockExceptions == True && tsInterruptible == True) $ do
      updateThreadState tid $ ts {tsBlockExceptions = True, tsInterruptible = True}
      stackPush $ RestoreExMask tsBlockExceptions tsInterruptible

    -- run action
    stackPush $ Apply [w]
    pure [f]

  -- maskUninterruptible# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)
  ( "maskUninterruptible#", [f, w]) -> do

    -- get async exception masking state
    ts@ThreadState{..} <- getCurrentThreadState
    tid <- gets ssCurrentThreadId

    ------------------------ debug
    liftIO $ print (tid, op, args)
    unless (null tsBlockedExceptions) $ do
      reportThreads
    ------------------------ debug

    -- set new masking state
    unless (tsBlockExceptions == True && tsInterruptible == False) $ do
      updateThreadState tid $ ts {tsBlockExceptions = True, tsInterruptible = False}
      stackPush $ RestoreExMask tsBlockExceptions tsInterruptible

    -- run action
    stackPush $ Apply [w]
    pure [f]

  -- unmaskAsyncExceptions# :: (State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)
  ( "unmaskAsyncExceptions#", [f, w]) -> do

    -- get async exception masking state
    ts@ThreadState{..} <- getCurrentThreadState
    tid <- gets ssCurrentThreadId

    ------------------------ debug
    liftIO $ print (tid, op, args)
    wps <- gets ssWeakPointers
    liftIO $ print wps
    unless (null tsBlockedExceptions) $ do
      reportThreads
    ------------------------ debug

    -- set new masking state
    unless (tsBlockExceptions == False && tsInterruptible == False) $ do
      updateThreadState tid $ ts {tsBlockExceptions = False, tsInterruptible = False}
      stackPush $ RestoreExMask tsBlockExceptions tsInterruptible
      {-
        TODO:
          - raise exception in
          - wake up the blocked thread ()
      -}
      -- TODO: implement this
      {-
        -- TODO: raise async exception eagerly, then run the io action
        maybePerformBlockedException ; non-zero (one) if an exception was raised, zero otherwise
          foreach in the blocked exceptions queue:
            throwToSingleThreaded (target, alias me) ; 1. remove from queues ; 2. raise async
            tryWakeupThread (source) ; NOTE: recheck blocking conditions, turns thread into running state if the blocker has disappeared
      -}
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!!
      pure () -- TODO
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!!

    -- run action
    stackPush $ Apply [w]
    pure [f]

  -- getMaskingState# :: State# RealWorld -> (# State# RealWorld, Int# #)
  ( "getMaskingState#", [_s]) -> do
    ThreadState{..} <- getCurrentThreadState
    {-
       returns: 0 == unmasked,
                1 == masked, non-interruptible,
                2 == masked, interruptible
    -}
    let status = case (tsBlockExceptions, tsInterruptible) of
          (False, False)  -> 0
          (True,  False)  -> 1
          (True,  True)   -> 2
          (False, True)   -> error "impossible exception mask, tsBlockExceptions: False, tsInterruptible: True"
    pure [IntV status]

  _ -> fallback op args t tc

raiseEx :: Atom -> M [Atom]
raiseEx ex = unwindStack where
  unwindStack = do
    stackPop >>= \case
      Nothing -> do
        -- the stack is empty, kill the thread
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        updateThreadState tid (ts {tsStatus = ThreadDied})
        pure []

      Just (Catch exHandler bEx iEx) -> do
        -- HINT: the catch primop does not modify the async exception masking, so the following code is needed only when the async exceptions are not masked
        unless bEx $ do
          -- mask async excpetions before running the handler
          ts <- getCurrentThreadState
          tid <- gets ssCurrentThreadId
          updateThreadState tid $ ts {tsBlockExceptions = True, tsInterruptible = iEx}
          stackPush $ RestoreExMask bEx iEx

        -- run the exception handler
        stackPush $ Apply [ex, Void]
        pure [exHandler]

      Just (CatchSTM _stmAction exHandler) -> do
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        let tlogStackTop : tlogStackTail = tsTLogStack ts
        -- HINT: abort current nested transaction, and reload the parent tlog then run the exception handler in it
        updateThreadState tid $ ts
          { tsActiveTLog  = Just tlogStackTop
          , tsTLogStack   = tlogStackTail
          }
        -- run the exception handler
        stackPush $ Apply [ex, Void]
        pure [exHandler]

      Just CatchRetry{} -> do
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        updateThreadState tid $ ts { tsTLogStack = tail $ tsTLogStack ts}
        unwindStack

      Just (Update addr) -> do
        -- update the (balckholed/running) thunk with the exception value
        store addr $ RaiseException ex
        unwindStack

      Just (Atomically stmAction) -> do
        ts <- getCurrentThreadState
        tid <- gets ssCurrentThreadId
        -- extra validation (optional)
        when (tsTLogStack ts /= []) $ error "internal error: non-empty tsTLogStack without tsActiveTLog"
        let Just tlog = tsActiveTLog ts
        isValid <- validateTLog tlog
        case isValid of
          True -> do
            -- abandon transaction
            updateThreadState tid $ ts {tsActiveTLog = Nothing}
            unsubscribeTVarWaitQueues tid tlog
            unwindStack
          False -> do
            -- restart transaction due to invalid STM state
            updateThreadState tid $ ts {tsActiveTLog = Just mempty}
            stackPush $ Atomically stmAction
            stackPush $ Apply [Void]
            pure [stmAction]

      _ -> unwindStack

{-
primop  UnmaskAsyncExceptionsOp "unmaskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True
-}
