{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Exceptions where

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = IntAtom i

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
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
    RtsBaseInterop{..} <- gets ssRtsBaseInterop
    raiseEx rtsDivZeroException

  -- raiseUnderflow# :: Void# -> o
  ( "raiseUnderflow#", [_s]) -> do
    RtsBaseInterop{..} <- gets ssRtsBaseInterop
    raiseEx rtsUnderflowException

  -- raiseOverflow# :: Void# -> o
  ( "raiseOverflow#", [_s]) -> do
    RtsBaseInterop{..} <- gets ssRtsBaseInterop
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

    -- set new masking state
    unless (tsBlockExceptions == False && tsInterruptible == False) $ do
      updateThreadState tid $ ts {tsBlockExceptions = False, tsInterruptible = False}
      stackPush $ RestoreExMask tsBlockExceptions tsInterruptible
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
    allocAtoms [IntV status]

  _ -> fallback op args t tc

raiseEx :: M sig m => AtomAddr -> m [AtomAddr]
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
        [voidAddr] <- allocAtoms [Void]
        stackPush $ Apply [ex, voidAddr]
        pure [exHandler]

      Just (Update addr) -> do
        -- update the (balckholed/running) thunk with the exception value
        store addr $ RaiseException ex
        unwindStack

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
