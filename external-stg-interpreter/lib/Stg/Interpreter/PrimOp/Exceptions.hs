{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Exceptions where

import Control.Monad.State

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = IntAtom i

evalPrimOp :: BuiltinStgApply -> PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp builtinStgApply fallback op args t tc = case (op, args) of

  ("unmaskAsyncExceptions#", [a, b]) -> builtinStgApply a [b]

  -----------------------------------------------------------

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
    evalStack <- gets ssEvalStack
    liftIO $ putStrLn $ show (evalStack) ++ " " ++ show op ++ " " ++ show args ++ " = ..."

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
    evalStack <- gets ssEvalStack
    liftIO $ putStrLn $ show (evalStack) ++ " " ++ show op ++ " " ++ show args ++ " = ..."

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
      undefined -- TODO

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
raiseEx ex = do
  exHandler <- findExHandlerAndUnwindStack
  stackPush $ Apply [ex, Void]
  pure [exHandler]

findExHandlerAndUnwindStack :: M Atom
findExHandlerAndUnwindStack = do
  -- TODO: update all balckholes with raise ex until the catch frame during the stack unwind
  stackPop >>= \case
    Nothing -> error "TODO: kill thread"
    Just (Catch exHandler b i) -> pure exHandler
    Just (Update blackHole) -> error "TODO: update black hole with raise exception"
    _ -> findExHandlerAndUnwindStack

{-
  TODO:
    - handle masking
    - handle blackhole update with raise exception op
    - handle stack underflow => kill thread
-}

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
