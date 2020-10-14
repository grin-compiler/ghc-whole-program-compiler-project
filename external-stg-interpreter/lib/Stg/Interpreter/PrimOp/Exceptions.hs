{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Exceptions where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: BuiltinStgApply -> PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp builtinStgApply fallback op args t tc = case (op, args) of

  ("catch#", [f, h, w]) -> builtinStgApply f [w]
  ("getMaskingState#", [w]) -> pure [Literal $ LitNumber LitNumInt 1] -- State# RealWorld -> (# State# RealWorld, Int# #)
  ("maskUninterruptible#", [a]) -> pure [a] -- TODO : (State# RealWorld -> (# State# RealWorld, a #)) -> (State# RealWorld -> (# State# RealWorld, a #))
  -- HACK:
  ("maskUninterruptible#", [a, b]) -> builtinStgApply a [b]

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Exceptions"
------------------------------------------------------------------------

-- Note [Strictness for mask/unmask/catch]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Consider this example, which comes from GHC.IO.Handle.Internals:
--    wantReadableHandle3 f ma b st
--      = case ... of
--          DEFAULT -> case ma of MVar a -> ...
--          0#      -> maskAsynchExceptions# (\st -> case ma of MVar a -> ...)
-- The outer case just decides whether to mask exceptions, but we don't want
-- thereby to hide the strictness in 'ma'!  Hence the use of strictApply1Dmd.

primop  CatchOp "catch#" GenPrimOp
          (State# RealWorld -> (# State# RealWorld, a #) )
       -> (b -> State# RealWorld -> (# State# RealWorld, a #) )
       -> State# RealWorld
       -> (# State# RealWorld, a #)
   with
   strictness  = { \ _arity -> mkClosedStrictSig [ lazyApply1Dmd
                                                 , lazyApply2Dmd
                                                 , topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  RaiseOp "raise#" GenPrimOp
   b -> o
      -- NB: the type variable "o" is "a", but with OpenKind
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True
     -- raise# certainly throws a Haskell exception and hence has_side_effects
     -- It doesn't actually make much difference because the fact that it
     -- returns bottom independently ensures that we are careful not to discard
     -- it.  But still, it's better to say the Right Thing.

-- Note [Arithmetic exception primops]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The RTS provides several primops to raise specific exceptions (raiseDivZero#,
-- raiseUnderflow#, raiseOverflow#). These primops are meant to be used by the
-- package implementing arbitrary precision numbers (Natural,Integer). It can't
-- depend on `base` package to raise exceptions in a normal way because it would
-- create a package dependency circle (base <-> bignum package).
--
-- See #14664

primtype Void#

primop  RaiseDivZeroOp "raiseDivZero#" GenPrimOp
   Void# -> o
   {Raise a 'DivideByZero' arithmetic exception.}
      -- NB: the type variable "o" is "a", but with OpenKind
      -- See Note [Arithmetic exception primops]
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  RaiseUnderflowOp "raiseUnderflow#" GenPrimOp
   Void# -> o
   {Raise an 'Underflow' arithmetic exception.}
      -- NB: the type variable "o" is "a", but with OpenKind
      -- See Note [Arithmetic exception primops]
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  RaiseOverflowOp "raiseOverflow#" GenPrimOp
   Void# -> o
   {Raise an 'Overflow' arithmetic exception.}
      -- NB: the type variable "o" is "a", but with OpenKind
      -- See Note [Arithmetic exception primops]
   with
   strictness  = { \ _arity -> mkClosedStrictSig [topDmd] botDiv }
   out_of_line = True
   has_side_effects = True

primop  RaiseIOOp "raiseIO#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, b #)
   with
   -- See Note [Precise exceptions and strictness analysis] in Demand.hs
   -- for why we give it topDiv
   -- strictness  = { \ _arity -> mkClosedStrictSig [topDmd, topDmd] topDiv }
   out_of_line = True
   has_side_effects = True

primop  MaskAsyncExceptionsOp "maskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  MaskUninterruptibleOp "maskUninterruptible#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
   out_of_line = True
   has_side_effects = True

primop  UnmaskAsyncExceptionsOp "unmaskAsyncExceptions#" GenPrimOp
        (State# RealWorld -> (# State# RealWorld, a #))
     -> (State# RealWorld -> (# State# RealWorld, a #))
   with
   strictness  = { \ _arity -> mkClosedStrictSig [strictApply1Dmd,topDmd] topDiv }
                 -- See Note [Strictness for mask/unmask/catch]
   out_of_line = True
   has_side_effects = True

primop  MaskStatus "getMaskingState#" GenPrimOp
        State# RealWorld -> (# State# RealWorld, Int# #)
   with
   out_of_line = True
   has_side_effects = True
-}