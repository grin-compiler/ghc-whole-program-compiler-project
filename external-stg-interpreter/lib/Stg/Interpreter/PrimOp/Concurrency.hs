{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Concurrency where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ("myThreadId#", [w]) -> pure [ThreadId] -- State# RealWorld -> (# State# RealWorld, ThreadId# #)
  ("noDuplicate#", [_s]) -> pure [] --  State# s -> State# s
  -------------------------

  -- fork# :: a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  -- forkOn# :: Int# -> a -> State# RealWorld -> (# State# RealWorld, ThreadId# #)
  -- killThread# :: ThreadId# -> a -> State# RealWorld -> State# RealWorld
  -- yield# :: State# RealWorld -> State# RealWorld
  -- myThreadId# :: State# RealWorld -> (# State# RealWorld, ThreadId# #)
  -- labelThread# :: ThreadId# -> Addr# -> State# RealWorld -> State# RealWorld
  -- isCurrentThreadBound# :: State# RealWorld -> (# State# RealWorld, Int# #)
  -- noDuplicate# :: State# s -> State# s
  -- threadStatus# :: ThreadId# -> State# RealWorld -> (# State# RealWorld, Int#, Int#, Int# #)

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