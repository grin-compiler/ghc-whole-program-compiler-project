{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Int8 where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Int8#"
        {Operations on 8-bit integers.}
------------------------------------------------------------------------

primtype Int8#

primop Int8Extend "extendInt8#" GenPrimOp Int8# -> Int#
primop Int8Narrow "narrowInt8#" GenPrimOp Int# -> Int8#

primop Int8NegOp "negateInt8#" Monadic Int8# -> Int8#

primop Int8AddOp "plusInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    commutable = True

primop Int8SubOp "subInt8#" Dyadic Int8# -> Int8# -> Int8#

primop Int8MulOp "timesInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    commutable = True

primop Int8QuotOp "quotInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    can_fail = True

primop Int8RemOp "remInt8#" Dyadic Int8# -> Int8# -> Int8#
  with
    can_fail = True

primop Int8QuotRemOp "quotRemInt8#" GenPrimOp Int8# -> Int8# -> (# Int8#, Int8# #)
  with
    can_fail = True

primop Int8EqOp "eqInt8#" Compare Int8# -> Int8# -> Int#
primop Int8GeOp "geInt8#" Compare Int8# -> Int8# -> Int#
primop Int8GtOp "gtInt8#" Compare Int8# -> Int8# -> Int#
primop Int8LeOp "leInt8#" Compare Int8# -> Int8# -> Int#
primop Int8LtOp "ltInt8#" Compare Int8# -> Int8# -> Int#
primop Int8NeOp "neInt8#" Compare Int8# -> Int8# -> Int#
-}