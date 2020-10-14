{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Int16 where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Int16#"
        {Operations on 16-bit integers.}
------------------------------------------------------------------------

primtype Int16#

primop Int16Extend "extendInt16#" GenPrimOp Int16# -> Int#
primop Int16Narrow "narrowInt16#" GenPrimOp Int# -> Int16#

primop Int16NegOp "negateInt16#" Monadic Int16# -> Int16#

primop Int16AddOp "plusInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    commutable = True

primop Int16SubOp "subInt16#" Dyadic Int16# -> Int16# -> Int16#

primop Int16MulOp "timesInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    commutable = True

primop Int16QuotOp "quotInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    can_fail = True

primop Int16RemOp "remInt16#" Dyadic Int16# -> Int16# -> Int16#
  with
    can_fail = True

primop Int16QuotRemOp "quotRemInt16#" GenPrimOp Int16# -> Int16# -> (# Int16#, Int16# #)
  with
    can_fail = True

primop Int16EqOp "eqInt16#" Compare Int16# -> Int16# -> Int#
primop Int16GeOp "geInt16#" Compare Int16# -> Int16# -> Int#
primop Int16GtOp "gtInt16#" Compare Int16# -> Int16# -> Int#
primop Int16LeOp "leInt16#" Compare Int16# -> Int16# -> Int#
primop Int16LtOp "ltInt16#" Compare Int16# -> Int16# -> Int#
primop Int16NeOp "neInt16#" Compare Int16# -> Int16# -> Int#
-}