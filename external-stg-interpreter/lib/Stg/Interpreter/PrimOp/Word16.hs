{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Word16 where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Word16#"
        {Operations on 16-bit unsigned integers.}
------------------------------------------------------------------------

primtype Word16#

primop Word16Extend "extendWord16#" GenPrimOp Word16# -> Word#
primop Word16Narrow "narrowWord16#" GenPrimOp Word# -> Word16#

primop Word16NotOp "notWord16#" Monadic Word16# -> Word16#

primop Word16AddOp "plusWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    commutable = True

primop Word16SubOp "subWord16#" Dyadic Word16# -> Word16# -> Word16#

primop Word16MulOp "timesWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    commutable = True

primop Word16QuotOp "quotWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    can_fail = True

primop Word16RemOp "remWord16#" Dyadic Word16# -> Word16# -> Word16#
  with
    can_fail = True

primop Word16QuotRemOp "quotRemWord16#" GenPrimOp Word16# -> Word16# -> (# Word16#, Word16# #)
  with
    can_fail = True

primop Word16EqOp "eqWord16#" Compare Word16# -> Word16# -> Int#
primop Word16GeOp "geWord16#" Compare Word16# -> Word16# -> Int#
primop Word16GtOp "gtWord16#" Compare Word16# -> Word16# -> Int#
primop Word16LeOp "leWord16#" Compare Word16# -> Word16# -> Int#
primop Word16LtOp "ltWord16#" Compare Word16# -> Word16# -> Int#
primop Word16NeOp "neWord16#" Compare Word16# -> Word16# -> Int#
-}