{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Word8 where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Word8#"
        {Operations on 8-bit unsigned integers.}
------------------------------------------------------------------------

primtype Word8#

primop Word8Extend "extendWord8#" GenPrimOp Word8# -> Word#
primop Word8Narrow "narrowWord8#" GenPrimOp Word# -> Word8#

primop Word8NotOp "notWord8#" Monadic Word8# -> Word8#

primop Word8AddOp "plusWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    commutable = True

primop Word8SubOp "subWord8#" Dyadic Word8# -> Word8# -> Word8#

primop Word8MulOp "timesWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    commutable = True

primop Word8QuotOp "quotWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    can_fail = True

primop Word8RemOp "remWord8#" Dyadic Word8# -> Word8# -> Word8#
  with
    can_fail = True

primop Word8QuotRemOp "quotRemWord8#" GenPrimOp Word8# -> Word8# -> (# Word8#, Word8# #)
  with
    can_fail = True

primop Word8EqOp "eqWord8#" Compare Word8# -> Word8# -> Int#
primop Word8GeOp "geWord8#" Compare Word8# -> Word8# -> Int#
primop Word8GtOp "gtWord8#" Compare Word8# -> Word8# -> Int#
primop Word8LeOp "leWord8#" Compare Word8# -> Word8# -> Int#
primop Word8LtOp "ltWord8#" Compare Word8# -> Word8# -> Int#
primop Word8NeOp "neWord8#" Compare Word8# -> Word8# -> Int#

-}