{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Char where

import Data.Char

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ("ord#", [Literal (LitChar c)]) -> do
    -- Char# -> Int#
    pure [Literal (LitNumber LitNumInt . fromIntegral $ ord c)]

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Char#"
        {Operations on 31-bit characters.}
------------------------------------------------------------------------

primtype Char#

primop   CharGtOp  "gtChar#"   Compare   Char# -> Char# -> Int#
primop   CharGeOp  "geChar#"   Compare   Char# -> Char# -> Int#

primop   CharEqOp  "eqChar#"   Compare
   Char# -> Char# -> Int#
   with commutable = True

primop   CharNeOp  "neChar#"   Compare
   Char# -> Char# -> Int#
   with commutable = True

primop   CharLtOp  "ltChar#"   Compare   Char# -> Char# -> Int#
primop   CharLeOp  "leChar#"   Compare   Char# -> Char# -> Int#

primop   OrdOp   "ord#"  GenPrimOp   Char# -> Int#
   with code_size = 0
-}
{-
-- Char
evalPrimOp CharGtOp [CharV a, CharV b] = IntV $ if a > b  then 1 else 0
evalPrimOp CharGeOp [CharV a, CharV b] = IntV $ if a >= b then 1 else 0
evalPrimOp CharEqOp [CharV a, CharV b] = IntV $ if a == b then 1 else 0
evalPrimOp CharNeOp [CharV a, CharV b] = IntV $ if a /= b then 1 else 0
evalPrimOp CharLtOp [CharV a, CharV b] = IntV $ if a < b  then 1 else 0
evalPrimOp CharLeOp [CharV a, CharV b] = IntV $ if a <= b then 1 else 0
evalPrimOp OrdOp    [CharV a] = IntV $ fromIntegral a -- HINT: noop ; same bit level representation
-}