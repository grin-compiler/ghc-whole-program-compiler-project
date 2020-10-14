{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.TagToEnum where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Tag to enum stuff"
        {Convert back and forth between values of enumerated types
        and small integers.}
------------------------------------------------------------------------

primop  DataToTagOp "dataToTag#" GenPrimOp
   a -> Int#  -- Zero-indexed; the first constructor has tag zero
   with
   strictness = { \ _arity -> mkClosedStrictSig [evalDmd] topDiv }
   -- See Note [dataToTag# magic] in GHC.Core.Op.ConstantFold

primop  TagToEnumOp "tagToEnum#" GenPrimOp
   Int# -> a
-}
