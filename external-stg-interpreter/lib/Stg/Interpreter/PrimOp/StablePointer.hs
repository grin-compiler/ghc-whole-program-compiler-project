{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.StablePointer where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- TODO: implement stable pointers properly
  ("makeStablePtr#", [a, s]) -> pure [StablePointer a] -- a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
  ("deRefStablePtr#", [StablePointer a, s]) -> pure [a] -- TODO: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Stable pointers and names"
------------------------------------------------------------------------

primtype StablePtr# a

primtype StableName# a

primop  MakeStablePtrOp "makeStablePtr#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefStablePtrOp "deRefStablePtr#" GenPrimOp
   StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  EqStablePtrOp "eqStablePtr#" GenPrimOp
   StablePtr# a -> StablePtr# a -> Int#
   with
   has_side_effects = True

primop  MakeStableNameOp "makeStableName#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
   with
   has_side_effects = True
   out_of_line      = True

primop  EqStableNameOp "eqStableName#" GenPrimOp
   StableName# a -> StableName# b -> Int#

primop  StableNameToIntOp "stableNameToInt#" GenPrimOp
   StableName# a -> Int#
-}