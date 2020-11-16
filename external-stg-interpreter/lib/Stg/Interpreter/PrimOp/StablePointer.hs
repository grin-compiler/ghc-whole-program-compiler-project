{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.StablePointer where

import Foreign.Ptr
import Control.Monad.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- makeStablePtr# :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
  ( "makeStablePtr#", [a, _s]) -> do
    stablePtrs <- gets ssStablePointers
    let next = IntMap.size stablePtrs
    modify' $ \s -> s {ssStablePointers = IntMap.insert next a stablePtrs}
    pure [PtrAtom (StablePtr next) . intPtrToPtr $ IntPtr next]

  -- deRefStablePtr# :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
  ( "deRefStablePtr#", [PtrAtom (StablePtr _index) p, _s]) -> do
    pure <$> lookupStablePointerPtr p

  -- eqStablePtr# :: StablePtr# a -> StablePtr# a -> Int#
  ( "eqStablePtr#", [PtrAtom _ a, PtrAtom _ b]) -> do
      pure [IntV $ if a == b then 1 else 0]

{-
  -- makeStableName# :: a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
  ( "makeStableName#", [a, _s]) -> do
    undefined -- TODO

  -- eqStableName# :: StableName# a -> StableName# b -> Int#
  ( "eqStableName#", [StableName a, StableName b]) -> do
    undefined -- TODO

  -- stableNameToInt# :: StableName# a -> Int#
  ( "stableNameToInt#", [StableName a]) -> do
    undefined -- TODO
-}
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
