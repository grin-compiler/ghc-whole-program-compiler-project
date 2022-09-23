{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.StablePointer where

import Foreign.Ptr
import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- Stable Pointer

  -- makeStablePtr# :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
  ( "makeStablePtr#", [a, _s]) -> do
    stablePtrs <- gets ssStablePointers
    next <- gets ssNextStablePointer
    modify' $ \s -> s {ssStablePointers = IntMap.insert next a stablePtrs, ssNextStablePointer = succ next}
    pure [PtrAtom (StablePtr next) . intPtrToPtr $ IntPtr next]

  -- deRefStablePtr# :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
  ( "deRefStablePtr#", [PtrAtom (StablePtr _index) p, _s]) -> do
    pure <$> lookupStablePointerPtr p
  -- TODO: handle this in a better and more uniform way
  ( "deRefStablePtr#", [PtrAtom RawPtr p, _s]) -> do
    pure <$> lookupStablePointerPtr p

  -- eqStablePtr# :: StablePtr# a -> StablePtr# a -> Int#
  ( "eqStablePtr#", [PtrAtom _ a, PtrAtom _ b]) -> do
    pure [IntV $ if a == b then 1 else 0]


  -- Stable Name

  -- makeStableName# :: a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
  ( "makeStableName#", [a, _s]) -> do
    snMap <- gets ssStableNameMap
    case Map.lookup a snMap of
      Just snId -> pure [StableName snId]
      Nothing -> do
        snId <- gets ssNextStableName
        modify' $ \s -> s {ssStableNameMap = Map.insert a snId snMap, ssNextStableName = succ snId}
        pure [StableName snId]

  -- stableNameToInt# :: StableName# a -> Int#
  ( "stableNameToInt#", [StableName snId]) -> do
    pure [IntV snId]

  -- OBSOLETE from GHC 9.4
  -- eqStableName# :: StableName# a -> StableName# b -> Int#
  ( "eqStableName#", [StableName a, StableName b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  _ -> fallback op args t tc
