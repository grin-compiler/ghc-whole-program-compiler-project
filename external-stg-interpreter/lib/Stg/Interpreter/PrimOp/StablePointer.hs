{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.StablePointer where

import Foreign.Ptr
import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

  -- Stable Pointer

      -- makeStablePtr# :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
  defOp "makeStablePtr#" $ \[a, _s] -> do
    stablePtrs <- gets ssStablePointers
    next <- gets ssNextStablePointer
    modify' $ \s -> s {ssStablePointers = IntMap.insert next a stablePtrs, ssNextStablePointer = succ next}
    pure [PtrAtom (StablePtr next) . intPtrToPtr $ IntPtr next]

      -- deRefStablePtr# :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
  defOp "deRefStablePtr#" $ \[PtrAtom (StablePtr _index) p, _s] -> do
    pure <$> lookupStablePointerPtr p

      -- eqStablePtr# :: StablePtr# a -> StablePtr# a -> Int#
  defOp "eqStablePtr#" $ \[PtrAtom _ a, PtrAtom _ b] -> do
    pure [IntV $ if a == b then 1 else 0]


  -- Stable Name

      -- makeStableName# :: a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
  defOp "makeStableName#" $ \[a, _s] -> do
    snMap <- gets ssStableNameMap
    case Map.lookup a snMap of
      Just snId -> pure [StableName snId]
      Nothing -> do
        snId <- gets ssNextStableName
        modify' $ \s -> s {ssStableNameMap = Map.insert a snId snMap, ssNextStableName = succ snId}
        pure [StableName snId]

      -- eqStableName# :: StableName# a -> StableName# b -> Int#
  defOp "eqStableName#" $ \[StableName a, StableName b] -> do
    pure [IntV $ if a == b then 1 else 0]

      -- stableNameToInt# :: StableName# a -> Int#
  defOp "stableNameToInt#" $ \[StableName snId] -> do
    pure [IntV snId]
