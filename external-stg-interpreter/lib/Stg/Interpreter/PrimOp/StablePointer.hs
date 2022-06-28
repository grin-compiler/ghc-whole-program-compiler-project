{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.StablePointer where

import Foreign.Ptr
import Control.Effect.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args, argsAddr) of

  -- Stable Pointer

  -- makeStablePtr# :: a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
  ( "makeStablePtr#", _, [a, _s]) -> do
    next <- freshStablePointerAddress
    stablePtrs <- gets ssStablePointers
    modify $ \s -> s {ssStablePointers = IntMap.insert next a stablePtrs}
    allocAtoms [PtrAtom (StablePtr next) . intPtrToPtr $ IntPtr next]

  -- deRefStablePtr# :: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
  ( "deRefStablePtr#", [PtrAtom (StablePtr _index) p, _s], _) -> do
    -- HINT: create list
    pure <$> lookupStablePointerPtr p
  -- TODO: handle this in a better and more uniform way
  ( "deRefStablePtr#", [PtrAtom RawPtr p, _s], _) -> do
    -- HINT: create list
    pure <$> lookupStablePointerPtr p

  -- eqStablePtr# :: StablePtr# a -> StablePtr# a -> Int#
  ( "eqStablePtr#", [PtrAtom _ a, PtrAtom _ b], _) -> do
    allocAtoms [IntV $ if a == b then 1 else 0]


  -- Stable Name

  -- makeStableName# :: a -> State# RealWorld -> (# State# RealWorld, StableName# a #)
  ( "makeStableName#", _, [a, _s]) -> do
    snMap <- gets ssStableNameMap
    case IntMap.lookup a snMap of
      Just snId -> allocAtoms [StableName snId]
      Nothing -> do
        snId <- freshStableNameAddress
        modify $ \s -> s {ssStableNameMap = IntMap.insert a snId snMap}
        allocAtoms [StableName snId]

  -- eqStableName# :: StableName# a -> StableName# b -> Int#
  ( "eqStableName#", [StableName a, StableName b], _) -> do
    allocAtoms [IntV $ if a == b then 1 else 0]

  -- stableNameToInt# :: StableName# a -> Int#
  ( "stableNameToInt#", [StableName snId], _) -> do
    allocAtoms [IntV snId]

  _ -> fallback op argsAddr t tc
