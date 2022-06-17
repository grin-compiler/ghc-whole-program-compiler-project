{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.WeakPointer where

import Control.Effect.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

newWeakPointer :: M sig m => AtomAddr -> AtomAddr -> Maybe AtomAddr -> m Int
newWeakPointer key value finalizer = do
  weakPointers <- gets ssWeakPointers
  next <- gets ssNextWeakPointer
  let desc = WeakPtrDescriptor
        { wpdKey          = key
        , wpdValue        = Just value
        , wpdFinalizer    = finalizer
        , wpdCFinalizers  = []
        }

  modify $ \s -> s {ssWeakPointers = IntMap.insert next desc weakPointers, ssNextWeakPointer = succ next}
  pure next

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args, argsAddr) of

  -- mkWeak# :: o -> b -> (State# RealWorld -> (# State# RealWorld, c #)) -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeak#", _, [key, value, finalizer, _w]) -> do
    wpId <- newWeakPointer key value (Just finalizer)
    allocAtoms [WeakPointer wpId]

  -- mkWeakNoFinalizer# :: o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeakNoFinalizer#", _, [key, value, _w]) -> do
    wpId <- newWeakPointer key value Nothing
    allocAtoms [WeakPointer wpId]

  -- addCFinalizerToWeak# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
  ( "addCFinalizerToWeak#", [_fun, _dataPtr, IntV hasEnv, _envPtr, WeakPointer wpId, _w], [fun, dataPtr, _, envPtr, _, _]) -> do
    wpd@WeakPtrDescriptor{..} <- lookupWeakPointerDescriptor wpId
    let desc = wpd {wpdCFinalizers = (fun, if hasEnv == 0 then Nothing else Just envPtr, dataPtr) : wpdCFinalizers}
    modify $ \s@StgState{..} -> s {ssWeakPointers = IntMap.insert wpId desc ssWeakPointers}
    allocAtoms [IntV $ if wpdValue == Nothing then 0 else 1]

  -- deRefWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
  ( "deRefWeak#", [WeakPointer wpId, _w], _) -> do
    WeakPtrDescriptor{..} <- lookupWeakPointerDescriptor wpId
    case wpdValue of
      Just v  -> (:) <$> storeNewAtom (IntV 1) <*> pure [v]
      Nothing -> allocAtoms [IntV 0, LiftedUndefined]

  -- finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, (State# RealWorld -> (# State# RealWorld, b #) ) #)
  -- TODO

  -- touch# :: o -> State# RealWorld -> State# RealWorld
  ( "touch#", [_o, _s], _) -> do
    -- see more about 'touch#': https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch
    pure []

  _ -> fallback op argsAddr t tc

{-

primop  FinalizeWeakOp "finalizeWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#,
              (State# RealWorld -> (# State# RealWorld, b #) ) #)
   { Finalize a weak pointer. The return value is an unboxed tuple
     containing the new state of the world and an "unboxed Maybe",
     represented by an {\tt Int#} and a (possibly invalid) finalization
     action. An {\tt Int#} of {\tt 1} indicates that the finalizer is valid. The
     return value {\tt b} from the finalizer should be ignored. }
   with
   has_side_effects = True
   out_of_line      = True
-}
