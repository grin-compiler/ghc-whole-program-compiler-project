{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.WeakPointer where

import Control.Monad.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

newWeakPointer :: Atom -> Atom -> Maybe Atom -> M Int
newWeakPointer key value finalizer = do
  weakPointers <- gets ssWeakPointers
  next <- gets ssNextWeakPointer
  let desc = WeakPtrDescriptor
        { wpdKey          = key
        , wpdVale         = Just value
        , wpdFinalizer    = finalizer
        , wpdCFinalizers  = []
        }

  modify' $ \s -> s {ssWeakPointers = IntMap.insert next desc weakPointers, ssNextWeakPointer = succ next}
  pure next

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- mkWeak# :: o -> b -> (State# RealWorld -> (# State# RealWorld, c #)) -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeak#", [key, value, finalizer, _w]) -> do
    wpId <- newWeakPointer key value (Just finalizer)
    pure [WeakPointer wpId]

  -- mkWeakNoFinalizer# :: o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeakNoFinalizer#", [key, value, _w]) -> do
    wpId <- newWeakPointer key value Nothing
    pure [WeakPointer wpId]

  -- addCFinalizerToWeak# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
  ( "addCFinalizerToWeak#", [fun, dataPtr, IntV hasEnv, envPtr, WeakPointer wpId, _w]) -> do
    wpd@WeakPtrDescriptor{..} <- lookupWeakPointerDescriptor wpId
    let desc = wpd {wpdCFinalizers = (fun, if hasEnv == 0 then Nothing else Just envPtr, dataPtr) : wpdCFinalizers}
    modify' $ \s@StgState{..} -> s {ssWeakPointers = IntMap.insert wpId desc ssWeakPointers}
    pure [IntV $ if wpdVale == Nothing then 0 else 1]

  -- deRefWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
  ( "deRefWeak#", [WeakPointer wpId, _w]) -> do
    WeakPtrDescriptor{..} <- lookupWeakPointerDescriptor wpId
    case wpdVale of
      Just v  -> pure [IntV 1, v]
      Nothing -> pure [IntV 0, LiftedUndefined]

  -- finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, (State# RealWorld -> (# State# RealWorld, b #) ) #)
  -- TODO

  -- touch# :: o -> State# RealWorld -> State# RealWorld
  ( "touch#", [_o, _s]) -> do
    -- see more about 'touch#': https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch
    pure []

  _ -> fallback op args t tc

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
