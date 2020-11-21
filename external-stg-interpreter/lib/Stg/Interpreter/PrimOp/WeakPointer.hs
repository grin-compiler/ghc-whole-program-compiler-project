{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.WeakPointer where

import Control.Monad.State
import qualified Data.Set as Set

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- mkWeak# :: o -> b -> (State# RealWorld -> (# State# RealWorld, c #)) -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeak#", [key, value, finalizer, _w]) -> do
    let wp = WeakPointer key value (Just finalizer)
    modify' $ \s@StgState{..} -> s {ssWeakPointers = Set.insert wp ssWeakPointers}
    pure [wp]

  -- mkWeakNoFinalizer# :: o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeakNoFinalizer#", [key, value, _w]) -> do
    let wp = WeakPointer key value Nothing
    modify' $ \s@StgState{..} -> s {ssWeakPointers = Set.insert wp ssWeakPointers}
    pure [wp]

  ( "touch#", [o, _s]) -> do
    -- o -> State# RealWorld -> State# RealWorld
    pure []

{-
  ("makeStablePtr#", [a, _s]) -> pure [StablePointer a] -- a -> State# RealWorld -> (# State# RealWorld, StablePtr# a #)
  ("deRefStablePtr#", [StablePointer a, _s]) -> pure [a] -- TODO: StablePtr# a -> State# RealWorld -> (# State# RealWorld, a #)
-}


  --------------------------

  -- mkWeak# :: o -> b
  --         -> (State# RealWorld -> (# State# RealWorld, c #))
  --         ->  State# RealWorld -> (# State# RealWorld, Weak# b #)

  -- mkWeakNoFinalizer# :: o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)

  -- addCFinalizerToWeak# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
  -- deRefWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
  -- finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, (State# RealWorld -> (# State# RealWorld, b #) ) #)
  -- touch# :: o -> State# RealWorld -> State# RealWorld

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Weak pointers"
------------------------------------------------------------------------

primtype Weak# b

-- note that tyvar "o" denotes openAlphaTyVar

primop  MkWeakOp "mkWeak#" GenPrimOp
   o -> b -> (State# RealWorld -> (# State# RealWorld, c #))
     -> State# RealWorld -> (# State# RealWorld, Weak# b #)
   { {\tt mkWeak# k v finalizer s} creates a weak reference to value {\tt k},
     with an associated reference to some value {\tt v}. If {\tt k} is still
     alive then {\tt v} can be retrieved using {\tt deRefWeak#}. Note that
     the type of {\tt k} must be represented by a pointer (i.e. of kind {\tt
     TYPE 'LiftedRep} or {\tt TYPE 'UnliftedRep}). }
   with
   has_side_effects = True
   out_of_line      = True

primop  MkWeakNoFinalizerOp "mkWeakNoFinalizer#" GenPrimOp
   o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
   with
   has_side_effects = True
   out_of_line      = True

primop  AddCFinalizerToWeakOp "addCFinalizerToWeak#" GenPrimOp
   Addr# -> Addr# -> Int# -> Addr# -> Weak# b
          -> State# RealWorld -> (# State# RealWorld, Int# #)
   { {\tt addCFinalizerToWeak# fptr ptr flag eptr w} attaches a C
     function pointer {\tt fptr} to a weak pointer {\tt w} as a finalizer. If
     {\tt flag} is zero, {\tt fptr} will be called with one argument,
     {\tt ptr}. Otherwise, it will be called with two arguments,
     {\tt eptr} and {\tt ptr}. {\tt addCFinalizerToWeak#} returns
     1 on success, or 0 if {\tt w} is already dead. }
   with
   has_side_effects = True
   out_of_line      = True

primop  DeRefWeakOp "deRefWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
   with
   has_side_effects = True
   out_of_line      = True

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

primop TouchOp "touch#" GenPrimOp
   o -> State# RealWorld -> State# RealWorld
   with
   code_size = { 0 }
   has_side_effects = True
-}