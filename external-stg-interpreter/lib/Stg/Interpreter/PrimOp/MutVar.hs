{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.MutVar where

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ("newMutVar#", [a, s]) -> do
    -- a -> State# s -> (# State# s, MutVar# s a #)
    state (\s@StgState{..} -> let next = IntMap.size ssMutVars in ([MutVar next], s {ssMutVars = IntMap.insert next a ssMutVars}))

  ("readMutVar#", [MutVar m, s]) -> do
    -- MutVar# s a -> State# s -> (# State# s, a #)
    a <- lookupMutVar m
    pure [a]

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Mutable variables"
        {Operations on MutVar\#s.}
------------------------------------------------------------------------

primtype MutVar# s a
        {A {\tt MutVar\#} behaves like a single-element mutable array.}

primop  NewMutVarOp "newMutVar#" GenPrimOp
   a -> State# s -> (# State# s, MutVar# s a #)
   {Create {\tt MutVar\#} with specified initial value in specified state thread.}
   with
   out_of_line = True
   has_side_effects = True

-- Note [Why MutVar# ops can't fail]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We don't label readMutVar# or writeMutVar# as can_fail.
-- This may seem a bit peculiar, because they surely *could*
-- fail spectacularly if passed a pointer to unallocated memory.
-- But MutVar#s are always correct by construction; we never
-- test if a pointer is valid before using it with these operations.
-- So we never have to worry about floating the pointer reference
-- outside a validity test. At the moment, has_side_effects blocks
-- up the relevant optimizations anyway, but we hope to draw finer
-- distinctions soon, which should improve matters for readMutVar#
-- at least.

primop  ReadMutVarOp "readMutVar#" GenPrimOp
   MutVar# s a -> State# s -> (# State# s, a #)
   {Read contents of {\tt MutVar\#}. Result is not yet evaluated.}
   with
   -- See Note [Why MutVar# ops can't fail]
   has_side_effects = True

primop  WriteMutVarOp "writeMutVar#"  GenPrimOp
   MutVar# s a -> a -> State# s -> State# s
   {Write contents of {\tt MutVar\#}.}
   with
   -- See Note [Why MutVar# ops can't fail]
   has_side_effects = True
   code_size = { primOpCodeSizeForeignCall } -- for the write barrier

primop  SameMutVarOp "sameMutVar#" GenPrimOp
   MutVar# s a -> MutVar# s a -> Int#

-- Note [Why not an unboxed tuple in atomicModifyMutVar2#?]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Looking at the type of atomicModifyMutVar2#, one might wonder why
-- it doesn't return an unboxed tuple. e.g.,
--
--   MutVar# s a -> (a -> (# a, b #)) -> State# s -> (# State# s, a, (# a, b #) #)
--
-- The reason is that atomicModifyMutVar2# relies on laziness for its atomicity.
-- Given a MutVar# containing x, atomicModifyMutVar2# merely replaces
-- its contents with a thunk of the form (fst (f x)). This can be done using an
-- atomic compare-and-swap as it is merely replacing a pointer.

primop  AtomicModifyMutVar2Op "atomicModifyMutVar2#" GenPrimOp
   MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
   { Modify the contents of a {\tt MutVar\#}, returning the previous
     contents and the result of applying the given function to the
     previous contents. Note that this isn't strictly
     speaking the correct type for this function; it should really be
     {\tt MutVar\# s a -> (a -> (a,b)) -> State\# s -> (\# State\# s, a, (a, b) \#)},
     but we don't know about pairs here. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  AtomicModifyMutVar_Op "atomicModifyMutVar_#" GenPrimOp
   MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
   { Modify the contents of a {\tt MutVar\#}, returning the previous
     contents and the result of applying the given function to the
     previous contents. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  CasMutVarOp "casMutVar#" GenPrimOp
  MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)
   with
   out_of_line = True
   has_side_effects = True
-}