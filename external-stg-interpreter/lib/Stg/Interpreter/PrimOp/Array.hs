{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Array where

import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ("newArray#", [Literal (LitNumber LitNumInt i), a, s]) -> do
    -- Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
    let v = V.replicate (fromIntegral i) a
    state (\s@StgState{..} -> let next = IntMap.size ssMutableArrays in ([MutableArray next], s {ssMutableArrays = IntMap.insert next v ssMutableArrays}))

  ("readArray#", [MutableArray a, Literal (LitNumber LitNumInt i), s]) -> do
    -- MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
    v <- lookupMutableArray a
    pure [v V.! (fromIntegral i)]

  ("writeArray#", [MutableArray m, Literal (LitNumber LitNumInt i), a, s]) -> do
    -- MutableArray# s a -> Int# -> a -> State# s -> State# s
    v <- lookupMutableArray m
    modify' $ \s@StgState{..} -> s {ssMutableArrays = IntMap.insert m (v V.// [(fromIntegral i, a)]) ssMutableArrays}
    pure [s]

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Arrays"
        {Operations on {\tt Array\#}.}
------------------------------------------------------------------------

primtype Array# a

primtype MutableArray# s a

primop  NewArrayOp "newArray#" GenPrimOp
   Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
   {Create a new mutable array with the specified number of elements,
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   out_of_line = True
   has_side_effects = True

primop  SameMutableArrayOp "sameMutableArray#" GenPrimOp
   MutableArray# s a -> MutableArray# s a -> Int#

primop  ReadArrayOp "readArray#" GenPrimOp
   MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
   {Read from specified index of mutable array. Result is not yet evaluated.}
   with
   has_side_effects = True
   can_fail         = True

primop  WriteArrayOp "writeArray#" GenPrimOp
   MutableArray# s a -> Int# -> a -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   has_side_effects = True
   can_fail         = True
   code_size        = 2 -- card update too

primop  SizeofArrayOp "sizeofArray#" GenPrimOp
   Array# a -> Int#
   {Return the number of elements in the array.}

primop  SizeofMutableArrayOp "sizeofMutableArray#" GenPrimOp
   MutableArray# s a -> Int#
   {Return the number of elements in the array.}

primop  IndexArrayOp "indexArray#" GenPrimOp
   Array# a -> Int# -> (# a #)
   {Read from the specified index of an immutable array. The result is packaged
    into an unboxed unary tuple; the result itself is not yet
    evaluated. Pattern matching on the tuple forces the indexing of the
    array to happen but does not evaluate the element itself. Evaluating
    the thunk prevents additional thunks from building up on the
    heap. Avoiding these thunks, in turn, reduces references to the
    argument array, allowing it to be garbage collected more promptly.}
   with
   can_fail         = True

primop  UnsafeFreezeArrayOp "unsafeFreezeArray#" GenPrimOp
   MutableArray# s a -> State# s -> (# State# s, Array# a #)
   {Make a mutable array immutable, without copying.}
   with
   has_side_effects = True

primop  UnsafeThawArrayOp  "unsafeThawArray#" GenPrimOp
   Array# a -> State# s -> (# State# s, MutableArray# s a #)
   {Make an immutable array mutable, without copying.}
   with
   out_of_line = True
   has_side_effects = True

primop  CopyArrayOp "copyArray#" GenPrimOp
  Array# a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Given a source array, an offset into the source array, a
   destination array, an offset into the destination array, and a
   number of elements to copy, copy the elements from the source array
   to the destination array. Both arrays must fully contain the
   specified ranges, but this is not checked. The two arrays must not
   be the same array in different states, but this is not checked
   either.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CopyMutableArrayOp "copyMutableArray#" GenPrimOp
  MutableArray# s a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Given a source array, an offset into the source array, a
   destination array, an offset into the destination array, and a
   number of elements to copy, copy the elements from the source array
   to the destination array. Both arrays must fully contain the
   specified ranges, but this is not checked. In the case where
   the source and destination are the same array the source and
   destination regions may overlap.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneArrayOp "cloneArray#" GenPrimOp
  Array# a -> Int# -> Int# -> Array# a
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneMutableArrayOp "cloneMutableArray#" GenPrimOp
  MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  FreezeArrayOp "freezeArray#" GenPrimOp
  MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, Array# a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  ThawArrayOp "thawArray#" GenPrimOp
  Array# a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop CasArrayOp  "casArray#" GenPrimOp
   MutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
   {Given an array, an offset, the expected old value, and
    the new value, perform an atomic compare and swap (i.e. write the new
    value if the current value and the old value are the same pointer).
    Returns 0 if the swap succeeds and 1 if it fails. Additionally, returns
    the element at the offset after the operation completes. This means that
    on a success the new value is returned, and on a failure the actual old
    value (not the expected one) is returned. Implies a full memory barrier.
    The use of a pointer equality on a lifted value makes this function harder
    to use correctly than {\tt casIntArray\#}. All of the difficulties
    of using {\tt reallyUnsafePtrEquality\#} correctly apply to
    {\tt casArray\#} as well.
   }
   with
   out_of_line = True
   has_side_effects = True
-}