{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Array where

import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i  = Literal (LitNumber LitNumInt i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newArray# :: Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
  ("newArray#", [IntV i, a, _s]) -> do
    mutableArrays <- gets ssMutableArrays
    let next  = IntMap.size mutableArrays
        v     = V.replicate (fromIntegral i) a
    modify' $ \s -> s {ssMutableArrays = IntMap.insert next v mutableArrays}
    pure [MutableArray next]

  -- sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Int#
  ("sameMutableArray#", [MutableArray a, MutableArray b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  -- readArray# :: MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
  ("readArray#", [MutableArray a, IntV i, _s]) -> do
    v <- lookupMutableArray a
    pure [v V.! (fromIntegral i)]

  -- writeArray# :: MutableArray# s a -> Int# -> a -> State# s -> State# s
  ("writeArray#", [MutableArray m, IntV i, a, _s]) -> do
    v <- lookupMutableArray m
    modify' $ \s@StgState{..} -> s {ssMutableArrays = IntMap.insert m (v V.// [(fromIntegral i, a)]) ssMutableArrays}
    pure []

  -- sizeofArray# :: Array# a -> Int#
  ("sizeofArray#", [Array a]) -> do
    v <- lookupArray a
    pure [IntV . fromIntegral $ V.length v]

  -- sizeofMutableArray# :: MutableArray# s a -> Int#
  ("sizeofMutableArray#", [MutableArray a]) -> do
    v <- lookupMutableArray a
    pure [IntV . fromIntegral $ V.length v]

  -- indexArray# :: Array# a -> Int# -> (# a #)
  ("indexArray#", [Array a, IntV i]) -> do
    v <- lookupArray a
    pure [v V.! (fromIntegral i)]

  -- TODO: unsafeFreezeArray# :: MutableArray# s a -> State# s -> (# State# s, Array# a #)
  -- TODO: unsafeThawArray# :: Array# a -> State# s -> (# State# s, MutableArray# s a #)
  -- TODO: copyArray# :: Array# a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  -- TODO: copyMutableArray# :: MutableArray# s a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  -- TODO: cloneArray# :: Array# a -> Int# -> Int# -> Array# a
  -- TODO: cloneMutableArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  -- TODO: freezeArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, Array# a #)
  -- TODO: thawArray# :: Array# a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  -- TODO: casArray# :: MutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Arrays"
        {Operations on {\tt Array\#}.}
------------------------------------------------------------------------

primtype Array# a

primtype MutableArray# s a

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