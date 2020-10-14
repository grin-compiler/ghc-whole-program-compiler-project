{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.SmallArray where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Small Arrays"

        {Operations on {\tt SmallArray\#}. A {\tt SmallArray\#} works
         just like an {\tt Array\#}, but with different space use and
         performance characteristics (that are often useful with small
         arrays). The {\tt SmallArray\#} and {\tt SmallMutableArray#}
         lack a `card table'. The purpose of a card table is to avoid
         having to scan every element of the array on each GC by
         keeping track of which elements have changed since the last GC
         and only scanning those that have changed. So the consequence
         of there being no card table is that the representation is
         somewhat smaller and the writes are somewhat faster (because
         the card table does not need to be updated). The disadvantage
         of course is that for a {\tt SmallMutableArray#} the whole
         array has to be scanned on each GC. Thus it is best suited for
         use cases where the mutable array is not long lived, e.g.
         where a mutable array is initialised quickly and then frozen
         to become an immutable {\tt SmallArray\#}.
        }

------------------------------------------------------------------------

primtype SmallArray# a

primtype SmallMutableArray# s a

primop  NewSmallArrayOp "newSmallArray#" GenPrimOp
   Int# -> a -> State# s -> (# State# s, SmallMutableArray# s a #)
   {Create a new mutable array with the specified number of elements,
    in the specified state thread,
    with each element containing the specified initial value.}
   with
   out_of_line = True
   has_side_effects = True

primop  SameSmallMutableArrayOp "sameSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> SmallMutableArray# s a -> Int#

primop  ShrinkSmallMutableArrayOp_Char "shrinkSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> State# s -> State# s
   {Shrink mutable array to new specified size, in
    the specified state thread. The new size argument must be less than or
    equal to the current size as reported by {\tt sizeofSmallMutableArray\#}.}
   with out_of_line = True
        has_side_effects = True

primop  ReadSmallArrayOp "readSmallArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> State# s -> (# State# s, a #)
   {Read from specified index of mutable array. Result is not yet evaluated.}
   with
   has_side_effects = True
   can_fail         = True

primop  WriteSmallArrayOp "writeSmallArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> a -> State# s -> State# s
   {Write to specified index of mutable array.}
   with
   has_side_effects = True
   can_fail         = True

primop  SizeofSmallArrayOp "sizeofSmallArray#" GenPrimOp
   SmallArray# a -> Int#
   {Return the number of elements in the array.}

primop  SizeofSmallMutableArrayOp "sizeofSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> Int#
   {Return the number of elements in the array. Note that this is deprecated
   as it is unsafe in the presence of resize operations on the
   same byte array.}
   with deprecated_msg = { Use 'getSizeofSmallMutableArray#' instead }

primop  GetSizeofSmallMutableArrayOp "getSizeofSmallMutableArray#" GenPrimOp
   SmallMutableArray# s a -> State# s -> (# State# s, Int# #)
   {Return the number of elements in the array.}

primop  IndexSmallArrayOp "indexSmallArray#" GenPrimOp
   SmallArray# a -> Int# -> (# a #)
   {Read from specified index of immutable array. Result is packaged into
    an unboxed singleton; the result itself is not yet evaluated.}
   with
   can_fail         = True

primop  UnsafeFreezeSmallArrayOp "unsafeFreezeSmallArray#" GenPrimOp
   SmallMutableArray# s a -> State# s -> (# State# s, SmallArray# a #)
   {Make a mutable array immutable, without copying.}
   with
   has_side_effects = True

primop  UnsafeThawSmallArrayOp  "unsafeThawSmallArray#" GenPrimOp
   SmallArray# a -> State# s -> (# State# s, SmallMutableArray# s a #)
   {Make an immutable array mutable, without copying.}
   with
   out_of_line = True
   has_side_effects = True

-- The code_size is only correct for the case when the copy family of
-- primops aren't inlined. It would be nice to keep track of both.

primop  CopySmallArrayOp "copySmallArray#" GenPrimOp
  SmallArray# a -> Int# -> SmallMutableArray# s a -> Int# -> Int# -> State# s -> State# s
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

primop  CopySmallMutableArrayOp "copySmallMutableArray#" GenPrimOp
  SmallMutableArray# s a -> Int# -> SmallMutableArray# s a -> Int# -> Int# -> State# s -> State# s
  {Given a source array, an offset into the source array, a
   destination array, an offset into the destination array, and a
   number of elements to copy, copy the elements from the source array
   to the destination array. The source and destination arrays can
   refer to the same array. Both arrays must fully contain the
   specified ranges, but this is not checked.
   The regions are allowed to overlap, although this is only possible when the same
   array is provided as both the source and the destination. }
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneSmallArrayOp "cloneSmallArray#" GenPrimOp
  SmallArray# a -> Int# -> Int# -> SmallArray# a
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CloneSmallMutableArrayOp "cloneSmallMutableArray#" GenPrimOp
  SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  FreezeSmallArrayOp "freezeSmallArray#" GenPrimOp
  SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallArray# a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  ThawSmallArrayOp "thawSmallArray#" GenPrimOp
  SmallArray# a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  {Given a source array, an offset into the source array, and a number
   of elements to copy, create a new array with the elements from the
   source array. The provided array must fully contain the specified
   range, but this is not checked.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop CasSmallArrayOp  "casSmallArray#" GenPrimOp
   SmallMutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
   {Unsafe, machine-level atomic compare and swap on an element within an array.
    See the documentation of {\tt casArray\#}.}
   with
   out_of_line = True
   has_side_effects = True
-}