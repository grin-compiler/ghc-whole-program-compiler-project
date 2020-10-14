{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.ArrayArray where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Arrays of arrays"
        {Operations on {\tt ArrayArray\#}. An {\tt ArrayArray\#} contains references to {\em unpointed}
         arrays, such as {\tt ByteArray\#s}. Hence, it is not parameterised by the element types,
         just like a {\tt ByteArray\#}, but it needs to be scanned during GC, just like an {\tt Array\#}.
         We represent an {\tt ArrayArray\#} exactly as a {\tt Array\#}, but provide element-type-specific
         indexing, reading, and writing.}
------------------------------------------------------------------------

primtype ArrayArray#

primtype MutableArrayArray# s

primop  NewArrayArrayOp "newArrayArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableArrayArray# s #)
   {Create a new mutable array of arrays with the specified number of elements,
    in the specified state thread, with each element recursively referring to the
    newly created array.}
   with
   out_of_line = True
   has_side_effects = True

primop  SameMutableArrayArrayOp "sameMutableArrayArray#" GenPrimOp
   MutableArrayArray# s -> MutableArrayArray# s -> Int#

primop  UnsafeFreezeArrayArrayOp "unsafeFreezeArrayArray#" GenPrimOp
   MutableArrayArray# s -> State# s -> (# State# s, ArrayArray# #)
   {Make a mutable array of arrays immutable, without copying.}
   with
   has_side_effects = True

primop  SizeofArrayArrayOp "sizeofArrayArray#" GenPrimOp
   ArrayArray# -> Int#
   {Return the number of elements in the array.}

primop  SizeofMutableArrayArrayOp "sizeofMutableArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int#
   {Return the number of elements in the array.}

primop IndexArrayArrayOp_ByteArray "indexByteArrayArray#" GenPrimOp
   ArrayArray# -> Int# -> ByteArray#
   with can_fail = True

primop IndexArrayArrayOp_ArrayArray "indexArrayArrayArray#" GenPrimOp
   ArrayArray# -> Int# -> ArrayArray#
   with can_fail = True

primop  ReadArrayArrayOp_ByteArray "readByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, ByteArray# #)
   with has_side_effects = True
        can_fail = True

primop  ReadArrayArrayOp_MutableByteArray "readMutableByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
   with has_side_effects = True
        can_fail = True

primop  ReadArrayArrayOp_ArrayArray "readArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, ArrayArray# #)
   with has_side_effects = True
        can_fail = True

primop  ReadArrayArrayOp_MutableArrayArray "readMutableArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_ByteArray "writeByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> ByteArray# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_MutableByteArray "writeMutableByteArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> MutableByteArray# s -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_ArrayArray "writeArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> ArrayArray# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteArrayArrayOp_MutableArrayArray "writeMutableArrayArrayArray#" GenPrimOp
   MutableArrayArray# s -> Int# -> MutableArrayArray# s -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  CopyArrayArrayOp "copyArrayArray#" GenPrimOp
  ArrayArray# -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the ArrayArray\# to the specified region in the MutableArrayArray\#.
   Both arrays must fully contain the specified ranges, but this is not checked.
   The two arrays must not be the same array in different states, but this is not checked either.}
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True

primop  CopyMutableArrayArrayOp "copyMutableArrayArray#" GenPrimOp
  MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableArrayArray# to the specified region in the second
   MutableArrayArray#.
   Both arrays must fully contain the specified ranges, but this is not checked.
   The regions are allowed to overlap, although this is only possible when the same
   array is provided as both the source and the destination.
   }
  with
  out_of_line      = True
  has_side_effects = True
  can_fail         = True
-}