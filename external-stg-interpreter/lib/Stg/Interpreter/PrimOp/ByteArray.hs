{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.ByteArray where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ("newPinnedByteArray#", [Literal (LitNumber LitNumInt size), s]) -> do
    -- Int# -> State# s -> (# State# s, MutableByteArray# s #)
    pure [RtsPrim]

  ("newAlignedPinnedByteArray#", [Literal (LitNumber LitNumInt size), Literal (LitNumber LitNumInt align), s]) -> do
    -- Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
    pure [RtsPrim]

  ("unsafeFreezeByteArray#", [a, s]) -> do
    -- MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
    pure [RtsPrim]

  ("byteArrayContents#", [a]) -> do
    -- ByteArray# -> Addr#
    pure [Literal LitNullAddr]

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Byte Arrays"
        {Operations on {\tt ByteArray\#}. A {\tt ByteArray\#} is a just a region of
         raw memory in the garbage-collected heap, which is not
         scanned for pointers. It carries its own size (in bytes).
         There are
         three sets of operations for accessing byte array contents:
         index for reading from immutable byte arrays, and read/write
         for mutable byte arrays.  Each set contains operations for a
         range of useful primitive data types.  Each operation takes
         an offset measured in terms of the size of the primitive type
         being read or written.}

------------------------------------------------------------------------

primtype ByteArray#

primtype MutableByteArray# s

primop  NewByteArrayOp_Char "newByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Create a new mutable byte array of specified size (in bytes), in
    the specified state thread.}
   with out_of_line = True
        has_side_effects = True

primop  NewPinnedByteArrayOp_Char "newPinnedByteArray#" GenPrimOp
   Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Create a mutable byte array that the GC guarantees not to move.}
   with out_of_line = True
        has_side_effects = True

primop  NewAlignedPinnedByteArrayOp_Char "newAlignedPinnedByteArray#" GenPrimOp
   Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
   {Create a mutable byte array, aligned by the specified amount, that the GC guarantees not to move.}
   with out_of_line = True
        has_side_effects = True

primop  MutableByteArrayIsPinnedOp "isMutableByteArrayPinned#" GenPrimOp
   MutableByteArray# s -> Int#
   {Determine whether a {\tt MutableByteArray\#} is guaranteed not to move
   during GC.}
   with out_of_line = True

primop  ByteArrayIsPinnedOp "isByteArrayPinned#" GenPrimOp
   ByteArray# -> Int#
   {Determine whether a {\tt ByteArray\#} is guaranteed not to move during GC.}
   with out_of_line = True

primop  ByteArrayContents_Char "byteArrayContents#" GenPrimOp
   ByteArray# -> Addr#
   {Intended for use with pinned arrays; otherwise very unsafe!}

primop  SameMutableByteArrayOp "sameMutableByteArray#" GenPrimOp
   MutableByteArray# s -> MutableByteArray# s -> Int#

primop  ShrinkMutableByteArrayOp_Char "shrinkMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> State# s
   {Shrink mutable byte array to new specified size (in bytes), in
    the specified state thread. The new size argument must be less than or
    equal to the current size as reported by {\tt sizeofMutableByteArray\#}.}
   with out_of_line = True
        has_side_effects = True

primop  ResizeMutableByteArrayOp_Char "resizeMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
   {Resize (unpinned) mutable byte array to new specified size (in bytes).
    The returned {\tt MutableByteArray\#} is either the original
    {\tt MutableByteArray\#} resized in-place or, if not possible, a newly
    allocated (unpinned) {\tt MutableByteArray\#} (with the original content
    copied over).

    To avoid undefined behaviour, the original {\tt MutableByteArray\#} shall
    not be accessed anymore after a {\tt resizeMutableByteArray\#} has been
    performed.  Moreover, no reference to the old one should be kept in order
    to allow garbage collection of the original {\tt MutableByteArray\#} in
    case a new {\tt MutableByteArray\#} had to be allocated.}
   with out_of_line = True
        has_side_effects = True

primop  UnsafeFreezeByteArrayOp "unsafeFreezeByteArray#" GenPrimOp
   MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
   {Make a mutable byte array immutable, without copying.}
   with
   has_side_effects = True

primop  SizeofByteArrayOp "sizeofByteArray#" GenPrimOp
   ByteArray# -> Int#
   {Return the size of the array in bytes.}

primop  SizeofMutableByteArrayOp "sizeofMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int#
   {Return the size of the array in bytes. Note that this is deprecated as it is
   unsafe in the presence of resize operations on the same byte
   array.}
   with deprecated_msg = { Use 'getSizeofMutableByteArray#' instead }

primop  GetSizeofMutableByteArrayOp "getSizeofMutableByteArray#" GenPrimOp
   MutableByteArray# s -> State# s -> (# State# s, Int# #)
   {Return the number of elements in the array.}

primop IndexByteArrayOp_Char "indexCharArray#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_WideChar "indexWideCharArray#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 31-bit character; offset in 4-byte words.}
   with can_fail = True

primop IndexByteArrayOp_Int "indexIntArray#" GenPrimOp
   ByteArray# -> Int# -> Int#
   with can_fail = True

primop IndexByteArrayOp_Word "indexWordArray#" GenPrimOp
   ByteArray# -> Int# -> Word#
   with can_fail = True

primop IndexByteArrayOp_Addr "indexAddrArray#" GenPrimOp
   ByteArray# -> Int# -> Addr#
   with can_fail = True

primop IndexByteArrayOp_Float "indexFloatArray#" GenPrimOp
   ByteArray# -> Int# -> Float#
   with can_fail = True

primop IndexByteArrayOp_Double "indexDoubleArray#" GenPrimOp
   ByteArray# -> Int# -> Double#
   with can_fail = True

primop IndexByteArrayOp_StablePtr "indexStablePtrArray#" GenPrimOp
   ByteArray# -> Int# -> StablePtr# a
   with can_fail = True

primop IndexByteArrayOp_Int8 "indexInt8Array#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 8-bit integer; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Int16 "indexInt16Array#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 16-bit integer; offset in 16-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Int32 "indexInt32Array#" GenPrimOp
   ByteArray# -> Int# -> INT32
   {Read 32-bit integer; offset in 32-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Int64 "indexInt64Array#" GenPrimOp
   ByteArray# -> Int# -> INT64
   {Read 64-bit integer; offset in 64-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word8 "indexWord8Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 8-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word16 "indexWord16Array#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 16-bit word; offset in 16-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word32 "indexWord32Array#" GenPrimOp
   ByteArray# -> Int# -> WORD32
   {Read 32-bit word; offset in 32-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word64 "indexWord64Array#" GenPrimOp
   ByteArray# -> Int# -> WORD64
   {Read 64-bit word; offset in 64-bit words.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsChar "indexWord8ArrayAsChar#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWideChar "indexWord8ArrayAsWideChar#" GenPrimOp
   ByteArray# -> Int# -> Char#
   {Read 31-bit character; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsAddr "indexWord8ArrayAsAddr#" GenPrimOp
   ByteArray# -> Int# -> Addr#
   {Read address; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsFloat "indexWord8ArrayAsFloat#" GenPrimOp
   ByteArray# -> Int# -> Float#
   {Read float; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsDouble "indexWord8ArrayAsDouble#" GenPrimOp
   ByteArray# -> Int# -> Double#
   {Read double; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsStablePtr "indexWord8ArrayAsStablePtr#" GenPrimOp
   ByteArray# -> Int# -> StablePtr# a
   {Read stable pointer; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt16 "indexWord8ArrayAsInt16#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read 16-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt32 "indexWord8ArrayAsInt32#" GenPrimOp
   ByteArray# -> Int# -> INT32
   {Read 32-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt64 "indexWord8ArrayAsInt64#" GenPrimOp
   ByteArray# -> Int# -> INT64
   {Read 64-bit int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsInt "indexWord8ArrayAsInt#" GenPrimOp
   ByteArray# -> Int# -> Int#
   {Read int; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord16 "indexWord8ArrayAsWord16#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read 16-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord32 "indexWord8ArrayAsWord32#" GenPrimOp
   ByteArray# -> Int# -> WORD32
   {Read 32-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord64 "indexWord8ArrayAsWord64#" GenPrimOp
   ByteArray# -> Int# -> WORD64
   {Read 64-bit word; offset in bytes.}
   with can_fail = True

primop IndexByteArrayOp_Word8AsWord "indexWord8ArrayAsWord#" GenPrimOp
   ByteArray# -> Int# -> Word#
   {Read word; offset in bytes.}
   with can_fail = True

primop  ReadByteArrayOp_Char "readCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_WideChar "readWideCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   {Read 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int "readIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   {Read integer; offset in machine words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word "readWordArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   {Read word; offset in machine words.}
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Addr "readAddrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Float "readFloatArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Double "readDoubleArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_StablePtr "readStablePtrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int8 "readInt8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int16 "readInt16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int32 "readInt32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Int64 "readInt64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8 "readWord8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word16 "readWord16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word32 "readWord32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word64 "readWord64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsChar "readWord8ArrayAsChar#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWideChar "readWord8ArrayAsWideChar#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsAddr "readWord8ArrayAsAddr#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsFloat "readWord8ArrayAsFloat#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsDouble "readWord8ArrayAsDouble#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsStablePtr "readWord8ArrayAsStablePtr#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt16 "readWord8ArrayAsInt16#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt32 "readWord8ArrayAsInt32#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt64 "readWord8ArrayAsInt64#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsInt "readWord8ArrayAsInt#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord16 "readWord8ArrayAsWord16#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord32 "readWord8ArrayAsWord32#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord64 "readWord8ArrayAsWord64#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail = True

primop  ReadByteArrayOp_Word8AsWord "readWord8ArrayAsWord#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Char "writeCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   {Write 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_WideChar "writeWideCharArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   {Write 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int "writeIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word "writeWordArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Addr "writeAddrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Float "writeFloatArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Double "writeDoubleArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_StablePtr "writeStablePtrArray#" GenPrimOp
   MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int8 "writeInt8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int16 "writeInt16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int32 "writeInt32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Int64 "writeInt64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
   with can_fail = True
        has_side_effects = True

primop  WriteByteArrayOp_Word8 "writeWord8Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word16 "writeWord16Array#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word32 "writeWord32Array#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word64 "writeWord64Array#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsChar "writeWord8ArrayAsChar#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWideChar "writeWord8ArrayAsWideChar#" GenPrimOp
   MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsAddr "writeWord8ArrayAsAddr#" GenPrimOp
   MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsFloat "writeWord8ArrayAsFloat#" GenPrimOp
   MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsDouble "writeWord8ArrayAsDouble#" GenPrimOp
   MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsStablePtr "writeWord8ArrayAsStablePtr#" GenPrimOp
   MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt16 "writeWord8ArrayAsInt16#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt32 "writeWord8ArrayAsInt32#" GenPrimOp
   MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt64 "writeWord8ArrayAsInt64#" GenPrimOp
   MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsInt "writeWord8ArrayAsInt#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord16 "writeWord8ArrayAsWord16#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord32 "writeWord8ArrayAsWord32#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord64 "writeWord8ArrayAsWord64#" GenPrimOp
   MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  WriteByteArrayOp_Word8AsWord "writeWord8ArrayAsWord#" GenPrimOp
   MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail = True

primop  CompareByteArraysOp "compareByteArrays#" GenPrimOp
   ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
   {{\tt compareByteArrays# src1 src1_ofs src2 src2_ofs n} compares
    {\tt n} bytes starting at offset {\tt src1_ofs} in the first
    {\tt ByteArray#} {\tt src1} to the range of {\tt n} bytes
    (i.e. same length) starting at offset {\tt src2_ofs} of the second
    {\tt ByteArray#} {\tt src2}.  Both arrays must fully contain the
    specified ranges, but this is not checked.  Returns an {\tt Int#}
    less than, equal to, or greater than zero if the range is found,
    respectively, to be byte-wise lexicographically less than, to
    match, or be greater than the second range.}
   with
   can_fail = True

primop  CopyByteArrayOp "copyByteArray#" GenPrimOp
  ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {{\tt copyByteArray# src src_ofs dst dst_ofs n} copies the range
   starting at offset {\tt src_ofs} of length {\tt n} from the
   {\tt ByteArray#} {\tt src} to the {\tt MutableByteArray#} {\tt dst}
   starting at offset {\tt dst_ofs}.  Both arrays must fully contain
   the specified ranges, but this is not checked.  The two arrays must
   not be the same array in different states, but this is not checked
   either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayOp "copyMutableByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableByteArray\# to the specified region in the second MutableByteArray\#.
   Both arrays must fully contain the specified ranges, but this is not checked. The regions are
   allowed to overlap, although this is only possible when the same array is provided
   as both the source and the destination.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

primop  CopyByteArrayToAddrOp "copyByteArrayToAddr#" GenPrimOp
  ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the ByteArray\# to the memory range starting at the Addr\#.
   The ByteArray\# and the memory region at Addr\# must fully contain the
   specified ranges, but this is not checked. The Addr\# must not point into the
   ByteArray\# (e.g. if the ByteArray\# were pinned), but this is not checked
   either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayToAddrOp "copyMutableByteArrayToAddr#" GenPrimOp
  MutableByteArray# s -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the MutableByteArray\# to the memory range starting at the
   Addr\#. The MutableByteArray\# and the memory region at Addr\# must fully
   contain the specified ranges, but this is not checked. The Addr\# must not
   point into the MutableByteArray\# (e.g. if the MutableByteArray\# were
   pinned), but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyAddrToByteArrayOp "copyAddrToByteArray#" GenPrimOp
  Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a memory range starting at the Addr\# to the specified range in the
   MutableByteArray\#. The memory region at Addr\# and the ByteArray\# must fully
   contain the specified ranges, but this is not checked. The Addr\# must not
   point into the MutableByteArray\# (e.g. if the MutableByteArray\# were pinned),
   but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  SetByteArrayOp "setByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
  {{\tt setByteArray# ba off len c} sets the byte range {\tt [off, off+len]} of
   the {\tt MutableByteArray#} to the byte {\tt c}.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

-- Atomic operations

primop  AtomicReadByteArrayOp_Int "atomicReadIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array and an offset in machine words, read an element. The
    index is assumed to be in bounds. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop  AtomicWriteByteArrayOp_Int "atomicWriteIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   {Given an array and an offset in machine words, write an element. The
    index is assumed to be in bounds. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop CasByteArrayOp_Int "casIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, an offset in machine words, the expected old value, and
    the new value, perform an atomic compare and swap i.e. write the new
    value if the current value matches the provided old value. Returns
    the value of the element before the operation. Implies a full memory
    barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAddByteArrayOp_Int "fetchAddIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to add,
    atomically add the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchSubByteArrayOp_Int "fetchSubIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to subtract,
    atomically subtract the value to the element. Returns the value of
    the element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAndByteArrayOp_Int "fetchAndIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to AND,
    atomically AND the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchNandByteArrayOp_Int "fetchNandIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to NAND,
    atomically NAND the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchOrByteArrayOp_Int "fetchOrIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to OR,
    atomically OR the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchXorByteArrayOp_Int "fetchXorIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to XOR,
    atomically XOR the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True
-}