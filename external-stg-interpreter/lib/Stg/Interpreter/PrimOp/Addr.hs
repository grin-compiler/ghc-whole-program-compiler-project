{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Addr where

import qualified Data.ByteString.Char8 as BS8

import Stg.Syntax
import Stg.Interpreter.Base

pattern CharV c = Literal (LitChar c)
pattern IntV i  = Literal (LitNumber LitNumInt i)
pattern WordV i = Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- plusAddr# :: Addr# -> Int# -> Addr#
  -- TODO: design and implement this properly!!
  ("plusAddr#", [StringPtr base str, Literal (LitNumber LitNumInt offset)]) -> do
    pure [StringPtr (base + fromIntegral offset) str]

  ("plusAddr#", [a, IntV offset]) -> do
    pure [a]

  -- minusAddr# :: Addr# -> Addr# -> Int#
  -- remAddr# :: Addr# -> Int# -> Int#
  -- addr2Int# :: Addr# -> Int#
  -- int2Addr# :: Int# -> Addr#
  -- gtAddr# :: Addr# -> Addr# -> Int#
  -- geAddr# :: Addr# -> Addr# -> Int#

  -- eqAddr# :: Addr# -> Addr# -> Int#
  ("eqAddr#", [a, b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  -- neAddr# :: Addr# -> Addr# -> Int#
  -- ltAddr# :: Addr# -> Addr# -> Int#
  -- leAddr# :: Addr# -> Addr# -> Int#

  -- indexCharOffAddr# :: Addr# -> Int# -> Char#
  ("indexCharOffAddr#", [StringPtr base str, IntV offset]) -> do
    pure [CharV $ BS8.index str (base + fromIntegral offset)]

  -- indexWideCharOffAddr# :: Addr# -> Int# -> Char#
  -- indexIntOffAddr# :: Addr# -> Int# -> Int#
  -- indexWordOffAddr# :: Addr# -> Int# -> Word#
  -- indexAddrOffAddr# :: Addr# -> Int# -> Addr#
  -- indexFloatOffAddr# :: Addr# -> Int# -> Float#
  -- indexDoubleOffAddr# :: Addr# -> Int# -> Double#
  -- indexStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a
  -- indexInt8OffAddr# :: Addr# -> Int# -> Int#
  -- indexInt16OffAddr# :: Addr# -> Int# -> Int#
  -- indexInt32OffAddr# :: Addr# -> Int# -> INT32
  -- indexInt64OffAddr# :: Addr# -> Int# -> INT64
  -- indexWord8OffAddr# :: Addr# -> Int# -> Word#
  -- indexWord16OffAddr# :: Addr# -> Int# -> Word#
  -- indexWord32OffAddr# :: Addr# -> Int# -> WORD32
  -- indexWord64OffAddr# :: Addr# -> Int# -> WORD64
  -- readCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Char# #)

  -- readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Char# #)
  {-
  ("readWideCharOffAddr#", [addr, IntV offset, _s]) -> do
    pure [CharV 'a']
  -}
  -- readIntOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
  -- readWordOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word# #)

  -- readAddrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Addr# #)
  -- TODO: design and implement this properly!!
  ("readAddrOffAddr#", [addr, IntV _, _s]) -> do
    pure [Literal LitNullAddr]

  -- readFloatOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Float# #)
  -- readDoubleOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Double# #)
  -- readStablePtrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, StablePtr# a #)

  -- readInt8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
  -- TODO: design and implement this properly!!
  ("readInt8OffAddr#", [addr, IntV offset, _s]) -> do
    pure [IntV 0]

  -- readInt16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
  -- readInt32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, INT32 #)
  -- readInt64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, INT64 #)

  -- readWord8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word# #)
  ("readWord8OffAddr#", [addr, IntV offset, _s]) -> do
    pure [WordV 2]

  -- readWord16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word# #)
  -- readWord32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, WORD32 #)

  -- readWord64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, WORD64 #)
  ("readWord64OffAddr#", [addr, IntV offset, _s]) -> do
    pure [WordV 1]

  -- writeCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s

  -- writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
  ("writeWideCharOffAddr#", [a, b, c, _s]) -> do
  -- "writeWideCharOffAddr#" args: [Literal LitNullAddr,Literal (LitNumber LitNumInt 0),Literal (LitChar 'a'),Void]
    pure [] -- TODO

  -- writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
  -- writeWordOffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s

  -- writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
  ("writeAddrOffAddr#", [a, b, c, _s]) -> do
    pure [] -- TODO

  -- writeFloatOffAddr# :: Addr# -> Int# -> Float# -> State# s -> State# s
  -- writeDoubleOffAddr# :: Addr# -> Int# -> Double# -> State# s -> State# s
  -- writeStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a -> State# s -> State# s

  -- writeInt8OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
  ("writeInt8OffAddr#", [a, b, c, _s]) -> do
  -- "writeInt8OffAddr#" args: [Literal LitNullAddr,Literal (LitNumber LitNumInt 0),Literal (LitNumber LitNumInt 85),Void]
    pure [] -- TODO

  -- writeInt16OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
  -- writeInt32OffAddr# :: Addr# -> Int# -> INT32 -> State# s -> State# s
  -- writeInt64OffAddr# :: Addr# -> Int# -> INT64 -> State# s -> State# s

  -- writeWord8OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
  ("writeWord8OffAddr#", [a, b, c, _s]) -> do
    pure [] -- TODO

  -- writeWord16OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
  -- writeWord32OffAddr# :: Addr# -> Int# -> WORD32 -> State# s -> State# s

  -- writeWord64OffAddr# :: Addr# -> Int# -> WORD64 -> State# s -> State# s
  ("writeWord64OffAddr#", [a, b, c, _s]) -> do
    pure [] -- TODO

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Addr#"
------------------------------------------------------------------------

primtype Addr#
        { An arbitrary machine address assumed to point outside
         the garbage-collected heap. }

pseudoop "nullAddr#" Addr#
        { The null address. }

primop   AddrAddOp "plusAddr#" GenPrimOp Addr# -> Int# -> Addr#
primop   AddrSubOp "minusAddr#" GenPrimOp Addr# -> Addr# -> Int#
         {Result is meaningless if two {\tt Addr\#}s are so far apart that their
         difference doesn't fit in an {\tt Int\#}.}
primop   AddrRemOp "remAddr#" GenPrimOp Addr# -> Int# -> Int#
         {Return the remainder when the {\tt Addr\#} arg, treated like an {\tt Int\#},
          is divided by the {\tt Int\#} arg.}
primop   Addr2IntOp  "addr2Int#"     GenPrimOp   Addr# -> Int#
        {Coerce directly from address to int.}
   with code_size = 0
        deprecated_msg = { This operation is strongly deprecated. }
primop   Int2AddrOp   "int2Addr#"    GenPrimOp  Int# -> Addr#
        {Coerce directly from int to address.}
   with code_size = 0
        deprecated_msg = { This operation is strongly deprecated. }

primop   AddrGtOp  "gtAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrGeOp  "geAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrEqOp  "eqAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrNeOp  "neAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrLtOp  "ltAddr#"   Compare   Addr# -> Addr# -> Int#
primop   AddrLeOp  "leAddr#"   Compare   Addr# -> Addr# -> Int#

primop IndexOffAddrOp_Char "indexCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 8-bit character; offset in bytes.}
   with can_fail = True

primop IndexOffAddrOp_WideChar "indexWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char#
   {Reads 31-bit character; offset in 4-byte words.}
   with can_fail = True

primop IndexOffAddrOp_Int "indexIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int#
   with can_fail = True

primop IndexOffAddrOp_Word "indexWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word#
   with can_fail = True

primop IndexOffAddrOp_Addr "indexAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr#
   with can_fail = True

primop IndexOffAddrOp_Float "indexFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float#
   with can_fail = True

primop IndexOffAddrOp_Double "indexDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double#
   with can_fail = True

primop IndexOffAddrOp_StablePtr "indexStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a
   with can_fail = True

primop IndexOffAddrOp_Int8 "indexInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#
   with can_fail = True

primop IndexOffAddrOp_Int16 "indexInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int#
   with can_fail = True

primop IndexOffAddrOp_Int32 "indexInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32
   with can_fail = True

primop IndexOffAddrOp_Int64 "indexInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64
   with can_fail = True

primop IndexOffAddrOp_Word8 "indexWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#
   with can_fail = True

primop IndexOffAddrOp_Word16 "indexWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word#
   with can_fail = True

primop IndexOffAddrOp_Word32 "indexWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32
   with can_fail = True

primop IndexOffAddrOp_Word64 "indexWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64
   with can_fail = True

primop ReadOffAddrOp_Char "readCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 8-bit character; offset in bytes.}
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_WideChar "readWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Char# #)
   {Reads 31-bit character; offset in 4-byte words.}
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int "readIntOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word "readWordOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Addr "readAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Addr# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Float "readFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Float# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Double "readDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Double# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_StablePtr "readStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, StablePtr# a #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int8 "readInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int16 "readInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Int# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int32 "readInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT32 #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Int64 "readInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, INT64 #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word8 "readWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word16 "readWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, Word# #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word32 "readWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD32 #)
   with has_side_effects = True
        can_fail         = True

primop ReadOffAddrOp_Word64 "readWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> State# s -> (# State# s, WORD64 #)
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Char "writeCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_WideChar "writeWideCharOffAddr#" GenPrimOp
   Addr# -> Int# -> Char# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int "writeIntOffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word "writeWordOffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Addr "writeAddrOffAddr#" GenPrimOp
   Addr# -> Int# -> Addr# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Float "writeFloatOffAddr#" GenPrimOp
   Addr# -> Int# -> Float# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Double "writeDoubleOffAddr#" GenPrimOp
   Addr# -> Int# -> Double# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_StablePtr "writeStablePtrOffAddr#" GenPrimOp
   Addr# -> Int# -> StablePtr# a -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int8 "writeInt8OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int16 "writeInt16OffAddr#" GenPrimOp
   Addr# -> Int# -> Int# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int32 "writeInt32OffAddr#" GenPrimOp
   Addr# -> Int# -> INT32 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Int64 "writeInt64OffAddr#" GenPrimOp
   Addr# -> Int# -> INT64 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word8 "writeWord8OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word16 "writeWord16OffAddr#" GenPrimOp
   Addr# -> Int# -> Word# -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word32 "writeWord32OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD32 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True

primop  WriteOffAddrOp_Word64 "writeWord64OffAddr#" GenPrimOp
   Addr# -> Int# -> WORD64 -> State# s -> State# s
   with has_side_effects = True
        can_fail         = True
-}