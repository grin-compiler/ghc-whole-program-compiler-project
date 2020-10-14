{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Word where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Word#"
        {Operations on native-sized unsigned words (32+ bits).}
------------------------------------------------------------------------

primtype Word#

primop   WordAddOp   "plusWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   WordAddCOp   "addWordC#"   GenPrimOp   Word# -> Word# -> (# Word#, Int# #)
         {Add unsigned integers reporting overflow.
          The first element of the pair is the result.  The second element is
          the carry flag, which is nonzero on overflow. See also {\tt plusWord2#}.}
   with code_size = 2
        commutable = True

primop   WordSubCOp   "subWordC#"   GenPrimOp   Word# -> Word# -> (# Word#, Int# #)
         {Subtract unsigned integers reporting overflow.
          The first element of the pair is the result.  The second element is
          the carry flag, which is nonzero on overflow.}
   with code_size = 2

primop   WordAdd2Op   "plusWord2#"   GenPrimOp   Word# -> Word# -> (# Word#, Word# #)
         {Add unsigned integers, with the high part (carry) in the first
          component of the returned pair and the low part in the second
          component of the pair. See also {\tt addWordC#}.}
   with code_size = 2
        commutable = True

primop   WordSubOp   "minusWord#"   Dyadic   Word# -> Word# -> Word#

primop   WordMulOp   "timesWord#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

-- Returns (# high, low #)
primop   WordMul2Op  "timesWord2#"   GenPrimOp
   Word# -> Word# -> (# Word#, Word# #)
   with commutable = True

primop   WordQuotOp   "quotWord#"   Dyadic   Word# -> Word# -> Word#
   with can_fail = True

primop   WordRemOp   "remWord#"   Dyadic   Word# -> Word# -> Word#
   with can_fail = True

primop   WordQuotRemOp "quotRemWord#" GenPrimOp
   Word# -> Word# -> (# Word#, Word# #)
   with can_fail = True

primop   WordQuotRem2Op "quotRemWord2#" GenPrimOp
   Word# -> Word# -> Word# -> (# Word#, Word# #)
         { Takes high word of dividend, then low word of dividend, then divisor.
           Requires that high word < divisor.}
   with can_fail = True

primop   AndOp   "and#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   OrOp   "or#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   XorOp   "xor#"   Dyadic   Word# -> Word# -> Word#
   with commutable = True

primop   NotOp   "not#"   Monadic   Word# -> Word#

primop   SllOp   "uncheckedShiftL#"   GenPrimOp   Word# -> Int# -> Word#
         {Shift left logical.   Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   SrlOp   "uncheckedShiftRL#"   GenPrimOp   Word# -> Int# -> Word#
         {Shift right logical.   Result undefined if shift  amount is not
          in the range 0 to word size - 1 inclusive.}

primop   Word2IntOp   "word2Int#"   GenPrimOp   Word# -> Int#
   with code_size = 0

primop   WordGtOp   "gtWord#"   Compare   Word# -> Word# -> Int#
primop   WordGeOp   "geWord#"   Compare   Word# -> Word# -> Int#
primop   WordEqOp   "eqWord#"   Compare   Word# -> Word# -> Int#
primop   WordNeOp   "neWord#"   Compare   Word# -> Word# -> Int#
primop   WordLtOp   "ltWord#"   Compare   Word# -> Word# -> Int#
primop   WordLeOp   "leWord#"   Compare   Word# -> Word# -> Int#

primop   PopCnt8Op   "popCnt8#"   Monadic   Word# -> Word#
    {Count the number of set bits in the lower 8 bits of a word.}
primop   PopCnt16Op   "popCnt16#"   Monadic   Word# -> Word#
    {Count the number of set bits in the lower 16 bits of a word.}
primop   PopCnt32Op   "popCnt32#"   Monadic   Word# -> Word#
    {Count the number of set bits in the lower 32 bits of a word.}
primop   PopCnt64Op   "popCnt64#"   GenPrimOp   WORD64 -> Word#
    {Count the number of set bits in a 64-bit word.}
primop   PopCntOp   "popCnt#"   Monadic   Word# -> Word#
    {Count the number of set bits in a word.}

primop   Pdep8Op   "pdep8#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to lower 8 bits of a word at locations specified by a mask.}
primop   Pdep16Op   "pdep16#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to lower 16 bits of a word at locations specified by a mask.}
primop   Pdep32Op   "pdep32#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to lower 32 bits of a word at locations specified by a mask.}
primop   Pdep64Op   "pdep64#"   GenPrimOp   WORD64 -> WORD64 -> WORD64
    {Deposit bits to a word at locations specified by a mask.}
primop   PdepOp   "pdep#"   Dyadic   Word# -> Word# -> Word#
    {Deposit bits to a word at locations specified by a mask.}

primop   Pext8Op   "pext8#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from lower 8 bits of a word at locations specified by a mask.}
primop   Pext16Op   "pext16#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from lower 16 bits of a word at locations specified by a mask.}
primop   Pext32Op   "pext32#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from lower 32 bits of a word at locations specified by a mask.}
primop   Pext64Op   "pext64#"   GenPrimOp   WORD64 -> WORD64 -> WORD64
    {Extract bits from a word at locations specified by a mask.}
primop   PextOp   "pext#"   Dyadic   Word# -> Word# -> Word#
    {Extract bits from a word at locations specified by a mask.}

primop   Clz8Op   "clz8#" Monadic   Word# -> Word#
    {Count leading zeros in the lower 8 bits of a word.}
primop   Clz16Op   "clz16#" Monadic   Word# -> Word#
    {Count leading zeros in the lower 16 bits of a word.}
primop   Clz32Op   "clz32#" Monadic   Word# -> Word#
    {Count leading zeros in the lower 32 bits of a word.}
primop   Clz64Op   "clz64#" GenPrimOp WORD64 -> Word#
    {Count leading zeros in a 64-bit word.}
primop   ClzOp     "clz#"   Monadic   Word# -> Word#
    {Count leading zeros in a word.}

primop   Ctz8Op   "ctz8#"  Monadic   Word# -> Word#
    {Count trailing zeros in the lower 8 bits of a word.}
primop   Ctz16Op   "ctz16#" Monadic   Word# -> Word#
    {Count trailing zeros in the lower 16 bits of a word.}
primop   Ctz32Op   "ctz32#" Monadic   Word# -> Word#
    {Count trailing zeros in the lower 32 bits of a word.}
primop   Ctz64Op   "ctz64#" GenPrimOp WORD64 -> Word#
    {Count trailing zeros in a 64-bit word.}
primop   CtzOp     "ctz#"   Monadic   Word# -> Word#
    {Count trailing zeros in a word.}

primop   BSwap16Op   "byteSwap16#"   Monadic   Word# -> Word#
    {Swap bytes in the lower 16 bits of a word. The higher bytes are undefined. }
primop   BSwap32Op   "byteSwap32#"   Monadic   Word# -> Word#
    {Swap bytes in the lower 32 bits of a word. The higher bytes are undefined. }
primop   BSwap64Op   "byteSwap64#"   Monadic   WORD64 -> WORD64
    {Swap bytes in a 64 bits of a word.}
primop   BSwapOp     "byteSwap#"     Monadic   Word# -> Word#
    {Swap bytes in a word.}

primop   BRev8Op    "bitReverse8#"   Monadic   Word# -> Word#
    {Reverse the order of the bits in a 8-bit word.}
primop   BRev16Op   "bitReverse16#"   Monadic   Word# -> Word#
    {Reverse the order of the bits in a 16-bit word.}
primop   BRev32Op   "bitReverse32#"   Monadic   Word# -> Word#
    {Reverse the order of the bits in a 32-bit word.}
primop   BRev64Op   "bitReverse64#"   Monadic   WORD64 -> WORD64
    {Reverse the order of the bits in a 64-bit word.}
primop   BRevOp     "bitReverse#"     Monadic   Word# -> Word#
    {Reverse the order of the bits in a word.}
-}
{-
-- Word
evalPrimOp WordAddOp  [WordV a, WordV b] = WordV $ a + b
evalPrimOp WordAddCOp [WordV a, WordV b] = TupleV
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimWord) then 1 else 0
                                        ]
evalPrimOp WordSubCOp [WordV a, WordV b] = TupleV
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimWord) then 1 else 0
                                        ]
evalPrimOp WordAdd2Op [WordV a, WordV b] = TupleV [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
  res = fromIntegral a + fromIntegral b :: Integer
  hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
  lo  = res - hi

evalPrimOp WordSubOp  [WordV a, WordV b] = WordV $ a - b
evalPrimOp WordMulOp  [WordV a, WordV b] = WordV $ a * b
evalPrimOp WordMul2Op [WordV a, WordV b] = TupleV [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
  res = fromIntegral a * fromIntegral b :: Integer
  hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
  lo  = res - hi

evalPrimOp WordQuotOp     [WordV a, WordV b] = WordV $ a `quot` b  -- NOTE: uint / uint in C
evalPrimOp WordRemOp      [WordV a, WordV b] = WordV $ a `rem` b   -- NOTE: uint % uint in C
evalPrimOp WordQuotRemOp  [WordV a, WordV b] = TupleV [WordV $ a `quot` b, WordV $ a `rem` b]
evalPrimOp WordQuotRem2Op [WordV hi, WordV lo, WordV b'] = TupleV [WordV . fromIntegral $ a `quot` b, WordV . fromIntegral $ a `rem` b] where
  a = fromIntegral hi * fromIntegral (1 + maxBound :: PrimWord) + fromIntegral lo :: Integer
  b = fromIntegral b' :: Integer

evalPrimOp AndOp  [WordV a, WordV b] = WordV $ a .&. b
evalPrimOp OrOp   [WordV a, WordV b] = WordV $ a .|. b
evalPrimOp XorOp  [WordV a, WordV b] = WordV $ a `xor` b
evalPrimOp NotOp  [WordV a] = WordV $ complement a
evalPrimOp SllOp  [WordV a, IntV b] = WordV $ unsafeShiftL a (fromIntegral b)
evalPrimOp SrlOp  [WordV a, IntV b] = WordV $ unsafeShiftR a (fromIntegral b) -- Shift right logical
evalPrimOp Word2IntOp [WordV a] = IntV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp WordGtOp   [WordV a, WordV b] = IntV $ if a > b  then 1 else 0
evalPrimOp WordGeOp   [WordV a, WordV b] = IntV $ if a >= b then 1 else 0
evalPrimOp WordEqOp   [WordV a, WordV b] = IntV $ if a == b then 1 else 0
evalPrimOp WordNeOp   [WordV a, WordV b] = IntV $ if a /= b then 1 else 0
evalPrimOp WordLtOp   [WordV a, WordV b] = IntV $ if a < b  then 1 else 0
evalPrimOp WordLeOp   [WordV a, WordV b] = IntV $ if a <= b then 1 else 0

evalPrimOp PopCnt8Op  [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word8)
evalPrimOp PopCnt16Op [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word16)
evalPrimOp PopCnt32Op [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word32)
evalPrimOp PopCnt64Op [WordV a] = WordV . fromIntegral $ popCount (fromIntegral a :: Word64)
evalPrimOp PopCntOp   [WordV a] = WordV . fromIntegral $ popCount a
-}
{-
  HINT:
    https://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets#Parallel_bit_deposit_and_extract
    https://www.felixcloutier.com/x86/pdep
-}

{-
  Pdep8Op
  Pdep16Op
  Pdep32Op
  Pdep64Op
  PdepOp
  Pext8Op
  Pext16Op
  Pext32Op
  Pext64Op
  PextOp
-}
{-
evalPrimOp Clz8Op   [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word8)
evalPrimOp Clz16Op  [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word16)
evalPrimOp Clz32Op  [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word32)
evalPrimOp Clz64Op  [WordV a] = WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word64)
evalPrimOp ClzOp    [WordV a] = WordV . fromIntegral $ countLeadingZeros a
evalPrimOp Ctz8Op   [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word8)
evalPrimOp Ctz16Op  [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word16)
evalPrimOp Ctz32Op  [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word32)
evalPrimOp Ctz64Op  [WordV a] = WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word64)
evalPrimOp CtzOp    [WordV a] = WordV . fromIntegral $ countTrailingZeros a

evalPrimOp BSwap16Op  [WordV a] = WordV . fromIntegral $ byteSwap16 (fromIntegral a :: Word16)
evalPrimOp BSwap32Op  [WordV a] = WordV . fromIntegral $ byteSwap32 (fromIntegral a :: Word32)
evalPrimOp BSwap64Op  [WordV a] = WordV . fromIntegral $ byteSwap64 (fromIntegral a :: Word64)
evalPrimOp BSwapOp    [WordV a] = WordV $ byteSwap64 a
-}