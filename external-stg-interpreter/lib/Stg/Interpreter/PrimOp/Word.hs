{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Word where

import Data.Word
import Data.Bits

import Stg.Syntax
import Stg.Interpreter.Base

type PrimWord = Word64

pattern IntV i    = Literal (LitNumber LitNumInt i)
pattern WordV w   = Literal (LitNumber LitNumWord w)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- plusWord# :: Word# -> Word# -> Word#
  ("plusWord#",     [WordV a, WordV b]) -> pure [WordV $ a + b]

  -- addWordC# :: Word# -> Word# -> (# Word#, Int# #)
  ("addWordC#",     [WordV a, WordV b]) -> pure
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimWord) then 1 else 0
                                        ]

  -- subWordC# :: Word# -> Word# -> (# Word#, Int# #)
  ("subWordC#",     [WordV a, WordV b]) -> pure
                                        [ WordV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimWord) then 1 else 0
                                        ]

  -- plusWord2# :: Word# -> Word# -> (# Word#, Word# #)
  ("plusWord2#",    [WordV a, WordV b]) -> pure [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
    res = fromIntegral a + fromIntegral b :: Integer
    hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
    lo  = res - hi

  -- minusWord# :: Word# -> Word# -> Word#
  ("minusWord#",    [WordV a, WordV b]) -> pure [WordV $ a - b]

  -- timesWord# :: Word# -> Word# -> Word#
  ("timesWord#",    [WordV a, WordV b]) -> pure [WordV $ a * b]

  -- timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
  ("timesWord2#",   [WordV a, WordV b]) -> pure [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
    res = fromIntegral a * fromIntegral b :: Integer
    hi  = res `quot` fromIntegral (1 + maxBound :: PrimWord)
    lo  = res - hi

  -- quotWord# :: Word# -> Word# -> Word#
  ("quotWord#",     [WordV a, WordV b]) -> pure [WordV $ a `quot` b]  -- NOTE: uint / uint in C

  -- remWord# :: Word# -> Word# -> Word#
  ("remWord#",      [WordV a, WordV b]) -> pure [WordV $ a `rem` b]   -- NOTE: uint % uint in C

  -- quotRemWord# :: Word# -> Word# -> (# Word#, Word# #)
  ("quotRemWord#",  [WordV a, WordV b]) -> pure [WordV $ a `quot` b, WordV $ a `rem` b]

  -- quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
  ("quotRemWord2#", [WordV hi, WordV lo, WordV b']) -> pure [WordV . fromIntegral $ a `quot` b, WordV . fromIntegral $ a `rem` b] where
    a = fromIntegral hi * fromIntegral (1 + maxBound :: PrimWord) + fromIntegral lo :: Integer
    b = fromIntegral b' :: Integer

  -- and# :: Word# -> Word# -> Word#
  ("and#",  [WordV a, WordV b])     -> pure [WordV $ a .&. b]

  -- or# :: Word# -> Word# -> Word#
  ("or#",   [WordV a, WordV b])     -> pure [WordV $ a .|. b]

  -- xor# :: Word# -> Word# -> Word#
  ("xor#",  [WordV a, WordV b])     -> pure [WordV $ a `xor` b]

  -- not# :: Word# -> Word#
  ("not#",  [WordV a])              -> pure [WordV $ complement a]

  -- uncheckedShiftL# :: Word# -> Int# -> Word#
  ("uncheckedShiftL#",  [WordV a, IntV b])  -> pure [WordV $ unsafeShiftL a (fromIntegral b)]

  -- uncheckedShiftRL# :: Word# -> Int# -> Word#
  ("uncheckedShiftRL#", [WordV a, IntV b])  -> pure [WordV $ unsafeShiftR a (fromIntegral b)] -- Shift right logical

  -- word2Int# :: Word# -> Int#
  ("word2Int#", [WordV a])          -> pure [IntV $ fromIntegral a] -- HINT: noop ; same bit level representation

  -- gtWord# :: Word# -> Word# -> Int#
  ("gtWord#",   [WordV a, WordV b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- geWord# :: Word# -> Word# -> Int#
  ("geWord#",   [WordV a, WordV b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- eqWord# :: Word# -> Word# -> Int#
  ("eqWord#",   [WordV a, WordV b]) -> pure [IntV $ if a == b then 1 else 0]

  -- neWord# :: Word# -> Word# -> Int#
  ("neWord#",   [WordV a, WordV b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- ltWord# :: Word# -> Word# -> Int#
  ("ltWord#",   [WordV a, WordV b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- leWord# :: Word# -> Word# -> Int#
  ("leWord#",   [WordV a, WordV b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- popCnt8# :: Word# -> Word#
  ("popCnt8#",  [WordV a])          -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word8)]

  -- popCnt16# :: Word# -> Word#
  ("popCnt16#", [WordV a])          -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word16)]

  -- popCnt32# :: Word# -> Word#
  ("popCnt32#", [WordV a])          -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word32)]

  -- popCnt64# :: WORD64 -> Word#
  ("popCnt64#", [WordV a])          -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word64)]

  -- popCnt# :: Word# -> Word#
  ("popCnt#",   [WordV a])          -> pure [WordV . fromIntegral $ popCount a]

{-
  HINT:
    https://en.wikipedia.org/wiki/Bit_Manipulation_Instruction_Sets#Parallel_bit_deposit_and_extract
    https://www.felixcloutier.com/x86/pdep
-}

  -- TODO: pdep8# :: Word# -> Word# -> Word#
  -- TODO: pdep16# :: Word# -> Word# -> Word#
  -- TODO: pdep32# :: Word# -> Word# -> Word#
  -- TODO: pdep64# :: WORD64 -> WORD64 -> WORD64
  -- TODO: pdep# :: Word# -> Word# -> Word#
  -- TODO: pext8# :: Word# -> Word# -> Word#
  -- TODO: pext16# :: Word# -> Word# -> Word#
  -- TODO: pext32# :: Word# -> Word# -> Word#
  -- TODO: pext64# :: WORD64 -> WORD64 -> WORD64
  -- TODO: pext# :: Word# -> Word# -> Word#

  -- clz8# :: Word# -> Word#
  ("clz8#",   [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word8)]

  -- clz16# :: Word# -> Word#
  ("clz16#",  [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word16)]

  -- clz32# :: Word# -> Word#
  ("clz32#",  [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word32)]

  -- clz64# :: WORD64 -> Word#
  ("clz64#",  [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word64)]

  -- clz# :: Word# -> Word#
  ("clz#",    [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word)]

  -- ctz8#  :: Word# -> Word#
  ("ctz8#",   [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word8)]

  -- ctz16# :: Word# -> Word#
  ("ctz16#",  [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word16)]

  -- ctz32# :: Word# -> Word#
  ("ctz32#",  [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word32)]

  -- ctz64# :: WORD64 -> Word#
  ("ctz64#",  [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word64)]

  -- ctz# :: Word# -> Word#
  ("ctz#",    [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word)]

  -- byteSwap16# :: Word# -> Word#
  ("byteSwap16#", [WordV a]) -> pure [WordV . fromIntegral $ byteSwap16 (fromIntegral a :: Word16)]

  -- byteSwap32# :: Word# -> Word#
  ("byteSwap32#", [WordV a]) -> pure [WordV . fromIntegral $ byteSwap32 (fromIntegral a :: Word32)]

  -- byteSwap64# :: WORD64 -> WORD64
  ("byteSwap64#", [WordV a]) -> pure [WordV . fromIntegral $ byteSwap64 (fromIntegral a :: Word64)]

  -- byteSwap# :: Word# -> Word#
  ("byteSwap#",   [WordV a]) -> pure [WordV . fromIntegral . byteSwap64 $ fromIntegral a]

  -- TODO: bitReverse8# :: Word# -> Word#
  -- TODO: bitReverse16# :: Word# -> Word#
  -- TODO: bitReverse32# :: Word# -> Word#
  -- TODO: bitReverse64# :: WORD64 -> WORD64
  -- TODO: bitReverse# :: Word# -> Word#

  _ -> fallback op args t tc
