{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, TypeApplications, Strict #-}
{-# LANGUAGE ScopedTypeVariables, MagicHash #-}
module Stg.Interpreter.PrimOp.Word where

import Data.Word
import Data.Bits
import GHC.Exts
import GHC.Word

import Stg.Syntax
import Stg.Interpreter.Base

type PrimWord = Word64

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word64V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of
  -- plusWord# :: Word# -> Word# -> Word#
  ( "plusWord#",     [WordV a, WordV b]) -> pure [WordV $ a + b]

  -- addWordC# :: Word# -> Word# -> (# Word#, Int# #)
  ( "addWordC#",     [WordV a, WordV b]) -> pure
                                        [ WordV $ a + b
                                        , IntV . carry $ fromIntegral a + fromIntegral b
                                        ] where
                                            carry :: Integer -> Int
                                            carry x = if x < fromIntegral (minBound :: PrimWord) || x > fromIntegral (maxBound :: PrimWord) then 1 else 0

  -- subWordC# :: Word# -> Word# -> (# Word#, Int# #)
  ( "subWordC#",     [WordV a, WordV b]) -> pure
                                        [ WordV $ a - b
                                        , IntV . carry $ fromIntegral a - fromIntegral b
                                        ] where
                                            carry :: Integer -> Int
                                            carry x = if x < fromIntegral (minBound :: PrimWord) || x > fromIntegral (maxBound :: PrimWord) then 1 else 0

  -- plusWord2# :: Word# -> Word# -> (# Word#, Word# #)
  ( "plusWord2#",    [WordV a, WordV b]) -> pure [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
    res = fromIntegral a + fromIntegral b :: Integer
    (hi, lo)  = res `quotRem` (1 + fromIntegral (maxBound :: PrimWord))

  -- minusWord# :: Word# -> Word# -> Word#
  ( "minusWord#",    [WordV a, WordV b]) -> pure [WordV $ a - b]

  -- timesWord# :: Word# -> Word# -> Word#
  ( "timesWord#",    [WordV a, WordV b]) -> pure [WordV $ a * b]

  -- timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
  ( "timesWord2#",   [WordV a, WordV b]) -> pure [WordV (fromIntegral hi), WordV (fromIntegral lo)] where
    res = fromIntegral a * fromIntegral b :: Integer
    (hi, lo)  = res `quotRem` (1 + fromIntegral (maxBound :: PrimWord))

  -- quotWord# :: Word# -> Word# -> Word#
  ( "quotWord#",     [WordV a, WordV b]) -> pure [WordV $ a `quot` b]  -- NOTE: uint / uint in C

  -- remWord# :: Word# -> Word# -> Word#
  ( "remWord#",      [WordV a, WordV b]) -> pure [WordV $ a `rem` b]   -- NOTE: uint % uint in C

  -- quotRemWord# :: Word# -> Word# -> (# Word#, Word# #)
  ( "quotRemWord#",  [WordV a, WordV b]) -> pure [WordV $ a `quot` b, WordV $ a `rem` b]

  -- quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
  ( "quotRemWord2#", [WordV hi, WordV lo, WordV b']) -> pure [WordV . fromIntegral $ a `quot` b, WordV . fromIntegral $ a `rem` b] where
    a = fromIntegral hi * (1 + fromIntegral (maxBound :: PrimWord)) + fromIntegral lo :: Integer
    b = fromIntegral b' :: Integer

  -- and# :: Word# -> Word# -> Word#
  ( "and#",  [WordV a, WordV b])     -> pure [WordV $ a .&. b]

  -- or# :: Word# -> Word# -> Word#
  ( "or#",   [WordV a, WordV b])     -> pure [WordV $ a .|. b]

  -- xor# :: Word# -> Word# -> Word#
  ( "xor#",  [WordV a, WordV b])     -> pure [WordV $ a `xor` b]

  -- not# :: Word# -> Word#
  ( "not#",  [WordV a])              -> pure [WordV $ complement a]

  -- uncheckedShiftL# :: Word# -> Int# -> Word#
  ( "uncheckedShiftL#",  [WordV a, IntV b])  -> pure [WordV $ unsafeShiftL a b]

  -- uncheckedShiftRL# :: Word# -> Int# -> Word#
  ( "uncheckedShiftRL#", [WordV a, IntV b])  -> pure [WordV $ unsafeShiftR a b] -- Shift right logical

  -- word2Int# :: Word# -> Int#
  ( "word2Int#", [WordV a])           -> pure [IntV $ fromIntegral a] -- HINT: noop ; same bit level representation

  -- gtWord# :: Word# -> Word# -> Int#
  ( "gtWord#",   [WordV a, WordV b])  -> pure [IntV $ if a > b  then 1 else 0]

  -- geWord# :: Word# -> Word# -> Int#
  ( "geWord#",   [WordV a, WordV b])  -> pure [IntV $ if a >= b then 1 else 0]

  -- eqWord# :: Word# -> Word# -> Int#
  ( "eqWord#",   [WordV a, WordV b])  -> pure [IntV $ if a == b then 1 else 0]

  -- neWord# :: Word# -> Word# -> Int#
  ( "neWord#",   [WordV a, WordV b])  -> pure [IntV $ if a /= b then 1 else 0]

  -- ltWord# :: Word# -> Word# -> Int#
  ( "ltWord#",   [WordV a, WordV b])  -> pure [IntV $ if a < b  then 1 else 0]

  -- leWord# :: Word# -> Word# -> Int#
  ( "leWord#",   [WordV a, WordV b])  -> pure [IntV $ if a <= b then 1 else 0]

  -- popCnt8# :: Word# -> Word#
  ( "popCnt8#",  [WordV a])           -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word8)]

  -- popCnt16# :: Word# -> Word#
  ( "popCnt16#", [WordV a])           -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word16)]

  -- popCnt32# :: Word# -> Word#
  ( "popCnt32#", [WordV a])           -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word32)]

  -- popCnt64# :: Word64# -> Word#
  ( "popCnt64#", [Word64V a])         -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word64)]

  -- popCnt# :: Word# -> Word#
  ( "popCnt#",   [WordV a])           -> pure [WordV . fromIntegral $ popCount a]

  -- pdep8# :: Word# -> Word# -> Word#
  ( "pdep8#", [WordV a, WordV b])     -> pure [WordV $ fromIntegral $ pdep8 (fromIntegral a) (fromIntegral b)]

  -- pdep16# :: Word# -> Word# -> Word#
  ( "pdep16#", [WordV a, WordV b])    -> pure [WordV $ fromIntegral $ pdep16 (fromIntegral a) (fromIntegral b)]

  -- pdep32# :: Word# -> Word# -> Word#
  ( "pdep32#", [WordV a, WordV b])    -> pure [WordV $ fromIntegral $ pdep32 (fromIntegral a) (fromIntegral b)]

  -- pdep64# :: Word64# -> Word64# -> Word64#
  ( "pdep64#", [Word64V a, Word64V b])  -> pure [Word64V $ fromIntegral $ pdep64 (fromIntegral a) (fromIntegral b)]

  -- pdep# :: Word# -> Word# -> Word#
  ( "pdep#", [WordV a, WordV b])      -> pure [WordV $ fromIntegral $ pdep (fromIntegral a) (fromIntegral b)]

  -- pext8# :: Word# -> Word# -> Word#
  ( "pext8#", [WordV a, WordV b])     -> pure [WordV $ fromIntegral $ pext8 (fromIntegral a) (fromIntegral b)]

  -- pext16# :: Word# -> Word# -> Word#
  ( "pext16#", [WordV a, WordV b])    -> pure [WordV $ fromIntegral $ pext16 (fromIntegral a) (fromIntegral b)]

  -- pext32# :: Word# -> Word# -> Word#
  ( "pext32#", [WordV a, WordV b])    -> pure [WordV $ fromIntegral $ pext32 (fromIntegral a) (fromIntegral b)]

  -- pext64# :: Word64# -> Word64# -> Word64#
  ( "pext64#", [Word64V a, Word64V b])    -> pure [Word64V $ fromIntegral $ pext64 (fromIntegral a) (fromIntegral b)]

  -- pext# :: Word# -> Word# -> Word#
  ( "pext#", [WordV a, WordV b])      -> pure [WordV $ fromIntegral $ pext (fromIntegral a) (fromIntegral b)]

  -- clz8# :: Word# -> Word#
  ( "clz8#",   [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word8)]

  -- clz16# :: Word# -> Word#
  ( "clz16#",  [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word16)]

  -- clz32# :: Word# -> Word#
  ( "clz32#",  [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word32)]

  -- clz64# :: Word64# -> Word#
  ( "clz64#",  [Word64V a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word64)]

  -- clz# :: Word# -> Word#
  ( "clz#",    [WordV a]) -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word)]

  -- ctz8#  :: Word# -> Word#
  ( "ctz8#",   [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word8)]

  -- ctz16# :: Word# -> Word#
  ( "ctz16#",  [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word16)]

  -- ctz32# :: Word# -> Word#
  ( "ctz32#",  [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word32)]

  -- ctz64# :: Word64# -> Word#
  ( "ctz64#",  [Word64V a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word64)]

  -- ctz# :: Word# -> Word#
  ( "ctz#",    [WordV a]) -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word)]

  -- byteSwap16# :: Word# -> Word#
  ( "byteSwap16#", [WordV a])   -> pure [WordV . fromIntegral $ byteSwap16 (fromIntegral a :: Word16)]

  -- byteSwap32# :: Word# -> Word#
  ( "byteSwap32#", [WordV a])   -> pure [WordV . fromIntegral $ byteSwap32 (fromIntegral a :: Word32)]

  -- byteSwap64# :: Word64# -> Word64#
  ( "byteSwap64#", [Word64V a])   -> pure [Word64V . fromIntegral $ byteSwap64 (fromIntegral a :: Word64)]

  -- byteSwap# :: Word# -> Word#
  ( "byteSwap#",   [WordV a])   -> pure [WordV . fromIntegral . byteSwap64 $ fromIntegral a]

  -- bitReverse8# :: Word# -> Word#
  ( "bitReverse8#", [WordV a])  -> pure [WordV $ bitReverse @Word8 $ fromIntegral a]

  -- bitReverse16# :: Word# -> Word#
  ( "bitReverse16#", [WordV a]) -> pure [WordV $ bitReverse @Word16 $ fromIntegral a]

  -- bitReverse32# :: Word# -> Word#
  ( "bitReverse32#", [WordV a]) -> pure [WordV $ bitReverse @Word32 $ fromIntegral a]

  -- bitReverse64# :: Word64# -> Word64#
  ( "bitReverse64#", [Word64V a]) -> pure [Word64V $ bitReverse @Word64 $ fromIntegral a]

  -- bitReverse# :: Word# -> Word#
  ( "bitReverse#", [WordV a])   -> pure [WordV $ bitReverse @Word64 $ fromIntegral a]

  _ -> fallback op args t tc

-- TODO: Replace this with GHC.Word bitReverse when base 4.14 is used.
bitReverse :: forall a . (FiniteBits a, Integral a) => a -> Word
bitReverse x =
  let n = finiteBitSize x - 1
      bits = [ (n - i, b)
             | i <- [ 0 .. (finiteBitSize x - 1) ]
             , let b = testBitDefault x i
             ]
  in fromIntegral (foldl (\y (i,b) -> if b then setBit y i else y) (zeroBits @a) bits)

-- TODO: Maybe this could be implemented showing the desired semantics, instead of
-- piggy-backing of GHC's implementation.

pdep8 :: Word -> Word -> Word
pdep8 (W# a) (W# b) = W# (pdep8# a b)

pdep16 :: Word -> Word -> Word
pdep16 (W# a) (W# b) = W# (pdep16# a b)

pdep32 :: Word -> Word -> Word
pdep32 (W# a) (W# b) = W# (pdep32# a b)

pdep64 :: Word64 -> Word64 -> Word64
pdep64 (W64# a) (W64# b) = W64# (pdep64# a b)

pdep :: Word -> Word -> Word
pdep (W# a) (W# b) = W# (pdep# a b)

pext8 :: Word -> Word -> Word
pext8 (W# a) (W# b) = W# (pext8# a b)

pext16 :: Word -> Word -> Word
pext16 (W# a) (W# b) = W# (pext16# a b)

pext32 :: Word -> Word -> Word
pext32 (W# a) (W# b) = W# (pext32# a b)

pext64 :: Word64 -> Word64 -> Word64
pext64 (W64# a) (W64# b) = W64# (pext64# a b)

pext :: Word -> Word -> Word
pext (W# a) (W# b) = W# (pext# a b)
