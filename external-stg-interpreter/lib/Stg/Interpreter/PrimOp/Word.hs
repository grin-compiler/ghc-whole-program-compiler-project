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
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

      -- plusWord# :: Word# -> Word# -> Word#
  defOp "plusWord#"     $ \[WordV a, WordV b] -> pure [WordV $ a + b]

      -- addWordC# :: Word# -> Word# -> (# Word#, Int# #)
  defOp "addWordC#"     $ \[WordV a, WordV b] ->
                                    let
                                      carry :: Integer -> Int
                                      carry x = if x < fromIntegral (minBound :: PrimWord) || x > fromIntegral (maxBound :: PrimWord) then 1 else 0
                                    in pure
                                        [ WordV $ a + b
                                        , IntV . carry $ fromIntegral a + fromIntegral b
                                        ]

      -- subWordC# :: Word# -> Word# -> (# Word#, Int# #)
  defOp "subWordC#"     $ \[WordV a, WordV b] ->
                                    let
                                      carry :: Integer -> Int
                                      carry x = if x < fromIntegral (minBound :: PrimWord) || x > fromIntegral (maxBound :: PrimWord) then 1 else 0
                                    in pure
                                        [ WordV $ a - b
                                        , IntV . carry $ fromIntegral a - fromIntegral b
                                        ]

      -- plusWord2# :: Word# -> Word# -> (# Word#, Word# #)
  defOp "plusWord2#"    $ \[WordV a, WordV b] ->
    let
      res = fromIntegral a + fromIntegral b :: Integer
      (hi, lo)  = res `quotRem` (1 + fromIntegral (maxBound :: PrimWord))
    in pure [WordV (fromIntegral hi), WordV (fromIntegral lo)]

      -- minusWord# :: Word# -> Word# -> Word#
  defOp "minusWord#"    $ \[WordV a, WordV b] -> pure [WordV $ a - b]

      -- timesWord# :: Word# -> Word# -> Word#
  defOp "timesWord#"    $ \[WordV a, WordV b] -> pure [WordV $ a * b]

      -- timesWord2# :: Word# -> Word# -> (# Word#, Word# #)
  defOp "timesWord2#"   $ \[WordV a, WordV b] ->
    let
      res = fromIntegral a * fromIntegral b :: Integer
      (hi, lo)  = res `quotRem` (1 + fromIntegral (maxBound :: PrimWord))
    in pure [WordV (fromIntegral hi), WordV (fromIntegral lo)]

      -- quotWord# :: Word# -> Word# -> Word#
  defOp "quotWord#"     $ \[WordV a, WordV b] -> pure [WordV $ a `quot` b]  -- NOTE: uint / uint in C

      -- remWord# :: Word# -> Word# -> Word#
  defOp "remWord#"      $ \[WordV a, WordV b] -> pure [WordV $ a `rem` b]   -- NOTE: uint % uint in C

      -- quotRemWord# :: Word# -> Word# -> (# Word#, Word# #)
  defOp "quotRemWord#"  $ \[WordV a, WordV b] -> pure [WordV $ a `quot` b, WordV $ a `rem` b]

      -- quotRemWord2# :: Word# -> Word# -> Word# -> (# Word#, Word# #)
  defOp "quotRemWord2#" $ \[WordV hi, WordV lo, WordV b'] ->
    let
      a = fromIntegral hi * (1 + fromIntegral (maxBound :: PrimWord)) + fromIntegral lo :: Integer
      b = fromIntegral b' :: Integer
    in pure [WordV . fromIntegral $ a `quot` b, WordV . fromIntegral $ a `rem` b]

      -- and# :: Word# -> Word# -> Word#
  defOp "and#"  $ \[WordV a, WordV b]     -> pure [WordV $ a .&. b]

      -- or# :: Word# -> Word# -> Word#
  defOp "or#"   $ \[WordV a, WordV b]     -> pure [WordV $ a .|. b]

      -- xor# :: Word# -> Word# -> Word#
  defOp "xor#"  $ \[WordV a, WordV b]     -> pure [WordV $ a `xor` b]

      -- not# :: Word# -> Word#
  defOp "not#"  $ \[WordV a]              -> pure [WordV $ complement a]

      -- uncheckedShiftL# :: Word# -> Int# -> Word#
  defOp "uncheckedShiftL#"  $ \[WordV a, IntV b]  -> pure [WordV $ unsafeShiftL a (fromIntegral b)]

      -- uncheckedShiftRL# :: Word# -> Int# -> Word#
  defOp "uncheckedShiftRL#" $ \[WordV a, IntV b]  -> pure [WordV $ unsafeShiftR a (fromIntegral b)] -- Shift right logical

      -- word2Int# :: Word# -> Int#
  defOp "word2Int#" $ \[WordV a]           -> pure [IntV $ fromIntegral a] -- HINT: noop ; same bit level representation

      -- gtWord# :: Word# -> Word# -> Int#
  defOp "gtWord#"   $ \[WordV a, WordV b]  -> pure [IntV $ if a > b  then 1 else 0]

      -- geWord# :: Word# -> Word# -> Int#
  defOp "geWord#"   $ \[WordV a, WordV b]  -> pure [IntV $ if a >= b then 1 else 0]

      -- eqWord# :: Word# -> Word# -> Int#
  defOp "eqWord#"   $ \[WordV a, WordV b]  -> pure [IntV $ if a == b then 1 else 0]

      -- neWord# :: Word# -> Word# -> Int#
  defOp "neWord#"   $ \[WordV a, WordV b]  -> pure [IntV $ if a /= b then 1 else 0]

      -- ltWord# :: Word# -> Word# -> Int#
  defOp "ltWord#"   $ \[WordV a, WordV b]  -> pure [IntV $ if a < b  then 1 else 0]

      -- leWord# :: Word# -> Word# -> Int#
  defOp "leWord#"   $ \[WordV a, WordV b]  -> pure [IntV $ if a <= b then 1 else 0]

      -- popCnt8# :: Word# -> Word#
  defOp "popCnt8#"  $ \[WordV a]           -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word8)]

      -- popCnt16# :: Word# -> Word#
  defOp "popCnt16#" $ \[WordV a]           -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word16)]

      -- popCnt32# :: Word# -> Word#
  defOp "popCnt32#" $ \[WordV a]           -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word32)]

      -- popCnt64# :: WORD64 -> Word#
  defOp "popCnt64#" $ \[WordV a]           -> pure [WordV . fromIntegral $ popCount (fromIntegral a :: Word64)]

      -- popCnt# :: Word# -> Word#
  defOp "popCnt#"   $ \[WordV a]           -> pure [WordV . fromIntegral $ popCount a]

      -- pdep8# :: Word# -> Word# -> Word#
  defOp "pdep8#" $ \[WordV a, WordV b]     -> pure [WordV $ fromIntegral $ pdep8 (fromIntegral a) (fromIntegral b)]

      -- pdep16# :: Word# -> Word# -> Word#
  defOp "pdep16#" $ \[WordV a, WordV b]    -> pure [WordV $ fromIntegral $ pdep16 (fromIntegral a) (fromIntegral b)]

      -- pdep32# :: Word# -> Word# -> Word#
  defOp "pdep32#" $ \[WordV a, WordV b]    -> pure [WordV $ fromIntegral $ pdep32 (fromIntegral a) (fromIntegral b)]

      -- pdep64# :: Word64 -> Word64 -> Word64
  defOp "pdep64#" $ \[WordV a, WordV b]    -> pure [WordV $ fromIntegral $ pdep64 (fromIntegral a) (fromIntegral b)]

      -- pdep# :: Word# -> Word# -> Word#
  defOp "pdep#" $ \[WordV a, WordV b]      -> pure [WordV $ fromIntegral $ pdep (fromIntegral a) (fromIntegral b)]

      -- pext8# :: Word# -> Word# -> Word#
  defOp "pext8#" $ \[WordV a, WordV b]     -> pure [WordV $ fromIntegral $ pext8 (fromIntegral a) (fromIntegral b)]

      -- pext16# :: Word# -> Word# -> Word#
  defOp "pext16#" $ \[WordV a, WordV b]    -> pure [WordV $ fromIntegral $ pext16 (fromIntegral a) (fromIntegral b)]

      -- pext32# :: Word# -> Word# -> Word#
  defOp "pext32#" $ \[WordV a, WordV b]    -> pure [WordV $ fromIntegral $ pext32 (fromIntegral a) (fromIntegral b)]

      -- pext64# :: Word64 -> Word64 -> Word64
  defOp "pext64#" $ \[WordV a, WordV b]    -> pure [WordV $ fromIntegral $ pext64 (fromIntegral a) (fromIntegral b)]

      -- pext# :: Word# -> Word# -> Word#
  defOp "pext#" $ \[WordV a, WordV b]      -> pure [WordV $ fromIntegral $ pext (fromIntegral a) (fromIntegral b)]

      -- clz8# :: Word# -> Word#
  defOp "clz8#"   $ \[WordV a] -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word8)]

      -- clz16# :: Word# -> Word#
  defOp "clz16#"  $ \[WordV a] -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word16)]

      -- clz32# :: Word# -> Word#
  defOp "clz32#"  $ \[WordV a] -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word32)]

      -- clz64# :: WORD64 -> Word#
  defOp "clz64#"  $ \[WordV a] -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word64)]

      -- clz# :: Word# -> Word#
  defOp "clz#"    $ \[WordV a] -> pure [WordV . fromIntegral $ countLeadingZeros (fromIntegral a :: Word)]

      -- ctz8#  :: Word# -> Word#
  defOp "ctz8#"   $ \[WordV a] -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word8)]

      -- ctz16# :: Word# -> Word#
  defOp "ctz16#"  $ \[WordV a] -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word16)]

      -- ctz32# :: Word# -> Word#
  defOp "ctz32#"  $ \[WordV a] -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word32)]

      -- ctz64# :: WORD64 -> Word#
  defOp "ctz64#"  $ \[WordV a] -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word64)]

      -- ctz# :: Word# -> Word#
  defOp "ctz#"    $ \[WordV a] -> pure [WordV . fromIntegral $ countTrailingZeros (fromIntegral a :: Word)]

      -- byteSwap16# :: Word# -> Word#
  defOp "byteSwap16#" $ \[WordV a]   -> pure [WordV . fromIntegral $ byteSwap16 (fromIntegral a :: Word16)]

      -- byteSwap32# :: Word# -> Word#
  defOp "byteSwap32#" $ \[WordV a]   -> pure [WordV . fromIntegral $ byteSwap32 (fromIntegral a :: Word32)]

      -- byteSwap64# :: WORD64 -> WORD64
  defOp "byteSwap64#" $ \[WordV a]   -> pure [WordV . fromIntegral $ byteSwap64 (fromIntegral a :: Word64)]

      -- byteSwap# :: Word# -> Word#
  defOp "byteSwap#"   $ \[WordV a]   -> pure [WordV . fromIntegral . byteSwap64 $ fromIntegral a]

      -- bitReverse8# :: Word# -> Word#
  defOp "bitReverse8#" $ \[WordV a]  -> pure [WordV $ bitReverse @Word8 $ fromIntegral a]

      -- bitReverse16# :: Word# -> Word#
  defOp "bitReverse16#" $ \[WordV a] -> pure [WordV $ bitReverse @Word16 $ fromIntegral a]

      -- bitReverse32# :: Word# -> Word#
  defOp "bitReverse32#" $ \[WordV a] -> pure [WordV $ bitReverse @Word32 $ fromIntegral a]

      -- bitReverse64# :: Word64 -> Word64
  defOp "bitReverse64#" $ \[WordV a] -> pure [WordV $ bitReverse @Word64 $ fromIntegral a]

      -- bitReverse# :: Word# -> Word#
  defOp "bitReverse#" $ \[WordV a]   -> pure [WordV $ bitReverse @Word64 $ fromIntegral a]

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

pdep8 :: Word8 -> Word8 -> Word8
pdep8 (W8# a) (W8# b) = W8# (pdep8# a b)

pdep16 :: Word16 -> Word16 -> Word16
pdep16 (W16# a) (W16# b) = W16# (pdep16# a b)

pdep32 :: Word32 -> Word32 -> Word32
pdep32 (W32# a) (W32# b) = W32# (pdep32# a b)

pdep64 :: Word64 -> Word64 -> Word64
pdep64 (W64# a) (W64# b) = W64# (pdep64# a b)

pdep :: Word -> Word -> Word
pdep (W# a) (W# b) = W# (pdep# a b)

pext8 :: Word8 -> Word8 -> Word8
pext8 (W8# a) (W8# b) = W8# (pext8# a b)

pext16 :: Word16 -> Word16 -> Word16
pext16 (W16# a) (W16# b) = W16# (pext16# a b)

pext32 :: Word32 -> Word32 -> Word32
pext32 (W32# a) (W32# b) = W32# (pext32# a b)

pext64 :: Word64 -> Word64 -> Word64
pext64 (W64# a) (W64# b) = W64# (pext64# a b)

pext :: Word -> Word -> Word
pext (W# a) (W# b) = W# (pext# a b)
