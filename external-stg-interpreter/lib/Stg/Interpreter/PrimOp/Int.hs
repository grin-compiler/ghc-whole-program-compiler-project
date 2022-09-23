{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module Stg.Interpreter.PrimOp.Int where

import GHC.Exts
import Foreign.Storable (sizeOf)
import Data.Int
import Data.Word
import Data.Bits
import Data.Char

import Stg.Syntax
import Stg.Interpreter.Base

type PrimInt  = Int64

pattern CharV c   = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- +# :: Int# -> Int# -> Int#
  ( "+#", [IntV a, IntV b]) -> pure [IntV $ a + b]

  -- -# :: Int# -> Int# -> Int#
  ( "-#", [IntV a, IntV b]) -> pure [IntV $ a - b]

  -- *# :: Int# -> Int# -> Int#
  ( "*#", [IntV a, IntV b]) -> pure [IntV $ a * b]

  -- timesInt2# :: Int# -> Int# -> (# Int#, Int#, Int# #)
  ( "timesInt2#", [IntV a, IntV b]) -> pure [IntV isHighNeeded, IntV hi, IntV lo] where
    (isHighNeeded, hi, lo) = genericIMul2 a b

    -- HINT: this code is from suite/tests/codeGen/should_run/cgrun079.hs
    --       genericIMul2 is a generic implementation of the timesInt2# primop
    genericIMul2 :: Int -> Int -> (Int,Int,Int)
    genericIMul2 x y = (c,h,l)
       where
          (p,l) = timesWord2 (fromIntegral x) (fromIntegral y)
          h = p - f x y - f y x
          c = if h == carryFill l then 0 else 1
          f u v = carryFill u .&. v

          -- Return either 00..00 or FF..FF depending on the carry
          carryFill :: Int -> Int
          carryFill x = x `shiftR` (wordSizeInBits - 1)

    wordSizeInBits :: Int
    wordSizeInBits = 8 * sizeOf (0 :: Word)

    timesWord2 :: Word -> Word -> (Int,Int)
    timesWord2 (W# x) (W# y) = case timesWord2# x y of
       (# h, l #) -> (I# (word2Int# h), I# (word2Int# l))

  -- mulIntMayOflo# :: Int# -> Int# -> Int#
  ( "mulIntMayOflo#",  [IntV a, IntV b]) -> pure [IntV $ if fromIntegral a * (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0]

  -- quotInt# :: Int# -> Int# -> Int#
  ( "quotInt#",        [IntV a, IntV b]) -> pure [IntV $ a `quot` b]  -- NOTE: int / int in C

  -- remInt# :: Int# -> Int# -> Int#
  ( "remInt#",         [IntV a, IntV b]) -> pure [IntV $ a `rem` b]   -- NOTE: int % int in C

  -- quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
  ( "quotRemInt#",     [IntV a, IntV b]) -> pure [IntV $ a `quot` b, IntV $ a `rem` b]

  -- andI# :: Int# -> Int# -> Int#
  ( "andI#",           [IntV a, IntV b]) -> pure [IntV $ a .&. b]

  -- orI# :: Int# -> Int# -> Int#
  ( "orI#",            [IntV a, IntV b]) -> pure [IntV $ a .|. b]

  -- xorI# :: Int# -> Int# -> Int#
  ( "xorI#",           [IntV a, IntV b]) -> pure [IntV $ a `xor` b]

  -- notI# :: Int# -> Int#
  ( "notI#",           [IntV a]) -> pure [IntV $ complement a]

  -- negateInt# :: Int# -> Int#
  ( "negateInt#",      [IntV a]) -> pure [IntV (-a)]

  -- addIntC# :: Int# -> Int# -> (# Int#, Int# #)
  ( "addIntC#",        [IntV a, IntV b]) -> pure
                                        [ IntV $ a + b
                                        , IntV . carry $ fromIntegral a + fromIntegral b
                                        ] where
                                            carry :: Integer -> Int
                                            carry x = if x < fromIntegral (minBound :: PrimInt) || x > fromIntegral (maxBound :: PrimInt) then 1 else 0

  -- subIntC# :: Int# -> Int# -> (# Int#, Int# #)
  ( "subIntC#",        [IntV a, IntV b]) -> pure
                                        [ IntV $ a - b
                                        , IntV . carry $ fromIntegral a - fromIntegral b
                                        ] where
                                            carry :: Integer -> Int
                                            carry x = if x < fromIntegral (minBound :: PrimInt) || x > fromIntegral (maxBound :: PrimInt) then 1 else 0

  -- ># :: Int# -> Int# -> Int#
  ( ">#",  [IntV a, IntV b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- >=# :: Int# -> Int# -> Int#
  ( ">=#", [IntV a, IntV b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- ==# :: Int# -> Int# -> Int#
  ( "==#", [IntV a, IntV b]) -> pure [IntV $ if a == b then 1 else 0]

  -- /=# :: Int# -> Int# -> Int#
  ( "/=#", [IntV a, IntV b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- <# :: Int# -> Int# -> Int#
  ( "<#",  [IntV a, IntV b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- <=# :: Int# -> Int# -> Int#
  ( "<=#", [IntV a, IntV b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- chr# :: Int# -> Char#
  ( "chr#",                [IntV (I# a)]) -> pure [CharV (C# (chr# a))] -- HINT: noop ; same bit level representation

  -- int2Word# :: Int# -> Word#
  ( "int2Word#",           [IntV a]) -> pure [WordV $ fromIntegral a] -- HINT: noop ; same bit level representation

  -- int2Float# :: Int# -> Float#
  ( "int2Float#",          [IntV a] ) -> pure [FloatV $ fromIntegral a]

  -- int2Double# :: Int# -> Double#
  ( "int2Double#",         [IntV a] ) -> pure [DoubleV $ fromIntegral a]

  -- word2Float# :: Word# -> Float#
  ( "word2Float#",         [WordV a]) -> pure [FloatV $ fromIntegral a]

  -- word2Double# :: Word# -> Double#
  ( "word2Double#",        [WordV a]) -> pure [DoubleV $ fromIntegral a]

  -- uncheckedIShiftL# :: Int# -> Int# -> Int#
  ( "uncheckedIShiftL#",   [IntV a, IntV b]) -> pure [IntV $ unsafeShiftL a b]

  -- uncheckedIShiftRA# :: Int# -> Int# -> Int#
  ( "uncheckedIShiftRA#",  [IntV a, IntV b]) -> pure [IntV $ unsafeShiftR a b] -- Shift right arithmetic

  -- uncheckedIShiftRL# :: Int# -> Int# -> Int#
  ( "uncheckedIShiftRL#",  [IntV a, IntV b]) -> pure [IntV $ fromIntegral $ unsafeShiftR (fromIntegral a :: Word64) b] -- Shift right logical

  _ -> fallback op args t tc
