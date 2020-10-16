{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Int where

import Data.Int
import Data.Word
import Data.Bits
import Data.Char

import Stg.Syntax
import Stg.Interpreter.Base

type PrimInt  = Int64
type PrimWord = Word64

pattern CharV c   = Literal (LitChar c)
pattern IntV i    = Literal (LitNumber LitNumInt i)
pattern WordV w   = Literal (LitNumber LitNumWord w)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- +# :: Int# -> Int# -> Int#
  ("+#", [IntV a, IntV b]) -> pure [IntV $ a + b]

  -- -# :: Int# -> Int# -> Int#
  ("-#", [IntV a, IntV b]) -> pure [IntV $ a - b]

  -- *# :: Int# -> Int# -> Int#
  ("*#", [IntV a, IntV b]) -> pure [IntV $ a * b]

  -- TODO: primop   IntMul2Op    "timesInt2#" GenPrimOp Int# -> Int# -> (# Int#, Int#, Int# #)

  -- mulIntMayOflo# :: Int# -> Int# -> Int#
  ("mulIntMayOflo#",  [IntV a, IntV b]) -> pure [IntV $ if fromIntegral a * (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0]

  -- quotInt# :: Int# -> Int# -> Int#
  ("quotInt#",        [IntV a, IntV b]) -> pure [IntV $ a `quot` b]  -- NOTE: int / int in C

  -- remInt# :: Int# -> Int# -> Int#
  ("remInt#",         [IntV a, IntV b]) -> pure [IntV $ a `rem` b]   -- NOTE: int % int in C

  -- quotRemInt# :: Int# -> Int# -> (# Int#, Int# #)
  ("quotRemInt#",     [IntV a, IntV b]) -> pure [IntV $ a `quot` b, IntV $ a `rem` b]

  -- andI# :: Int# -> Int# -> Int#
  ("andI#",           [IntV a, IntV b]) -> pure [IntV $ a .&. b]

  -- orI# :: Int# -> Int# -> Int#
  ("orI#",            [IntV a, IntV b]) -> pure [IntV $ a .|. b]

  -- xorI# :: Int# -> Int# -> Int#
  ("xorI#",           [IntV a, IntV b]) -> pure [IntV $ a `xor` b]

  -- notI# :: Int# -> Int#
  ("notI#",           [IntV a]) -> pure [IntV $ complement a]

  -- negateInt# :: Int# -> Int#
  ("negateInt#",      [IntV a]) -> pure [IntV (-a)]

  -- addIntC# :: Int# -> Int# -> (# Int#, Int# #)
  ("addIntC#",        [IntV a, IntV b]) -> pure
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0
                                        ]

  -- subIntC# :: Int# -> Int# -> (# Int#, Int# #)
  ("subIntC#",        [IntV a, IntV b]) -> pure
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimInt) then 1 else 0
                                        ]

  -- ># :: Int# -> Int# -> Int#
  (">#",  [IntV a, IntV b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- >=# :: Int# -> Int# -> Int#
  (">=#", [IntV a, IntV b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- ==# :: Int# -> Int# -> Int#
  ("==#", [IntV a, IntV b]) -> pure [IntV $ if a == b then 1 else 0]

  -- /=# :: Int# -> Int# -> Int#
  ("/=#", [IntV a, IntV b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- <# :: Int# -> Int# -> Int#
  ("<#",  [IntV a, IntV b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- <=# :: Int# -> Int# -> Int#
  ("<=#", [IntV a, IntV b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- chr# :: Int# -> Char#
  ("chr#",                [IntV a]) -> pure [CharV . chr $ fromIntegral a] -- HINT: noop ; same bit level representation

  -- int2Word# :: Int# -> Word#
  ("int2Word#",           [IntV a]) -> pure [WordV $ fromIntegral a] -- HINT: noop ; same bit level representation

  -- int2Float# :: Int# -> Float#
  ("int2Float#",          [IntV a] ) -> pure [FloatV $ fromIntegral a]

  -- int2Double# :: Int# -> Double#
  ("int2Double#",         [IntV a] ) -> pure [DoubleV $ fromIntegral a]

  -- word2Float# :: Word# -> Float#
  ("word2Float#",         [WordV a]) -> pure [FloatV $ fromIntegral a]

  -- word2Double# :: Word# -> Double#
  ("word2Double#",        [WordV a]) -> pure [DoubleV $ fromIntegral a]

  -- uncheckedIShiftL# :: Int# -> Int# -> Int#
  ("uncheckedIShiftL#",   [IntV a, IntV b]) -> pure [IntV $ unsafeShiftL a (fromIntegral b)]

  -- uncheckedIShiftRA# :: Int# -> Int# -> Int#
  ("uncheckedIShiftRA#",  [IntV a, IntV b]) -> pure [IntV $ unsafeShiftR a (fromIntegral b)] -- Shift right arithmetic

  -- uncheckedIShiftRL# :: Int# -> Int# -> Int#
  ("uncheckedIShiftRL#",  [IntV a, IntV b]) -> pure [IntV $ fromIntegral $ unsafeShiftR (fromIntegral a :: PrimWord) (fromIntegral b)] -- Shift right logical

  _ -> fallback op args t tc
