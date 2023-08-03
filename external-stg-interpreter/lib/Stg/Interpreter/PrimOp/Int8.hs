{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int8 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int
import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int8V i   = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word8V i  = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    i8  = fromIntegral :: Int -> Int8
    i   = fromIntegral :: Int8 -> Int
 case (op, args) of

  -- int8ToInt# :: Int8# -> Int#
  ( "int8ToInt#",    [Int8V a])           -> pure [IntV a]

  -- intToInt8# :: Int# -> Int8#
  ( "intToInt8#",    [IntV a])            -> pure [Int8V . i $ i8 a]

  -- negateInt8# :: Int8# -> Int8#
  ( "negateInt8#",   [Int8V a])           -> pure [Int8V . i . negate $ i8 a]

  -- plusInt8# :: Int8# -> Int8# -> Int8#
  ( "plusInt8#",     [Int8V a, Int8V b])  -> pure [Int8V . i $ i8 a + i8 b]

  -- subInt8# :: Int8# -> Int8# -> Int8#
  ( "subInt8#",      [Int8V a, Int8V b])  -> pure [Int8V . i $ i8 a - i8 b]

  -- timesInt8# :: Int8# -> Int8# -> Int8#
  ( "timesInt8#",    [Int8V a, Int8V b])  -> pure [Int8V . i $ i8 a * i8 b]

  -- quotInt8# :: Int8# -> Int8# -> Int8#
  ( "quotInt8#",     [Int8V a, Int8V b])  -> pure [Int8V . i $ i8 a `quot` i8 b]  -- NOTE: int8 / int8 in C

  -- remInt8# :: Int8# -> Int8# -> Int8#
  ( "remInt8#",      [Int8V a, Int8V b])  -> pure [Int8V . i $ i8 a `rem` i8 b]   -- NOTE: int8 % int8 in C

  -- quotRemInt8# :: Int8# -> Int8# -> (# Int8#, Int8# #)
  ( "quotRemInt8#",  [Int8V a, Int8V b])  -> pure [Int8V . i $ i8 a `quot` i8 b, Int8V . i $ i8 a `rem` i8 b]

  -- uncheckedShiftLInt8# :: Int8# -> Int# -> Int8#
  ( "uncheckedShiftLInt8#",  [Int8V a, IntV b]) -> pure [Int8V . i $ unsafeShiftL (i8 a) b]

  -- uncheckedShiftRAInt8# :: Int8# -> Int# -> Int8#
  ( "uncheckedShiftRAInt8#", [Int8V a, IntV b]) -> pure [Int8V . i $ unsafeShiftR (i8 a) b] -- Shift right arithmetic

  -- uncheckedShiftRLInt8# :: Int8# -> Int# -> Int8#
  ( "uncheckedShiftRLInt8#", [Int8V a, IntV b]) -> pure [Int8V . fromIntegral $ unsafeShiftR (fromIntegral a :: Word8) b] -- Shift right logical

  -- int8ToWord8# :: Int8# -> Word8#
  ( "int8ToWord8#",  [Int8V a])           -> pure [Word8V $ fromIntegral a]

  -- eqInt8# :: Int8# -> Int8# -> Int#
  ( "eqInt8#",       [Int8V a, Int8V b])  -> pure [IntV $ if a == b then 1 else 0]

  -- geInt8# :: Int8# -> Int8# -> Int#
  ( "geInt8#",       [Int8V a, Int8V b])  -> pure [IntV $ if a >= b then 1 else 0]

  -- gtInt8# :: Int8# -> Int8# -> Int#
  ( "gtInt8#",       [Int8V a, Int8V b])  -> pure [IntV $ if a > b  then 1 else 0]

  -- leInt8# :: Int8# -> Int8# -> Int#
  ( "leInt8#",       [Int8V a, Int8V b])  -> pure [IntV $ if a <= b then 1 else 0]

  -- ltInt8# :: Int8# -> Int8# -> Int#
  ( "ltInt8#",       [Int8V a, Int8V b])  -> pure [IntV $ if a < b  then 1 else 0]

  -- neInt8# :: Int8# -> Int8# -> Int#
  ( "neInt8#",       [Int8V a, Int8V b])  -> pure [IntV $ if a /= b then 1 else 0]

  -- OBSOLETE from GHC 9.2
  -- extendInt8# :: Int8# -> Int#
  ( "extendInt8#",   [Int8V a])           -> pure [IntV a]

  -- OBSOLETE from GHC 9.2
  -- narrowInt8# :: Int# -> Int8#
  ( "narrowInt8#",   [IntV a])            -> pure [Int8V . i $ i8 a]

  _ -> fallback op args t tc
