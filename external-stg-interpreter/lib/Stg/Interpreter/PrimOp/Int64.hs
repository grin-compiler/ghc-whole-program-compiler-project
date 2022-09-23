{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int64 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int
import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int64V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word64V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    i64 = fromIntegral :: Int -> Int64
    i   = fromIntegral :: Int64 -> Int
 case (op, args) of

  -- int64ToInt# :: Int64# -> Int#
  ( "int64ToInt#",    [Int64V a])           -> pure [IntV a]

  -- intToInt64# :: Int# -> Int64#
  ( "intToInt64#",    [IntV a])             -> pure [Int64V . i $ i64 a]

  -- negateInt64# :: Int64# -> Int64#
  ( "negateInt64#",   [Int64V a])           -> pure [Int64V . i . negate $ i64 a]

  -- plusInt64# :: Int64# -> Int64# -> Int64#
  ( "plusInt64#",     [Int64V a, Int64V b]) -> pure [Int64V . i $ i64 a + i64 b]

  -- subInt64# :: Int64# -> Int64# -> Int64#
  ( "subInt64#",      [Int64V a, Int64V b]) -> pure [Int64V . i $ i64 a - i64 b]

  -- timesInt64# :: Int64# -> Int64# -> Int64#
  ( "timesInt64#",    [Int64V a, Int64V b]) -> pure [Int64V . i $ i64 a * i64 b]

  -- quotInt64# :: Int64# -> Int64# -> Int64#
  ( "quotInt64#",     [Int64V a, Int64V b]) -> pure [Int64V . i $ i64 a `quot` i64 b]  -- NOTE: int64 / int64 in C

  -- remInt64# :: Int64# -> Int64# -> Int64#
  ( "remInt64#",      [Int64V a, Int64V b]) -> pure [Int64V . i $ i64 a `rem` i64 b]   -- NOTE: int64 % int64 in C

  -- uncheckedIShiftL64#  :: Int64# -> Int# -> Int64#
  ( "uncheckedIShiftL64#",  [Int64V a, IntV b]) -> pure [Int64V . i $ unsafeShiftL (i64 a) b]

  -- uncheckedIShiftRA64# :: Int64# -> Int# -> Int64#
  ( "uncheckedIShiftRA64#", [Int64V a, IntV b]) -> pure [Int64V . i $ unsafeShiftR (i64 a) b] -- Shift right arithmetic

  -- uncheckedIShiftRL64# :: Int64# -> Int# -> Int64#
  ( "uncheckedIShiftRL64#", [Int64V a, IntV b]) -> pure [Int64V $ fromIntegral $ unsafeShiftR (fromIntegral a :: Word64) b] -- Shift right logical

  -- int64ToWord64# :: Int64# -> Word64#
  ( "int64ToWord64#", [Int64V a])           -> pure [Word64V $ fromIntegral a]

  -- eqInt64# :: Int64# -> Int64# -> Int#
  ( "eqInt64#",       [Int64V a, Int64V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geInt64# :: Int64# -> Int64# -> Int#
  ( "geInt64#",       [Int64V a, Int64V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtInt64# :: Int64# -> Int64# -> Int#
  ( "gtInt64#",       [Int64V a, Int64V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leInt64# :: Int64# -> Int64# -> Int#
  ( "leInt64#",       [Int64V a, Int64V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltInt64# :: Int64# -> Int64# -> Int#
  ( "ltInt64#",       [Int64V a, Int64V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neInt64# :: Int64# -> Int64# -> Int#
  ( "neInt64#",       [Int64V a, Int64V b]) -> pure [IntV $ if a /= b then 1 else 0]

  _ -> fallback op args t tc
