{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int32 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int
import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int32V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    i32 = fromIntegral :: Int -> Int32
    i   = fromIntegral :: Int32 -> Int
 case (op, args) of

  -- int32ToInt# :: Int32# -> Int#
  ( "int32ToInt#",    [Int32V a])           -> pure [IntV a]

  -- intToInt32# :: Int# -> Int32#
  ( "intToInt32#",    [IntV a])             -> pure [Int32V . i $ i32 a]

  -- negateInt32# :: Int32# -> Int32#
  ( "negateInt32#",   [Int32V a])           -> pure [Int32V . i . negate $ i32 a]

  -- plusInt32# :: Int32# -> Int32# -> Int32#
  ( "plusInt32#",     [Int32V a, Int32V b]) -> pure [Int32V . i $ i32 a + i32 b]

  -- subInt32# :: Int32# -> Int32# -> Int32#
  ( "subInt32#",      [Int32V a, Int32V b]) -> pure [Int32V . i $ i32 a - i32 b]

  -- timesInt32# :: Int32# -> Int32# -> Int32#
  ( "timesInt32#",    [Int32V a, Int32V b]) -> pure [Int32V . i $ i32 a * i32 b]

  -- quotInt32# :: Int32# -> Int32# -> Int32#
  ( "quotInt32#",     [Int32V a, Int32V b]) -> pure [Int32V . i $ i32 a `quot` i32 b]  -- NOTE: int32 / int32 in C

  -- remInt32# :: Int32# -> Int32# -> Int32#
  ( "remInt32#",      [Int32V a, Int32V b]) -> pure [Int32V . i $ i32 a `rem` i32 b]   -- NOTE: int32 % int32 in C

  -- quotRemInt32# :: Int32# -> Int32# -> (# Int32#, Int32# #)
  ( "quotRemInt32#",  [Int32V a, Int32V b]) -> pure [Int32V . i $ i32 a `quot` i32 b, Int32V . i $ i32 a `rem` i32 b]

  -- uncheckedShiftLInt32#  :: Int32# -> Int# -> Int32#
  ( "uncheckedShiftLInt32#",  [Int32V a, IntV b]) -> pure [Int32V . i $ unsafeShiftL (i32 a) b]

  -- uncheckedShiftRAInt32# :: Int32# -> Int# -> Int32#
  ( "uncheckedShiftRAInt32#", [Int32V a, IntV b]) -> pure [Int32V . i $ unsafeShiftR (i32 a) b] -- Shift right arithmetic

  -- uncheckedShiftRLInt32# :: Int32# -> Int# -> Int32#
  ( "uncheckedShiftRLInt32#", [Int32V a, IntV b]) -> pure [Int32V $ fromIntegral $ unsafeShiftR (fromIntegral a :: Word32) b] -- Shift right logical

  -- int32ToWord32# :: Int32# -> Word32#
  ( "int32ToWord32#", [Int32V a])           -> pure [Word32V $ fromIntegral a]

  -- eqInt32# :: Int32# -> Int32# -> Int#
  ( "eqInt32#",       [Int32V a, Int32V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geInt32# :: Int32# -> Int32# -> Int#
  ( "geInt32#",       [Int32V a, Int32V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtInt32# :: Int32# -> Int32# -> Int#
  ( "gtInt32#",       [Int32V a, Int32V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leInt32# :: Int32# -> Int32# -> Int#
  ( "leInt32#",       [Int32V a, Int32V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltInt32# :: Int32# -> Int32# -> Int#
  ( "ltInt32#",       [Int32V a, Int32V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neInt32# :: Int32# -> Int32# -> Int#
  ( "neInt32#",       [Int32V a, Int32V b]) -> pure [IntV $ if a /= b then 1 else 0]

  _ -> fallback op args t tc
