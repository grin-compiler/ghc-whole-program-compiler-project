{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int
import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word16V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    i16 = fromIntegral :: Int -> Int16
    i   = fromIntegral :: Int16 -> Int
 case (op, args) of

  -- int16ToInt# :: Int16# -> Int#
  ( "int16ToInt#",    [Int16V a])           -> pure [IntV a]

  -- intToInt16# :: Int# -> Int16#
  ( "intToInt16#",    [IntV a])             -> pure [Int16V . i $ i16 a]

  -- negateInt16# :: Int16# -> Int16#
  ( "negateInt16#",   [Int16V a])           -> pure [Int16V . i . negate $ i16 a]

  -- plusInt16# :: Int16# -> Int16# -> Int16#
  ( "plusInt16#",     [Int16V a, Int16V b]) -> pure [Int16V . i $ i16 a + i16 b]

  -- subInt16# :: Int16# -> Int16# -> Int16#
  ( "subInt16#",      [Int16V a, Int16V b]) -> pure [Int16V . i $ i16 a - i16 b]

  -- timesInt16# :: Int16# -> Int16# -> Int16#
  ( "timesInt16#",    [Int16V a, Int16V b]) -> pure [Int16V . i $ i16 a * i16 b]

  -- quotInt16# :: Int16# -> Int16# -> Int16#
  ( "quotInt16#",     [Int16V a, Int16V b]) -> pure [Int16V . i $ i16 a `quot` i16 b]  -- NOTE: int16 / int16 in C

  -- remInt16# :: Int16# -> Int16# -> Int16#
  ( "remInt16#",      [Int16V a, Int16V b]) -> pure [Int16V . i $ i16 a `rem` i16 b]   -- NOTE: int16 % int16 in C

  -- quotRemInt16# :: Int16# -> Int16# -> (# Int16#, Int16# #)
  ( "quotRemInt16#",  [Int16V a, Int16V b]) -> pure [Int16V . i $ i16 a `quot` i16 b, Int16V . i $ i16 a `rem` i16 b]

  -- uncheckedShiftLInt16# :: Int16# -> Int# -> Int16#
  ( "uncheckedShiftLInt16#",  [Int16V a, IntV b]) -> pure [Int16V . i $ unsafeShiftL (i16 a) b]

  -- uncheckedShiftRAInt16# :: Int16# -> Int# -> Int16#
  ( "uncheckedShiftRAInt16#", [Int16V a, IntV b]) -> pure [Int16V . i $ unsafeShiftR (i16 a) b] -- Shift right arithmetic

  -- uncheckedShiftRLInt16# :: Int16# -> Int# -> Int16#
  ( "uncheckedShiftRLInt16#", [Int16V a, IntV b]) -> pure [Int16V $ fromIntegral $ unsafeShiftR (fromIntegral a :: Word16) b] -- Shift right logical

  -- int16ToWord16# :: Int16# -> Word16#
  ( "int16ToWord16#", [Int16V a])           -> pure [Word16V $ fromIntegral a]

  -- eqInt16# :: Int16# -> Int16# -> Int#
  ( "eqInt16#",       [Int16V a, Int16V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geInt16# :: Int16# -> Int16# -> Int#
  ( "geInt16#",       [Int16V a, Int16V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtInt16# :: Int16# -> Int16# -> Int#
  ( "gtInt16#",       [Int16V a, Int16V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leInt16# :: Int16# -> Int16# -> Int#
  ( "leInt16#",       [Int16V a, Int16V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltInt16# :: Int16# -> Int16# -> Int#
  ( "ltInt16#",       [Int16V a, Int16V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neInt16# :: Int16# -> Int16# -> Int#
  ( "neInt16#",       [Int16V a, Int16V b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- OBSOLETE from GHC 9.2
  -- extendInt16# :: Int16# -> Int#
  ( "extendInt16#",   [Int16V a])           -> pure [IntV a]

  -- OBSOLETE from GHC 9.2
  -- narrowInt16# :: Int# -> Int16#
  ( "narrowInt16#",   [IntV a])             -> pure [Int16V . i $ i16 a]

  _ -> fallback op args t tc
