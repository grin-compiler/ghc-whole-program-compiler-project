{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 let
    i16 = fromIntegral :: Int -> Int16
    i   = fromIntegral :: Int16 -> Int
 args <- getAtoms argsAddr
 case (op, args) of

  -- extendInt16# :: Int16# -> Int#
  ( "extendInt16#",   [Int16V a])           -> allocAtoms [IntV a]

  -- narrowInt16# :: Int# -> Int16#
  ( "narrowInt16#",   [IntV a])             -> allocAtoms [Int16V . i $ i16 a]

  -- negateInt16# :: Int16# -> Int16#
  ( "negateInt16#",   [Int16V a])           -> allocAtoms [Int16V . i . negate $ i16 a]

  -- plusInt16# :: Int16# -> Int16# -> Int16#
  ( "plusInt16#",     [Int16V a, Int16V b]) -> allocAtoms [Int16V . i $ i16 a + i16 b]

  -- subInt16# :: Int16# -> Int16# -> Int16#
  ( "subInt16#",      [Int16V a, Int16V b]) -> allocAtoms [Int16V . i $ i16 a - i16 b]

  -- timesInt16# :: Int16# -> Int16# -> Int16#
  ( "timesInt16#",    [Int16V a, Int16V b]) -> allocAtoms [Int16V . i $ i16 a * i16 b]

  -- quotInt16# :: Int16# -> Int16# -> Int16#
  ( "quotInt16#",     [Int16V a, Int16V b]) -> allocAtoms [Int16V . i $ i16 a `quot` i16 b]  -- NOTE: int16 / int16 in C

  -- remInt16# :: Int16# -> Int16# -> Int16#
  ( "remInt16#",      [Int16V a, Int16V b]) -> allocAtoms [Int16V . i $ i16 a `rem` i16 b]   -- NOTE: int16 % int16 in C

  -- quotRemInt16# :: Int16# -> Int16# -> (# Int16#, Int16# #)
  ( "quotRemInt16#",  [Int16V a, Int16V b]) -> allocAtoms [Int16V . i $ i16 a `quot` i16 b, Int16V . i $ i16 a `rem` i16 b]

  -- eqInt16# :: Int16# -> Int16# -> Int#
  ( "eqInt16#",       [Int16V a, Int16V b]) -> allocAtoms [IntV $ if a == b then 1 else 0]

  -- geInt16# :: Int16# -> Int16# -> Int#
  ( "geInt16#",       [Int16V a, Int16V b]) -> allocAtoms [IntV $ if a >= b then 1 else 0]

  -- gtInt16# :: Int16# -> Int16# -> Int#
  ( "gtInt16#",       [Int16V a, Int16V b]) -> allocAtoms [IntV $ if a > b  then 1 else 0]

  -- leInt16# :: Int16# -> Int16# -> Int#
  ( "leInt16#",       [Int16V a, Int16V b]) -> allocAtoms [IntV $ if a <= b then 1 else 0]

  -- ltInt16# :: Int16# -> Int16# -> Int#
  ( "ltInt16#",       [Int16V a, Int16V b]) -> allocAtoms [IntV $ if a < b  then 1 else 0]

  -- neInt16# :: Int16# -> Int16# -> Int#
  ( "neInt16#",       [Int16V a, Int16V b]) -> allocAtoms [IntV $ if a /= b then 1 else 0]

  _ -> fallback op argsAddr t tc
