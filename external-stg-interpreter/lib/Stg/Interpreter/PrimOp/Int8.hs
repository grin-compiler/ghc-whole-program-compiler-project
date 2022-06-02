{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int8 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int8V i   = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> M [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 let
    i8  = fromIntegral :: Int -> Int8
    i   = fromIntegral :: Int8 -> Int
 args <- getAtoms argsAddr
 case (op, args) of

  -- extendInt8# :: Int8# -> Int#
  ( "extendInt8#",   [Int8V a])           -> allocAtoms [IntV a]

  -- narrowInt8# :: Int# -> Int8#
  ( "narrowInt8#",   [IntV a])            -> allocAtoms [Int8V . i $ i8 a]

  -- negateInt8# :: Int8# -> Int8#
  ( "negateInt8#",   [Int8V a])           -> allocAtoms [Int8V . i . negate $ i8 a]

  -- plusInt8# :: Int8# -> Int8# -> Int8#
  ( "plusInt8#",     [Int8V a, Int8V b])  -> allocAtoms [Int8V . i $ i8 a + i8 b]

  -- subInt8# :: Int8# -> Int8# -> Int8#
  ( "subInt8#",      [Int8V a, Int8V b])  -> allocAtoms [Int8V . i $ i8 a - i8 b]

  -- timesInt8# :: Int8# -> Int8# -> Int8#
  ( "timesInt8#",    [Int8V a, Int8V b])  -> allocAtoms [Int8V . i $ i8 a * i8 b]

  -- quotInt8# :: Int8# -> Int8# -> Int8#
  ( "quotInt8#",     [Int8V a, Int8V b])  -> allocAtoms [Int8V . i $ i8 a `quot` i8 b]  -- NOTE: int8 / int8 in C

  -- remInt8# :: Int8# -> Int8# -> Int8#
  ( "remInt8#",      [Int8V a, Int8V b])  -> allocAtoms [Int8V . i $ i8 a `rem` i8 b]   -- NOTE: int8 % int8 in C

  -- quotRemInt8# :: Int8# -> Int8# -> (# Int8#, Int8# #)
  ( "quotRemInt8#",  [Int8V a, Int8V b])  -> allocAtoms [Int8V . i $ i8 a `quot` i8 b, Int8V . i $ i8 a `rem` i8 b]

  -- eqInt8# :: Int8# -> Int8# -> Int#
  ( "eqInt8#",       [Int8V a, Int8V b])  -> allocAtoms [IntV $ if a == b then 1 else 0]

  -- geInt8# :: Int8# -> Int8# -> Int#
  ( "geInt8#",       [Int8V a, Int8V b])  -> allocAtoms [IntV $ if a >= b then 1 else 0]

  -- gtInt8# :: Int8# -> Int8# -> Int#
  ( "gtInt8#",       [Int8V a, Int8V b])  -> allocAtoms [IntV $ if a > b  then 1 else 0]

  -- leInt8# :: Int8# -> Int8# -> Int#
  ( "leInt8#",       [Int8V a, Int8V b])  -> allocAtoms [IntV $ if a <= b then 1 else 0]

  -- ltInt8# :: Int8# -> Int8# -> Int#
  ( "ltInt8#",       [Int8V a, Int8V b])  -> allocAtoms [IntV $ if a < b  then 1 else 0]

  -- neInt8# :: Int8# -> Int8# -> Int#
  ( "neInt8#",       [Int8V a, Int8V b])  -> allocAtoms [IntV $ if a /= b then 1 else 0]

  _ -> fallback op argsAddr t tc
