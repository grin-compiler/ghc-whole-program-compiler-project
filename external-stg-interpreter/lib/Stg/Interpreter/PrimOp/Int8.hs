{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int8 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int8V i   = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do
  let
    i8  = fromIntegral :: Int -> Int8
    i   = fromIntegral :: Int8 -> Int

      -- extendInt8# :: Int8# -> Int#
  defOp "extendInt8#"   $ \[Int8V a]           -> pure [IntV a]

      -- narrowInt8# :: Int# -> Int8#
  defOp "narrowInt8#"   $ \[IntV a]            -> pure [Int8V . i $ i8 a]

      -- negateInt8# :: Int8# -> Int8#
  defOp "negateInt8#"   $ \[Int8V a]           -> pure [Int8V . i . negate $ i8 a]

      -- plusInt8# :: Int8# -> Int8# -> Int8#
  defOp "plusInt8#"     $ \[Int8V a, Int8V b]  -> pure [Int8V . i $ i8 a + i8 b]

      -- subInt8# :: Int8# -> Int8# -> Int8#
  defOp "subInt8#"      $ \[Int8V a, Int8V b]  -> pure [Int8V . i $ i8 a - i8 b]

      -- timesInt8# :: Int8# -> Int8# -> Int8#
  defOp "timesInt8#"    $ \[Int8V a, Int8V b]  -> pure [Int8V . i $ i8 a * i8 b]

      -- quotInt8# :: Int8# -> Int8# -> Int8#
  defOp "quotInt8#"     $ \[Int8V a, Int8V b]  -> pure [Int8V . i $ i8 a `quot` i8 b]  -- NOTE: int8 / int8 in C

      -- remInt8# :: Int8# -> Int8# -> Int8#
  defOp "remInt8#"      $ \[Int8V a, Int8V b]  -> pure [Int8V . i $ i8 a `rem` i8 b]   -- NOTE: int8 % int8 in C

      -- quotRemInt8# :: Int8# -> Int8# -> (# Int8#, Int8# #)
  defOp "quotRemInt8#"  $ \[Int8V a, Int8V b]  -> pure [Int8V . i $ i8 a `quot` i8 b, Int8V . i $ i8 a `rem` i8 b]

      -- eqInt8# :: Int8# -> Int8# -> Int#
  defOp "eqInt8#"       $ \[Int8V a, Int8V b]  -> pure [IntV $ if a == b then 1 else 0]

      -- geInt8# :: Int8# -> Int8# -> Int#
  defOp "geInt8#"       $ \[Int8V a, Int8V b]  -> pure [IntV $ if a >= b then 1 else 0]

      -- gtInt8# :: Int8# -> Int8# -> Int#
  defOp "gtInt8#"       $ \[Int8V a, Int8V b]  -> pure [IntV $ if a > b  then 1 else 0]

      -- leInt8# :: Int8# -> Int8# -> Int#
  defOp "leInt8#"       $ \[Int8V a, Int8V b]  -> pure [IntV $ if a <= b then 1 else 0]

      -- ltInt8# :: Int8# -> Int8# -> Int#
  defOp "ltInt8#"       $ \[Int8V a, Int8V b]  -> pure [IntV $ if a < b  then 1 else 0]

      -- neInt8# :: Int8# -> Int8# -> Int#
  defOp "neInt8#"       $ \[Int8V a, Int8V b]  -> pure [IntV $ if a /= b then 1 else 0]
