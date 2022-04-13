{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Int16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do
  let
    i16 = fromIntegral :: Int -> Int16
    i   = fromIntegral :: Int16 -> Int

      -- extendInt16# :: Int16# -> Int#
  defOp "extendInt16#"   $ \[Int16V a]           -> pure [IntV a]

      -- narrowInt16# :: Int# -> Int16#
  defOp "narrowInt16#"   $ \[IntV a]             -> pure [Int16V . i $ i16 a]

      -- negateInt16# :: Int16# -> Int16#
  defOp "negateInt16#"   $ \[Int16V a]           -> pure [Int16V . i . negate $ i16 a]

      -- plusInt16# :: Int16# -> Int16# -> Int16#
  defOp "plusInt16#"     $ \[Int16V a, Int16V b] -> pure [Int16V . i $ i16 a + i16 b]

      -- subInt16# :: Int16# -> Int16# -> Int16#
  defOp "subInt16#"      $ \[Int16V a, Int16V b] -> pure [Int16V . i $ i16 a - i16 b]

      -- timesInt16# :: Int16# -> Int16# -> Int16#
  defOp "timesInt16#"    $ \[Int16V a, Int16V b] -> pure [Int16V . i $ i16 a * i16 b]

      -- quotInt16# :: Int16# -> Int16# -> Int16#
  defOp "quotInt16#"     $ \[Int16V a, Int16V b] -> pure [Int16V . i $ i16 a `quot` i16 b]  -- NOTE: int16 / int16 in C

      -- remInt16# :: Int16# -> Int16# -> Int16#
  defOp "remInt16#"      $ \[Int16V a, Int16V b] -> pure [Int16V . i $ i16 a `rem` i16 b]   -- NOTE: int16 % int16 in C

      -- quotRemInt16# :: Int16# -> Int16# -> (# Int16#, Int16# #)
  defOp "quotRemInt16#"  $ \[Int16V a, Int16V b] -> pure [Int16V . i $ i16 a `quot` i16 b, Int16V . i $ i16 a `rem` i16 b]

      -- eqInt16# :: Int16# -> Int16# -> Int#
  defOp "eqInt16#"       $ \[Int16V a, Int16V b] -> pure [IntV $ if a == b then 1 else 0]

      -- geInt16# :: Int16# -> Int16# -> Int#
  defOp "geInt16#"       $ \[Int16V a, Int16V b] -> pure [IntV $ if a >= b then 1 else 0]

      -- gtInt16# :: Int16# -> Int16# -> Int#
  defOp "gtInt16#"       $ \[Int16V a, Int16V b] -> pure [IntV $ if a > b  then 1 else 0]

      -- leInt16# :: Int16# -> Int16# -> Int#
  defOp "leInt16#"       $ \[Int16V a, Int16V b] -> pure [IntV $ if a <= b then 1 else 0]

      -- ltInt16# :: Int16# -> Int16# -> Int#
  defOp "ltInt16#"       $ \[Int16V a, Int16V b] -> pure [IntV $ if a < b  then 1 else 0]

      -- neInt16# :: Int16# -> Int16# -> Int#
  defOp "neInt16#"       $ \[Int16V a, Int16V b] -> pure [IntV $ if a /= b then 1 else 0]
