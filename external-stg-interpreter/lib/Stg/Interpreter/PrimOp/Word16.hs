{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Word16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word16V i = WordAtom i -- Literal (LitNumber LitNumWord i)

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do
  let
    w16 = fromIntegral :: Word -> Word16
    w   = fromIntegral :: Word16 -> Word

      -- extendWord16# :: Word16# -> Word#
  defOp "extendWord16#"   $ \[Word16V a]            -> pure [WordV a]

      -- narrowWord16# :: Word# -> Word16#
  defOp "narrowWord16#"   $ \[WordV a]              -> pure [Word16V . w . w16 $ a]

      -- notWord16# :: Word16# -> Word16#
  defOp "notWord16#"      $ \[Word16V a]            -> pure [Word16V . w . complement $ w16 a]

      -- plusWord16# :: Word16# -> Word16# -> Word16#
  defOp "plusWord16#"     $ \[Word16V a, Word16V b] -> pure [Word16V . w $ w16 a + w16 b]

      -- subWord16# :: Word16# -> Word16# -> Word16#
  defOp "subWord16#"      $ \[Word16V a, Word16V b] -> pure [Word16V . w  $ w16 a - w16 b]

      -- timesWord16# :: Word16# -> Word16# -> Word16#
  defOp "timesWord16#"    $ \[Word16V a, Word16V b] -> pure [Word16V . w  $ w16 a * w16 b]

      -- quotWord16# :: Word16# -> Word16# -> Word16#
  defOp "quotWord16#"     $ \[Word16V a, Word16V b] -> pure [Word16V . w  $ w16 a `quot` w16 b]  -- NOTE: uint16 / uint16 in C

      -- remWord16# :: Word16# -> Word16# -> Word16#
  defOp "remWord16#"      $ \[Word16V a, Word16V b] -> pure [Word16V . w  $ w16 a `rem` w16 b]   -- NOTE: uint16 % uint16 in C

      -- quotRemWord16# :: Word16# -> Word16# -> (# Word16#, Word16# #)
  defOp "quotRemWord16#"  $ \[Word16V a, Word16V b] -> pure [Word16V . w $ w16 a `quot` w16 b, Word16V . w $ w16 a `rem` w16 b]

      -- eqWord16# :: Word16# -> Word16# -> Int#
  defOp "eqWord16#"       $ \[Word16V a, Word16V b] -> pure [IntV $ if a == b then 1 else 0]

      -- geWord16# :: Word16# -> Word16# -> Int#
  defOp "geWord16#"       $ \[Word16V a, Word16V b] -> pure [IntV $ if a >= b then 1 else 0]

      -- gtWord16# :: Word16# -> Word16# -> Int#
  defOp "gtWord16#"       $ \[Word16V a, Word16V b] -> pure [IntV $ if a > b  then 1 else 0]

      -- leWord16# :: Word16# -> Word16# -> Int#
  defOp "leWord16#"       $ \[Word16V a, Word16V b] -> pure [IntV $ if a <= b then 1 else 0]

      -- ltWord16# :: Word16# -> Word16# -> Int#
  defOp "ltWord16#"       $ \[Word16V a, Word16V b] -> pure [IntV $ if a < b  then 1 else 0]

      -- neWord16# :: Word16# -> Word16# -> Int#
  defOp "neWord16#"       $ \[Word16V a, Word16V b] -> pure [IntV $ if a /= b then 1 else 0]
