{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Char where

import Data.Char

import Stg.Syntax
import Stg.Interpreter.Base

pattern CharV c = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

      -- gtChar# :: Char# -> Char# -> Int#
  defOp "gtChar#" $ \[CharV a, CharV b] -> pure [IntV $ if a > b  then 1 else 0]

      -- geChar# :: Char# -> Char# -> Int#
  defOp "geChar#" $ \[CharV a, CharV b] -> pure [IntV $ if a >= b then 1 else 0]

      -- eqChar# :: Char# -> Char# -> Int#
  defOp "eqChar#" $ \[CharV a, CharV b] -> pure [IntV $ if a == b then 1 else 0]

      -- neChar# :: Char# -> Char# -> Int#
  defOp "neChar#" $ \[CharV a, CharV b] -> pure [IntV $ if a /= b then 1 else 0]

      -- ltChar# :: Char# -> Char# -> Int#
  defOp "ltChar#" $ \[CharV a, CharV b] -> pure [IntV $ if a < b  then 1 else 0]

      -- leChar# :: Char# -> Char# -> Int#
  defOp "leChar#" $ \[CharV a, CharV b] -> pure [IntV $ if a <= b then 1 else 0]

      -- ord# :: Char# -> Int#
  defOp "ord#"    $ \[CharV c] -> pure [IntV . fromIntegral $ ord c]
