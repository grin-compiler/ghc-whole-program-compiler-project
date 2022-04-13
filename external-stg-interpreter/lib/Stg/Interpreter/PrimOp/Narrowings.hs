{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Narrowings where

import Data.Int
import Data.Word

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

      -- narrow8Int# :: Int# -> Int#
  defOp "narrow8Int#"   $ \[IntV a]  -> pure [IntV  $ fromIntegral (fromIntegral a :: Int8)]

      -- narrow16Int# :: Int# -> Int#
  defOp "narrow16Int#"  $ \[IntV a]  -> pure [IntV  $ fromIntegral (fromIntegral a :: Int16)]

      -- narrow32Int# :: Int# -> Int#
  defOp "narrow32Int#"  $ \[IntV a]  -> pure [IntV  $ fromIntegral (fromIntegral a :: Int32)]

      -- narrow8Word# :: Word# -> Word#
  defOp "narrow8Word#"  $ \[WordV a] -> pure [WordV $ fromIntegral (fromIntegral a :: Word8)]

      -- narrow16Word# :: Word# -> Word#
  defOp "narrow16Word#" $ \[WordV a] -> pure [WordV $ fromIntegral (fromIntegral a :: Word16)]

      -- narrow32Word# :: Word# -> Word#
  defOp "narrow32Word#" $ \[WordV a] -> pure [WordV $ fromIntegral (fromIntegral a :: Word32)]
