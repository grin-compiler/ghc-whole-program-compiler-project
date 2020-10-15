{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Word16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word

pattern IntV i    = Literal (LitNumber LitNumInt i)
pattern WordV w   = Literal (LitNumber LitNumWord w)
pattern Word16V w = Literal (LitNumber LitNumWord w)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- TODO: extendWord16# :: Word16# -> Word#

  -- narrowWord16# :: Word# -> Word16#
  ("narrowWord16#",   [WordV a])  -> pure [Word16V $ fromIntegral (fromIntegral a :: Word16)]

  -- notWord16# :: Word16# -> Word16#
  -- TODO: implement correctly ("notWord16#",  [Word16V a])  -> pure [Word16V $ complement a]

  -- plusWord16# :: Word16# -> Word16# -> Word16#
  ("plusWord16#",     [Word16V a, Word16V b]) -> pure [Word16V $ a + b]

  -- subWord16# :: Word16# -> Word16# -> Word16#
  ("subWord16#",      [Word16V a, Word16V b]) -> pure [Word16V $ a - b]

  -- timesWord16# :: Word16# -> Word16# -> Word16#
  ("timesWord16#",    [Word16V a, Word16V b]) -> pure [Word16V $ a * b]

  -- quotWord16# :: Word16# -> Word16# -> Word16#
  ("quotWord16#",     [Word16V a, Word16V b]) -> pure [Word16V $ a `quot` b]  -- NOTE: uint16 / uint16 in C

  -- remWord16# :: Word16# -> Word16# -> Word16#
  ("remWord16#",      [Word16V a, Word16V b]) -> pure [Word16V $ a `rem` b]   -- NOTE: uint16 % uint16 in C

  -- quotRemWord16# :: Word16# -> Word16# -> (# Word16#, Word16# #)
  ("quotRemWord16#",  [Word16V a, Word16V b]) -> pure [Word16V $ a `quot` b, Word16V $ a `rem` b]

  -- eqWord16# :: Word16# -> Word16# -> Int#
  ("eqWord16#",       [Word16V a, Word16V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geWord16# :: Word16# -> Word16# -> Int#
  ("geWord16#",       [Word16V a, Word16V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtWord16# :: Word16# -> Word16# -> Int#
  ("gtWord16#",       [Word16V a, Word16V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leWord16# :: Word16# -> Word16# -> Int#
  ("leWord16#",       [Word16V a, Word16V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltWord16# :: Word16# -> Word16# -> Int#
  ("ltWord16#",       [Word16V a, Word16V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neWord16# :: Word16# -> Word16# -> Int#
  ("neWord16#",       [Word16V a, Word16V b]) -> pure [IntV $ if a /= b then 1 else 0]

  _ -> fallback op args t tc
