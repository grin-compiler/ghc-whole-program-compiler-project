{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Word8 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word8V i  = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- TODO: extendWord8# :: Word8# -> Word#

  -- narrowWord8# :: Word# -> Word8#
  ("narrowWord8#",  [WordV a])  -> pure [Word8V $ fromIntegral (fromIntegral a :: Word8)]

  -- notWord8# :: Word8# -> Word8#
  -- TODO: implement correctly ("notWord8#",  [Word8V a])  -> pure [Word8V $ complement a]

  -- plusWord8# :: Word8# -> Word8# -> Word8#
  ("plusWord8#",    [Word8V a, Word8V b]) -> pure [Word8V $ a + b]

  -- subWord8# :: Word8# -> Word8# -> Word8#
  ("subWord8#",     [Word8V a, Word8V b]) -> pure [Word8V $ a - b]

  -- timesWord8# :: Word8# -> Word8# -> Word8#
  ("timesWord8#",   [Word8V a, Word8V b]) -> pure [Word8V $ a * b]

  -- quotWord8# :: Word8# -> Word8# -> Word8#
  ("quotWord8#",    [Word8V a, Word8V b]) -> pure [Word8V $ a `quot` b]  -- NOTE: uint8 / uint8 in C

  -- remWord8# :: Word8# -> Word8# -> Word8#
  ("remWord8#",     [Word8V a, Word8V b]) -> pure [Word8V $ a `rem` b]   -- NOTE: uint8 % uint8 in C

  -- quotRemWord8# :: Word8# -> Word8# -> (# Word8#, Word8# #)
  ("quotRemWord8#", [Word8V a, Word8V b]) -> pure [Word8V $ a `quot` b, Word8V $ a `rem` b]

  -- eqWord8# :: Word8# -> Word8# -> Int#
  ("eqWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geWord8# :: Word8# -> Word8# -> Int#
  ("geWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtWord8# :: Word8# -> Word8# -> Int#
  ("gtWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leWord8# :: Word8# -> Word8# -> Int#
  ("leWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltWord8# :: Word8# -> Word8# -> Int#
  ("ltWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neWord8# :: Word8# -> Word8# -> Int#
  ("neWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a /= b then 1 else 0]

  _ -> fallback op args t tc
