{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Word16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word16V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 let
    w16 = fromIntegral :: Word -> Word16
    w   = fromIntegral :: Word16 -> Word
 args <- getAtoms argsAddr
 case (op, args) of

  -- extendWord16# :: Word16# -> Word#
  ( "extendWord16#",  [Word16V a])            -> allocAtoms [WordV a]

  -- narrowWord16# :: Word# -> Word16#
  ( "narrowWord16#",  [WordV a])              -> allocAtoms [Word16V . w . w16 $ a]

  -- notWord16# :: Word16# -> Word16#
  ( "notWord16#",     [Word16V a])            -> allocAtoms [Word16V . w . complement $ w16 a]

  -- plusWord16# :: Word16# -> Word16# -> Word16#
  ( "plusWord16#",    [Word16V a, Word16V b]) -> allocAtoms [Word16V . w $ w16 a + w16 b]

  -- subWord16# :: Word16# -> Word16# -> Word16#
  ( "subWord16#",     [Word16V a, Word16V b]) -> allocAtoms [Word16V . w  $ w16 a - w16 b]

  -- timesWord16# :: Word16# -> Word16# -> Word16#
  ( "timesWord16#",   [Word16V a, Word16V b]) -> allocAtoms [Word16V . w  $ w16 a * w16 b]

  -- quotWord16# :: Word16# -> Word16# -> Word16#
  ( "quotWord16#",    [Word16V a, Word16V b]) -> allocAtoms [Word16V . w  $ w16 a `quot` w16 b]  -- NOTE: uint16 / uint16 in C

  -- remWord16# :: Word16# -> Word16# -> Word16#
  ( "remWord16#",     [Word16V a, Word16V b]) -> allocAtoms [Word16V . w  $ w16 a `rem` w16 b]   -- NOTE: uint16 % uint16 in C

  -- quotRemWord16# :: Word16# -> Word16# -> (# Word16#, Word16# #)
  ( "quotRemWord16#", [Word16V a, Word16V b]) -> allocAtoms [Word16V . w $ w16 a `quot` w16 b, Word16V . w $ w16 a `rem` w16 b]

  -- eqWord16# :: Word16# -> Word16# -> Int#
  ( "eqWord16#",      [Word16V a, Word16V b]) -> allocAtoms [IntV $ if a == b then 1 else 0]

  -- geWord16# :: Word16# -> Word16# -> Int#
  ( "geWord16#",      [Word16V a, Word16V b]) -> allocAtoms [IntV $ if a >= b then 1 else 0]

  -- gtWord16# :: Word16# -> Word16# -> Int#
  ( "gtWord16#",      [Word16V a, Word16V b]) -> allocAtoms [IntV $ if a > b  then 1 else 0]

  -- leWord16# :: Word16# -> Word16# -> Int#
  ( "leWord16#",      [Word16V a, Word16V b]) -> allocAtoms [IntV $ if a <= b then 1 else 0]

  -- ltWord16# :: Word16# -> Word16# -> Int#
  ( "ltWord16#",      [Word16V a, Word16V b]) -> allocAtoms [IntV $ if a < b  then 1 else 0]

  -- neWord16# :: Word16# -> Word16# -> Int#
  ( "neWord16#",      [Word16V a, Word16V b]) -> allocAtoms [IntV $ if a /= b then 1 else 0]

  _ -> fallback op argsAddr t tc
