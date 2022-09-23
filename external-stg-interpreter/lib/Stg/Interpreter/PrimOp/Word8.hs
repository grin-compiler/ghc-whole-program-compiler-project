{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Word8 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int8V i   = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word8V i  = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    w8  = fromIntegral :: Word -> Word8
    w   = fromIntegral :: Word8 -> Word
 case (op, args) of

  -- word8ToWord# :: Word8# -> Word#
  ( "word8ToWord#",  [Word8V a])           -> pure [WordV a]

  -- wordToWord8# :: Word# -> Word8#
  ( "wordToWord8#",  [WordV a])            -> pure [Word8V . w . w8 $ a]

  -- plusWord8# :: Word8# -> Word8# -> Word8#
  ( "plusWord8#",    [Word8V a, Word8V b]) -> pure [Word8V . w $ w8 a + w8 b]

  -- subWord8# :: Word8# -> Word8# -> Word8#
  ( "subWord8#",     [Word8V a, Word8V b]) -> pure [Word8V . w  $ w8 a - w8 b]

  -- timesWord8# :: Word8# -> Word8# -> Word8#
  ( "timesWord8#",   [Word8V a, Word8V b]) -> pure [Word8V . w  $ w8 a * w8 b]

  -- quotWord8# :: Word8# -> Word8# -> Word8#
  ( "quotWord8#",    [Word8V a, Word8V b]) -> pure [Word8V . w  $ w8 a `quot` w8 b]  -- NOTE: uint8 / uint8 in C

  -- remWord8# :: Word8# -> Word8# -> Word8#
  ( "remWord8#",     [Word8V a, Word8V b]) -> pure [Word8V . w  $ w8 a `rem` w8 b]   -- NOTE: uint8 % uint8 in C

  -- quotRemWord8# :: Word8# -> Word8# -> (# Word8#, Word8# #)
  ( "quotRemWord8#", [Word8V a, Word8V b]) -> pure [Word8V . w $ w8 a `quot` w8 b, Word8V . w $ w8 a `rem` w8 b]

  -- andWord8# :: Word8# -> Word8# -> Word8#
  ( "andWord8#",     [Word8V a, Word8V b]) -> pure [Word8V . w  $ w8 a .&. w8 b]   -- NOTE: uint8 & uint8 in C

  -- orWord8# :: Word8# -> Word8# -> Word8#
  ( "orWord8#",      [Word8V a, Word8V b]) -> pure [Word8V . w  $ w8 a .|. w8 b]   -- NOTE: uint8 | uint8 in C

  -- xorWord8# :: Word8# -> Word8# -> Word8#
  ( "xorWord8#",     [Word8V a, Word8V b]) -> pure [Word8V . w  $ w8 a `xor` w8 b]   -- NOTE: uint8 ^ uint8 in C

  -- notWord8# :: Word8# -> Word8#
  ( "notWord8#",     [Word8V a])           -> pure [Word8V . w . complement $ w8 a]

  -- uncheckedShiftLWord8# :: Word8# -> Int# -> Word8#
  ( "uncheckedShiftLWord8#",  [Word8V a, IntV b]) -> pure [Word8V . w $ unsafeShiftL (w8 a) b]

  -- uncheckedShiftRLWord8# :: Word8# -> Int# -> Word8#
  ( "uncheckedShiftRLWord8#", [Word8V a, IntV b]) -> pure [Word8V . w $ unsafeShiftR (w8 a) b] -- Shift right logical

  -- word8ToInt8# :: Word8# -> Int8#
  ( "word8ToInt8#",  [Word8V a])           -> pure [Int8V $ fromIntegral a]

  -- eqWord8# :: Word8# -> Word8# -> Int#
  ( "eqWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geWord8# :: Word8# -> Word8# -> Int#
  ( "geWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtWord8# :: Word8# -> Word8# -> Int#
  ( "gtWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leWord8# :: Word8# -> Word8# -> Int#
  ( "leWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltWord8# :: Word8# -> Word8# -> Int#
  ( "ltWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neWord8# :: Word8# -> Word8# -> Int#
  ( "neWord8#",      [Word8V a, Word8V b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- OBSOLETE from GHC 9.2
  -- extendWord8# :: Word8# -> Word#
  ( "extendWord8#",  [Word8V a])           -> pure [WordV a]

  -- OBSOLETE from GHC 9.2
  -- narrowWord8# :: Word# -> Word8#
  ( "narrowWord8#",  [WordV a])            -> pure [Word8V . w . w8 $ a]

  _ -> fallback op args t tc
