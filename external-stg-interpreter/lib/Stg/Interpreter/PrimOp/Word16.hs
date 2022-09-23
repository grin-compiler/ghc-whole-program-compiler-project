{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Word16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word16V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    w16 = fromIntegral :: Word -> Word16
    w   = fromIntegral :: Word16 -> Word
 case (op, args) of

  -- word16ToWord# :: Word16# -> Word#
  ( "word16ToWord#",  [Word16V a])            -> pure [WordV a]

  -- wordToWord16# :: Word# -> Word16#
  ( "wordToWord16#",  [WordV a])              -> pure [Word16V . w . w16 $ a]

  -- plusWord16# :: Word16# -> Word16# -> Word16#
  ( "plusWord16#",    [Word16V a, Word16V b]) -> pure [Word16V . w $ w16 a + w16 b]

  -- subWord16# :: Word16# -> Word16# -> Word16#
  ( "subWord16#",     [Word16V a, Word16V b]) -> pure [Word16V . w  $ w16 a - w16 b]

  -- timesWord16# :: Word16# -> Word16# -> Word16#
  ( "timesWord16#",   [Word16V a, Word16V b]) -> pure [Word16V . w  $ w16 a * w16 b]

  -- quotWord16# :: Word16# -> Word16# -> Word16#
  ( "quotWord16#",    [Word16V a, Word16V b]) -> pure [Word16V . w  $ w16 a `quot` w16 b]  -- NOTE: uint16 / uint16 in C

  -- remWord16# :: Word16# -> Word16# -> Word16#
  ( "remWord16#",     [Word16V a, Word16V b]) -> pure [Word16V . w  $ w16 a `rem` w16 b]   -- NOTE: uint16 % uint16 in C

  -- quotRemWord16# :: Word16# -> Word16# -> (# Word16#, Word16# #)
  ( "quotRemWord16#", [Word16V a, Word16V b]) -> pure [Word16V . w $ w16 a `quot` w16 b, Word16V . w $ w16 a `rem` w16 b]

  -- andWord16# :: Word16# -> Word16# -> Word16#
  ( "andWord16#",     [Word16V a, Word16V b]) -> pure [Word16V . w  $ w16 a .&. w16 b]   -- NOTE: uint16 & uint16 in C

  -- orWord16# :: Word16# -> Word16# -> Word16#
  ( "orWord16#",      [Word16V a, Word16V b]) -> pure [Word16V . w  $ w16 a .|. w16 b]   -- NOTE: uint16 | uint16 in C

  -- xorWord16# :: Word16# -> Word16# -> Word16#
  ( "xorWord16#",     [Word16V a, Word16V b]) -> pure [Word16V . w  $ w16 a `xor` w16 b]   -- NOTE: uint16 ^ uint16 in C

  -- notWord16# :: Word16# -> Word16#
  ( "notWord16#",     [Word16V a])            -> pure [Word16V . w . complement $ w16 a]

  -- uncheckedShiftLWord16# :: Word16# -> Int# -> Word16#
  ( "uncheckedShiftLWord16#",  [Word16V a, IntV b]) -> pure [Word16V . w $ unsafeShiftL (w16 a) b]

  -- uncheckedShiftRLWord16# ::  Word16# -> Int# -> Word16#
  ( "uncheckedShiftRLWord16#", [Word16V a, IntV b]) -> pure [Word16V . w $ unsafeShiftR (w16 a) b] -- Shift right logical

  -- word16ToInt16# :: Word16# -> Int16#
  ( "word16ToInt16#",  [Word16V a])           -> pure [Int16V $ fromIntegral a]

  -- eqWord16# :: Word16# -> Word16# -> Int#
  ( "eqWord16#",      [Word16V a, Word16V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geWord16# :: Word16# -> Word16# -> Int#
  ( "geWord16#",      [Word16V a, Word16V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtWord16# :: Word16# -> Word16# -> Int#
  ( "gtWord16#",      [Word16V a, Word16V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leWord16# :: Word16# -> Word16# -> Int#
  ( "leWord16#",      [Word16V a, Word16V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltWord16# :: Word16# -> Word16# -> Int#
  ( "ltWord16#",      [Word16V a, Word16V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neWord16# :: Word16# -> Word16# -> Int#
  ( "neWord16#",      [Word16V a, Word16V b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- OBSOLETE from GHC 9.2
  -- extendWord16# :: Word16# -> Word#
  ( "extendWord16#",  [Word16V a])            -> pure [WordV a]

  -- OBSOLETE from GHC 9.2
  -- narrowWord16# :: Word# -> Word16#
  ( "narrowWord16#",  [WordV a])              -> pure [Word16V . w . w16 $ a]

  _ -> fallback op args t tc
