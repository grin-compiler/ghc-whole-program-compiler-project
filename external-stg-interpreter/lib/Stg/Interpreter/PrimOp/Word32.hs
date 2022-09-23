{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Word32 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int32V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    w32 = fromIntegral :: Word -> Word32
    w   = fromIntegral :: Word32 -> Word
 case (op, args) of

  -- word32ToWord# :: Word32# -> Word#
  ( "word32ToWord#",  [Word32V a])            -> pure [WordV a]

  -- wordToWord32# :: Word# -> Word32#
  ( "wordToWord32#",  [WordV a])              -> pure [Word32V . w . w32 $ a]

  -- plusWord32# :: Word32# -> Word32# -> Word32#
  ( "plusWord32#",    [Word32V a, Word32V b]) -> pure [Word32V . w $ w32 a + w32 b]

  -- subWord32# :: Word32# -> Word32# -> Word32#
  ( "subWord32#",     [Word32V a, Word32V b]) -> pure [Word32V . w  $ w32 a - w32 b]

  -- timesWord32# :: Word32# -> Word32# -> Word32#
  ( "timesWord32#",   [Word32V a, Word32V b]) -> pure [Word32V . w  $ w32 a * w32 b]

  -- quotWord32# :: Word32# -> Word32# -> Word32#
  ( "quotWord32#",    [Word32V a, Word32V b]) -> pure [Word32V . w  $ w32 a `quot` w32 b]  -- NOTE: uint32 / uint32 in C

  -- remWord32# :: Word32# -> Word32# -> Word32#
  ( "remWord32#",     [Word32V a, Word32V b]) -> pure [Word32V . w  $ w32 a `rem` w32 b]   -- NOTE: uint32 % uint32 in C

  -- quotRemWord32# :: Word32# -> Word32# -> (# Word32#, Word32# #)
  ( "quotRemWord32#", [Word32V a, Word32V b]) -> pure [Word32V . w $ w32 a `quot` w32 b, Word32V . w $ w32 a `rem` w32 b]

  -- andWord32# :: Word32# -> Word32# -> Word32#
  ( "andWord32#",     [Word32V a, Word32V b]) -> pure [Word32V . w  $ w32 a .&. w32 b]   -- NOTE: uint32 & uint32 in C

  -- orWord32# :: Word32# -> Word32# -> Word32#
  ( "orWord32#",      [Word32V a, Word32V b]) -> pure [Word32V . w  $ w32 a .|. w32 b]   -- NOTE: uint32 | uint32 in C

  -- xorWord32# :: Word32# -> Word32# -> Word32#
  ( "xorWord32#",     [Word32V a, Word32V b]) -> pure [Word32V . w  $ w32 a `xor` w32 b]   -- NOTE: uint32 ^ uint32 in C

  -- notWord32# :: Word32# -> Word32#
  ( "notWord32#",     [Word32V a])            -> pure [Word32V . w . complement $ w32 a]

  -- uncheckedShiftLWord32# :: Word32# -> Int# -> Word32#
  ( "uncheckedShiftLWord32#",  [Word32V a, IntV b]) -> pure [Word32V . w $ unsafeShiftL (w32 a) b]

  -- uncheckedShiftRLWord32# ::  Word32# -> Int# -> Word32#
  ( "uncheckedShiftRLWord32#", [Word32V a, IntV b]) -> pure [Word32V . w $ unsafeShiftR (w32 a) b] -- Shift right logical

  -- word32ToInt32# :: Word32# -> Int32#
  ( "word32ToInt32#",  [Word32V a])           -> pure [Int32V $ fromIntegral a]

  -- eqWord32# :: Word32# -> Word32# -> Int#
  ( "eqWord32#",      [Word32V a, Word32V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geWord32# :: Word32# -> Word32# -> Int#
  ( "geWord32#",      [Word32V a, Word32V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtWord32# :: Word32# -> Word32# -> Int#
  ( "gtWord32#",      [Word32V a, Word32V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leWord32# :: Word32# -> Word32# -> Int#
  ( "leWord32#",      [Word32V a, Word32V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltWord32# :: Word32# -> Word32# -> Int#
  ( "ltWord32#",      [Word32V a, Word32V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neWord32# :: Word32# -> Word32# -> Int#
  ( "neWord32#",      [Word32V a, Word32V b]) -> pure [IntV $ if a /= b then 1 else 0]

  _ -> fallback op args t tc

