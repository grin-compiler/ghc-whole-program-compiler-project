{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Word64 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Word
import Data.Bits

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int64V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word64V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = do
 let
    w64 = fromIntegral :: Word -> Word64
    w   = fromIntegral :: Word64 -> Word
 case (op, args) of

  -- word64ToWord# :: Word64# -> Word#
  ( "word64ToWord#",  [Word64V a])            -> pure [WordV a]

  -- wordToWord64# :: Word# -> Word64#
  ( "wordToWord64#",  [WordV a])              -> pure [Word64V . w . w64 $ a]

  -- plusWord64# :: Word64# -> Word64# -> Word64#
  ( "plusWord64#",    [Word64V a, Word64V b]) -> pure [Word64V . w $ w64 a + w64 b]

  -- subWord64# :: Word64# -> Word64# -> Word64#
  ( "subWord64#",     [Word64V a, Word64V b]) -> pure [Word64V . w  $ w64 a - w64 b]

  -- timesWord64# :: Word64# -> Word64# -> Word64#
  ( "timesWord64#",   [Word64V a, Word64V b]) -> pure [Word64V . w  $ w64 a * w64 b]

  -- quotWord64# :: Word64# -> Word64# -> Word64#
  ( "quotWord64#",    [Word64V a, Word64V b]) -> pure [Word64V . w  $ w64 a `quot` w64 b]  -- NOTE: uint64 / uint64 in C

  -- remWord64# :: Word64# -> Word64# -> Word64#
  ( "remWord64#",     [Word64V a, Word64V b]) -> pure [Word64V . w  $ w64 a `rem` w64 b]   -- NOTE: uint64 % uint64 in C

  -- and64# :: Word64# -> Word64# -> Word64#
  ( "and64#",         [Word64V a, Word64V b]) -> pure [Word64V . w  $ w64 a .&. w64 b]   -- NOTE: uint64 & uint64 in C

  -- or64# :: Word64# -> Word64# -> Word64#
  ( "or64#",          [Word64V a, Word64V b]) -> pure [Word64V . w  $ w64 a .|. w64 b]   -- NOTE: uint64 | uint64 in C

  -- xor64# :: Word64# -> Word64# -> Word64#
  ( "xor64#",         [Word64V a, Word64V b]) -> pure [Word64V . w  $ w64 a `xor` w64 b]   -- NOTE: uint64 ^ uint64 in C

  -- not64# :: Word64# -> Word64#
  ( "not64#",         [Word64V a])            -> pure [Word64V . w . complement $ w64 a]

  -- uncheckedShiftL64#  :: Word64# -> Int# -> Word64#
  ( "uncheckedShiftL64#",   [Word64V a, IntV b]) -> pure [Word64V . w $ unsafeShiftL (w64 a) b]

  -- uncheckedShiftRL64# :: Word64# -> Int# -> Word64#
  ( "uncheckedShiftRL64#",  [Word64V a, IntV b]) -> pure [Word64V . w $ unsafeShiftR (w64 a) b] -- Shift right logical

  -- word64ToInt64# :: Word64# -> Int64#
  ( "word64ToInt64#",  [Word64V a])           -> pure [Int64V $ fromIntegral a]

  -- eqWord64# :: Word64# -> Word64# -> Int#
  ( "eqWord64#",      [Word64V a, Word64V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geWord64# :: Word64# -> Word64# -> Int#
  ( "geWord64#",      [Word64V a, Word64V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtWord64# :: Word64# -> Word64# -> Int#
  ( "gtWord64#",      [Word64V a, Word64V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leWord64# :: Word64# -> Word64# -> Int#
  ( "leWord64#",      [Word64V a, Word64V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltWord64# :: Word64# -> Word64# -> Int#
  ( "ltWord64#",      [Word64V a, Word64V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neWord64# :: Word64# -> Word64# -> Int#
  ( "neWord64#",      [Word64V a, Word64V b]) -> pure [IntV $ if a /= b then 1 else 0]

  _ -> fallback op args t tc
