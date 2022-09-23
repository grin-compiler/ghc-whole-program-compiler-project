{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, Strict #-}
module Stg.Interpreter.PrimOp.Double where

import GHC.Word
import GHC.Int
import GHC.Float
import GHC.Exts
import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int64V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- >## :: Double# -> Double# -> Int#
  ( ">##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- >=## :: Double# -> Double# -> Int#
  ( ">=##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- ==## :: Double# -> Double# -> Int#
  ( "==##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a == b then 1 else 0]

  -- /=## :: Double# -> Double# -> Int#
  ( "/=##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- <## :: Double# -> Double# -> Int#
  ( "<##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- <=## :: Double# -> Double# -> Int#
  ( "<=##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- +## :: Double# -> Double# -> Double#
  ( "+##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a + b]

  -- -## :: Double# -> Double# -> Double#
  ( "-##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a - b]

  -- *## :: Double# -> Double# -> Double#
  ( "*##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a * b]

  -- /## :: Double# -> Double# -> Double#
  ( "/##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a / b]

  -- negateDouble# :: Double# -> Double#
  ( "negateDouble#", [DoubleV a]) -> pure [DoubleV (-a)]

  -- fabsDouble# :: Double# -> Double#
  ( "fabsDouble#", [DoubleV a]) -> pure [DoubleV (abs a)]

  -- double2Int# :: Double# -> Int#
  ( "double2Int#", [DoubleV a]) -> pure [IntV $ truncate a]

  -- double2Float# :: Double# -> Float#
  ( "double2Float#", [DoubleV a]) -> pure [FloatV $ realToFrac a]

  -- expDouble# :: Double# -> Double#
  ( "expDouble#", [DoubleV a]) -> pure [DoubleV $ exp a]

  -- expm1Double# :: Double# -> Double#
  ( "expm1Double#", [DoubleV a]) -> pure [DoubleV $ expm1Double a]

  -- logDouble# :: Double# -> Double#
  ( "logDouble#", [DoubleV a]) -> pure [DoubleV $ log a]

  -- log1pDouble# :: Double# -> Double#
  ( "log1pDouble#", [DoubleV a]) -> pure [DoubleV $ log1pDouble a]

  -- sqrtDouble# :: Double# -> Double#
  ( "sqrtDouble#", [DoubleV a]) -> pure [DoubleV $ sqrt a]

  -- sinDouble# :: Double# -> Double#
  ( "sinDouble#", [DoubleV a]) -> pure [DoubleV $ sin a]

  -- cosDouble# :: Double# -> Double#
  ( "cosDouble#", [DoubleV a]) -> pure [DoubleV $ cos a]

  -- tanDouble# :: Double# -> Double#
  ( "tanDouble#", [DoubleV a]) -> pure [DoubleV $ tan a]

  -- asinDouble# :: Double# -> Double#
  ( "asinDouble#", [DoubleV a]) -> pure [DoubleV $ asin a]

  -- acosDouble# :: Double# -> Double#
  ( "acosDouble#", [DoubleV a]) -> pure [DoubleV $ acos a]

  -- atanDouble# :: Double# -> Double#
  ( "atanDouble#", [DoubleV a]) -> pure [DoubleV $ atan a]

  -- sinhDouble# :: Double# -> Double#
  ( "sinhDouble#", [DoubleV a]) -> pure [DoubleV $ sinh a]

  -- coshDouble# :: Double# -> Double#
  ( "coshDouble#", [DoubleV a]) -> pure [DoubleV $ cosh a]

  -- tanhDouble# :: Double# -> Double#
  ( "tanhDouble#", [DoubleV a]) -> pure [DoubleV $ tanh a]

  -- asinhDouble# :: Double# -> Double#
  ( "asinhDouble#", [DoubleV a]) -> pure [DoubleV $ asinhDouble a]

  -- acoshDouble# :: Double# -> Double#
  ( "acoshDouble#", [DoubleV a]) -> pure [DoubleV $ acoshDouble a]

  -- atanhDouble# :: Double# -> Double#
  ( "atanhDouble#", [DoubleV a]) -> pure [DoubleV $ atanhDouble a]

  -- **## :: Double# -> Double# -> Double#
  ( "**##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a ** b]

  -- decodeDouble_2Int# :: Double# -> (# Int#, Word#, Word#, Int# #)
  ( "decodeDouble_2Int#", [DoubleV (D# x)]) -> do
    -- NOTE: map back to GHC primop
    let !(# a, b, c, d #) = decodeDouble_2Int# x
    pure [IntV (I# a), WordV (W# b), WordV (W# c), IntV (I# d)]

  -- decodeDouble_Int64# :: Double# -> (# Int64#, Int# #)
  ( "decodeDouble_Int64#", [DoubleV (D# x)]) -> do
    -- NOTE: map back to GHC primop
    let !(# a, b #) = decodeDouble_Int64# x
    pure [Int64V (I# a), IntV (I# b)]

  _ -> fallback op args t tc
