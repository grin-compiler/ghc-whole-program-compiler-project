{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, Strict #-}
module Stg.Interpreter.PrimOp.Double where

import GHC.Word
import GHC.Int
import GHC.Float
import GHC.Exts
import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

      -- >## :: Double# -> Double# -> Int#
  defOp ">##" $ \[DoubleV a, DoubleV b] -> pure [IntV $ if a > b  then 1 else 0]

      -- >=## :: Double# -> Double# -> Int#
  defOp ">=##" $ \[DoubleV a, DoubleV b] -> pure [IntV $ if a >= b then 1 else 0]

      -- ==## :: Double# -> Double# -> Int#
  defOp "==##" $ \[DoubleV a, DoubleV b] -> pure [IntV $ if a == b then 1 else 0]

      -- /=## :: Double# -> Double# -> Int#
  defOp "/=##" $ \[DoubleV a, DoubleV b] -> pure [IntV $ if a /= b then 1 else 0]

      -- <## :: Double# -> Double# -> Int#
  defOp "<##" $ \[DoubleV a, DoubleV b] -> pure [IntV $ if a < b  then 1 else 0]

      -- <=## :: Double# -> Double# -> Int#
  defOp "<=##" $ \[DoubleV a, DoubleV b] -> pure [IntV $ if a <= b then 1 else 0]

      -- +## :: Double# -> Double# -> Double#
  defOp "+##" $ \[DoubleV a, DoubleV b] -> pure [DoubleV $ a + b]

      -- -## :: Double# -> Double# -> Double#
  defOp "-##" $ \[DoubleV a, DoubleV b] -> pure [DoubleV $ a - b]

      -- *## :: Double# -> Double# -> Double#
  defOp "*##" $ \[DoubleV a, DoubleV b] -> pure [DoubleV $ a * b]

      -- /## :: Double# -> Double# -> Double#
  defOp "/##" $ \[DoubleV a, DoubleV b] -> pure [DoubleV $ a / b]

      -- negateDouble# :: Double# -> Double#
  defOp "negateDouble#" $ \[DoubleV a] -> pure [DoubleV (-a)]

      -- fabsDouble# :: Double# -> Double#
  defOp "fabsDouble#" $ \[DoubleV a] -> pure [DoubleV (abs a)]

      -- double2Int# :: Double# -> Int#
  defOp "double2Int#" $ \[DoubleV a] -> pure [IntV $ truncate a]

      -- double2Float# :: Double# -> Float#
  defOp "double2Float#" $ \[DoubleV a] -> pure [FloatV $ realToFrac a]

      -- expDouble# :: Double# -> Double#
  defOp "expDouble#" $ \[DoubleV a] -> pure [DoubleV $ exp a]

      -- expm1Double# :: Double# -> Double#
  defOp "expm1Double#" $ \[DoubleV a] -> pure [DoubleV $ expm1Double a]

      -- logDouble# :: Double# -> Double#
  defOp "logDouble#" $ \[DoubleV a] -> pure [DoubleV $ log a]

      -- log1pDouble# :: Double# -> Double#
  defOp "log1pDouble#" $ \[DoubleV a] -> pure [DoubleV $ log1pDouble a]

      -- sqrtDouble# :: Double# -> Double#
  defOp "sqrtDouble#" $ \[DoubleV a] -> pure [DoubleV $ sqrt a]

      -- sinDouble# :: Double# -> Double#
  defOp "sinDouble#" $ \[DoubleV a] -> pure [DoubleV $ sin a]

      -- cosDouble# :: Double# -> Double#
  defOp "cosDouble#" $ \[DoubleV a] -> pure [DoubleV $ cos a]

      -- tanDouble# :: Double# -> Double#
  defOp "tanDouble#" $ \[DoubleV a] -> pure [DoubleV $ tan a]

      -- asinDouble# :: Double# -> Double#
  defOp "asinDouble#" $ \[DoubleV a] -> pure [DoubleV $ asin a]

      -- acosDouble# :: Double# -> Double#
  defOp "acosDouble#" $ \[DoubleV a] -> pure [DoubleV $ acos a]

      -- atanDouble# :: Double# -> Double#
  defOp "atanDouble#" $ \[DoubleV a] -> pure [DoubleV $ atan a]

      -- sinhDouble# :: Double# -> Double#
  defOp "sinhDouble#" $ \[DoubleV a] -> pure [DoubleV $ sinh a]

      -- coshDouble# :: Double# -> Double#
  defOp "coshDouble#" $ \[DoubleV a] -> pure [DoubleV $ cosh a]

      -- tanhDouble# :: Double# -> Double#
  defOp "tanhDouble#" $ \[DoubleV a] -> pure [DoubleV $ tanh a]

      -- asinhDouble# :: Double# -> Double#
  defOp "asinhDouble#" $ \[DoubleV a] -> pure [DoubleV $ asinhDouble a]

      -- acoshDouble# :: Double# -> Double#
  defOp "acoshDouble#" $ \[DoubleV a] -> pure [DoubleV $ acoshDouble a]

      -- atanhDouble# :: Double# -> Double#
  defOp "atanhDouble#" $ \[DoubleV a] -> pure [DoubleV $ atanhDouble a]

      -- **## :: Double# -> Double# -> Double#
  defOp "**##" $ \[DoubleV a, DoubleV b] -> pure [DoubleV $ a ** b]

      -- decodeDouble_2Int# :: Double# -> (# Int#, Word#, Word#, Int# #)
  defOp "decodeDouble_2Int#" $ \[DoubleV (D# x)] -> do
    -- NOTE: map back to GHC primop
    let !(# a, b, c, d #) = decodeDouble_2Int# x
    pure [IntV (I# a), WordV (W# b), WordV (W# c), IntV (I# d)]

      -- decodeDouble_Int64# :: Double# -> (# Int#, Int# #)
  defOp "decodeDouble_Int64#" $ \[DoubleV (D# x)] -> do
    -- NOTE: map back to GHC primop
    let !(# a, b #) = decodeDouble_Int64# x
    pure [IntV (I# a), IntV (I# b)]
