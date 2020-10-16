{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Double where

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = Literal (LitNumber LitNumInt i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- >## :: Double# -> Double# -> Int#
  (">##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- >=## :: Double# -> Double# -> Int#
  (">=##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- ==## :: Double# -> Double# -> Int#
  ("==##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a == b then 1 else 0]

  -- /=## :: Double# -> Double# -> Int#
  ("/=##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a /= b then 1 else 0]

  -- <## :: Double# -> Double# -> Int#
  ("<##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- <=## :: Double# -> Double# -> Int#
  ("<=##", [DoubleV a, DoubleV b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- +## :: Double# -> Double# -> Double#
  ("+##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a + b]

  -- -## :: Double# -> Double# -> Double#
  ("-##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a - b]

  -- *## :: Double# -> Double# -> Double#
  ("*##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a * b]

  -- /## :: Double# -> Double# -> Double#
  ("/##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a / b]

  -- negateDouble# :: Double# -> Double#
  ("negateDouble#", [DoubleV a]) -> pure [DoubleV (-a)]

  -- fabsDouble# :: Double# -> Double#
  ("fabsDouble#", [DoubleV a]) -> pure [DoubleV (abs a)]

  -- double2Int# :: Double# -> Int#
  ("double2Int#", [DoubleV a]) -> pure [IntV $ truncate a]

  -- double2Float# :: Double# -> Float#
  ("double2Float#", [DoubleV a]) -> pure [FloatV $ realToFrac a]

  -- expDouble# :: Double# -> Double#
  ("expDouble#", [DoubleV a]) -> pure [DoubleV $ exp a]

  -- TODO: expm1Double# :: Double# -> Double#

  -- logDouble# :: Double# -> Double#
  ("logDouble#", [DoubleV a]) -> pure [DoubleV $ log a]

  -- TODO: log1pDouble# :: Double# -> Double#

  -- sqrtDouble# :: Double# -> Double#
  ("sqrtDouble#", [DoubleV a]) -> pure [DoubleV $ sqrt a]

  -- sinDouble# :: Double# -> Double#
  ("sinDouble#", [DoubleV a]) -> pure [DoubleV $ sin a]

  -- cosDouble# :: Double# -> Double#
  ("cosDouble#", [DoubleV a]) -> pure [DoubleV $ cos a]

  -- tanDouble# :: Double# -> Double#
  ("tanDouble#", [DoubleV a]) -> pure [DoubleV $ tan a]

  -- asinDouble# :: Double# -> Double#
  ("asinDouble#", [DoubleV a]) -> pure [DoubleV $ asin a]

  -- acosDouble# :: Double# -> Double#
  ("acosDouble#", [DoubleV a]) -> pure [DoubleV $ acos a]

  -- atanDouble# :: Double# -> Double#
  ("atanDouble#", [DoubleV a]) -> pure [DoubleV $ atan a]

  -- sinhDouble# :: Double# -> Double#
  ("sinhDouble#", [DoubleV a]) -> pure [DoubleV $ sinh a]

  -- coshDouble# :: Double# -> Double#
  ("coshDouble#", [DoubleV a]) -> pure [DoubleV $ cosh a]

  -- tanhDouble# :: Double# -> Double#
  ("tanhDouble#", [DoubleV a]) -> pure [DoubleV $ tanh a]

  -- TODO: asinhDouble# :: Double# -> Double#
  -- TODO: acoshDouble# :: Double# -> Double#
  -- TODO: atanhDouble# :: Double# -> Double#

  -- **## :: Double# -> Double# -> Double#
  ("**##", [DoubleV a, DoubleV b]) -> pure [DoubleV $ a ** b]

  -- TODO: decodeDouble_2Int# :: Double# -> (# Int#, Word#, Word#, Int# #)
  -- TODO: decodeDouble_Int64# :: Double# -> (# INT64, Int# #)

  _ -> fallback op args t tc
