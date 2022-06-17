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

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args) of

  -- >## :: Double# -> Double# -> Int#
  ( ">##", [DoubleV a, DoubleV b]) -> allocAtoms [IntV $ if a > b  then 1 else 0]

  -- >=## :: Double# -> Double# -> Int#
  ( ">=##", [DoubleV a, DoubleV b]) -> allocAtoms [IntV $ if a >= b then 1 else 0]

  -- ==## :: Double# -> Double# -> Int#
  ( "==##", [DoubleV a, DoubleV b]) -> allocAtoms [IntV $ if a == b then 1 else 0]

  -- /=## :: Double# -> Double# -> Int#
  ( "/=##", [DoubleV a, DoubleV b]) -> allocAtoms [IntV $ if a /= b then 1 else 0]

  -- <## :: Double# -> Double# -> Int#
  ( "<##", [DoubleV a, DoubleV b]) -> allocAtoms [IntV $ if a < b  then 1 else 0]

  -- <=## :: Double# -> Double# -> Int#
  ( "<=##", [DoubleV a, DoubleV b]) -> allocAtoms [IntV $ if a <= b then 1 else 0]

  -- +## :: Double# -> Double# -> Double#
  ( "+##", [DoubleV a, DoubleV b]) -> allocAtoms [DoubleV $ a + b]

  -- -## :: Double# -> Double# -> Double#
  ( "-##", [DoubleV a, DoubleV b]) -> allocAtoms [DoubleV $ a - b]

  -- *## :: Double# -> Double# -> Double#
  ( "*##", [DoubleV a, DoubleV b]) -> allocAtoms [DoubleV $ a * b]

  -- /## :: Double# -> Double# -> Double#
  ( "/##", [DoubleV a, DoubleV b]) -> allocAtoms [DoubleV $ a / b]

  -- negateDouble# :: Double# -> Double#
  ( "negateDouble#", [DoubleV a]) -> allocAtoms [DoubleV (-a)]

  -- fabsDouble# :: Double# -> Double#
  ( "fabsDouble#", [DoubleV a]) -> allocAtoms [DoubleV (abs a)]

  -- double2Int# :: Double# -> Int#
  ( "double2Int#", [DoubleV a]) -> allocAtoms [IntV $ truncate a]

  -- double2Float# :: Double# -> Float#
  ( "double2Float#", [DoubleV a]) -> allocAtoms [FloatV $ realToFrac a]

  -- expDouble# :: Double# -> Double#
  ( "expDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ exp a]

  -- expm1Double# :: Double# -> Double#
  ( "expm1Double#", [DoubleV a]) -> allocAtoms [DoubleV $ expm1Double a]

  -- logDouble# :: Double# -> Double#
  ( "logDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ log a]

  -- log1pDouble# :: Double# -> Double#
  ( "log1pDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ log1pDouble a]

  -- sqrtDouble# :: Double# -> Double#
  ( "sqrtDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ sqrt a]

  -- sinDouble# :: Double# -> Double#
  ( "sinDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ sin a]

  -- cosDouble# :: Double# -> Double#
  ( "cosDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ cos a]

  -- tanDouble# :: Double# -> Double#
  ( "tanDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ tan a]

  -- asinDouble# :: Double# -> Double#
  ( "asinDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ asin a]

  -- acosDouble# :: Double# -> Double#
  ( "acosDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ acos a]

  -- atanDouble# :: Double# -> Double#
  ( "atanDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ atan a]

  -- sinhDouble# :: Double# -> Double#
  ( "sinhDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ sinh a]

  -- coshDouble# :: Double# -> Double#
  ( "coshDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ cosh a]

  -- tanhDouble# :: Double# -> Double#
  ( "tanhDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ tanh a]

  -- asinhDouble# :: Double# -> Double#
  ( "asinhDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ asinhDouble a]

  -- acoshDouble# :: Double# -> Double#
  ( "acoshDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ acoshDouble a]

  -- atanhDouble# :: Double# -> Double#
  ( "atanhDouble#", [DoubleV a]) -> allocAtoms [DoubleV $ atanhDouble a]

  -- **## :: Double# -> Double# -> Double#
  ( "**##", [DoubleV a, DoubleV b]) -> allocAtoms [DoubleV $ a ** b]

  -- decodeDouble_2Int# :: Double# -> (# Int#, Word#, Word#, Int# #)
  ( "decodeDouble_2Int#", [DoubleV (D# x)]) -> do
    -- NOTE: map back to GHC primop
    let !(# a, b, c, d #) = decodeDouble_2Int# x
    allocAtoms [IntV (I# a), WordV (W# b), WordV (W# c), IntV (I# d)]

  -- decodeDouble_Int64# :: Double# -> (# Int#, Int# #)
  ( "decodeDouble_Int64#", [DoubleV (D# x)]) -> do
    -- NOTE: map back to GHC primop
    let !(# a, b #) = decodeDouble_Int64# x
    allocAtoms [IntV (I# a), IntV (I# b)]

  _ -> fallback op argsAddr t tc
