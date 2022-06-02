{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
{-# LANGUAGE MagicHash, UnboxedTuples, BangPatterns #-}
module Stg.Interpreter.PrimOp.Float where

import GHC.Exts
import GHC.Float
import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

evalPrimOp :: PrimOpEval -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> M [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args) of

  -- gtFloat# :: Float# -> Float# -> Int#
  ( "gtFloat#",      [FloatV a, FloatV b]) -> allocAtoms [IntV $ if a > b  then 1 else 0]

  -- geFloat# :: Float# -> Float# -> Int#
  ( "geFloat#",      [FloatV a, FloatV b]) -> allocAtoms [IntV $ if a >= b then 1 else 0]

  -- eqFloat# :: Float# -> Float# -> Int#
  ( "eqFloat#",      [FloatV a, FloatV b]) -> allocAtoms [IntV $ if a == b then 1 else 0]

  -- neFloat# :: Float# -> Float# -> Int#
  ( "neFloat#",      [FloatV a, FloatV b]) -> allocAtoms [IntV $ if a /= b then 1 else 0]

  -- ltFloat# :: Float# -> Float# -> Int#
  ( "ltFloat#",      [FloatV a, FloatV b]) -> allocAtoms [IntV $ if a < b  then 1 else 0]

  -- leFloat# :: Float# -> Float# -> Int#
  ( "leFloat#",      [FloatV a, FloatV b]) -> allocAtoms [IntV $ if a <= b then 1 else 0]

  -- plusFloat# :: Float# -> Float# -> Float#
  ( "plusFloat#",    [FloatV a, FloatV b]) -> allocAtoms [FloatV $ a + b]

  -- minusFloat# :: Float# -> Float# -> Float#
  ( "minusFloat#",   [FloatV a, FloatV b]) -> allocAtoms [FloatV $ a - b]

  -- timesFloat# :: Float# -> Float# -> Float#
  ( "timesFloat#",   [FloatV a, FloatV b]) -> allocAtoms [FloatV $ a * b]

  -- divideFloat# :: Float# -> Float# -> Float#
  ( "divideFloat#",  [FloatV a, FloatV b]) -> allocAtoms [FloatV $ a / b]

  -- negateFloat# :: Float# -> Float#
  ( "negateFloat#",  [FloatV a]) -> allocAtoms [FloatV (-a)]

  -- fabsFloat# :: Float# -> Float#
  ( "fabsFloat#",    [FloatV a]) -> allocAtoms [FloatV (abs a)]

  -- float2Int# :: Float# -> Int#
  ( "float2Int#",    [FloatV a]) -> allocAtoms [IntV $ truncate a]

  -- expFloat# :: Float# -> Float#
  ( "expFloat#",     [FloatV a]) -> allocAtoms [FloatV $ exp a]

  -- expm1Float# :: Float# -> Float#
  ( "expm1Float#",   [FloatV a]) -> allocAtoms [FloatV $ expm1Float a]

  -- logFloat# :: Float# -> Float#
  ( "logFloat#",     [FloatV a]) -> allocAtoms [FloatV $ log a]

  -- log1pFloat# :: Float# -> Float#
  ( "log1pFloat#",   [FloatV a]) -> allocAtoms [FloatV $ log1pFloat a]

  -- sqrtFloat# :: Float# -> Float#
  ( "sqrtFloat#",    [FloatV a]) -> allocAtoms [FloatV $ sqrt a]

  -- sinFloat# :: Float# -> Float#
  ( "sinFloat#",     [FloatV a]) -> allocAtoms [FloatV $ sin a]

  -- cosFloat# :: Float# -> Float#
  ( "cosFloat#",     [FloatV a]) -> allocAtoms [FloatV $ cos a]

  -- tanFloat# :: Float# -> Float#
  ( "tanFloat#",     [FloatV a]) -> allocAtoms [FloatV $ tan a]

  -- asinFloat# :: Float# -> Float#
  ( "asinFloat#",    [FloatV a]) -> allocAtoms [FloatV $ asin a]

  -- acosFloat# :: Float# -> Float#
  ( "acosFloat#",    [FloatV a]) -> allocAtoms [FloatV $ acos a]

  -- atanFloat# :: Float# -> Float#
  ( "atanFloat#",    [FloatV a]) -> allocAtoms [FloatV $ atan a]

  -- sinhFloat# :: Float# -> Float#
  ( "sinhFloat#",    [FloatV a]) -> allocAtoms [FloatV $ sinh a]

  -- coshFloat# :: Float# -> Float#
  ( "coshFloat#",    [FloatV a]) -> allocAtoms [FloatV $ cosh a]

  -- tanhFloat# :: Float# -> Float#
  ( "tanhFloat#",    [FloatV a]) -> allocAtoms [FloatV $ tanh a]

  -- asinhFloat# :: Float# -> Float#
  ( "asinhFloat#",   [FloatV a]) -> allocAtoms [FloatV $ asinhFloat a]

  -- acoshFloat# :: Float# -> Float#
  ( "acoshFloat#",   [FloatV a]) -> allocAtoms [FloatV $ acoshFloat a]

  -- atanhFloat# :: Float# -> Float#
  ( "atanhFloat#",   [FloatV a]) -> allocAtoms [FloatV $ atanhFloat a]

  -- powerFloat# :: Float# -> Float# -> Float#
  ( "powerFloat#",   [FloatV a, FloatV b]) -> allocAtoms [FloatV $ powerFloat a b]

  -- float2Double# ::  Float# -> Double#
  ( "float2Double#", [FloatV a]) -> allocAtoms [DoubleV $ realToFrac a]

  -- decodeFloat_Int# :: Float# -> (# Int#, Int# #)
  ( "decodeFloat_Int#", [FloatV (F# a)]) -> do
    let !(# mantissa, exponent #) = decodeFloat_Int# a
    allocAtoms [IntV (I# mantissa), IntV (I# exponent)]

  _ -> fallback op argsAddr t tc
