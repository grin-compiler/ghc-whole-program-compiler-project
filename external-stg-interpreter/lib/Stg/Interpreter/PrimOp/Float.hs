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

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

      -- gtFloat# :: Float# -> Float# -> Int#
  defOp "gtFloat#"      $ \[FloatV a, FloatV b] -> pure [IntV $ if a > b  then 1 else 0]

      -- geFloat# :: Float# -> Float# -> Int#
  defOp "geFloat#"      $ \[FloatV a, FloatV b] -> pure [IntV $ if a >= b then 1 else 0]

      -- eqFloat# :: Float# -> Float# -> Int#
  defOp "eqFloat#"      $ \[FloatV a, FloatV b] -> pure [IntV $ if a == b then 1 else 0]

      -- neFloat# :: Float# -> Float# -> Int#
  defOp "neFloat#"      $ \[FloatV a, FloatV b] -> pure [IntV $ if a /= b then 1 else 0]

      -- ltFloat# :: Float# -> Float# -> Int#
  defOp "ltFloat#"      $ \[FloatV a, FloatV b] -> pure [IntV $ if a < b  then 1 else 0]

      -- leFloat# :: Float# -> Float# -> Int#
  defOp "leFloat#"      $ \[FloatV a, FloatV b] -> pure [IntV $ if a <= b then 1 else 0]

      -- plusFloat# :: Float# -> Float# -> Float#
  defOp "plusFloat#"    $ \[FloatV a, FloatV b] -> pure [FloatV $ a + b]

      -- minusFloat# :: Float# -> Float# -> Float#
  defOp "minusFloat#"   $ \[FloatV a, FloatV b] -> pure [FloatV $ a - b]

      -- timesFloat# :: Float# -> Float# -> Float#
  defOp "timesFloat#"   $ \[FloatV a, FloatV b] -> pure [FloatV $ a * b]

      -- divideFloat# :: Float# -> Float# -> Float#
  defOp "divideFloat#"  $ \[FloatV a, FloatV b] -> pure [FloatV $ a / b]

      -- negateFloat# :: Float# -> Float#
  defOp "negateFloat#"  $ \[FloatV a] -> pure [FloatV (-a)]

      -- fabsFloat# :: Float# -> Float#
  defOp "fabsFloat#"    $ \[FloatV a] -> pure [FloatV (abs a)]

      -- float2Int# :: Float# -> Int#
  defOp "float2Int#"    $ \[FloatV a] -> pure [IntV $ truncate a]

      -- expFloat# :: Float# -> Float#
  defOp "expFloat#"     $ \[FloatV a] -> pure [FloatV $ exp a]

      -- expm1Float# :: Float# -> Float#
  defOp "expm1Float#"   $ \[FloatV a] -> pure [FloatV $ expm1Float a]

      -- logFloat# :: Float# -> Float#
  defOp "logFloat#"     $ \[FloatV a] -> pure [FloatV $ log a]

      -- log1pFloat# :: Float# -> Float#
  defOp "log1pFloat#"   $ \[FloatV a] -> pure [FloatV $ log1pFloat a]

      -- sqrtFloat# :: Float# -> Float#
  defOp "sqrtFloat#"    $ \[FloatV a] -> pure [FloatV $ sqrt a]

      -- sinFloat# :: Float# -> Float#
  defOp "sinFloat#"     $ \[FloatV a] -> pure [FloatV $ sin a]

      -- cosFloat# :: Float# -> Float#
  defOp "cosFloat#"     $ \[FloatV a] -> pure [FloatV $ cos a]

      -- tanFloat# :: Float# -> Float#
  defOp "tanFloat#"     $ \[FloatV a] -> pure [FloatV $ tan a]

      -- asinFloat# :: Float# -> Float#
  defOp "asinFloat#"    $ \[FloatV a] -> pure [FloatV $ asin a]

      -- acosFloat# :: Float# -> Float#
  defOp "acosFloat#"    $ \[FloatV a] -> pure [FloatV $ acos a]

      -- atanFloat# :: Float# -> Float#
  defOp "atanFloat#"    $ \[FloatV a] -> pure [FloatV $ atan a]

      -- sinhFloat# :: Float# -> Float#
  defOp "sinhFloat#"    $ \[FloatV a] -> pure [FloatV $ sinh a]

      -- coshFloat# :: Float# -> Float#
  defOp "coshFloat#"    $ \[FloatV a] -> pure [FloatV $ cosh a]

      -- tanhFloat# :: Float# -> Float#
  defOp "tanhFloat#"    $ \[FloatV a] -> pure [FloatV $ tanh a]

      -- asinhFloat# :: Float# -> Float#
  defOp "asinhFloat#"   $ \[FloatV a] -> pure [FloatV $ asinhFloat a]

      -- acoshFloat# :: Float# -> Float#
  defOp "acoshFloat#"   $ \[FloatV a] -> pure [FloatV $ acoshFloat a]

      -- atanhFloat# :: Float# -> Float#
  defOp "atanhFloat#"   $ \[FloatV a] -> pure [FloatV $ atanhFloat a]

      -- powerFloat# :: Float# -> Float# -> Float#
  defOp "powerFloat#"   $ \[FloatV a, FloatV b] -> pure [FloatV $ powerFloat a b]

      -- float2Double# ::  Float# -> Double#
  defOp "float2Double#" $ \[FloatV a] -> pure [DoubleV $ realToFrac a]

      -- decodeFloat_Int# :: Float# -> (# Int#, Int# #)
  defOp "decodeFloat_Int#" $ \[FloatV (F# a)] -> do
    let !(# mantissa, exponent #) = decodeFloat_Int# a
    pure [IntV (I# mantissa), IntV (I# exponent)]
