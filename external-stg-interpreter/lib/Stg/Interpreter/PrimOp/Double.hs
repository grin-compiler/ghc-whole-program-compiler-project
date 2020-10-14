{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Double where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Double#"
        {Operations on double-precision (64 bit) floating-point numbers.}
------------------------------------------------------------------------

primtype Double#

primop   DoubleGtOp ">##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop   DoubleGeOp ">=##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop DoubleEqOp "==##"   Compare
   Double# -> Double# -> Int#
   with commutable = True
        fixity = infix 4

primop DoubleNeOp "/=##"   Compare
   Double# -> Double# -> Int#
   with commutable = True
        fixity = infix 4

primop   DoubleLtOp "<##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop   DoubleLeOp "<=##"   Compare   Double# -> Double# -> Int#
   with fixity = infix 4

primop   DoubleAddOp   "+##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True
        fixity = infixl 6

primop   DoubleSubOp   "-##"   Dyadic   Double# -> Double# -> Double#
   with fixity = infixl 6

primop   DoubleMulOp   "*##"   Dyadic
   Double# -> Double# -> Double#
   with commutable = True
        fixity = infixl 7

primop   DoubleDivOp   "/##"   Dyadic
   Double# -> Double# -> Double#
   with can_fail = True
        fixity = infixl 7

primop   DoubleNegOp   "negateDouble#"  Monadic   Double# -> Double#

primop   DoubleFabsOp  "fabsDouble#"    Monadic   Double# -> Double#

primop   Double2IntOp   "double2Int#"          GenPrimOp  Double# -> Int#
   {Truncates a {\tt Double#} value to the nearest {\tt Int#}.
    Results are undefined if the truncation if truncation yields
    a value outside the range of {\tt Int#}.}

primop   Double2FloatOp   "double2Float#" GenPrimOp Double# -> Float#

primop   DoubleExpOp   "expDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleExpM1Op "expm1Double#"    Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleLogOp   "logDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleLog1POp   "log1pDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleSqrtOp   "sqrtDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleSinOp   "sinDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleCosOp   "cosDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleTanOp   "tanDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAsinOp   "asinDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleAcosOp   "acosDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   DoubleAtanOp   "atanDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleSinhOp   "sinhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleCoshOp   "coshDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleTanhOp   "tanhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAsinhOp   "asinhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAcoshOp   "acoshDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleAtanhOp   "atanhDouble#"      Monadic
   Double# -> Double#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoublePowerOp   "**##" Dyadic
   Double# -> Double# -> Double#
   {Exponentiation.}
   with
   code_size = { primOpCodeSizeForeignCall }

primop   DoubleDecode_2IntOp   "decodeDouble_2Int#" GenPrimOp
   Double# -> (# Int#, Word#, Word#, Int# #)
   {Convert to integer.
    First component of the result is -1 or 1, indicating the sign of the
    mantissa. The next two are the high and low 32 bits of the mantissa
    respectively, and the last is the exponent.}
   with out_of_line = True

primop   DoubleDecode_Int64Op   "decodeDouble_Int64#" GenPrimOp
   Double# -> (# INT64, Int# #)
   {Decode {\tt Double\#} into mantissa and base-2 exponent.}
   with out_of_line = True
-}
{-
evalPrimOp DoubleGtOp     [DoubleV a, DoubleV b] = IntV $ if a > b  then 1 else 0
evalPrimOp DoubleGeOp     [DoubleV a, DoubleV b] = IntV $ if a >= b then 1 else 0
evalPrimOp DoubleEqOp     [DoubleV a, DoubleV b] = IntV $ if a == b then 1 else 0
evalPrimOp DoubleNeOp     [DoubleV a, DoubleV b] = IntV $ if a /= b then 1 else 0
evalPrimOp DoubleLtOp     [DoubleV a, DoubleV b] = IntV $ if a < b  then 1 else 0
evalPrimOp DoubleLeOp     [DoubleV a, DoubleV b] = IntV $ if a <= b then 1 else 0
evalPrimOp DoubleAddOp    [DoubleV a, DoubleV b] = DoubleV $ a + b
evalPrimOp DoubleSubOp    [DoubleV a, DoubleV b] = DoubleV $ a - b
evalPrimOp DoubleMulOp    [DoubleV a, DoubleV b] = DoubleV $ a * b
evalPrimOp DoubleDivOp    [DoubleV a, DoubleV b] = DoubleV $ a / b
evalPrimOp DoubleNegOp    [DoubleV a] = DoubleV (-a)
evalPrimOp DoubleFabsOp   [DoubleV a] = DoubleV (abs a)
evalPrimOp Double2IntOp   [DoubleV a] = IntV $ truncate a
evalPrimOp Double2FloatOp [DoubleV a] = FloatV $ realToFrac a
evalPrimOp DoubleExpOp    [DoubleV a] = DoubleV $ exp a
evalPrimOp DoubleLogOp    [DoubleV a] = DoubleV $ log a
evalPrimOp DoubleSqrtOp   [DoubleV a] = DoubleV $ sqrt a
evalPrimOp DoubleSinOp    [DoubleV a] = DoubleV $ sin a
evalPrimOp DoubleCosOp    [DoubleV a] = DoubleV $ cos a
evalPrimOp DoubleTanOp    [DoubleV a] = DoubleV $ tan a
evalPrimOp DoubleAsinOp   [DoubleV a] = DoubleV $ asin a
evalPrimOp DoubleAcosOp   [DoubleV a] = DoubleV $ acos a
evalPrimOp DoubleAtanOp   [DoubleV a] = DoubleV $ atan a
evalPrimOp DoubleSinhOp   [DoubleV a] = DoubleV $ sinh a
evalPrimOp DoubleCoshOp   [DoubleV a] = DoubleV $ cosh a
evalPrimOp DoubleTanhOp   [DoubleV a] = DoubleV $ tanh a
evalPrimOp DoublePowerOp  [DoubleV a, DoubleV b] = DoubleV $ a ** b
-}
{-
  DoubleDecode_2IntOp
  DoubleDecode_Int64Op
-}
