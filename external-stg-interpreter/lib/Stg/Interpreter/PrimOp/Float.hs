{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Float where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Float#"
        {Operations on single-precision (32-bit) floating-point numbers.}
------------------------------------------------------------------------

primtype Float#

primop   FloatGtOp  "gtFloat#"   Compare   Float# -> Float# -> Int#
primop   FloatGeOp  "geFloat#"   Compare   Float# -> Float# -> Int#

primop   FloatEqOp  "eqFloat#"   Compare
   Float# -> Float# -> Int#
   with commutable = True

primop   FloatNeOp  "neFloat#"   Compare
   Float# -> Float# -> Int#
   with commutable = True

primop   FloatLtOp  "ltFloat#"   Compare   Float# -> Float# -> Int#
primop   FloatLeOp  "leFloat#"   Compare   Float# -> Float# -> Int#

primop   FloatAddOp   "plusFloat#"      Dyadic
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatSubOp   "minusFloat#"      Dyadic      Float# -> Float# -> Float#

primop   FloatMulOp   "timesFloat#"      Dyadic
   Float# -> Float# -> Float#
   with commutable = True

primop   FloatDivOp   "divideFloat#"      Dyadic
   Float# -> Float# -> Float#
   with can_fail = True

primop   FloatNegOp   "negateFloat#"      Monadic    Float# -> Float#

primop   FloatFabsOp  "fabsFloat#"        Monadic    Float# -> Float#

primop   Float2IntOp   "float2Int#"      GenPrimOp  Float# -> Int#
   {Truncates a {\tt Float#} value to the nearest {\tt Int#}.
    Results are undefined if the truncation if truncation yields
    a value outside the range of {\tt Int#}.}

primop   FloatExpOp   "expFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatExpM1Op   "expm1Float#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatLogOp   "logFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatLog1POp  "log1pFloat#"     Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatSqrtOp   "sqrtFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatSinOp   "sinFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatCosOp   "cosFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatTanOp   "tanFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAsinOp   "asinFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatAcosOp   "acosFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }
   can_fail = True

primop   FloatAtanOp   "atanFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatSinhOp   "sinhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatCoshOp   "coshFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatTanhOp   "tanhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAsinhOp   "asinhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAcoshOp   "acoshFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatAtanhOp   "atanhFloat#"      Monadic
   Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   FloatPowerOp   "powerFloat#"      Dyadic
   Float# -> Float# -> Float#
   with
   code_size = { primOpCodeSizeForeignCall }

primop   Float2DoubleOp   "float2Double#" GenPrimOp  Float# -> Double#

primop   FloatDecode_IntOp   "decodeFloat_Int#" GenPrimOp
   Float# -> (# Int#, Int# #)
   {Convert to integers.
    First {\tt Int\#} in result is the mantissa; second is the exponent.}
   with out_of_line = True
-}
{-
-- Float#
evalPrimOp FloatGtOp      [FloatV a, FloatV b] = IntV $ if a > b  then 1 else 0
evalPrimOp FloatGeOp      [FloatV a, FloatV b] = IntV $ if a >= b then 1 else 0
evalPrimOp FloatEqOp      [FloatV a, FloatV b] = IntV $ if a == b then 1 else 0
evalPrimOp FloatNeOp      [FloatV a, FloatV b] = IntV $ if a /= b then 1 else 0
evalPrimOp FloatLtOp      [FloatV a, FloatV b] = IntV $ if a < b  then 1 else 0
evalPrimOp FloatLeOp      [FloatV a, FloatV b] = IntV $ if a <= b then 1 else 0
evalPrimOp FloatAddOp     [FloatV a, FloatV b] = FloatV $ a + b
evalPrimOp FloatSubOp     [FloatV a, FloatV b] = FloatV $ a - b
evalPrimOp FloatMulOp     [FloatV a, FloatV b] = FloatV $ a * b
evalPrimOp FloatDivOp     [FloatV a, FloatV b] = FloatV $ a / b
evalPrimOp FloatNegOp     [FloatV a] = FloatV (-a)
evalPrimOp FloatFabsOp    [FloatV a] = FloatV (abs a)
evalPrimOp Float2IntOp    [FloatV a] = IntV $ truncate a
evalPrimOp FloatExpOp     [FloatV a] = FloatV $ exp a
evalPrimOp FloatLogOp     [FloatV a] = FloatV $ log a
evalPrimOp FloatSqrtOp    [FloatV a] = FloatV $ sqrt a
evalPrimOp FloatSinOp     [FloatV a] = FloatV $ sin a
evalPrimOp FloatCosOp     [FloatV a] = FloatV $ cos a
evalPrimOp FloatTanOp     [FloatV a] = FloatV $ tan a
evalPrimOp FloatAsinOp    [FloatV a] = FloatV $ asin a
evalPrimOp FloatAcosOp    [FloatV a] = FloatV $ acos a
evalPrimOp FloatAtanOp    [FloatV a] = FloatV $ atan a
evalPrimOp FloatSinhOp    [FloatV a] = FloatV $ sinh a
evalPrimOp FloatCoshOp    [FloatV a] = FloatV $ cosh a
evalPrimOp FloatTanhOp    [FloatV a] = FloatV $ tanh a
evalPrimOp FloatPowerOp   [FloatV a, FloatV b] = FloatV $ a ** b
evalPrimOp Float2DoubleOp [FloatV a] = DoubleV $ realToFrac a
-}

{-
  FloatDecode_IntOp
-}
