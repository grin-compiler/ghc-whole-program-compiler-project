{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Int where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ("<=#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (if a <= b then 1 else 0))] -- Int# -> Int# -> Int#
  (">=#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (if a >= b then 1 else 0))] -- Int# -> Int# -> Int#

  ("+#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (a + b))] -- Int# -> Int# -> Int#
  ("*#", [Literal (LitNumber LitNumInt a), Literal (LitNumber LitNumInt b)]) -> pure [Literal (LitNumber LitNumInt (a * b))] -- Int# -> Int# -> Int#

  -- "uncheckedIShiftL#" args: [Literal (LitNumber LitNumInt 0),Literal (LitNumber LitNumInt 2)]
  ("uncheckedIShiftL#", [a, b]) -> do
    -- Int# -> Int# -> Int#
    pure [a] -- TODO

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Int#"
        {Operations on native-size integers (32+ bits).}
------------------------------------------------------------------------

primtype Int#

primop   IntAddOp    "+#"    Dyadic
   Int# -> Int# -> Int#
   with commutable = True
        fixity = infixl 6

primop   IntSubOp    "-#"    Dyadic   Int# -> Int# -> Int#
   with fixity = infixl 6

primop   IntMulOp    "*#"
   Dyadic   Int# -> Int# -> Int#
   {Low word of signed integer multiply.}
   with commutable = True
        fixity = infixl 7

primop   IntMul2Op    "timesInt2#" GenPrimOp
   Int# -> Int# -> (# Int#, Int#, Int# #)
   {Return a triple (isHighNeeded,high,low) where high and low are respectively
   the high and low bits of the double-word result. isHighNeeded is a cheap way
   to test if the high word is a sign-extension of the low word (isHighNeeded =
   0#) or not (isHighNeeded = 1#).}

primop   IntMulMayOfloOp  "mulIntMayOflo#"
   Dyadic   Int# -> Int# -> Int#
   {Return non-zero if there is any possibility that the upper word of a
    signed integer multiply might contain useful information.  Return
    zero only if you are completely sure that no overflow can occur.
    On a 32-bit platform, the recommended implementation is to do a
    32 x 32 -> 64 signed multiply, and subtract result[63:32] from
    (result[31] >>signed 31).  If this is zero, meaning that the
    upper word is merely a sign extension of the lower one, no
    overflow can occur.

    On a 64-bit platform it is not always possible to
    acquire the top 64 bits of the result.  Therefore, a recommended
    implementation is to take the absolute value of both operands, and
    return 0 iff bits[63:31] of them are zero, since that means that their
    magnitudes fit within 31 bits, so the magnitude of the product must fit
    into 62 bits.

    If in doubt, return non-zero, but do make an effort to create the
    correct answer for small args, since otherwise the performance of
    \texttt{(*) :: Integer -> Integer -> Integer} will be poor.
   }
   with commutable = True

primop   IntQuotOp    "quotInt#"    Dyadic
   Int# -> Int# -> Int#
   {Rounds towards zero. The behavior is undefined if the second argument is
    zero.
   }
   with can_fail = True

primop   IntRemOp    "remInt#"    Dyadic
   Int# -> Int# -> Int#
   {Satisfies \texttt{(quotInt\# x y) *\# y +\# (remInt\# x y) == x}. The
    behavior is undefined if the second argument is zero.
   }
   with can_fail = True

primop   IntQuotRemOp "quotRemInt#"    GenPrimOp
   Int# -> Int# -> (# Int#, Int# #)
   {Rounds towards zero.}
   with can_fail = True

primop   AndIOp   "andI#"   Dyadic    Int# -> Int# -> Int#
   {Bitwise "and".}
   with commutable = True

primop   OrIOp   "orI#"     Dyadic    Int# -> Int# -> Int#
   {Bitwise "or".}
   with commutable = True

primop   XorIOp   "xorI#"   Dyadic    Int# -> Int# -> Int#
   {Bitwise "xor".}
   with commutable = True

primop   NotIOp   "notI#"   Monadic   Int# -> Int#
   {Bitwise "not", also known as the binary complement.}

primop   IntNegOp    "negateInt#"    Monadic   Int# -> Int#
   {Unary negation.
    Since the negative {\tt Int#} range extends one further than the
    positive range, {\tt negateInt#} of the most negative number is an
    identity operation. This way, {\tt negateInt#} is always its own inverse.}

primop   IntAddCOp   "addIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
         {Add signed integers reporting overflow.
          First member of result is the sum truncated to an {\tt Int#};
          second member is zero if the true sum fits in an {\tt Int#},
          nonzero if overflow occurred (the sum is either too large
          or too small to fit in an {\tt Int#}).}
   with code_size = 2
        commutable = True

primop   IntSubCOp   "subIntC#"    GenPrimOp   Int# -> Int# -> (# Int#, Int# #)
         {Subtract signed integers reporting overflow.
          First member of result is the difference truncated to an {\tt Int#};
          second member is zero if the true difference fits in an {\tt Int#},
          nonzero if overflow occurred (the difference is either too large
          or too small to fit in an {\tt Int#}).}
   with code_size = 2

primop   IntGtOp  ">#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   IntGeOp  ">=#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   IntEqOp  "==#"   Compare
   Int# -> Int# -> Int#
   with commutable = True
        fixity = infix 4

primop   IntNeOp  "/=#"   Compare
   Int# -> Int# -> Int#
   with commutable = True
        fixity = infix 4

primop   IntLtOp  "<#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   IntLeOp  "<=#"   Compare   Int# -> Int# -> Int#
   with fixity = infix 4

primop   ChrOp   "chr#"   GenPrimOp   Int# -> Char#
   with code_size = 0

primop   Int2WordOp "int2Word#" GenPrimOp Int# -> Word#
   with code_size = 0

primop   Int2FloatOp   "int2Float#"      GenPrimOp  Int# -> Float#
primop   Int2DoubleOp   "int2Double#"          GenPrimOp  Int# -> Double#

primop   Word2FloatOp   "word2Float#"      GenPrimOp  Word# -> Float#
primop   Word2DoubleOp   "word2Double#"          GenPrimOp  Word# -> Double#

primop   ISllOp   "uncheckedIShiftL#" GenPrimOp  Int# -> Int# -> Int#
         {Shift left.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   ISraOp   "uncheckedIShiftRA#" GenPrimOp Int# -> Int# -> Int#
         {Shift right arithmetic.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
primop   ISrlOp   "uncheckedIShiftRL#" GenPrimOp Int# -> Int# -> Int#
         {Shift right logical.  Result undefined if shift amount is not
          in the range 0 to word size - 1 inclusive.}
-}
{-
-- Int
evalPrimOp IntAddOp         [IntV a, IntV b] = IntV $ a + b
evalPrimOp IntSubOp         [IntV a, IntV b] = IntV $ a - b
evalPrimOp IntMulOp         [IntV a, IntV b] = IntV $ a * b
evalPrimOp IntMulMayOfloOp  [IntV a, IntV b] = IntV $ if fromIntegral a * (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0
evalPrimOp IntQuotOp        [IntV a, IntV b] = IntV $ a `quot` b  -- NOTE: int / int in C
evalPrimOp IntRemOp         [IntV a, IntV b] = IntV $ a `rem` b   -- NOTE: int % int in C
evalPrimOp IntQuotRemOp     [IntV a, IntV b] = TupleV [IntV $ a `quot` b, IntV $ a `rem` b]
evalPrimOp AndIOp           [IntV a, IntV b] = IntV $ a .&. b
evalPrimOp OrIOp            [IntV a, IntV b] = IntV $ a .|. b
evalPrimOp XorIOp           [IntV a, IntV b] = IntV $ a `xor` b
evalPrimOp NotIOp           [IntV a] = IntV $ complement a
evalPrimOp IntNegOp         [IntV a] = IntV (-a)
evalPrimOp IntAddCOp [IntV a, IntV b] = TupleV
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a + (fromIntegral b :: Integer) > fromIntegral (maxBound :: PrimInt) then 1 else 0
                                        ]
evalPrimOp IntSubCOp [IntV a, IntV b] = TupleV
                                        [ IntV $ a + b
                                        , IntV $ if fromIntegral a - (fromIntegral b :: Integer) < fromIntegral (minBound :: PrimInt) then 1 else 0
                                        ]
evalPrimOp IntGtOp  [IntV a, IntV b] = IntV $ if a > b  then 1 else 0
evalPrimOp IntGeOp  [IntV a, IntV b] = IntV $ if a >= b then 1 else 0
evalPrimOp IntEqOp  [IntV a, IntV b] = IntV $ if a == b then 1 else 0
evalPrimOp IntNeOp  [IntV a, IntV b] = IntV $ if a /= b then 1 else 0
evalPrimOp IntLtOp  [IntV a, IntV b] = IntV $ if a < b  then 1 else 0
evalPrimOp IntLeOp  [IntV a, IntV b] = IntV $ if a <= b then 1 else 0
evalPrimOp ChrOp          [IntV a]  = CharV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp Int2WordOp     [IntV a]  = WordV $ fromIntegral a -- HINT: noop ; same bit level representation
evalPrimOp Int2FloatOp    [IntV a]  = FloatV $ fromIntegral a
evalPrimOp Int2DoubleOp   [IntV a]  = DoubleV $ fromIntegral a
evalPrimOp Word2FloatOp   [WordV a] = FloatV $ fromIntegral a
evalPrimOp Word2DoubleOp  [WordV a] = DoubleV $ fromIntegral a
evalPrimOp ISllOp [IntV a, IntV b] = IntV $ unsafeShiftL a (fromIntegral b)
evalPrimOp ISraOp [IntV a, IntV b] = IntV $ unsafeShiftR a (fromIntegral b) -- Shift right arithmetic
evalPrimOp ISrlOp [IntV a, IntV b] = IntV $ fromIntegral $ unsafeShiftR (fromIntegral a :: PrimWord) (fromIntegral b) -- Shift right logical
-}
