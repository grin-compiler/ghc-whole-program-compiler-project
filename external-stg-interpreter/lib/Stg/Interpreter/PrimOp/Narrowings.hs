{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Narrowings where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ("narrow8Int#", [i]) -> do
    -- Int# -> Int#
    pure [i] -- TODO

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Narrowings"
        {Explicit narrowing of native-sized ints or words.}
------------------------------------------------------------------------

primop   Narrow8IntOp      "narrow8Int#"      Monadic   Int# -> Int#
primop   Narrow16IntOp     "narrow16Int#"     Monadic   Int# -> Int#
primop   Narrow32IntOp     "narrow32Int#"     Monadic   Int# -> Int#
primop   Narrow8WordOp     "narrow8Word#"     Monadic   Word# -> Word#
primop   Narrow16WordOp    "narrow16Word#"    Monadic   Word# -> Word#
primop   Narrow32WordOp    "narrow32Word#"    Monadic   Word# -> Word#

evalPrimOp Narrow8IntOp   [IntV a]  = IntV  $ fromIntegral (fromIntegral a :: Int8)
evalPrimOp Narrow16IntOp  [IntV a]  = IntV  $ fromIntegral (fromIntegral a :: Int16)
evalPrimOp Narrow32IntOp  [IntV a]  = IntV  $ fromIntegral (fromIntegral a :: Int32)
evalPrimOp Narrow8WordOp  [WordV a] = WordV $ fromIntegral (fromIntegral a :: Word8)
evalPrimOp Narrow16WordOp [WordV a] = WordV $ fromIntegral (fromIntegral a :: Word16)
evalPrimOp Narrow32WordOp [WordV a] = WordV $ fromIntegral (fromIntegral a :: Word32)
-}
