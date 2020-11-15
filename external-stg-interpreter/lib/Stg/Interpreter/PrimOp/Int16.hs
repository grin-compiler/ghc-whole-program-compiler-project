{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Int16 where

import Stg.Syntax
import Stg.Interpreter.Base

import Data.Int

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- extendInt16# :: Int16# -> Int#
  ( "extendInt16#",  [Int16V a]) -> pure [IntV  $ fromIntegral (fromIntegral a :: Int)]

  -- narrowInt16# :: Int# -> Int16#
  ( "narrowInt16#",  [IntV a]) -> pure [Int16V  $ fromIntegral (fromIntegral a :: Int16)]

  -- negateInt16# :: Int16# -> Int16#
  ( "negateInt16#",  [Int16V a]) -> pure [Int16V (-a)]

  -- plusInt16# :: Int16# -> Int16# -> Int16#
  ( "plusInt16#",    [Int16V a, Int16V b]) -> pure [Int16V $ a + b]

  -- subInt16# :: Int16# -> Int16# -> Int16#
  ( "subInt16#",     [Int16V a, Int16V b]) -> pure [Int16V $ a - b]

  -- timesInt16# :: Int16# -> Int16# -> Int16#
  ( "timesInt16#",   [Int16V a, Int16V b]) -> pure [Int16V $ a * b]

  -- quotInt16# :: Int16# -> Int16# -> Int16#
  ( "quotInt16#",    [Int16V a, Int16V b]) -> pure [Int16V $ a `quot` b]  -- NOTE: int16 / int16 in C

  -- remInt16# :: Int16# -> Int16# -> Int16#
  ( "remInt16#",     [Int16V a, Int16V b]) -> pure [Int16V $ a `rem` b]   -- NOTE: int16 % int16 in C

  -- quotRemInt16# :: Int16# -> Int16# -> (# Int16#, Int16# #)
  ( "quotRemInt16#", [Int16V a, Int16V b]) -> pure [Int16V $ a `quot` b, Int16V $ a `rem` b]

  -- eqInt16# :: Int16# -> Int16# -> Int#
  ( "eqInt16#",      [Int16V a, Int16V b]) -> pure [IntV $ if a == b then 1 else 0]

  -- geInt16# :: Int16# -> Int16# -> Int#
  ( "geInt16#",      [Int16V a, Int16V b]) -> pure [IntV $ if a >= b then 1 else 0]

  -- gtInt16# :: Int16# -> Int16# -> Int#
  ( "gtInt16#",      [Int16V a, Int16V b]) -> pure [IntV $ if a > b  then 1 else 0]

  -- leInt16# :: Int16# -> Int16# -> Int#
  ( "leInt16#",      [Int16V a, Int16V b]) -> pure [IntV $ if a <= b then 1 else 0]

  -- ltInt16# :: Int16# -> Int16# -> Int#
  ( "ltInt16#",      [Int16V a, Int16V b]) -> pure [IntV $ if a < b  then 1 else 0]

  -- neInt16# :: Int16# -> Int16# -> Int#
  ( "neInt16#",      [Int16V a, Int16V b]) -> pure [IntV $ if a /= b then 1 else 0]

  _ -> fallback op args t tc
