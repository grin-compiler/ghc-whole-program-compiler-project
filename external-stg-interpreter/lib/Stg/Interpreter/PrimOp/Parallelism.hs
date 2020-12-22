{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Parallelism where

import Stg.Syntax
import Stg.Interpreter.Base

{-
  NOTE:
    - these primops are for multi core evaluation
    - on single core evaluation they do nothing
    - the ext-stg interpreter is a single core evaluator
-}

pattern IntV i = IntAtom i

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- par# :: a -> Int#
  -- DEPRECATED: Use 'spark#' instead
  ( "par#", [_a]) -> do
    pure [IntV 1]

  -- spark# :: a -> State# s -> (# State# s, a #)
  ( "spark#", [a, _s]) -> do
    pure [a]

  -- seq# :: a -> State# s -> (# State# s, a #)
  ( "seq#", [a, _s]) -> do
    stackPush $ Apply []
    pure [a]

  -- getSpark# :: State# s -> (# State# s, Int#, a #)
  ( "getSpark#", [_s]) -> do
    pure [IntV 0, LiftedUndefined]

  -- numSparks# :: State# s -> (# State# s, Int# #)
  ( "numSparks#", [_s]) -> do
    pure [IntV 0]

  _ -> fallback op args t tc
