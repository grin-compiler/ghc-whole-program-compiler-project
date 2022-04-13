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

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

      -- par# :: a -> Int#
      -- DEPRECATED: Use 'spark#' instead
  defOp "par#" $ \[_a] -> do
    pure [IntV 1]

      -- spark# :: a -> State# s -> (# State# s, a #)
  defOp "spark#" $ \[a, _s] -> do
    pure [a]

      -- seq# :: a -> State# s -> (# State# s, a #)
  defOp "seq#" $ \[a, _s] -> do
    stackPush $ Apply []
    pure [a]

      -- getSpark# :: State# s -> (# State# s, Int#, a #)
  defOp "getSpark#" $ \[_s] -> do
    pure [IntV 0, LiftedUndefined]

      -- numSparks# :: State# s -> (# State# s, Int# #)
  defOp "numSparks#" $ \[_s] -> do
    pure [IntV 0]
