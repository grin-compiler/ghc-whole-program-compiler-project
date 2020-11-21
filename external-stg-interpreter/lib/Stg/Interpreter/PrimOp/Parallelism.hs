{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Parallelism where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- par# :: a -> Int#
  -- DEPRECATED: Use 'spark#' instead

  -- SEE: newSpark c function
  -- spark# :: a -> State# s -> (# State# s, a #)

  -- SEE: newSpark c function
  -- seq# :: a -> State# s -> (# State# s, a #)

  -- SEE: stg_getSparkzh
  -- getSpark# :: State# s -> (# State# s, Int#, a #)

  -- SEE: stg_numSparkszh
  -- numSparks# :: State# s -> (# State# s, Int# #)

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Parallelism"
------------------------------------------------------------------------

primop  ParOp "par#" GenPrimOp
   a -> Int#
   with
      -- Note that Par is lazy to avoid that the sparked thing
      -- gets evaluated strictly, which it should *not* be
   has_side_effects = True
   code_size = { primOpCodeSizeForeignCall }
   deprecated_msg = { Use 'spark#' instead }

primop SparkOp "spark#" GenPrimOp
   a -> State# s -> (# State# s, a #)
   with has_side_effects = True
   code_size = { primOpCodeSizeForeignCall }

primop SeqOp "seq#" GenPrimOp
   a -> State# s -> (# State# s, a #)
   -- See Note [seq# magic] in GHC.Core.Op.ConstantFold

primop GetSparkOp "getSpark#" GenPrimOp
   State# s -> (# State# s, Int#, a #)
   with
   has_side_effects = True
   out_of_line = True

primop NumSparks "numSparks#" GenPrimOp
   State# s -> (# State# s, Int# #)
   { Returns the number of sparks in the local spark pool. }
   with
   has_side_effects = True
   out_of_line = True
-}
