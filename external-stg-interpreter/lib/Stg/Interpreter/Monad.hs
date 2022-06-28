{-# LANGUAGE ConstraintKinds #-}
module Stg.Interpreter.Monad where

import Control.Effect.State
import Control.Effect.Lift
import Control.Effect.Fail
--import Control.Monad.Loops
--import Control.Monad.IO.Class

import Stg.Syntax
import Stg.Interpreter.State.Allocator
import Stg.Interpreter.State.StgState

-- interpreter monad

--type M = StateT StgState IO
type M sig m =
  ( Has (State StgState) sig m
  , Has (Lift IO) sig m
--  , Has Fail sig m
  , MonadFail m
  )

-- primop related

type FCallEval m = ForeignCall -> [AtomAddr] -> Type -> m [AtomAddr]
type PrimOpEval m = Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
type EvalOnNewThread m = m [AtomAddr] -> m [AtomAddr]

---------------------------------------------
