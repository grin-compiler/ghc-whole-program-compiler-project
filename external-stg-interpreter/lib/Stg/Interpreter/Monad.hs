{-# LANGUAGE ConstraintKinds, DataKinds #-}
module Stg.Interpreter.Monad where

import Control.Effect.State
import Control.Effect.Labelled
import Control.Effect.Lift
import Control.Effect.Fail
import Control.Effect.NonDet
--import Control.Monad.Loops
--import Control.Monad.IO.Class

import Stg.Syntax
import Stg.Interpreter.State.Global
import Stg.Interpreter.State.StgState
import Stg.Interpreter.State.Address

-- interpreter monad

--type M = StateT StgState IO
type M sig m =
  ( Has (State StgState) sig m
  , Has NonDet sig m
  , HasLabelled "Global" (State GlobalState) sig m
  , Has (Lift IO) sig m
--  , Has Fail sig m
  , MonadFail m
  )

-- primop related

type FCallEval m = ForeignCall -> [AtomAddr] -> Type -> m [AtomAddr]
type PrimOpEval m = Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
type EvalOnNewThread m = m [AtomAddr] -> m [AtomAddr]

---------------------------------------------
