{-# LANGUAGE TypeApplications, DataKinds #-}
module Stg.Interpreter.BaseState
  ( module Stg.Interpreter.State.Global
  , module Stg.Interpreter.State.Allocator
  , module Stg.Interpreter.State.Address
  , module Stg.Interpreter.State.Atom
  , module Stg.Interpreter.State.PrimTypes
  , module Stg.Interpreter.State.Stack
  , module Stg.Interpreter.State.Heap
  , module Stg.Interpreter.State.Env
  , module Stg.Interpreter.State.Thread
  , module Stg.Interpreter.State.Rts
  , module Stg.Interpreter.State.StgState
  , module Stg.Interpreter.Monad
  , module Stg.Interpreter.ErrorReport
  , module Control.Effect.State
  , module Control.Effect.Lift
  , module Control.Monad
  , evalGlobal
  ) where

import Stg.Interpreter.State.Global
import Stg.Interpreter.State.Allocator
import Stg.Interpreter.State.Address
import Stg.Interpreter.State.Atom
import Stg.Interpreter.State.PrimTypes
import Stg.Interpreter.State.Stack
import Stg.Interpreter.State.Heap
import Stg.Interpreter.State.Env
import Stg.Interpreter.State.Thread
import Stg.Interpreter.State.Rts
import Stg.Interpreter.State.StgState
import Stg.Interpreter.Monad
import Stg.Interpreter.ErrorReport

import Control.Effect.State
import Control.Effect.Lift
import Control.Monad

import Control.Carrier.State.Strict
import Control.Effect.Labelled

evalGlobal s m = evalState s (runLabelled @"Global" m)
