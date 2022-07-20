module Stg.Interpreter.State.Env where

import Data.Map (Map)

import Stg.Syntax
import Stg.Interpreter.State.Address

type Env = Map Id (StaticOrigin, AtomAddr)   -- NOTE: must contain only the defined local variables

data StaticOrigin
  = SO_CloArg
  | SO_Let
  | SO_Scrut
  | SO_AltArg
  | SO_TopLevel
  | SO_Builtin
  | SO_ClosureResult
  deriving (Show, Eq, Ord)
