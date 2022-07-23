module Stg.Interpreter.State.Global where

import Stg.Interpreter.State.Allocator

data GlobalState
  = GlobalState
  -- allocator related
  { ssAllocator :: !AllocatorState

  -- evaluation mode
  , ssEvalMode  :: EvalMode
  }
  deriving (Show)

emptyGlobalState :: EvalMode -> GlobalState
emptyGlobalState evalMode = GlobalState
  { ssAllocator = emptyAllocatorState
  , ssEvalMode  = evalMode
  }

data EvalMode
  = ConcreteEval
  | AbstractEval
  deriving (Show, Eq, Ord)
