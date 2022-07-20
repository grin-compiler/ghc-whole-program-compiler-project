module Stg.Interpreter.State.Heap where

import Stg.Syntax
import Stg.Interpreter.State.Env
import Stg.Interpreter.State.Address

type StgRhsClosure = Rhs  -- NOTE: must be StgRhsClosure only!

data HeapObject
  = Con
    { hoIsLNE       :: Bool
    , hoCon         :: DataCon
    , hoConArgs     :: [AtomAddr]
    }
  | Closure
    { hoIsLNE       :: Bool
    , hoName        :: Id
    , hoCloBody     :: StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [AtomAddr]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }
  | BlackHole HeapObject
  | ApStack                   -- HINT: needed for the async exceptions
    { hoResult        :: [AtomAddr]
    , hoStackPieceTop :: Maybe StackAddr
    }
  | RaiseException AtomAddr
  deriving (Show, Eq, Ord)
