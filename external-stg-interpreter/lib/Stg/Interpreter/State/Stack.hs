module Stg.Interpreter.State.Stack where

import Stg.Syntax
import Stg.Interpreter.State.Allocator
import Stg.Interpreter.State.Env

data StackContinuation
  = CaseOf        !Env !Binder !AltType ![Alt]  -- pattern match on the result ; carries the closure's local environment
  | Update        !HeapAddr                     -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply         ![AtomAddr]                   -- apply args on the result heap object
  | Catch         !AtomAddr !Bool !Bool         -- catch frame ; exception handler, block async exceptions, interruptible
  | RestoreExMask !Bool !Bool                   -- saved: block async exceptions, interruptible
  | RunScheduler  !ScheduleReason
  | DataToTagOp
  deriving (Show, Eq, Ord)

data ScheduleReason
  = SR_ThreadFinished
  | SR_ThreadBlocked
  | SR_ThreadYield
  deriving (Show, Eq, Ord)
