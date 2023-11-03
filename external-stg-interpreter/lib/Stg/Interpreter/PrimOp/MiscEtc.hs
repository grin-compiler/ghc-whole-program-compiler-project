{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.MiscEtc where

import Control.Monad.State
import Foreign.C
import Foreign.Ptr

import Stg.Syntax
import Stg.Interpreter.Base
import Stg.Interpreter.Debugger.Region (evalRegionCommand)

pattern Int64V i = IntAtom i

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- getCCSOf# :: a -> State# s -> (# State# s, Addr# #)
  ( "getCCSOf#", [HeapPtr _, Void]) -> do
    pure [PtrAtom CostCentreStackPtr nullPtr]


  -- getCurrentCCS# :: a -> State# s -> (# State# s, Addr# #)
  ( "getCurrentCCS#", [_, _]) -> do
    -- HINT: follows the non profiling mode semantics
    pure [PtrAtom RawPtr nullPtr]

  -- clearCCS# :: (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)

  -- traceEvent# :: Addr# -> State# s -> State# s
  ( "traceEvent#", [PtrAtom _ p, _s]) -> do
    msg <- liftIO $ peekCString $ castPtr p
    evalRegionCommand msg
    addrState <- getAddressState
    modify' $ \s@StgState{..} -> s {ssTraceEvents = (msg, addrState) : ssTraceEvents}
    pure []

  -- traceBinaryEvent# :: Addr# -> Int# -> State# s -> State# s

  -- traceMarker# :: Addr# -> State# s -> State# s
  ( "traceMarker#", [PtrAtom _ p, _s]) -> do
    msg <- liftIO $ peekCString $ castPtr p
    evalRegionCommand msg
    tid <- gets ssCurrentThreadId
    liftIO $ print (tid, msg)
    addrState <- getAddressState
    modify' $ \s@StgState{..} -> s {ssTraceMarkers = (msg, tid, addrState) : ssTraceMarkers}
    pure []

  -- setThreadAllocationCounter# :: Int64# -> State# RealWorld -> State# RealWorld
  ( "setThreadAllocationCounter#", [Int64V n, _s]) -> do
    -- TODO
    pure []

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Misc"
        {These aren't nearly as wired in as Etc...}
------------------------------------------------------------------------

primop  GetCCSOfOp "getCCSOf#" GenPrimOp
   a -> State# s -> (# State# s, Addr# #)

primop  GetCurrentCCSOp "getCurrentCCS#" GenPrimOp
   a -> State# s -> (# State# s, Addr# #)
   { Returns the current {\tt CostCentreStack} (value is {\tt NULL} if
     not profiling).  Takes a dummy argument which can be used to
     avoid the call to {\tt getCurrentCCS\#} being floated out by the
     simplifier, which would result in an uninformative stack
     ("CAF"). }

primop  ClearCCSOp "clearCCS#" GenPrimOp
   (State# s -> (# State# s, a #)) -> State# s -> (# State# s, a #)
   { Run the supplied IO action with an empty CCS.  For example, this
     is used by the interpreter to run an interpreted computation
     without the call stack showing that it was invoked from GHC. }
   with
   out_of_line = True

------------------------------------------------------------------------
section "Etc"
        {Miscellaneous built-ins}
------------------------------------------------------------------------

primtype Proxy# a
   { The type constructor {\tt Proxy#} is used to bear witness to some
   type variable. It's used when you want to pass around proxy values
   for doing things like modelling type applications. A {\tt Proxy#}
   is not only unboxed, it also has a polymorphic kind, and has no
   runtime representation, being totally free. }

pseudoop "proxy#"
   Proxy# a
   { Witness for an unboxed {\tt Proxy#} value, which has no runtime
   representation. }

pseudoop   "seq"
   a -> b -> b
   { The value of {\tt seq a b} is bottom if {\tt a} is bottom, and
     otherwise equal to {\tt b}. In other words, it evaluates the first
     argument {\tt a} to weak head normal form (WHNF). {\tt seq} is usually
     introduced to improve performance by avoiding unneeded laziness.

     A note on evaluation order: the expression {\tt seq a b} does
     {\it not} guarantee that {\tt a} will be evaluated before {\tt b}.
     The only guarantee given by {\tt seq} is that the both {\tt a}
     and {\tt b} will be evaluated before {\tt seq} returns a value.
     In particular, this means that {\tt b} may be evaluated before
     {\tt a}. If you need to guarantee a specific order of evaluation,
     you must use the function {\tt pseq} from the "parallel" package. }
   with fixity = infixr 0
         -- This fixity is only the one picked up by Haddock. If you
         -- change this, do update 'ghcPrimIface' in 'GHC.Iface.Load'.

pseudoop   "unsafeCoerce#"
   a -> b
   { The function {\tt unsafeCoerce\#} allows you to side-step the typechecker entirely. That
        is, it allows you to coerce any type into any other type. If you use this function,
        you had better get it right, otherwise segmentation faults await. It is generally
        used when you want to write a program that you know is well-typed, but where Haskell's
        type system is not expressive enough to prove that it is well typed.

        The following uses of {\tt unsafeCoerce\#} are supposed to work (i.e. not lead to
        spurious compile-time or run-time crashes):

         * Casting any lifted type to {\tt Any}

         * Casting {\tt Any} back to the real type

         * Casting an unboxed type to another unboxed type of the same size.
           (Casting between floating-point and integral types does not work.
           See the {\tt GHC.Float} module for functions to do work.)

         * Casting between two types that have the same runtime representation.  One case is when
           the two types differ only in "phantom" type parameters, for example
           {\tt Ptr Int} to {\tt Ptr Float}, or {\tt [Int]} to {\tt [Float]} when the list is
           known to be empty.  Also, a {\tt newtype} of a type {\tt T} has the same representation
           at runtime as {\tt T}.

        Other uses of {\tt unsafeCoerce\#} are undefined.  In particular, you should not use
        {\tt unsafeCoerce\#} to cast a T to an algebraic data type D, unless T is also
        an algebraic data type.  For example, do not cast {\tt Int->Int} to {\tt Bool}, even if
        you later cast that {\tt Bool} back to {\tt Int->Int} before applying it.  The reasons
        have to do with GHC's internal representation details (for the cognoscenti, data values
        can be entered but function closures cannot).  If you want a safe type to cast things
        to, use {\tt Any}, which is not an algebraic data type.

        }
   with can_fail = True

-- NB. It is tempting to think that casting a value to a type that it doesn't have is safe
-- as long as you don't "do anything" with the value in its cast form, such as seq on it.  This
-- isn't the case: the compiler can insert seqs itself, and if these happen at the wrong type,
-- Bad Things Might Happen.  See bug #1616: in this case we cast a function of type (a,b) -> (a,b)
-- to () -> () and back again.  The strictness analyser saw that the function was strict, but
-- the wrapper had type () -> (), and hence the wrapper de-constructed the (), the worker re-constructed
-- a new (), with the result that the code ended up with "case () of (a,b) -> ...".

primop  TraceEventOp "traceEvent#" GenPrimOp
   Addr# -> State# s -> State# s
   { Emits an event via the RTS tracing framework.  The contents
     of the event is the zero-terminated byte string passed as the first
     argument.  The event will be emitted either to the {\tt .eventlog} file,
     or to stderr, depending on the runtime RTS flags. }
   with
   has_side_effects = True
   out_of_line      = True

primop  TraceEventBinaryOp "traceBinaryEvent#" GenPrimOp
   Addr# -> Int# -> State# s -> State# s
   { Emits an event via the RTS tracing framework.  The contents
     of the event is the binary object passed as the first argument with
     the the given length passed as the second argument. The event will be
     emitted to the {\tt .eventlog} file. }
   with
   has_side_effects = True
   out_of_line      = True

primop  TraceMarkerOp "traceMarker#" GenPrimOp
   Addr# -> State# s -> State# s
   { Emits a marker event via the RTS tracing framework.  The contents
     of the event is the zero-terminated byte string passed as the first
     argument.  The event will be emitted either to the {\tt .eventlog} file,
     or to stderr, depending on the runtime RTS flags. }
   with
   has_side_effects = True
   out_of_line      = True

primop  SetThreadAllocationCounter "setThreadAllocationCounter#" GenPrimOp
   INT64 -> State# RealWorld -> State# RealWorld
   { Sets the allocation counter for the current thread to the given value. }
   with
   has_side_effects = True
   out_of_line      = True
-}