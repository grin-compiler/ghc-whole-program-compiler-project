{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.GHCiBytecode where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Bytecode operations"
        {Support for manipulating bytecode objects used by the interpreter and
        linker.

        Bytecode objects are heap objects which represent top-level bindings and
        contain a list of instructions and data needed by these instructions.}
------------------------------------------------------------------------

primtype BCO
   { Primitive bytecode type. }

primop   AddrToAnyOp "addrToAny#" GenPrimOp
   Addr# -> (# a #)
   { Convert an {\tt Addr\#} to a followable Any type. }
   with
   code_size = 0

primop   AnyToAddrOp "anyToAddr#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, Addr# #)
   { Retrieve the address of any Haskell value. This is
     essentially an {\texttt unsafeCoerce\#}, but if implemented as such
     the core lint pass complains and fails to compile.
     As a primop, it is opaque to core/stg, and only appears
     in cmm (where the copy propagation pass will get rid of it).
     Note that "a" must be a value, not a thunk! It's too late
     for strictness analysis to enforce this, so you're on your
     own to guarantee this. Also note that {\texttt Addr\#} is not a GC
     pointer - up to you to guarantee that it does not become
     a dangling pointer immediately after you get it.}
   with
   code_size = 0

primop   MkApUpd0_Op "mkApUpd0#" GenPrimOp
   BCO -> (# a #)
   { Wrap a BCO in a {\tt AP_UPD} thunk which will be updated with the value of
     the BCO when evaluated. }
   with
   out_of_line = True

primop  NewBCOOp "newBCO#" GenPrimOp
   ByteArray# -> ByteArray# -> Array# a -> Int# -> ByteArray# -> State# s -> (# State# s, BCO #)
   { {\tt newBCO\# instrs lits ptrs arity bitmap} creates a new bytecode object. The
     resulting object encodes a function of the given arity with the instructions
     encoded in {\tt instrs}, and a static reference table usage bitmap given by
     {\tt bitmap}. }
   with
   has_side_effects = True
   out_of_line      = True

primop  UnpackClosureOp "unpackClosure#" GenPrimOp
   a -> (# Addr#, ByteArray#, Array# b #)
   { {\tt unpackClosure\# closure} copies the closure and pointers in the
     payload of the given closure into two new arrays, and returns a pointer to
     the first word of the closure's info table, a non-pointer array for the raw
     bytes of the closure, and a pointer array for the pointers in the payload. }
   with
   out_of_line = True

primop  ClosureSizeOp "closureSize#" GenPrimOp
   a -> Int#
   { {\tt closureSize\# closure} returns the size of the given closure in
     machine words. }
   with
   out_of_line = True

primop  GetApStackValOp "getApStackVal#" GenPrimOp
   a -> Int# -> (# Int#, b #)
   with
   out_of_line = True
-}