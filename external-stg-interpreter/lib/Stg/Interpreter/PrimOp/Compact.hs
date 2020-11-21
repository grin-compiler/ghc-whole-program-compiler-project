{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Compact where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- compactNew# :: Word# -> State# RealWorld -> (# State# RealWorld, Compact# #)
  -- compactResize# :: Compact# -> Word# -> State# RealWorld -> State# RealWorld
  -- compactContains# :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, Int# #)
  -- compactContainsAny# :: a -> State# RealWorld -> (# State# RealWorld, Int# #)
  -- compactGetFirstBlock# :: Compact# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)
  -- compactGetNextBlock# :: Compact# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)
  -- compactAllocateBlock# :: Word# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
  -- compactFixupPointers# :: Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Compact#, Addr# #)
  -- compactAdd# :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
  -- compactAddWithSharing# :: Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
  -- compactSize# :: Compact# -> State# RealWorld -> (# State# RealWorld, Word# #)

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Compact normal form"

        {Primitives for working with compact regions. The {\tt ghc\-compact}
         library and the {\tt compact} library demonstrate how to use these
         primitives. The documentation below draws a distinction between
         a CNF and a compact block. A CNF contains one or more compact
         blocks. The source file {\tt rts\/sm\/CNF.c}
         diagrams this relationship. When discussing a compact
         block, an additional distinction is drawn between capacity and
         utilized bytes. The capacity is the maximum number of bytes that
         the compact block can hold. The utilized bytes is the number of
         bytes that are actually used by the compact block.
        }

------------------------------------------------------------------------

primtype Compact#

primop  CompactNewOp "compactNew#" GenPrimOp
   Word# -> State# RealWorld -> (# State# RealWorld, Compact# #)
   { Create a new CNF with a single compact block. The argument is
     the capacity of the compact block (in bytes, not words).
     The capacity is rounded up to a multiple of the allocator block size
     and is capped to one mega block. }
   with
   has_side_effects = True
   out_of_line      = True

primop  CompactResizeOp "compactResize#" GenPrimOp
   Compact# -> Word# -> State# RealWorld ->
   State# RealWorld
   { Set the new allocation size of the CNF. This value (in bytes)
     determines the capacity of each compact block in the CNF. It
     does not retroactively affect existing compact blocks in the CNF. }
   with
   has_side_effects = True
   out_of_line      = True

primop  CompactContainsOp "compactContains#" GenPrimOp
   Compact# -> a -> State# RealWorld -> (# State# RealWorld, Int# #)
   { Returns 1\# if the object is contained in the CNF, 0\# otherwise. }
   with
   out_of_line      = True

primop  CompactContainsAnyOp "compactContainsAny#" GenPrimOp
   a -> State# RealWorld -> (# State# RealWorld, Int# #)
   { Returns 1\# if the object is in any CNF at all, 0\# otherwise. }
   with
   out_of_line      = True

primop  CompactGetFirstBlockOp "compactGetFirstBlock#" GenPrimOp
   Compact# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)
   { Returns the address and the utilized size (in bytes) of the
     first compact block of a CNF.}
   with
   out_of_line      = True

primop  CompactGetNextBlockOp "compactGetNextBlock#" GenPrimOp
   Compact# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr#, Word# #)
   { Given a CNF and the address of one its compact blocks, returns the
     next compact block and its utilized size, or {\tt nullAddr\#} if the
     argument was the last compact block in the CNF. }
   with
   out_of_line      = True

primop  CompactAllocateBlockOp "compactAllocateBlock#" GenPrimOp
   Word# -> Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)
   { Attempt to allocate a compact block with the capacity (in
     bytes) given by the first argument. The {\texttt Addr\#} is a pointer
     to previous compact block of the CNF or {\texttt nullAddr\#} to create a
     new CNF with a single compact block.

     The resulting block is not known to the GC until
     {\texttt compactFixupPointers\#} is called on it, and care must be taken
     so that the address does not escape or memory will be leaked.
   }
   with
   has_side_effects = True
   out_of_line      = True

primop  CompactFixupPointersOp "compactFixupPointers#" GenPrimOp
   Addr# -> Addr# -> State# RealWorld -> (# State# RealWorld, Compact#, Addr# #)
   { Given the pointer to the first block of a CNF and the
     address of the root object in the old address space, fix up
     the internal pointers inside the CNF to account for
     a different position in memory than when it was serialized.
     This method must be called exactly once after importing
     a serialized CNF. It returns the new CNF and the new adjusted
     root address. }
   with
   has_side_effects = True
   out_of_line      = True

primop CompactAdd "compactAdd#" GenPrimOp
   Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
   { Recursively add a closure and its transitive closure to a
     {\texttt Compact\#} (a CNF), evaluating any unevaluated components
     at the same time. Note: {\texttt compactAdd\#} is not thread-safe, so
     only one thread may call {\texttt compactAdd\#} with a particular
     {\texttt Compact\#} at any given time. The primop does not
     enforce any mutual exclusion; the caller is expected to
     arrange this. }
   with
   has_side_effects = True
   out_of_line      = True

primop CompactAddWithSharing "compactAddWithSharing#" GenPrimOp
   Compact# -> a -> State# RealWorld -> (# State# RealWorld, a #)
   { Like {\texttt compactAdd\#}, but retains sharing and cycles
   during compaction. }
   with
   has_side_effects = True
   out_of_line      = True

primop CompactSize "compactSize#" GenPrimOp
   Compact# -> State# RealWorld -> (# State# RealWorld, Word# #)
   { Return the total capacity (in bytes) of all the compact blocks
     in the CNF. }
   with
   has_side_effects = True
   out_of_line      = True
-}