module Stg.Interpreter.State.Atom where

import Data.ByteString.Char8 (ByteString)
import Data.Word
import Foreign.Ptr
import Stg.Syntax
import Stg.Interpreter.State.Allocator

data ArrIdx
  = MutArrIdx !MutableArrayAddr
  | ArrIdx    !ArrayAddr
  deriving (Show, Eq, Ord)

data SmallArrIdx
  = SmallMutArrIdx !SmallMutableArrayAddr
  | SmallArrIdx    !SmallArrayAddr
  deriving (Show, Eq, Ord)

data ArrayArrIdx
  = ArrayMutArrIdx !MutableArrayArrayAddr
  | ArrayArrIdx    !ArrayArrayAddr
  deriving (Show, Eq, Ord)

data ByteArrayIdx
  = ByteArrayIdx
  { baId        :: !MutableByteArrayAddr
  , baPinned    :: !Bool
  , baAlignment :: !Int
  }
  deriving (Show, Eq, Ord)

data PtrOrigin
  = CStringPtr    !ByteString         -- null terminated string
  | ByteArrayPtr  !ByteArrayIdx       -- raw ptr to the byte array
  | RawPtr                            -- raw ptr to a values with unknown origin (i.e. FFI)
  | StablePtr     !StablePointerAddr  -- stable pointer must have AddrRep
  | LabelPtr      !Name !LabelSpec    -- foreign symbol/label name + label sepcification (i.e. data or function)
  deriving (Show, Eq, Ord)

-- TODO: detect coercions during the evaluation
data Atom     -- Q: should atom fit into a cpu register? A: yes
  -- values
  = Literal           !Lit  -- TODO: remove this
  | Void
  | PtrAtom           !PtrOrigin !(Ptr Word8)
  | IntAtom           !Int
  | WordAtom          !Word
  | FloatAtom         !Float
  | DoubleAtom        !Double
  | LiftedUndefined
  -- indirections to store
  | HeapPtr           !HeapAddr
  | MVar              !MVarAddr
  | MutVar            !MutVarAddr
  | Array             !ArrIdx
  | MutableArray      !ArrIdx
  | SmallArray        !SmallArrIdx
  | SmallMutableArray !SmallArrIdx
  | ArrayArray        !ArrayArrIdx
  | MutableArrayArray !ArrayArrIdx
  | ByteArray         !ByteArrayIdx
  | MutableByteArray  !ByteArrayIdx
  | WeakPointer       !WeakPointerAddr
  | StableName        !StableNameAddr
  | ThreadId          !ThreadAddr
  -- lattice for abstract values
  | Lattice           !Lattice
  deriving (Show, Eq, Ord)

data Lattice
  = Top
  deriving (Show, Eq, Ord)

{-
  Atom lattice:
    - concrete atom value
    - set of atoms (optionally limited size)
    - atom type (Q: is this the same as rep type or prim type?)
    - set of atom type (due to unsafe coercions

  IDEA: define lattices for value atoms (i.e. IntAtom) , not indirection atoms (e.g. HeapPtr or Array)
        indirection atoms will have a corresponding lattice in their stores

  Q: how to join two:
      Arrays  A: (single set of elements) or (extend size to fit the bigger array, then join values at each index to a set)
      HeapPtr A: TODO: design the heap object lattice
      IntAtom A: set or type
-}
