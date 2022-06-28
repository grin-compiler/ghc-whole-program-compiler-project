module Stg.Interpreter.State.StgState where

import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Vector (Vector)

import Stg.Syntax
import Stg.Interpreter.State.Allocator
import Stg.Interpreter.State.Atom
import Stg.Interpreter.State.PrimTypes
import Stg.Interpreter.State.Stack
import Stg.Interpreter.State.Heap
import Stg.Interpreter.State.Env
import Stg.Interpreter.State.Thread
import Stg.Interpreter.State.Rts

type ReturnValue = [AtomAddr]


{-
  Q: do we want homogeneous or heterogeneous Heap ; e.g. single intmap with mixed things or multiple intmaps/vector with multiple address spaces
-}
{-
newtype Printable a = Printable {unPrintable :: a}
instance Show (Printable a) where
  show _ = "Printable"
-}
{-
newtype Store a = Store (Int, IntMap a)
  deriving (Show, Functor)
-}
data StgState
  = StgState
  { ssHeap                :: !(IntMap HeapObject)                           -- abs-int: IntMap (Set HeapObject) | lattice
  , ssStaticGlobalEnv     :: !(Map Id (StaticOrigin, AtomAddr))             -- NOTE: top level bindings only!
  , ssStack               :: !(IntMap (StackContinuation, Maybe StackAddr)) -- abs-int: IntMap (Set (StackContinuation, Maybe Int)) | lattice
  , ssAtomStore           :: !(IntMap Atom)                                 -- abs-int: IntMap (Set Atom) | lattice

  -- string constants ; models the program memory's static constant region
  -- HINT: the value is a PtrAtom that points to the key BS's content
  , ssCStringConstants    :: Map ByteString AtomAddr

  -- threading
  , ssThreads             :: IntMap ThreadState         -- abs-int: IntMap (Set ...) | lattice

  -- thread scheduler related
  , ssCurrentThreadId     :: Int
  , ssScheduledThreadIds  :: [Int]  -- HINT: one round

  -- primop related

  , ssStableNameMap       :: IntMap Int                 -- HINT: AtomAddr -> Int ; -- abs-int: IntMap (Set ...) | lattice
  , ssWeakPointers        :: IntMap WeakPtrDescriptor   -- abs-int: IntMap (Set ...) | lattice
  , ssStablePointers      :: IntMap AtomAddr            -- abs-int: IntMap (Set ...) | lattice
  , ssMutableByteArrays   :: IntMap ByteArrayDescriptor -- abs-int: IntMap (Set ...) | lattice
  , ssMVars               :: IntMap MVarDescriptor      -- abs-int: IntMap (Set ...) | lattice
  , ssArrays              :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssMutableArrays       :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssSmallArrays         :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssSmallMutableArrays  :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssArrayArrays         :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssMutableArrayArrays  :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice

  -- allocator related
  , ssAllocator           :: !AllocatorState

  -- RTS related
  , ssRtsBaseInterop      :: RtsBaseInterop
  , ssRtsStaticEnv        :: RtsStaticEnv

--  , ssStgErrorAction      :: Printable (M ())
  }
  deriving (Show)

-- for the primop tests
emptyUndefinedStgState :: StgState
emptyUndefinedStgState = emptyStgState

emptyStgState :: StgState
emptyStgState = StgState
  { ssHeap                = mempty
  , ssStaticGlobalEnv     = mempty
  , ssStack               = mempty
  , ssAtomStore           = mempty

  , ssCStringConstants    = mempty

  -- threading
  , ssThreads             = mempty
  , ssCurrentThreadId     = error "uninitialized ssCurrentThreadId"
  , ssScheduledThreadIds  = []

  -- primop related
  , ssStableNameMap       = mempty
  , ssWeakPointers        = mempty
  , ssStablePointers      = mempty
  , ssMutableByteArrays   = mempty
  , ssMVars               = mempty
  , ssArrays              = mempty
  , ssMutableArrays       = mempty
  , ssSmallArrays         = mempty
  , ssSmallMutableArrays  = mempty
  , ssArrayArrays         = mempty
  , ssMutableArrayArrays  = mempty

  , ssAllocator           = emptyAllocatorState
  , ssRtsBaseInterop      = error "uninitialized ssRtsBaseInterop"
  , ssRtsStaticEnv        = error "uninitialized ssRtsStaticEnv"
  }
