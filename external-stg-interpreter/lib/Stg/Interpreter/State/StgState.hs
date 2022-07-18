module Stg.Interpreter.State.StgState where

import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
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
  { ssHeap                :: !(Map HeapAddr HeapObject)                             -- abs-int: Map (Set HeapObject) | lattice
  , ssStaticGlobalEnv     :: !(Map Id (StaticOrigin, AtomAddr))                     -- NOTE: top level bindings only!
  , ssStack               :: !(Map StackAddr (StackContinuation, Maybe StackAddr))  -- abs-int: Map (Set (StackContinuation, Maybe Int)) | lattice
  , ssAtomStore           :: !(Map AtomAddr Atom)                                   -- abs-int: Map (Set Atom) | lattice

  -- string constants ; models the program memory's static constant region
  -- HINT: the value is a PtrAtom that points to the key BS's content
  , ssCStringConstants    :: Map ByteString AtomAddr

  -- threading
  , ssThreads             :: Map ThreadAddr ThreadState   -- abs-int: Map (Set ...) | lattice

  -- thread scheduler related
  , ssCurrentThreadId     :: ThreadAddr
  , ssScheduledThreadIds  :: [ThreadAddr]  -- HINT: one round

  -- primop related

  , ssStableNameMap       :: Map AtomAddr StableNameAddr                  -- HINT: AtomAddr -> Int ; -- abs-int: Map (Set ...) | lattice
  , ssWeakPointers        :: Map WeakPointerAddr WeakPtrDescriptor        -- abs-int: Map (Set ...) | lattice
  , ssStablePointers      :: Map StablePointerAddr AtomAddr               -- abs-int: Map (Set ...) | lattice
  , ssMutableByteArrays   :: Map MutableByteArrayAddr ByteArrayDescriptor -- abs-int: Map (Set ...) | lattice
  , ssMutVars             :: Map MutVarAddr AtomAddr                      -- abs-int: Map (Set ...) | lattice
  , ssMVars               :: Map MVarAddr MVarDescriptor                  -- abs-int: Map (Set ...) | lattice
  , ssArrays              :: Map ArrayAddr (Vector AtomAddr)              -- abs-int: Map (Set ...) | lattice
  , ssMutableArrays       :: Map MutableArrayAddr (Vector AtomAddr)       -- abs-int: Map (Set ...) | lattice
  , ssSmallArrays         :: Map SmallArrayAddr (Vector AtomAddr)         -- abs-int: Map (Set ...) | lattice
  , ssSmallMutableArrays  :: Map SmallMutableArrayAddr (Vector AtomAddr)  -- abs-int: Map (Set ...) | lattice
  , ssArrayArrays         :: Map ArrayArrayAddr (Vector AtomAddr)         -- abs-int: Map (Set ...) | lattice
  , ssMutableArrayArrays  :: Map MutableArrayArrayAddr (Vector AtomAddr)  -- abs-int: Map (Set ...) | lattice

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
  , ssMutVars             = mempty
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
