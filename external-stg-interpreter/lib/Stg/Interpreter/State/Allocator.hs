module Stg.Interpreter.State.Allocator where

data AllocatorState
  = AllocatorState
  { ssNextAtomAddr          :: {-# UNPACK #-} !Int
  , ssNextStackAddr         :: {-# UNPACK #-} !Int
  , ssNextThreadId          :: !Int
  , ssNextHeapAddr          :: {-# UNPACK #-} !Int
  , ssNextStableName        :: !Int
  , ssNextWeakPointer       :: !Int
  , ssNextStablePointer     :: !Int
  , ssNextMutableByteArray  :: !Int
  , ssNextMutVar            :: !Int
  , ssNextMVar              :: !Int
  , ssNextArray             :: !Int
  , ssNextMutableArray      :: !Int
  , ssNextSmallArray        :: !Int
  , ssNextSmallMutableArray :: !Int
  , ssNextArrayArray        :: !Int
  , ssNextMutableArrayArray :: !Int
  }
  deriving (Show)

emptyAllocatorState :: AllocatorState
emptyAllocatorState = AllocatorState
  { ssNextAtomAddr          = 0
  , ssNextStackAddr         = 0
  , ssNextThreadId          = 0
  , ssNextHeapAddr          = 0
  , ssNextStableName        = 0
  , ssNextWeakPointer       = 0
  , ssNextStablePointer     = 0
  , ssNextMutableByteArray  = 0
  , ssNextMutVar            = 0
  , ssNextMVar              = 0
  , ssNextArray             = 0
  , ssNextMutableArray      = 0
  , ssNextSmallArray        = 0
  , ssNextSmallMutableArray = 0
  , ssNextArrayArray        = 0
  , ssNextMutableArrayArray = 0
  }
