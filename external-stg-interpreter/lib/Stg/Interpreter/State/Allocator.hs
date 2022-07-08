module Stg.Interpreter.State.Allocator where

import Stg.Syntax

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
  , ssNextMVar              = 0
  , ssNextArray             = 0
  , ssNextMutableArray      = 0
  , ssNextSmallArray        = 0
  , ssNextSmallMutableArray = 0
  , ssNextArrayArray        = 0
  , ssNextMutableArrayArray = 0
  }

data Addr
  = AddrInt         Int -- NOTE: only for the pretty printer
  | AddrId          Id
{-
  | AddrIdExp       {addrId :: String, addrExp :: Exp}
  | AddrIdExpKAddr  {addrId :: String, addrExp :: Exp, addrReturn :: KAddr}
  | AddrIdKAddr     {addrId :: String, addrReturn :: KAddr}
  | AddrExp         {addrExp :: Exp}
  | AddrExpEnv      {addrExp :: Exp, addrEnv :: Env}
  | AddrAAC         Exp Env Exp Env Store
  | AddrHalt
-}
  deriving (Show, Eq, Ord)

newtype AtomAddr              = AtomAddr              Addr deriving (Show, Eq, Ord)
newtype StackAddr             = StackAddr             Addr deriving (Show, Eq, Ord)
newtype HeapAddr              = HeapAddr              Addr deriving (Show, Eq, Ord)
newtype ThreadAddr            = ThreadAddr            Addr deriving (Show, Eq, Ord)
newtype StableNameAddr        = StableNameAddr        Addr deriving (Show, Eq, Ord)
newtype StablePointerAddr     = StablePointerAddr     Addr deriving (Show, Eq, Ord)
newtype WeakPointerAddr       = WeakPointerAddr       Addr deriving (Show, Eq, Ord)
newtype MutableByteArrayAddr  = MutableByteArrayAddr  Addr deriving (Show, Eq, Ord)
newtype MVarAddr              = MVarAddr              Addr deriving (Show, Eq, Ord)
newtype ArrayAddr             = ArrayAddr             Addr deriving (Show, Eq, Ord)
newtype MutableArrayAddr      = MutableArrayAddr      Addr deriving (Show, Eq, Ord)
newtype SmallArrayAddr        = SmallArrayAddr        Addr deriving (Show, Eq, Ord)
newtype SmallMutableArrayAddr = SmallMutableArrayAddr Addr deriving (Show, Eq, Ord)
newtype ArrayArrayAddr        = ArrayArrayAddr        Addr deriving (Show, Eq, Ord)
newtype MutableArrayArrayAddr = MutableArrayArrayAddr Addr deriving (Show, Eq, Ord)
newtype MutVarAddr            = MutVarAddr            Addr deriving (Show, Eq, Ord)
