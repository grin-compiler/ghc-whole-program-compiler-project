module Stg.Interpreter.State.Rts where

import Data.Map (Map)

import Stg.Syntax
import Stg.Interpreter.State.Address

data RtsBaseInterop
  = RtsBaseInterop
  -- data constructors needed for FFI argument boxing from the base library
  { rtsCharCon      :: DataCon
  , rtsIntCon       :: DataCon
  , rtsInt8Con      :: DataCon
  , rtsInt16Con     :: DataCon
  , rtsInt32Con     :: DataCon
  , rtsInt64Con     :: DataCon
  , rtsWordCon      :: DataCon
  , rtsWord8Con     :: DataCon
  , rtsWord16Con    :: DataCon
  , rtsWord32Con    :: DataCon
  , rtsWord64Con    :: DataCon
  , rtsPtrCon       :: DataCon
  , rtsFunPtrCon    :: DataCon
  , rtsFloatCon     :: DataCon
  , rtsDoubleCon    :: DataCon
  , rtsStablePtrCon :: DataCon
  , rtsTrueCon      :: DataCon
  , rtsFalseCon     :: DataCon

  -- closures used by FFI wrapper code ; heap address of the closure
  , rtsUnpackCString              :: AtomAddr
  , rtsTopHandlerRunIO            :: AtomAddr
  , rtsTopHandlerRunNonIO         :: AtomAddr
  , rtsTopHandlerFlushStdHandles  :: AtomAddr

  -- closures used by the exception primitives
  , rtsDivZeroException   :: AtomAddr
  , rtsUnderflowException :: AtomAddr
  , rtsOverflowException  :: AtomAddr

  -- rts helper custom closures
  , rtsApplyFun1Arg :: AtomAddr
  , rtsTuple2Proj0  :: AtomAddr

  -- builtin special store, see FFI (i.e. getOrSetGHCConcSignalSignalHandlerStore)
  , rtsGlobalStore  :: Map Name AtomAddr
  }
  deriving (Show)

data RtsStaticEnv
  = RtsStaticEnv
  -- program contants
  { rtsProgName   :: String
  , rtsProgArgs   :: [String]
  }
  deriving (Show)
