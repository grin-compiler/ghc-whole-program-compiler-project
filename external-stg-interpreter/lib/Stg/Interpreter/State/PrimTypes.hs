module Stg.Interpreter.State.PrimTypes where

import Control.Monad.Primitive
import qualified Data.Primitive.ByteArray as BA
import Stg.Interpreter.State.Allocator

data ByteArrayDescriptor
  = ByteArrayDescriptor
  { baaMutableByteArray :: !(BA.MutableByteArray RealWorld)
  , baaByteArray        :: !(Maybe BA.ByteArray)  -- HINT: ByteArray can only be created via unsafeFreeze from a MutableByteArray
  , baaPinned           :: !Bool
  , baaAlignment        :: !Int
  }
instance Show ByteArrayDescriptor where
  show _ = "ByteArrayDescriptor (TODO)"

instance Eq ByteArrayDescriptor where
  _ == _ = True -- TODO

instance Ord ByteArrayDescriptor where
  _ `compare` _ = EQ -- TODO

data WeakPtrDescriptor
  = WeakPtrDescriptor
  { wpdKey          :: AtomAddr
  , wpdValue        :: Maybe AtomAddr -- live or dead
  , wpdFinalizer    :: Maybe AtomAddr -- closure
  , wpdCFinalizers  :: [(AtomAddr, Maybe AtomAddr, AtomAddr)] -- fun, env ptr, data ptr
  }
  deriving (Show, Eq, Ord)

data MVarDescriptor
  = MVarDescriptor
  { mvdValue    :: Maybe AtomAddr
  , mvdQueue    :: [ThreadAddr]   -- thread id, blocking in this mvar ; this is required only for the fairness ; INVARIANT: BlockedOnReads are present at the beginning of the queue
  }
  deriving (Show, Eq, Ord)
