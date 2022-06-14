{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, TupleSections #-}
module Stg.Interpreter.Base where

import Data.Word
import Foreign.Ptr
import Control.Monad.State.Strict
import Control.Monad.Loops
import Data.Foldable
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector)
import qualified Data.Primitive.ByteArray as BA
import Control.Monad.Primitive
import System.Posix.DynamicLinker
import Control.Concurrent.MVar
import Control.Concurrent.Chan.Unagi.Bounded
import Foreign.ForeignPtr.Unsafe
import Data.Time.Clock
import System.IO

import GHC.Stack
import Text.Printf
import Debug.Trace
import Stg.Syntax

type StgRhsClosure = Rhs  -- NOTE: must be StgRhsClosure only!

data ArrIdx
  = MutArrIdx !Int
  | ArrIdx    !Int
  deriving (Show, Eq, Ord)

data SmallArrIdx
  = SmallMutArrIdx !Int
  | SmallArrIdx    !Int
  deriving (Show, Eq, Ord)

data ArrayArrIdx
  = ArrayMutArrIdx !Int
  | ArrayArrIdx    !Int
  deriving (Show, Eq, Ord)

data ByteArrayIdx
  = ByteArrayIdx
  { baId        :: !Int
  , baPinned    :: !Bool
  , baAlignment :: !Int
  }
  deriving (Show, Eq, Ord)

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

data PtrOrigin
  = CStringPtr    !ByteString       -- null terminated string
  | ByteArrayPtr  !ByteArrayIdx     -- raw ptr to the byte array
  | RawPtr                          -- raw ptr to a values with unknown origin (i.e. FFI)
  | StablePtr     !Int              -- stable pointer must have AddrRep
  | LabelPtr      !Name !LabelSpec  -- foreign symbol/label name + label sepcification (i.e. data or function)
  deriving (Show, Eq, Ord)

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
  , mvdQueue    :: [Int] -- thread id, blocking in this mvar ; this is required only for the fairness ; INVARIANT: BlockedOnReads are present at the beginning of the queue
  }
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
  | HeapPtr           !Addr
  | MVar              !Int
  | MutVar            !Int
  | Array             !ArrIdx
  | MutableArray      !ArrIdx
  | SmallArray        !SmallArrIdx
  | SmallMutableArray !SmallArrIdx
  | ArrayArray        !ArrayArrIdx
  | MutableArrayArray !ArrayArrIdx
  | ByteArray         !ByteArrayIdx
  | MutableByteArray  !ByteArrayIdx
  | WeakPointer       !Int
  | StableName        !Int
  | ThreadId          !Int
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
  deriving (Show, Eq, Ord)

type ReturnValue = [AtomAddr]

data HeapObject
  = Con
    { hoIsLNE       :: Bool
    , hoCon         :: DataCon
    , hoConArgs     :: [AtomAddr]
    }
  | Closure
    { hoIsLNE       :: Bool
    , hoName        :: Id
    , hoCloBody     :: StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [AtomAddr]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }
  | BlackHole HeapObject
  | ApStack                   -- HINT: needed for the async exceptions
    { hoResult      :: [AtomAddr]
    , hoStack       :: [StackContinuation] -- TODO: replace this with a single stack addr, same idea as in ThreadState.stackTop
    }
  | RaiseException AtomAddr
  deriving (Show, Eq, Ord)

data StackContinuation
  = CaseOf        !Env !Binder !AltType ![Alt]  -- pattern match on the result ; carries the closure's local environment
  | Update        !Addr                         -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply         ![AtomAddr]                   -- apply args on the result heap object
  | Catch         !AtomAddr !Bool !Bool         -- catch frame ; exception handler, block async exceptions, interruptible
  | RestoreExMask !Bool !Bool                   -- saved: block async exceptions, interruptible
  | RunScheduler  !ScheduleReason
  | DataToTagOp
  deriving (Show, Eq, Ord)

data ScheduleReason
  = SR_ThreadFinished
  | SR_ThreadBlocked
  | SR_ThreadYield
  deriving (Show, Eq, Ord)

{-
  Q: do we want homogeneous or heterogeneous Heap ; e.g. single intmap with mixed things or multiple intmaps/vector with multiple address spaces
-}

newtype Printable a = Printable {unPrintable :: a}
instance Show (Printable a) where
  show _ = "Printable"

newtype PrintableMVar a = PrintableMVar {unPrintableMVar :: MVar a} deriving Eq
instance Show (PrintableMVar a) where
  show _ = "MVar"

type StackAddr  = Int
type HeapAddr   = Int
type Addr   = Int
type Heap   = IntMap HeapObject                           -- abs-int: IntMap (Set HeapObject) | lattice
type Env    = Map Id (StaticOrigin, AtomAddr)             -- NOTE: must contain only the defined local variables
type Stack  = IntMap (StackContinuation, Maybe StackAddr) -- abs-int: IntMap (Set (StackContinuation, Maybe Int)) | lattice

type AtomAddr   = Int
type AtomStore  = IntMap Atom                             -- abs-int: IntMap (Set Atom) | lattice

data StaticOrigin
  = SO_CloArg
  | SO_Let
  | SO_Scrut
  | SO_AltArg
  | SO_TopLevel
  | SO_Builtin
  | SO_ClosureResult
  deriving (Show, Eq, Ord)

data StgState
  = StgState
  { ssHeap                :: !Heap
  , ssStaticGlobalEnv     :: !Env   -- NOTE: top level bindings only!
  , ssStack               :: !Stack
  , ssAtomStore           :: !AtomStore

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
  , ssMutVars             :: IntMap AtomAddr            -- abs-int: IntMap (Set ...) | lattice
  , ssArrays              :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssMutableArrays       :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssSmallArrays         :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssSmallMutableArrays  :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssArrayArrays         :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice
  , ssMutableArrayArrays  :: IntMap (Vector AtomAddr)   -- abs-int: IntMap (Set ...) | lattice

  -- allocator related

  , ssNextAtomAddr          :: {-# UNPACK #-} !Int
  , ssNextStackAddr         :: {-# UNPACK #-} !Int
  , ssNextThreadId          :: !Int
  , ssNextHeapAddr          :: {-# UNPACK #-} !Int
  , ssNextStableName        :: !Int
  , ssNextWeakPointer       :: !Int
  , ssNextStablePointer     :: !Int
  , ssNextMutableByteArray  :: !Int
  , ssNextMVar              :: !Int
  , ssNextMutVar            :: !Int
  , ssNextArray             :: !Int
  , ssNextMutableArray      :: !Int
  , ssNextSmallArray        :: !Int
  , ssNextSmallMutableArray :: !Int
  , ssNextArrayArray        :: !Int
  , ssNextMutableArrayArray :: !Int

  -- FFI related
  , ssCBitsMap            :: DL
  , ssStateStore          :: PrintableMVar StgState

  -- RTS related
  , ssRtsSupport          :: Rts

  , ssStgErrorAction      :: Printable (M ())
  }
  deriving (Show)

-- for the primop tests
emptyUndefinedStgState :: StgState
emptyUndefinedStgState = emptyStgState undefined undefined

emptyStgState :: PrintableMVar StgState
              -> DL
              -> StgState
emptyStgState stateStore dl = StgState
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
  , ssMutVars             = mempty
  , ssArrays              = mempty
  , ssMutableArrays       = mempty
  , ssSmallArrays         = mempty
  , ssSmallMutableArrays  = mempty
  , ssArrayArrays         = mempty
  , ssMutableArrayArrays  = mempty

  , ssNextAtomAddr          = 0
  , ssNextStackAddr         = 0
  , ssNextThreadId          = 0
  , ssNextHeapAddr          = 0
  , ssNextStableName        = 0
  , ssNextWeakPointer       = 0
  , ssNextStablePointer     = 0
  , ssNextMutableByteArray  = 0
  , ssNextMVar              = 0
  , ssNextMutVar            = 0
  , ssNextArray             = 0
  , ssNextMutableArray      = 0
  , ssNextSmallArray        = 0
  , ssNextSmallMutableArray = 0
  , ssNextArrayArray        = 0
  , ssNextMutableArrayArray = 0

  -- FFI related
  , ssCBitsMap            = dl
  , ssStateStore          = stateStore

  , ssRtsSupport          = error "uninitialized ssRtsSupport"
  }

data Rts
  = Rts
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

  -- program contants
  , rtsProgName     :: String
  , rtsProgArgs     :: [String]
  }
  deriving (Show)

type M = StateT StgState IO

-- atom operations

freshAtomAddress :: HasCallStack => M AtomAddr
freshAtomAddress = do
  state $ \s@StgState{..} -> (ssNextAtomAddr, s {ssNextAtomAddr = succ ssNextAtomAddr})

storeNewAtom :: Atom -> M AtomAddr
storeNewAtom a = do
  addr <- freshAtomAddress
  modify' $ \s@StgState{..} -> s {ssAtomStore = IntMap.insert addr a ssAtomStore}
  pure addr

getAtom :: AtomAddr -> M Atom
getAtom atomAddr = do
  gets (IntMap.lookup atomAddr . ssAtomStore) >>= \case
    Nothing   -> stgErrorM $ "missing atom at address: " ++ show atomAddr
    Just atom -> pure atom

getAtoms :: [AtomAddr] -> M [Atom]
getAtoms = mapM getAtom

allocAtoms :: [Atom] -> M [AtomAddr]
allocAtoms = mapM storeNewAtom

-- stack operations

freshStackAddress :: HasCallStack => M Addr
freshStackAddress = do
  state $ \s@StgState{..} -> (ssNextStackAddr, s {ssNextStackAddr = succ ssNextStackAddr})

getStackFrame :: Addr -> M (StackContinuation, Maybe Addr)
getStackFrame stackAddr = do
  gets (IntMap.lookup stackAddr . ssStack) >>= \case
    Nothing     -> stgErrorM $ "missing stack frame at address: " ++ show stackAddr
    Just frame  -> pure frame

stackPush :: StackContinuation -> M ()
stackPush sc = do
  a <- freshStackAddress
  cts <- getCurrentThreadState
  let pushFun ts@ThreadState{..} = ts {tsStackTop = Just a}
      stackFrame = (sc, tsStackTop cts)
  modify' $ \s@StgState{..} -> s
    { ssThreads = IntMap.adjust pushFun ssCurrentThreadId ssThreads
    , ssStack = IntMap.insert a stackFrame ssStack
    }

stackPop :: M (Maybe StackContinuation)
stackPop = do
  cts <- getCurrentThreadState
  case tsStackTop cts of
    Nothing -> pure Nothing
    Just oldStackTop -> do
      (sc, newStackTop) <- getStackFrame oldStackTop
      let popFun ts@ThreadState{..} = ts {tsStackTop = newStackTop}
      modify' $ \s@StgState{..} -> s
        { ssThreads = IntMap.adjust popFun ssCurrentThreadId ssThreads
        }
      pure $ Just sc

mkStack :: Maybe Addr -> [StackContinuation] -> M (Maybe Addr)
mkStack prevFrameAddr frames = do
  let pushFrame stackTop sc = do
        a <- freshStackAddress
        let stackFrame = (sc, stackTop)
        modify' $ \s@StgState{..} -> s {ssStack = IntMap.insert a stackFrame ssStack}
        pure $ Just a
  foldlM pushFrame prevFrameAddr frames

-- heap operations

freshHeapAddress :: HasCallStack => M Addr
freshHeapAddress = do
  state $ \s@StgState{..} -> (ssNextHeapAddr, s {ssNextHeapAddr = succ ssNextHeapAddr})

allocAndStore :: HasCallStack => HeapObject -> M Addr
allocAndStore o = do
  a <- freshHeapAddress
  store a o
  pure a

store :: HasCallStack => Addr -> HeapObject -> M ()
store a o = do
  modify' $ \s@StgState{..} -> s { ssHeap = IntMap.insert a o ssHeap }

  {-
  gets ssTracingState >>= \case
    NoTracing   -> pure ()
    DoTracing h -> do
      origin <- gets ssCurrentClosureAddr
      liftIO $ hPutStrLn h $ show a ++ "\t" ++ show origin
  -}

{-
  conclusion:
    write origins to file to save memory
    use binary format to save space

  TODO: implement GC
          - simple (full heap traversal)
          - generational
-}

stgErrorM :: String -> M a
stgErrorM msg = do
  tid <- gets ssCurrentThreadId
  liftIO $ putStrLn $ " * stgErrorM: " ++ show msg
  liftIO $ putStrLn $ "current thread id: " ++ show tid
  reportThread tid
  action <- unPrintable <$> gets ssStgErrorAction
  action
  error "stgErrorM"

addBinderToEnv :: StaticOrigin -> Binder -> AtomAddr -> Env -> Env
addBinderToEnv so b a = Map.insert (Id b) (so, a)

addZippedBindersToEnv :: StaticOrigin -> [(Binder, AtomAddr)] -> Env -> Env
addZippedBindersToEnv so bvList env = foldl' (\e (b, v) -> Map.insert (Id b) (so, v) e) env bvList

addManyBindersToEnv :: StaticOrigin -> [Binder] -> [AtomAddr] -> Env -> Env
addManyBindersToEnv so binders values = addZippedBindersToEnv so $ zip binders values

lookupEnvSO :: HasCallStack => Env -> Binder -> M (StaticOrigin, AtomAddr)
lookupEnvSO localEnv b = do
  env <- if binderTopLevel b
          then gets ssStaticGlobalEnv
          else pure localEnv
  case Map.lookup (Id b) env of
    Just a  -> pure a
    Nothing -> case binderUniqueName b of
      -- HINT: GHC.Prim module does not exist it's a wired in module
      "ghc-prim_GHC.Prim.void#"           -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.realWorld#"      -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.coercionToken#"  -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.proxy#"          -> (SO_Builtin,) <$> storeNewAtom Void
      "ghc-prim_GHC.Prim.(##)"            -> (SO_Builtin,) <$> storeNewAtom Void
      _ -> stgErrorM $ "unknown variable: " ++ show b

lookupEnv :: HasCallStack => Env -> Binder -> M AtomAddr
lookupEnv localEnv b = snd <$> lookupEnvSO localEnv b

readHeap :: HasCallStack => Atom -> M HeapObject
readHeap (HeapPtr l) = do
  h <- gets ssHeap
  case IntMap.lookup l h of
    Nothing -> stgErrorM $ "unknown heap address: " ++ show l
    Just o  -> pure o
readHeap v = error $ "readHeap: could not read heap object: " ++ show v

readHeapCon :: HasCallStack => Atom -> M HeapObject
readHeapCon a = readHeap a >>= \o -> case o of
    Con{} -> pure o
    _     -> stgErrorM $ "expected con but got: "-- ++ show o

readHeapClosure :: HasCallStack => Atom -> M HeapObject
readHeapClosure a = readHeap a >>= \o -> case o of
    Closure{} -> pure o
    _ -> stgErrorM $ "expected closure but got: "-- ++ show o

-- primop related

type PrimOpEval = Name -> [AtomAddr] -> Type -> Maybe TyCon -> M [AtomAddr]
type EvalOnNewThread = M [AtomAddr] -> M [AtomAddr]

lookupWeakPointerDescriptor :: HasCallStack => Int -> M WeakPtrDescriptor
lookupWeakPointerDescriptor wpId = do
  IntMap.lookup wpId <$> gets ssWeakPointers >>= \case
    Nothing -> stgErrorM $ "unknown WeakPointer: " ++ show wpId
    Just a  -> pure a

lookupStablePointerPtr :: HasCallStack => Ptr Word8 -> M AtomAddr
lookupStablePointerPtr sp = do
  let IntPtr spId = ptrToIntPtr sp
  lookupStablePointer spId

lookupStablePointer :: HasCallStack => Int -> M AtomAddr
lookupStablePointer spId = do
  IntMap.lookup spId <$> gets ssStablePointers >>= \case
    Nothing -> stgErrorM $ "unknown StablePointer: " ++ show spId
    Just a  -> pure a

lookupMutVar :: HasCallStack => Int -> M AtomAddr
lookupMutVar m = do
  IntMap.lookup m <$> gets ssMutVars >>= \case
    Nothing -> stgErrorM $ "unknown MutVar: " ++ show m
    Just a  -> pure a

lookupMVar :: HasCallStack => Int -> M MVarDescriptor
lookupMVar m = do
  IntMap.lookup m <$> gets ssMVars >>= \case
    Nothing -> stgErrorM $ "unknown MVar: " ++ show m
    Just a  -> pure a

lookupArray :: HasCallStack => Int -> M (Vector AtomAddr)
lookupArray m = do
  IntMap.lookup m <$> gets ssArrays >>= \case
    Nothing -> stgErrorM $ "unknown Array: " ++ show m
    Just a  -> pure a

lookupMutableArray :: HasCallStack => Int -> M (Vector AtomAddr)
lookupMutableArray m = do
  IntMap.lookup m <$> gets ssMutableArrays >>= \case
    Nothing -> stgErrorM $ "unknown MutableArray: " ++ show m
    Just a  -> pure a

lookupSmallArray :: HasCallStack => Int -> M (Vector AtomAddr)
lookupSmallArray m = do
  IntMap.lookup m <$> gets ssSmallArrays >>= \case
    Nothing -> stgErrorM $ "unknown SmallArray: " ++ show m
    Just a  -> pure a

lookupSmallMutableArray :: HasCallStack => Int -> M (Vector AtomAddr)
lookupSmallMutableArray m = do
  IntMap.lookup m <$> gets ssSmallMutableArrays >>= \case
    Nothing -> stgErrorM $ "unknown SmallMutableArray: " ++ show m
    Just a  -> pure a

lookupArrayArray :: HasCallStack => Int -> M (Vector AtomAddr)
lookupArrayArray m = do
  IntMap.lookup m <$> gets ssArrayArrays >>= \case
    Nothing -> stgErrorM $ "unknown ArrayArray: " ++ show m
    Just a  -> pure a

lookupMutableArrayArray :: HasCallStack => Int -> M (Vector AtomAddr)
lookupMutableArrayArray m = do
  IntMap.lookup m <$> gets ssMutableArrayArrays >>= \case
    Nothing -> stgErrorM $ "unknown MutableArrayArray: " ++ show m
    Just a  -> pure a

lookupByteArrayDescriptor :: HasCallStack => Int -> M ByteArrayDescriptor
lookupByteArrayDescriptor m = do
  IntMap.lookup m <$> gets ssMutableByteArrays >>= \case
    Nothing -> stgErrorM $ "unknown ByteArrayDescriptor: " ++ show m
    Just a  -> pure a

lookupByteArrayDescriptorI :: HasCallStack => ByteArrayIdx -> M ByteArrayDescriptor
lookupByteArrayDescriptorI = lookupByteArrayDescriptor . baId

{-# NOINLINE liftIOAndBorrowStgState #-}
liftIOAndBorrowStgState :: HasCallStack => IO a -> M a
liftIOAndBorrowStgState action = do
  stateStore <- gets $ unPrintableMVar . ssStateStore
  -- HINT: remember the local thread id
  myThread <- gets ssCurrentThreadId
  before <- get
  (result, after) <- liftIO $ do
    -- save current state
    putMVar stateStore before
    -- execute acition
    r <- action
    -- load the state back
    s <- takeMVar stateStore
    pure (r, s)

  put after
  -- HINT: continue the local thread
  switchToThread myThread
  pure result

-- string constants
-- NOTE: the string gets extended with a null terminator
getCStringConstantPtrAtom :: ByteString -> M AtomAddr
getCStringConstantPtrAtom key = do
  strMap <- gets ssCStringConstants
  case Map.lookup key strMap of
    Just a  -> pure a
    Nothing -> do
      let bsCString = BS8.snoc key '\0'
          (bsFPtr, bsOffset, _bsLen) = BS.toForeignPtr bsCString
          a = PtrAtom (CStringPtr bsCString) $ plusPtr (unsafeForeignPtrToPtr bsFPtr) bsOffset
      addr <- storeNewAtom a
      modify' $ \s -> s {ssCStringConstants = Map.insert key addr strMap}
      pure addr

---------------------------------------------
-- threading

data AsyncExceptionMask
  = NotBlocked
  | Blocked     {isInterruptible :: !Bool}
  deriving (Eq, Ord, Show)

data ThreadState
  = ThreadState
  { tsCurrentResult     :: [AtomAddr] -- Q: do we need this? A: yes, i.e. MVar read primops can write this after unblocking the thread
  , tsStackTop          :: Maybe StackAddr
  , tsStatus            :: !ThreadStatus
  , tsBlockedExceptions :: [Int] -- ids of the threads waitng to send an async exception
  , tsBlockExceptions   :: !Bool  -- block async exceptions
  , tsInterruptible     :: !Bool  -- interruptible blocking of async exception
--  , tsAsyncExMask     :: !AsyncExceptionMask
  , tsBound             :: !Bool
  , tsLocked            :: !Bool  -- Q: what is this for? is this necessary?
  , tsCapability        :: !Int   -- NOTE: the thread is running on this capability ; Q: is this necessary?
  , tsLabel             :: !(Maybe ByteString)
  }
  deriving (Eq, Ord, Show)

-- thread operations

createThread :: M (Int, ThreadState)
createThread = do
  let ts = ThreadState
        { tsCurrentResult     = []
        , tsStackTop          = Nothing
        , tsStatus            = ThreadRunning
        , tsBlockedExceptions = []
        , tsBlockExceptions   = False
        , tsInterruptible     = False
        , tsBound             = False
        , tsLocked            = False
        , tsCapability        = 0 -- TODO: implement capability handling
        , tsLabel             = Nothing
        }
  threads <- gets ssThreads
  threadId <- gets ssNextThreadId
  modify' $ \s -> s {ssThreads = IntMap.insert threadId ts threads, ssNextThreadId = succ threadId}
  pure (threadId, ts)

updateThreadState :: Int -> ThreadState -> M ()
updateThreadState tid ts = do
  modify' $ \s@StgState{..} -> s {ssThreads = IntMap.insert tid ts ssThreads}

getThreadState :: HasCallStack => Int -> M ThreadState
getThreadState tid = do
  IntMap.lookup tid <$> gets ssThreads >>= \case
    Nothing -> stgErrorM $ "unknown ThreadState: " ++ show tid
    Just a  -> pure a

getCurrentThreadState :: M ThreadState
getCurrentThreadState = do
  tid <- gets ssCurrentThreadId
  getThreadState tid

switchToThread :: Int -> M () -- TODO: check what code uses this
switchToThread tid = do
  modify' $ \s -> s {ssCurrentThreadId = tid}
{-
  used by:
    FFI.hs:   ffiCallbackBridge
    Base.hs:  liftIOAndBorrowStgState
-}
{-
insertThread :: Int -> ThreadState -> M ()
insertThread = updateThreadState
-}
-- NOTE: only fork# and forkOn# uses requestContextSwitch
requestContextSwitch :: M ()
requestContextSwitch = do
  -- NOTE: the semantics does not require immediate yielding, some latency is allowed
  --        for simplicity we yield immediately
  stackPush $ RunScheduler SR_ThreadYield
{-
  used by:
    fork#   - yield
    forkOn# - yield
-}

scheduleToTheEnd :: Int -> M ()
scheduleToTheEnd tid = do
  modify' $ \s -> s {ssScheduledThreadIds = ssScheduledThreadIds s ++ [tid]}

{-
  used by:
    takeMVar#   - block
    putMVar#    - block
    readMVar#   - block
    delay#      - block
    waitRead#   - block
    waitWrite#  - block
    yield#      - yield

    fork#       - yield (adds the new thread)
    forkOn#     - yield (adds the new thread)
-}

{-
  scheduler operations:
    block
    yield
    finished

  Q: how to add a newly created thread?
     manually or via return-to-scheduler op?

TODO:
  distinct immediate reschedule and relaxed (soonish) context switch
NOTE:
  on the native stg machine the closure/basic block entry point allocates memory, so it is a safe point for context switch
  in the native code the contect switch happens at safe points
-}

--------------

-- NOTE: the BlockReason data type is some kind of reification of the blocked operation
data BlockReason
  = BlockedOnMVar         Int (Maybe AtomAddr) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int       -- mvar id
  | BlockedOnBlackHole
  | BlockedOnThrowAsyncEx Int AtomAddr  -- target thread id, exception
  | BlockedOnSTM
  | BlockedOnForeignCall            -- RTS name: BlockedOnCCall
  | BlockedOnRead         Int       -- file descriptor
  | BlockedOnWrite        Int       -- file descriptor
  | BlockedOnDelay        UTCTime   -- target time to wake up thread
  deriving (Eq, Ord, Show)

data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished  -- RTS name: ThreadComplete
  | ThreadDied      -- RTS name: ThreadKilled
  deriving (Eq, Ord, Show)

isThreadLive :: ThreadStatus -> Bool
isThreadLive = \case
  ThreadFinished  -> False
  ThreadDied      -> False
  _ -> True

{-
threadStatus :: ThreadId -> IO ThreadStatus
threadStatus (ThreadId t) = IO $ \s ->
   case threadStatus# t s of
    (# s', stat, _cap, _locked #) -> (# s', mk_stat (I# stat) #)
   where
        -- NB. keep these in sync with includes/rts/Constants.h
     mk_stat 0  = ThreadRunning
     mk_stat 1  = ThreadBlocked BlockedOnMVar
     mk_stat 2  = ThreadBlocked BlockedOnBlackHole
     mk_stat 6  = ThreadBlocked BlockedOnSTM
     mk_stat 10 = ThreadBlocked BlockedOnForeignCall
     mk_stat 11 = ThreadBlocked BlockedOnForeignCall
     mk_stat 12 = ThreadBlocked BlockedOnException
     mk_stat 14 = ThreadBlocked BlockedOnMVar -- possibly: BlockedOnMVarRead
     -- NB. these are hardcoded in rts/PrimOps.cmm
     mk_stat 16 = ThreadFinished
     mk_stat 17 = ThreadDied
     mk_stat _  = ThreadBlocked BlockedOnOther
-}

{-
data BlockedStatus
  = NotBlocked
  | BlockedOnMVar
  | BlockedOnMVarRead
  | BlockedOnBlackHole
  | BlockedOnRead
  | BlockedOnWrite
  | BlockedOnDelay
  | BlockedOnSTM
  -- Win32 only
  | BlockedOnDoProc
  -- Only relevant for THREADED_RTS
  | BlockedOnCCall
  | BlockedOnCCall_Interruptible

  -- Involved in a message sent to tso->msg_cap
  | BlockedOnMsgThrowTo
  | ThreadMigrating
-}

{-
#define NotBlocked          0
#define BlockedOnMVar       1
#define BlockedOnMVarRead   14 /* TODO: renumber me, see #9003 */
#define BlockedOnBlackHole  2
#define BlockedOnRead       3
#define BlockedOnWrite      4
#define BlockedOnDelay      5
#define BlockedOnSTM        6

/* Win32 only: */
#define BlockedOnDoProc     7

/* Only relevant for THREADED_RTS: */
#define BlockedOnCCall      10
#define BlockedOnCCall_Interruptible 11
   /* same as above but permit killing the worker thread */

/* Involved in a message sent to tso->msg_cap */
#define BlockedOnMsgThrowTo 12

/* The thread is not on any run queues, but can be woken up
   by tryWakeupThread() */
#define ThreadMigrating     13

-}
{-
/*
 * Constants for the what_next field of a TSO, which indicates how it
 * is to be run.
 */
#define ThreadRunGHC    1       /* return to address on top of stack */
#define ThreadInterpret 2       /* interpret this thread */
#define ThreadKilled    3       /* thread has died, don't run it */
#define ThreadComplete  4       /* thread has finished */
-}

reportThreads :: M ()
reportThreads = do
  threadIds <- IntMap.keys <$> gets ssThreads
  liftIO $ putStrLn $ "thread Ids: " ++ show threadIds
  mapM_ reportThread threadIds

reportThread :: Int -> M ()
reportThread tid = do
  endTS <- getThreadState tid
  tsStack <- getThreadStack tid
  liftIO $ reportThreadIO tid endTS tsStack

getThreadStack :: Int -> M [StackContinuation]
getThreadStack tid = do
  ThreadState{..} <- getThreadState tid
  flip unfoldrM tsStackTop $ \case
    Nothing       -> pure Nothing
    Just stackTop -> Just <$> getStackFrame stackTop

reportThreadIO :: Int -> ThreadState -> [StackContinuation] -> IO ()
reportThreadIO tid endTS tsStack = do
    putStrLn ""
    putStrLn $ show ("tid", tid, "tsStatus", tsStatus endTS)
    putStrLn "stack:"
    putStrLn $ unlines $ map show $ zip [0..] $ map showStackCont tsStack
    putStrLn ""

showStackCont :: StackContinuation -> String
showStackCont = \case
  CaseOf _ b _ _ -> "CaseOf, result var: " ++ show (Id b)
  c -> show c
