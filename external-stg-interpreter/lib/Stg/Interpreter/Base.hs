{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.Base where

import Data.Word
import Foreign.Ptr
import Control.Monad.State.Strict
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import Data.Vector (Vector)
import qualified Data.Primitive.ByteArray as BA
import Control.Monad.Primitive
import System.Posix.DynamicLinker
import Control.Concurrent.MVar
import Foreign.ForeignPtr.Unsafe
import Data.Time.Clock

import GHC.Stack
import Text.Printf
import Debug.Trace
import Stg.Syntax

newtype Id = Id {unId :: Binder}

instance Eq Id where
  (Id a) == (Id b) = binderUniqueName a == binderUniqueName b -- FIXME: make this fast

instance Ord Id where
  compare (Id a) (Id b) = compare (binderUniqueName a) (binderUniqueName b) -- FIXME: make this fast

instance Show Id where
  show (Id a) = BS8.unpack $ binderUniqueName a

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
  deriving (Show, Eq, Ord)

data WeakPtrDescriptor
  = WeakPtrDescriptor
  { wpdKey          :: Atom
  , wpdVale         :: Maybe Atom -- live or dead
  , wpdFinalizer    :: Maybe Atom -- closure
  , wpdCFinalizers  :: [(Atom, Maybe Atom, Atom)] -- fun, env ptr, data ptr
  }
  deriving (Show, Eq, Ord)

data MVarDescriptor
  = MVarDescriptor
  { mvdValue    :: Maybe Atom
  , mvdQueue    :: [Int] -- thread id, blocking in this mvar ; this is required only for the fairness ; INVARIANT: BlockedOnReads are present at the beginning of the queue
  }
  deriving (Show, Eq, Ord)

-- TODO: detect coercions during the evaluation
data Atom     -- Q: should atom fit into a cpu register? A: yes
  = HeapPtr       !Addr
  | Literal       !Lit  -- TODO: remove this
  | Void
  | PtrAtom       !PtrOrigin !(Ptr Word8)
  | IntAtom       !Int
  | WordAtom      !Word
  | FloatAtom     !Float
  | DoubleAtom    !Double
  | MVar          !Int
  | MutVar        !Int
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
  | LiftedUndefined
  deriving (Show, Eq, Ord)

type ReturnValue = [Atom]

data HeapObject
  = Con
    { hoCon         :: DataCon
    , hoConArgs     :: [Atom]
    }
  | Closure
    { hoName        :: Id
    , hoCloBody     :: StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [Atom]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }
  | BlackHole HeapObject
  deriving (Show, Eq, Ord)

data StackContinuation
  = CaseOf  !Env !Binder !AltType ![Alt]  -- pattern match on the result ; carries the closure's local environment
  | Update  !Addr                         -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply   ![Atom]                       -- apply args on the result heap object
  | Catch   !Atom !Bool !Bool             -- catch frame ; exception handler, block async exceptions, interruptible
  | RestoreExMask !Bool !Bool             -- saved: block async exceptions, interruptible
  deriving (Show, Eq, Ord)

type Addr   = Int
type Heap   = IntMap HeapObject
type Env    = Map Id Atom   -- NOTE: must contain only the defined local variables
type Stack  = [StackContinuation]

{-
  Q: do we want homogeneous or heterogeneous Heap ; e.g. single intmap with mixed things or multiple intmaps/vector with multiple address spaces
-}

newtype PrintableMVar a = PrintableMVar {unPrintableMVar :: MVar a} deriving Eq
instance Show (PrintableMVar a) where
  show _ = "MVar"

data StgState
  = StgState
  { ssHeap                :: !Heap
  , ssStaticGlobalEnv     :: !Env   -- NOTE: top level bindings only!
  , ssEvalStack :: [Id]
  , ssNextAddr  :: !Int

  -- string constants ; models the program memory's static constant region
  -- HINT: the value is a PtrAtom that points to the key BS's content
  , ssCStringConstants    :: Map ByteString Atom

  -- threading
  , ssThreads             :: IntMap ThreadState
  , ssCurrentThreadId     :: Int
  , ssScheduledThreadIds  :: [Int]  -- HINT: one round

  -- primop related

  , ssStableNameMap       :: Map Atom Int
  , ssWeakPointers        :: IntMap WeakPtrDescriptor
  , ssStablePointers      :: IntMap Atom
  , ssMutableByteArrays   :: IntMap ByteArrayDescriptor
  , ssMVars               :: IntMap MVarDescriptor
  , ssMutVars             :: IntMap Atom
  , ssArrays              :: IntMap (Vector Atom)
  , ssMutableArrays       :: IntMap (Vector Atom)
  , ssSmallArrays         :: IntMap (Vector Atom)
  , ssSmallMutableArrays  :: IntMap (Vector Atom)
  , ssArrayArrays         :: IntMap (Vector Atom)
  , ssMutableArrayArrays  :: IntMap (Vector Atom)

  -- FFI related
  , ssCBitsMap            :: DL
  , ssStateStore          :: PrintableMVar StgState

  , ssRtsSupport          :: Rts

  -- debug
  , ssExecutedClosures    :: Set Int
  , ssExecutedPrimOps     :: Set Name
  , ssExecutedFFI         :: Set ForeignCall
  , ssAddressAfterInit    :: Int
  }
  deriving (Show)

emptyStgState :: PrintableMVar StgState -> DL -> StgState
emptyStgState stateStore dl = StgState
  { ssHeap                = mempty
  , ssStaticGlobalEnv     = mempty
  , ssEvalStack           = []
  , ssNextAddr            = 0

  , ssCStringConstants    = mempty

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

  -- FFI related
  , ssCBitsMap            = dl
  , ssStateStore          = stateStore

  , ssRtsSupport          = undefined
  , ssExecutedClosures    = Set.empty
  , ssExecutedPrimOps     = Set.empty
  , ssExecutedFFI         = Set.empty
  , ssAddressAfterInit    = 0
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
  , rtsUnpackCString      :: Atom
  , rtsTopHandlerRunIO    :: Atom
  , rtsTopHandlerRunNonIO :: Atom

  -- closures used by the exception primitives
  , rtsDivZeroException   :: Atom
  , rtsUnderflowException :: Atom
  , rtsOverflowException  :: Atom

  -- rts helper custom closures
  , rtsApply2Args   :: Atom
  , rtsTuple2Proj0  :: Atom
  }
  deriving (Show)

type M = StateT StgState IO

-- thread operations

createThread :: M (Int, ThreadState)
createThread = do
  let ts = ThreadState
        { tsCurrentResult   = []
        , tsStack           = []
        , tsStatus          = ThreadRunning
        , tsBlockExceptions = False
        , tsInterruptible   = True
        , tsBound           = False
        , tsLocked          = False
        , tsCapability      = 0 -- TODO: implement capability handling
        , tsLabel           = Nothing
        }
  threads <- gets ssThreads
  let threadId  = IntMap.size threads
  modify' $ \s -> s {ssThreads = IntMap.insert threadId ts threads}
  pure (threadId, ts)

switchToThread :: Int -> M ()
switchToThread tid = do
  modify' $ \s -> s {ssCurrentThreadId = tid}

updateThreadState :: Int -> ThreadState -> M ()
updateThreadState tid ts = do
  modify' $ \s@StgState{..} -> s {ssThreads = IntMap.insert tid ts ssThreads}

lookupThreadState :: HasCallStack => Int -> M ThreadState
lookupThreadState tid = do
  IntMap.lookup tid <$> gets ssThreads >>= \case
    Nothing -> stgErrorM $ "unknown ThreadState: " ++ show tid
    Just a  -> pure a

getCurrentThreadState :: M ThreadState
getCurrentThreadState = do
  tid <- gets ssCurrentThreadId
  lookupThreadState tid

scheduleToTheEnd :: Int -> M ()
scheduleToTheEnd tid = pure () -- TODO

requestContextSwitch :: M ()
requestContextSwitch = pure () -- TODO

scheduleThreads :: M ()
scheduleThreads = pure () -- TODO

-- stack operations

{-
  , ssThreads             :: IntMap ThreadState
  , ssCurrentThreadId     :: Int

  = ThreadState
  { tsCurrentResult   :: [Atom] -- Q: do we need this?
  , tsStack           :: ![StackContinuation]
  , tsStatus          :: !ThreadStatus
  , tsBlockExceptions :: !Bool
  , tsInterruptible   :: !Bool
  }
-}
stackPush :: StackContinuation -> M ()
stackPush sc = do
  let pushFun ts@ThreadState{..} = ts {tsStack = sc : tsStack}
  modify' $ \s@StgState{..} -> s {ssThreads = IntMap.adjust pushFun ssCurrentThreadId ssThreads}

stackPop :: M (Maybe StackContinuation)
stackPop = do
  let tailFun ts@ThreadState{..} = ts {tsStack = tail tsStack}
  Just ts@ThreadState{..} <- state $ \s@StgState{..} -> (IntMap.lookup ssCurrentThreadId ssThreads, s {ssThreads = IntMap.adjust tailFun ssCurrentThreadId ssThreads})
  pure $ case tsStack of
    []    -> Nothing
    c : _ -> Just c

-- heap operations

freshHeapAddress :: HasCallStack => M Addr
freshHeapAddress = do
  limit <- gets ssNextAddr
  --liftIO $ print limit
  state $ \s@StgState{..} -> (ssNextAddr, s {ssNextAddr = succ ssNextAddr})

allocAndStore :: HasCallStack => HeapObject -> M Addr
allocAndStore o = do
  a <- freshHeapAddress
  store a o
  pure a

store :: HasCallStack => Addr -> HeapObject -> M ()
store a o = modify' $ \s -> s {ssHeap = IntMap.insert a o (ssHeap s)}

stgErrorM :: String -> M a
stgErrorM msg = do
  es <- gets ssEvalStack
  error $ unlines $ msg : "eval stack:" : map (\x -> "  " ++ show x) es

addBinderToEnv :: Binder -> Atom -> Env -> Env
addBinderToEnv b = Map.insert (Id b)

addZippedBindersToEnv :: [(Binder, Atom)] -> Env -> Env
addZippedBindersToEnv bvList env = foldl' (\e (b, v) -> Map.insert (Id b) v e) env bvList

addManyBindersToEnv :: [Binder] -> [Atom] -> Env -> Env
addManyBindersToEnv binders values = addZippedBindersToEnv $ zip binders values

lookupEnv :: HasCallStack => Env -> Binder -> M Atom
lookupEnv localEnv b
 | binderId b == BinderId (Unique '0' 21) -- void#
 = pure Void
 | binderId b == BinderId (Unique '0' 15) -- realWorld#
 = pure Void
 | otherwise
 = do
  env <- if binderTopLevel b
          then gets ssStaticGlobalEnv
          else pure localEnv
  case Map.lookup (Id b) env of
    Nothing -> stgErrorM $ "unknown variable: " ++ show b
    Just a  -> pure a

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
    _     -> stgErrorM $ "expected con but got: " ++ show o

readHeapClosure :: HasCallStack => Atom -> M HeapObject
readHeapClosure a = readHeap a >>= \o -> case o of
    Closure{} -> pure o
    _ -> stgErrorM $ "expected closure but got: " ++ show o

-- primop related

type PrimOpEval = Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]

type BuiltinStgEval = Atom -> M [Atom]
type BuiltinStgApply = Atom -> [Atom] -> M [Atom]

lookupWeakPointerDescriptor :: HasCallStack => Int -> M WeakPtrDescriptor
lookupWeakPointerDescriptor wpId = do
  IntMap.lookup wpId <$> gets ssWeakPointers >>= \case
    Nothing -> stgErrorM $ "unknown WeakPointer: " ++ show wpId
    Just a  -> pure a

lookupStablePointerPtr :: HasCallStack => Ptr Word8 -> M Atom
lookupStablePointerPtr sp = do
  let IntPtr spId = ptrToIntPtr sp
  lookupStablePointer spId

lookupStablePointer :: HasCallStack => Int -> M Atom
lookupStablePointer spId = do
  IntMap.lookup spId <$> gets ssStablePointers >>= \case
    Nothing -> stgErrorM $ "unknown StablePointer: " ++ show spId
    Just a  -> pure a

lookupMutVar :: HasCallStack => Int -> M Atom
lookupMutVar m = do
  IntMap.lookup m <$> gets ssMutVars >>= \case
    Nothing -> stgErrorM $ "unknown MutVar: " ++ show m
    Just a  -> pure a

lookupMVar :: HasCallStack => Int -> M MVarDescriptor
lookupMVar m = do
  IntMap.lookup m <$> gets ssMVars >>= \case
    Nothing -> stgErrorM $ "unknown MVar: " ++ show m
    Just a  -> pure a

lookupArray :: HasCallStack => Int -> M (Vector Atom)
lookupArray m = do
  IntMap.lookup m <$> gets ssArrays >>= \case
    Nothing -> stgErrorM $ "unknown Array: " ++ show m
    Just a  -> pure a

lookupMutableArray :: HasCallStack => Int -> M (Vector Atom)
lookupMutableArray m = do
  IntMap.lookup m <$> gets ssMutableArrays >>= \case
    Nothing -> stgErrorM $ "unknown MutableArray: " ++ show m
    Just a  -> pure a

lookupSmallArray :: HasCallStack => Int -> M (Vector Atom)
lookupSmallArray m = do
  IntMap.lookup m <$> gets ssSmallArrays >>= \case
    Nothing -> stgErrorM $ "unknown SmallArray: " ++ show m
    Just a  -> pure a

lookupSmallMutableArray :: HasCallStack => Int -> M (Vector Atom)
lookupSmallMutableArray m = do
  IntMap.lookup m <$> gets ssSmallMutableArrays >>= \case
    Nothing -> stgErrorM $ "unknown SmallMutableArray: " ++ show m
    Just a  -> pure a

lookupArrayArray :: HasCallStack => Int -> M (Vector Atom)
lookupArrayArray m = do
  IntMap.lookup m <$> gets ssArrayArrays >>= \case
    Nothing -> stgErrorM $ "unknown ArrayArray: " ++ show m
    Just a  -> pure a

lookupMutableArrayArray :: HasCallStack => Int -> M (Vector Atom)
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

-- debug
markExecuted :: Int -> M ()
markExecuted i = modify' $ \s@StgState{..} -> s {ssExecutedClosures = Set.insert i ssExecutedClosures}

markPrimOp :: Name -> M ()
markPrimOp i = modify' $ \s@StgState{..} -> s {ssExecutedPrimOps = Set.insert i ssExecutedPrimOps}

markFFI :: ForeignCall -> M ()
markFFI i = modify' $ \s@StgState{..} -> s {ssExecutedFFI = Set.insert i ssExecutedFFI}

-- string constants
-- NOTE: the string gets extended with a null terminator
getCStringConstantPtrAtom :: ByteString -> M Atom
getCStringConstantPtrAtom key = do
  strMap <- gets ssCStringConstants
  case Map.lookup key strMap of
    Just a  -> pure a
    Nothing -> do
      let bsCString = BS8.snoc key '\0'
          (bsFPtr, bsOffset, _bsLen) = BS.toForeignPtr bsCString
          a = PtrAtom (CStringPtr bsCString) $ plusPtr (unsafeForeignPtrToPtr bsFPtr) bsOffset
      modify' $ \s -> s {ssCStringConstants = Map.insert key a strMap}
      pure a

---------------------------------------------
-- threading

data AsyncExceptionMask
  = NonBlocked
  | Blocked     {aemInterruptible :: Bool}
  deriving (Eq, Ord, Show)

data ThreadState
  = ThreadState
  { tsCurrentResult   :: [Atom] -- Q: do we need this? A: yes, i.e. MVar read primops can write this after unblocking the thread
  , tsStack           :: ![StackContinuation]
  , tsStatus          :: !ThreadStatus
--  , tsAsyncExMask     :: !AsyncExceptionMask
  , tsBlockExceptions :: !Bool
  , tsInterruptible   :: !Bool
  , tsBound           :: !Bool
  , tsLocked          :: !Bool
  , tsCapability      :: !Int   -- NOTE: the thread is running on this capability
  , tsLabel           :: !(Maybe ByteString)
  }
  deriving (Eq, Ord, Show)

--------------

data BlockReason
  = BlockedOnMVar         Int (Maybe Atom) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int -- mvar id
  | BlockedOnBlackHole
  | BlockedOnThrowAsyncException
  | BlockedOnSTM
  | BlockedOnForeignCall
  | BlockedOnRead         Int -- file descriptor
  | BlockedOnWrite        Int -- file descriptor
  | BlockedOnDelay        UTCTime -- target time to wake up thread
  deriving (Eq, Ord, Show)

data ThreadStatus
  = ThreadRunning
  | ThreadBlocked   BlockReason
  | ThreadFinished
  | ThreadDied
  deriving (Eq, Ord, Show)
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
