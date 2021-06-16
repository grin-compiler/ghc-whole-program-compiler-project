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
  deriving (Show, Eq, Ord)

data WeakPtrDescriptor
  = WeakPtrDescriptor
  { wpdKey          :: Atom
  , wpdValue        :: Maybe Atom -- live or dead
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
    { hoIsLNE       :: Bool
    , hoCon         :: DataCon
    , hoConArgs     :: [Atom]
    }
  | Closure
    { hoIsLNE       :: Bool
    , hoName        :: Id
    , hoCloBody     :: StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [Atom]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }
  | BlackHole HeapObject
  | ApStack                   -- HINT: needed for the async exceptions
    { hoResult      :: [Atom]
    , hoStack       :: KAddr
    }
  | RaiseException Atom
  deriving (Show, Eq, Ord)

data StackContinuation
  = CaseOf  !Int !Id !Env !Binder !AltType ![Alt]  -- closure addr & name (debug) ; pattern match on the result ; carries the closure's local environment
  | Update  !Addr                         -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply   ![Atom]                       -- apply args on the result heap object
  | Catch   !Atom !Bool !Bool             -- catch frame ; exception handler, block async exceptions, interruptible
  | RestoreExMask !Bool !Bool             -- saved: block async exceptions, interruptible
  | RunScheduler  !ScheduleReason
  | DataToTagOp
  deriving (Show, Eq, Ord)

data ScheduleReason
  = SR_ThreadFinished
  | SR_ThreadBlocked
  | SR_ThreadYield
  deriving (Show, Eq, Ord)

data StackFrame
  = EmptyFrame
  | StackFrame
    { sfStackContinuation :: StackContinuation
    , sfNextFrameAddress  :: KAddr
    }
  deriving (Show, Eq, Ord)

{-
  Q: do we want homogeneous or heterogeneous Heap ; e.g. single intmap with mixed things or multiple intmaps/vector with multiple address spaces
-}

newtype PrintableMVar a = PrintableMVar {unPrintableMVar :: MVar a} deriving Eq
instance Show (PrintableMVar a) where
  show _ = "MVar"

newtype DebuggerChan = DebuggerChan {getDebuggerChan :: (OutChan DebugCommand, InChan DebugOutput)} deriving Eq
instance Show DebuggerChan where
  show _ = "DebuggerChan"

newtype NextDebugCommand = NextDebugCommand (Element DebugCommand, IO DebugCommand)
instance Show NextDebugCommand where
  show _ = "NextDebugCommand"
instance Eq NextDebugCommand where
   _ == _ = True
instance Ord NextDebugCommand where
   compare _ _ = EQ

data DebugCommand
  = CmdListClosures
  | CmdClearClosureList
  | CmdCurrentClosure
  | CmdAddBreakpoint    Name Int
  | CmdRemoveBreakpoint Name
  | CmdStep
  | CmdContinue
  | CmdPeekHeap         Addr
  | CmdStop
  | CmdInternal         String -- HINT: non-reified commands for quick experimentation
  deriving (Show)

data DebugOutput
  = DbgOutCurrentClosure  !(Maybe Id) !Addr !Env
  | DbgOutClosureList     ![Name]
  | DbgOutThreadReport    !Int !ThreadState !Name !Addr
  | DbgOutHeapObject      !Addr !HeapObject
  deriving (Show)

data DebugState
  = DbgRunProgram
  | DbgStepByStep
  deriving (Show, Eq, Ord)

data TracingState
  = NoTracing
  | DoTracing Handle
  deriving (Show)

type Addr   = Int
type Heap   = IntMap HeapObject
type Env    = Map Id Atom   -- NOTE: must contain only the defined local variables
type Stack  = [StackContinuation]
type KAddr  = Int -- NOTE: kontinuation address in the stack store

data StgState
  = StgState
  { ssHeap                :: !Heap
  , ssStaticGlobalEnv     :: !Env   -- NOTE: top level bindings only!
  , ssStackStore          :: IntMap StackFrame

  -- GC
  , ssLastGCAddr          :: !Int
  , ssGCInput             :: PrintableMVar ([Atom], StgState)
  , ssGCOutput            :: PrintableMVar RefSet
  , ssGCIsRunning         :: Bool

  -- string constants ; models the program memory's static constant region
  -- HINT: the value is a PtrAtom that points to the key BS's content
  , ssCStringConstants    :: Map ByteString Atom

  -- threading
  , ssThreads             :: IntMap ThreadState

  -- thread scheduler related
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

  , ssNextThreadId          :: !Int
  , ssNextStackAddr         :: !Int
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

  -- debug
  , ssCurrentClosureEnv   :: Env
  , ssCurrentClosure      :: Maybe Id
  , ssCurrentClosureAddr  :: Int
  , ssExecutedClosures    :: !(Set Int)
  , ssExecutedPrimOps     :: !(Set Name)
  , ssExecutedFFI         :: !(Set ForeignCall)
  , ssExecutedPrimCalls   :: !(Set PrimCall)
  , ssHeapStartAddress    :: !Int

  -- debugger API
  , ssDebuggerChan        :: DebuggerChan
  , ssNextDebugCommand    :: NextDebugCommand

  , ssEvaluatedClosures   :: !(Set Name)
  , ssBreakpoints         :: !(Map Name Int)
  , ssDebugState          :: DebugState

  -- region tracker
  , ssMarkers             :: !(Map Name (Set Region))
  , ssRegions             :: !(Map Region (Maybe AddressState, [(AddressState, AddressState)]) )

  -- retainer db
  , ssReferenceMap        :: !(IntMap IntSet)
  , ssRetainerMap         :: !(IntMap IntSet)
  , ssGCRootSet           :: !IntSet

  -- tracing
  , ssTracingState        :: TracingState

  -- origin db
  , ssOrigin              :: !(IntMap (Id, Int, Int)) -- HINT: closure, closure address, thread id

  -- GC marker
  , ssGCMarkers           :: ![AddressState]

  -- tracing primops
  , ssTraceEvents         :: ![(String, AddressState)]
  , ssTraceMarkers        :: ![(String, AddressState)]
  }
  deriving (Show)

-- for the primop tests
emptyUndefinedStgState :: StgState
emptyUndefinedStgState = emptyStgState undefined undefined undefined undefined DbgRunProgram NoTracing undefined undefined

emptyStgState :: PrintableMVar StgState
              -> DL
              -> DebuggerChan
              -> NextDebugCommand
              -> DebugState
              -> TracingState
              -> PrintableMVar ([Atom], StgState)
              -> PrintableMVar RefSet
              -> StgState
emptyStgState stateStore dl dbgChan nextDbgCmd dbgState tracingState gcIn gcOut = StgState
  { ssHeap                = mempty
  , ssStaticGlobalEnv     = mempty
  , ssStackStore          = mempty

  -- GC
  , ssLastGCAddr          = 0
  , ssGCInput             = gcIn
  , ssGCOutput            = gcOut
  , ssGCIsRunning         = False

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

  , ssNextThreadId          = 0
  , ssNextStackAddr         = 0
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

  -- debug
  , ssCurrentClosureEnv   = mempty
  , ssCurrentClosure      = Nothing
  , ssCurrentClosureAddr  = -1
  , ssExecutedClosures    = Set.empty
  , ssExecutedPrimOps     = Set.empty
  , ssExecutedFFI         = Set.empty
  , ssExecutedPrimCalls   = Set.empty
  , ssHeapStartAddress    = 0

  -- debugger api
  , ssDebuggerChan        = dbgChan
  , ssNextDebugCommand    = nextDbgCmd

  , ssEvaluatedClosures   = Set.empty
  , ssBreakpoints         = mempty
  , ssDebugState          = dbgState

  -- region tracker
  , ssMarkers             = mempty
  , ssRegions             = mempty

  -- retainer db
  , ssReferenceMap        = mempty
  , ssRetainerMap         = mempty
  , ssGCRootSet           = IntSet.empty

  -- tracing
  , ssTracingState        = tracingState

  -- origin db
  , ssOrigin              = mempty

  -- GC marker
  , ssGCMarkers           = []

  -- tracing primops
  , ssTraceEvents         = []
  , ssTraceMarkers        = []
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
  , rtsUnpackCString              :: Atom
  , rtsTopHandlerRunIO            :: Atom
  , rtsTopHandlerRunNonIO         :: Atom
  , rtsTopHandlerFlushStdHandles  :: Atom

  -- closures used by the exception primitives
  , rtsDivZeroException   :: Atom
  , rtsUnderflowException :: Atom
  , rtsOverflowException  :: Atom

  -- rts helper custom closures
  , rtsApplyFun1Arg :: Atom
  , rtsTuple2Proj0  :: Atom

  -- builtin special store, see FFI (i.e. getOrSetGHCConcSignalSignalHandlerStore)
  , rtsGlobalStore  :: Map Name Atom

  -- program contants
  , rtsProgName     :: String
  , rtsProgArgs     :: [String]
  }
  deriving (Show)

type M = StateT StgState IO


-- stack operations

allocEmptyStackFrame :: M KAddr
allocEmptyStackFrame = do
  kaddr <- freshStackAddress
  modify' $ \s@StgState{..} -> s { ssStackStore = IntMap.insert kaddr EmptyFrame ssStackStore }
  pure kaddr

freshStackAddress :: HasCallStack => M KAddr
freshStackAddress = do
  limit <- gets ssNextStackAddr
  state $ \s@StgState{..} -> (ssNextStackAddr, s {ssNextStackAddr = succ ssNextStackAddr})

stackPush :: StackContinuation -> M ()
stackPush sc = do
  tid <- gets ssCurrentThreadId
  ts@ThreadState{..} <- getThreadState tid

  topKAddr <- freshStackAddress
  let stackFrame = StackFrame sc tsTopStackFrame
  modify' $ \s@StgState{..} -> s { ssStackStore = IntMap.insert topKAddr stackFrame ssStackStore }

  updateThreadState tid ts {tsTopStackFrame = topKAddr}

stackPop :: M (Maybe StackContinuation)
stackPop = do
  tid <- gets ssCurrentThreadId
  ts@ThreadState{..} <- getThreadState tid
  stackStore <- gets ssStackStore
  case IntMap.lookup tsTopStackFrame stackStore of
    Nothing             -> stgErrorM $ "missing stack frame for address: " ++ show tsTopStackFrame
    Just EmptyFrame     -> pure Nothing
    Just StackFrame{..} -> do
      updateThreadState tid ts {tsTopStackFrame = sfNextFrameAddress}
      pure $ Just sfStackContinuation

-- heap operations

freshHeapAddress :: HasCallStack => M Addr
freshHeapAddress = do
  limit <- gets ssNextHeapAddr
  state $ \s@StgState{..} -> (ssNextHeapAddr, s {ssNextHeapAddr = succ ssNextHeapAddr})

allocAndStore :: HasCallStack => HeapObject -> M Addr
allocAndStore o = do
  a <- freshHeapAddress
  store a o
  pure a

store :: HasCallStack => Addr -> HeapObject -> M ()
store a o = do
  modify' $ \s@StgState{..} -> s { ssHeap = IntMap.insert a o ssHeap }

  do
    m <- gets ssCurrentClosure
    case m of
      Nothing       -> pure ()
      Just originId -> do
        originAddr <- gets ssCurrentClosureAddr
        tid <- gets ssCurrentThreadId
        modify' $ \s@StgState{..} -> s { ssOrigin = IntMap.insert a (originId, originAddr, tid) ssOrigin }
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
  curClosure <- gets ssCurrentClosure
  liftIO $ putStrLn $ "current closure: " ++ show curClosure
  error "stgErrorM"

addBinderToEnv :: Binder -> Atom -> Env -> Env
addBinderToEnv b = Map.insert (Id b)

addZippedBindersToEnv :: [(Binder, Atom)] -> Env -> Env
addZippedBindersToEnv bvList env = foldl' (\e (b, v) -> Map.insert (Id b) v e) env bvList

addManyBindersToEnv :: [Binder] -> [Atom] -> Env -> Env
addManyBindersToEnv binders values = addZippedBindersToEnv $ zip binders values

-- NOTE: for builtin uniques see: compiler/GHC/Builtin/Names.hs i.e. voidPrimIdKey
lookupEnv :: HasCallStack => Env -> Binder -> M Atom
lookupEnv localEnv b
 | binderId b == BinderId (Unique '0' 124) -- coercionToken#
 = pure Void
 | binderId b == BinderId (Unique '0' 21) -- void#
 = pure Void
 | binderId b == BinderId (Unique '0' 15) -- realWorld#
 = pure Void
 | binderId b == BinderId (Unique '0' 502) -- proxy#
 = pure Void
 | binderId b == BinderId (Unique '8' 1) -- GHC.Prim.(##)
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
    _     -> stgErrorM $ "expected con but got: "-- ++ show o

readHeapClosure :: HasCallStack => Atom -> M HeapObject
readHeapClosure a = readHeap a >>= \o -> case o of
    Closure{} -> pure o
    _ -> stgErrorM $ "expected closure but got: "-- ++ show o

-- primop related

type PrimOpEval = Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]

--type BuiltinStgEval = Atom -> M [Atom]
--type BuiltinStgApply = Atom -> [Atom] -> M [Atom]
type EvalOnNewThread = M [Atom] -> M [Atom]

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
setInsert :: Ord a => a -> Set a -> Set a
setInsert a s
  | Set.member a s  = s
  | otherwise       = Set.insert a s

markClosure :: Name -> M ()
markClosure n = modify' $ \s@StgState{..} -> s {ssEvaluatedClosures = setInsert n ssEvaluatedClosures}

markExecuted :: Int -> M ()
markExecuted i = modify' $ \s@StgState{..} -> s {ssExecutedClosures = setInsert i ssExecutedClosures}

markPrimOp :: Name -> M ()
markPrimOp i = modify' $ \s@StgState{..} -> s {ssExecutedPrimOps = setInsert i ssExecutedPrimOps}

markFFI :: ForeignCall -> M ()
markFFI i = modify' $ \s@StgState{..} -> s {ssExecutedFFI = setInsert i ssExecutedFFI}

markPrimCall :: PrimCall -> M ()
markPrimCall i = modify' $ \s@StgState{..} -> s {ssExecutedPrimCalls = setInsert i ssExecutedPrimCalls}

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
  = NotBlocked
  | Blocked     {isInterruptible :: !Bool}
  deriving (Eq, Ord, Show)

data ThreadState
  = ThreadState
  { tsCurrentResult     :: [Atom] -- Q: do we need this? A: yes, i.e. MVar read primops can write this after unblocking the thread
  , tsTopStackFrame     :: !KAddr
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
  emptyKAddr <- allocEmptyStackFrame
  let ts = ThreadState
        { tsCurrentResult     = []
        , tsTopStackFrame     = emptyKAddr
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
  = BlockedOnMVar         Int (Maybe Atom) -- mvar id, the value that need to put to mvar in case of blocking putMVar#, in case of takeMVar this is Nothing
  | BlockedOnMVarRead     Int       -- mvar id
  | BlockedOnBlackHole
  | BlockedOnThrowAsyncEx Int Atom  -- target thread id, exception
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
  liftIO $ reportThreadIO tid endTS

reportThreadIO :: Int -> ThreadState -> IO ()
reportThreadIO tid endTS = do
    putStrLn ""
    putStrLn $ show ("tid", tid, "tsStatus", tsStatus endTS)
    putStrLn "stack:"
    --putStrLn $ unlines $ map show $ zip [0..] $ map showStackCont $ tsStack endTS
    putStrLn ""

showStackCont :: StackContinuation -> String
showStackCont = \case
  CaseOf clAddr clo _ b _ _ -> "CaseOf, closure name: " ++ show clo ++ ", addr: " ++ show clAddr ++ ", result var: " ++ show (Id b)
  c -> show c

-------------------------
-- GC

data RefSet
  = RefSet
  { rsHeap                :: !IntSet
  , rsWeakPointers        :: !IntSet
  , rsMVars               :: !IntSet
  , rsMutVars             :: !IntSet
  , rsArrays              :: !IntSet
  , rsMutableArrays       :: !IntSet
  , rsSmallArrays         :: !IntSet
  , rsSmallMutableArrays  :: !IntSet
  , rsArrayArrays         :: !IntSet
  , rsMutableArrayArrays  :: !IntSet
  , rsMutableByteArrays   :: !IntSet
  , rsStableNames         :: !IntSet
  , rsStablePointers      :: !IntSet
  }

emptyRefSet :: RefSet
emptyRefSet = RefSet
  { rsHeap                = IntSet.empty
  , rsWeakPointers        = IntSet.empty
  , rsMVars               = IntSet.empty
  , rsMutVars             = IntSet.empty
  , rsArrays              = IntSet.empty
  , rsMutableArrays       = IntSet.empty
  , rsSmallArrays         = IntSet.empty
  , rsSmallMutableArrays  = IntSet.empty
  , rsArrayArrays         = IntSet.empty
  , rsMutableArrayArrays  = IntSet.empty
  , rsMutableByteArrays   = IntSet.empty
  , rsStableNames         = IntSet.empty
  , rsStablePointers      = IntSet.empty
  }

-- Debugger
data AddressState
  = AddressState
  { asNextThreadId          :: !Int
  , asNextHeapAddr          :: !Int
  , asNextStableName        :: !Int
  , asNextWeakPointer       :: !Int
  , asNextStablePointer     :: !Int
  , asNextMutableByteArray  :: !Int
  , asNextMVar              :: !Int
  , asNextMutVar            :: !Int
  , asNextArray             :: !Int
  , asNextMutableArray      :: !Int
  , asNextSmallArray        :: !Int
  , asNextSmallMutableArray :: !Int
  , asNextArrayArray        :: !Int
  , asNextMutableArrayArray :: !Int
  }
  deriving (Eq, Ord, Show)

emptyAddressState :: AddressState
emptyAddressState = AddressState
  { asNextThreadId          = 0
  , asNextHeapAddr          = 0
  , asNextStableName        = 0
  , asNextWeakPointer       = 0
  , asNextStablePointer     = 0
  , asNextMutableByteArray  = 0
  , asNextMVar              = 0
  , asNextMutVar            = 0
  , asNextArray             = 0
  , asNextMutableArray      = 0
  , asNextSmallArray        = 0
  , asNextSmallMutableArray = 0
  , asNextArrayArray        = 0
  , asNextMutableArrayArray = 0
  }

getAddressState :: M AddressState
getAddressState = do
  convertAddressState <$> get

convertAddressState :: StgState -> AddressState
convertAddressState StgState{..} = AddressState
  { asNextThreadId          = ssNextThreadId
  , asNextHeapAddr          = ssNextHeapAddr
  , asNextStableName        = ssNextStableName
  , asNextWeakPointer       = ssNextWeakPointer
  , asNextStablePointer     = ssNextStablePointer
  , asNextMutableByteArray  = ssNextMutableByteArray
  , asNextMVar              = ssNextMVar
  , asNextMutVar            = ssNextMutVar
  , asNextArray             = ssNextArray
  , asNextMutableArray      = ssNextMutableArray
  , asNextSmallArray        = ssNextSmallArray
  , asNextSmallMutableArray = ssNextSmallMutableArray
  , asNextArrayArray        = ssNextArrayArray
  , asNextMutableArrayArray = ssNextMutableArrayArray
  }

data Region
  = Region
  { regionStart :: Name
  , regionEnd   :: Name
  }
  deriving (Eq, Ord, Show)
