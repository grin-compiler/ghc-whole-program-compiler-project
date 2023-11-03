{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.Base where

import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad.State.Strict
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
import Text.Pretty.Simple (pShowNoColor, pShow)
import qualified Data.Text.Lazy.IO as Text

import GHC.Stack
import Text.Printf
import Debug.Trace
import Stg.Syntax
import Stg.IRLocation

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
  | InfoTablePtr                    -- GHC Cmm STG machine's info table
  | CostCentreStackPtr              -- GHC Cmm STG machine's cost centre stack
  | StablePtr     !Int              -- stable pointer must have AddrRep
  | LabelPtr      !Name !LabelSpec  -- foreign symbol/label name + label sepcification (i.e. data or function)
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

data TVarDescriptor
  = TVarDescriptor
  { tvdValue  :: Atom
  , tvdQueue  :: IntSet -- thread id, STM wake up queue
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
  | TVar          !Int
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
  | Rubbish
  | Unbinded          !Id -- program point that created this value (used for debug purposes)
  deriving (Show, Eq, Ord)

type ReturnValue = [Atom]

newtype CutShow a = CutShow {getCutShowItem :: a}
  deriving (Eq, Ord)

instance Show (CutShow a) where
  show _ = "<cut-show>"

data HeapObject
  = Con
    { hoIsLNE       :: Bool
    , hoCon         :: DC
    , hoConArgs     :: [Atom]
    }
  | Closure
    { hoIsLNE       :: Bool
    , hoName        :: Id
    , hoCloBody     :: CutShow StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [Atom]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }
  | BlackHole -- NOTE: each blackhole has exactly one corresponding thread and one update frame
    { hoBHOwnerThreadId :: Int        -- owner thread id
    , hoBHOriginalThunk :: HeapObject -- original heap object
    , hoBHWaitQueue     :: [Int]      -- blocking queue of thread ids
    }
  | ApStack                   -- HINT: needed for the async exceptions
    { hoResult      :: [Atom]
    , hoStack       :: [StackContinuation]
    }
  | RaiseException Atom
  deriving (Show, Eq, Ord)

data StackContinuation
  -- basic block related
  = CaseOf  !Int !Id !Env !Id !(CutShow AltType) !(CutShow [Alt])  -- closure addr & name (debug) ; pattern match on the result ; carries the closure's local environment
  -- closure related
  | Update  !Addr                         -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply   ![Atom]                       -- apply args on the result heap object
  -- exception related
  | Catch         !Atom !Bool !Bool         -- catch frame ; exception handler, block async exceptions, interruptible
  | RestoreExMask  (Bool, Bool) !Bool !Bool -- saved: block async exceptions, interruptible -- old -> new-to-restore-to
  -- thread related
  | RunScheduler  !ScheduleReason
  -- stm related
  | Atomically    !Atom
  | CatchRetry    !Atom !Atom !Bool !TLog      -- first STM action, alternative STM action, is running alt code?
  | CatchSTM      !Atom !Atom             -- catch STM frame ; stm action, exception handler
  -- * special primop calling stack frames
  -- STM + async exception related
  | AtomicallyOp  !Atom
  -- tag/enum related
  | DataToTagOp
  -- rts helper
  | RaiseOp       !Atom
  -- object lifetime related
  | KeepAlive     !Atom
  -- ext stg interpreter debug related
  | DebugFrame    !DebugFrame             -- for debug purposes, it does not required for STG evaluation
  deriving (Show, Eq, Ord)

data DebugFrame
  = RestoreProgramPoint !(Maybe Id) !ProgramPoint
  | DisablePrimOpTrace
  deriving (Show, Eq, Ord)

data ScheduleReason
  = SR_ThreadFinished
  | SR_ThreadFinishedMain
  | SR_ThreadFinishedFFICallback
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

data DebuggerChan
  = DebuggerChan
  { dbgSyncRequest    :: MVar DebugCommand
  , dbgSyncResponse   :: MVar DebugOutput
  , dbgAsyncEventIn   :: InChan DebugEvent
  , dbgAsyncEventOut  :: OutChan DebugEvent
  }
  deriving Eq

instance Show DebuggerChan where
  show _ = "DebuggerChan"

data DebugEvent
  = DbgEventHitBreakpoint !Breakpoint
  | DbgEventStopped
  deriving (Show)

data Breakpoint
  = BkpStgPoint   StgPoint
  | BkpPrimOp     Name
  | BkpFFISymbol  Name
  | BkpCustom     Name
  deriving (Eq, Ord, Show)

data DebugCommand
  = CmdListClosures
  | CmdClearClosureList
  | CmdCurrentClosure
  | CmdAddBreakpoint    Breakpoint Int
  | CmdRemoveBreakpoint Breakpoint
  | CmdStep
  | CmdContinue
  | CmdPeekHeap         Addr
  | CmdStop
  | CmdInternal         String -- HINT: non-reified commands for quick experimentation
  deriving (Show)

data DebugOutput
  = DbgOutCurrentClosure  !(Maybe Id) !Addr !Env
  | DbgOutClosureList     ![Name]
  | DbgOutThreadReport    !Int !ThreadState !Name !Addr String
  | DbgOutStgState        !StgState
  | DbgOutHeapObject      !Addr !HeapObject
  | DbgOutResult          ![Atom]
  | DbgOutString          !String
  | DbgOutByteString      !ByteString
  | DbgOut
  deriving (Show)

data DebugState
  = DbgRunProgram
  | DbgStepByStep
  deriving (Show, Eq, Ord)

data TracingState
  = NoTracing
  | DoTracing
    { thOriginDB          :: Handle
    , thWholeProgramPath  :: Handle
    }
  deriving (Show)

data CallGraph
  = CallGraph
  { cgInterClosureCallGraph :: !(StrictMap.Map (StaticOrigin, ProgramPoint, ProgramPoint) Int)
  , cgIntraClosureCallGraph :: !(StrictMap.Map (ProgramPoint, StaticOrigin, ProgramPoint) Int)
  }
  deriving (Show)

joinCallGraph :: CallGraph -> CallGraph -> CallGraph
joinCallGraph (CallGraph a1 b1) (CallGraph a2 b2) = CallGraph (StrictMap.unionWith (+) a1 a2) (StrictMap.unionWith (+) b1 b2)

emptyCallGraph :: CallGraph
emptyCallGraph = CallGraph
  { cgInterClosureCallGraph = mempty
  , cgIntraClosureCallGraph = mempty
  }

type Addr   = Int
type Heap   = IntMap HeapObject
type Env    = Map Id (StaticOrigin, Atom)   -- NOTE: must contain only the defined local variables
type Stack  = [StackContinuation]

envToAtoms :: Env -> [Atom]
envToAtoms = map snd . Map.elems

data StaticOrigin
  = SO_CloArg
  | SO_Let
  | SO_Scrut
  | SO_AltArg
  | SO_TopLevel
  | SO_Builtin
  | SO_ClosureResult
  deriving (Show, Eq, Ord)

data DebugSettings
  = DebugSettings
  { dsKeepGCFacts :: Bool
  }
  deriving (Show, Eq, Ord)

defaultDebugSettings :: DebugSettings
defaultDebugSettings
  = DebugSettings
  { dsKeepGCFacts = False
  }

newtype GCSymbol = GCSymbol {unGCSymbol :: ByteString}
  deriving (Show, Eq, Ord)

data StgState
  = StgState
  { ssHeap                :: !Heap
  , ssStaticGlobalEnv     :: !Env   -- NOTE: top level bindings only!
  , ssDynamicHeapStart    :: !Int

  -- GC
  , ssLastGCTime          :: !UTCTime
  , ssLastGCAddr          :: !Int
  , ssGCInput             :: PrintableMVar ([Atom], StgState)
  , ssGCOutput            :: PrintableMVar RefSet
  , ssGCIsRunning         :: Bool
  , ssGCCounter           :: Int
  , ssRequestMajorGC      :: Bool
  , ssCAFSet              :: IntSet

  -- let-no-escape support
  , ssTotalLNECount       :: !Int

  -- string constants ; models the program memory's static constant region
  -- HINT: the value is a PtrAtom that points to the key BS's content
  , ssCStringConstants    :: Map ByteString Atom

  -- threading
  , ssThreads             :: IntMap ThreadState

  -- thread scheduler related
  , ssCurrentThreadId     :: Int
  , ssScheduledThreadIds  :: [Int]  -- HINT: one round
  , ssThreadStepBudget    :: !Int

  -- primop related

  , ssStableNameMap       :: Map Atom Int
  , ssWeakPointers        :: IntMap WeakPtrDescriptor
  , ssStablePointers      :: IntMap Atom
  , ssMutableByteArrays   :: IntMap ByteArrayDescriptor
  , ssMVars               :: IntMap MVarDescriptor
  , ssTVars               :: IntMap TVarDescriptor
  , ssMutVars             :: IntMap Atom
  , ssArrays              :: IntMap (Vector Atom)
  , ssMutableArrays       :: IntMap (Vector Atom)
  , ssSmallArrays         :: IntMap (Vector Atom)
  , ssSmallMutableArrays  :: IntMap (Vector Atom)
  , ssArrayArrays         :: IntMap (Vector Atom)
  , ssMutableArrayArrays  :: IntMap (Vector Atom)

  , ssNextThreadId          :: !Int
  , ssNextHeapAddr          :: {-# UNPACK #-} !Int
  , ssNextStableName        :: !Int
  , ssNextWeakPointer       :: !Int
  , ssNextStablePointer     :: !Int
  , ssNextMutableByteArray  :: !Int
  , ssNextMVar              :: !Int
  , ssNextMutVar            :: !Int
  , ssNextTVar              :: !Int
  , ssNextArray             :: !Int
  , ssNextMutableArray      :: !Int
  , ssNextSmallArray        :: !Int
  , ssNextSmallMutableArray :: !Int
  , ssNextArrayArray        :: !Int
  , ssNextMutableArrayArray :: !Int

  -- FFI related
  , ssCBitsMap            :: DL
  , ssStateStore          :: PrintableMVar StgState

  -- FFI + createAdjustor
  , ssCWrapperHsTypeMap   :: !(Map Name (Bool, Name, [Name]))

  -- RTS related
  , ssRtsSupport          :: Rts

  -- debug
  , ssIsQuiet             :: Bool
  , ssLocalEnv            :: [Atom]
  , ssCurrentClosureEnv   :: Env
  , ssCurrentClosure      :: Maybe Id
  , ssCurrentClosureAddr  :: Int
  , ssExecutedClosures    :: !(Set Int)
  , ssExecutedClosureIds  :: !(Set Id)
  , ssExecutedPrimOps     :: !(Set Name)
  , ssExecutedFFI         :: !(Set ForeignCall)
  , ssExecutedPrimCalls   :: !(Set PrimCall)
  , ssClosureCallCounter  :: !Int
  , ssPrimOpTrace         :: !Bool

  -- call graph
  , ssCallGraph           :: !CallGraph
  , ssCurrentProgramPoint :: !ProgramPoint

  -- debugger API
  , ssDebuggerChan        :: DebuggerChan

  , ssEvaluatedClosures   :: !(Set Name)
  , ssBreakpoints         :: !(Map Breakpoint Int)
  , ssStepCounter         :: !Int
  , ssDebugFuel           :: !(Maybe Int)
  , ssDebugState          :: DebugState
  , ssStgErrorAction      :: Printable (M ())

  -- region tracker
  , ssMarkers             :: !(Map Name (Set Region))
  , ssRegionStack         :: !(Map (Int, Region) [(Int, AddressState, CallGraph)]) -- HINT: key = threadId + region ; value = index + start + call-graph
  , ssRegionInstances     :: !(Map Region (IntMap (AddressState, AddressState))) -- region => instance-index => start end
  , ssRegionCounter       :: !(Map Region Int)

  -- retainer db
  , ssReferenceMap        :: !(Map GCSymbol (Set GCSymbol))
  , ssRetainerMap         :: !(Map GCSymbol (Set GCSymbol))
  , ssGCRootSet           :: !(Set GCSymbol)

  -- tracing
  , ssTracingState        :: TracingState

  -- origin db
  , ssOrigin              :: !(IntMap (Id, Int, Int)) -- HINT: closure, closure address, thread id

  -- GC marker
  , ssGCMarkers           :: ![AddressState]

  -- tracing primops
  , ssTraceEvents         :: ![(String, AddressState)]
  , ssTraceMarkers        :: ![(String, Int, AddressState)]

  -- internal dev mode debug settings
  , ssDebugSettings       :: DebugSettings
  }
  deriving (Show)

-- for the primop tests
fakeStgStateForPrimopTests :: StgState
fakeStgStateForPrimopTests = emptyStgState undefined undefined undefined undefined undefined DbgRunProgram NoTracing defaultDebugSettings undefined undefined

emptyStgState :: UTCTime
              -> Bool
              -> PrintableMVar StgState
              -> DL
              -> DebuggerChan
              -> DebugState
              -> TracingState
              -> DebugSettings
              -> PrintableMVar ([Atom], StgState)
              -> PrintableMVar RefSet
              -> StgState
emptyStgState now isQuiet stateStore dl dbgChan dbgState tracingState debugSettings gcIn gcOut = StgState
  { ssHeap                = mempty
  , ssStaticGlobalEnv     = mempty
  , ssDynamicHeapStart    = 0

  -- GC
  , ssLastGCTime          = now
  , ssLastGCAddr          = 0
  , ssGCInput             = gcIn
  , ssGCOutput            = gcOut
  , ssGCIsRunning         = False
  , ssGCCounter           = 0
  , ssRequestMajorGC      = False
  , ssCAFSet              = IntSet.empty

  -- let-no-escape support
  , ssTotalLNECount       = 0

  , ssCStringConstants    = mempty

  -- threading
  , ssThreads             = mempty
  , ssCurrentThreadId     = error "uninitialized ssCurrentThreadId"
  , ssScheduledThreadIds  = []
  , ssThreadStepBudget    = 0

  -- primop related

  , ssStableNameMap       = mempty
  , ssWeakPointers        = mempty
  , ssStablePointers      = mempty
  , ssMutableByteArrays   = mempty
  , ssMVars               = mempty
  , ssMutVars             = mempty
  , ssTVars               = mempty
  , ssArrays              = mempty
  , ssMutableArrays       = mempty
  , ssSmallArrays         = mempty
  , ssSmallMutableArrays  = mempty
  , ssArrayArrays         = mempty
  , ssMutableArrayArrays  = mempty

  , ssNextThreadId          = 0
  , ssNextHeapAddr          = 0
  , ssNextStableName        = 0
  , ssNextWeakPointer       = 0
  , ssNextStablePointer     = 0
  , ssNextMutableByteArray  = 0
  , ssNextMVar              = 0
  , ssNextMutVar            = 0
  , ssNextTVar              = 0
  , ssNextArray             = 0
  , ssNextMutableArray      = 0
  , ssNextSmallArray        = 0
  , ssNextSmallMutableArray = 0
  , ssNextArrayArray        = 0
  , ssNextMutableArrayArray = 0

  -- FFI related
  , ssCBitsMap            = dl
  , ssStateStore          = stateStore

  -- FFI + createAdjustor
  , ssCWrapperHsTypeMap   = mempty

  , ssRtsSupport          = error "uninitialized ssRtsSupport"

  -- debug
  , ssIsQuiet             = isQuiet
  , ssLocalEnv            = mempty
  , ssCurrentClosureEnv   = mempty
  , ssCurrentClosure      = Nothing
  , ssCurrentClosureAddr  = -1
  , ssExecutedClosures    = Set.empty
  , ssExecutedClosureIds  = Set.empty
  , ssExecutedPrimOps     = Set.empty
  , ssExecutedFFI         = Set.empty
  , ssExecutedPrimCalls   = Set.empty
  , ssClosureCallCounter  = 0
  , ssPrimOpTrace         = False

  -- call graph
  , ssCallGraph           = emptyCallGraph
  , ssCurrentProgramPoint = PP_Global

  -- debugger api
  , ssDebuggerChan        = dbgChan

  , ssEvaluatedClosures   = Set.empty
  , ssBreakpoints         = mempty
  , ssDebugState          = dbgState
  , ssStepCounter         = 0
  , ssDebugFuel           = Nothing
  , ssStgErrorAction      = Printable $ pure ()

  -- region tracker
  , ssMarkers             = mempty
  , ssRegionStack         = mempty
  , ssRegionInstances     = mempty
  , ssRegionCounter       = mempty

  -- retainer db
  , ssReferenceMap        = mempty
  , ssRetainerMap         = mempty
  , ssGCRootSet           = Set.empty

  -- tracing
  , ssTracingState        = tracingState

  -- origin db
  , ssOrigin              = mempty

  -- GC marker
  , ssGCMarkers           = []

  -- tracing primops
  , ssTraceEvents         = []
  , ssTraceMarkers        = []

  -- internal dev mode debug settings
  , ssDebugSettings       = debugSettings
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

  -- closures used by the STM primitives
  , rtsNestedAtomically   :: Atom -- (exception)

  -- closures used by the GC deadlock detection
  , rtsBlockedIndefinitelyOnMVar  :: Atom -- (exception)
  , rtsBlockedIndefinitelyOnSTM   :: Atom -- (exception)
  , rtsNonTermination             :: Atom -- (exception)

  -- rts helper custom closures
  , rtsApplyFun1Arg :: Atom
  , rtsTuple2Proj0  :: Atom

  -- builtin special store, see FFI (i.e. getOrSetGHCConcSignalSignalHandlerStore)
  , rtsGlobalStore  :: Map Name Atom

  -- program contants
  , rtsProgName     :: String
  , rtsProgArgs     :: [String]

  -- native C data symbols
  , rtsDataSymbol_enabled_capabilities  :: Ptr CInt
  }
  deriving (Show)

type M = StateT StgState IO


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
  let tailFun ts@ThreadState{..} = ts {tsStack = drop 1 tsStack}
  Just ts@ThreadState{..} <- state $ \s@StgState{..} ->
    ( IntMap.lookup ssCurrentThreadId ssThreads
    , s {ssThreads = IntMap.adjust tailFun ssCurrentThreadId ssThreads}
    )
  pure $ case tsStack of
    []    -> Nothing
    c : _ -> Just c

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

stgErrorM :: HasCallStack => String -> M a
stgErrorM msg = do
  tid <- gets ssCurrentThreadId
  liftIO $ do
    putStrLn $ " * stgErrorM: " ++ show msg
    putStrLn $ "current thread id: " ++ show tid
  reportThread tid
  curClosure <- gets ssCurrentClosure
  liftIO $ do
    putStrLn $ "current closure: " ++ show curClosure
    putStrLn $ " * native estgi call stack:"
    putStrLn $ prettyCallStack callStack
  action <- unPrintable <$> gets ssStgErrorAction
  action
  error "stgErrorM"

addBinderToEnv :: StaticOrigin -> Binder -> Atom -> Env -> Env
addBinderToEnv so b a = Map.insert (Id b) (so, a)

addZippedBindersToEnv :: StaticOrigin -> [(Binder, Atom)] -> Env -> Env
addZippedBindersToEnv so bvList env = foldl' (\e (b, v) -> Map.insert (Id b) (so, v) e) env bvList

addManyBindersToEnv :: StaticOrigin -> [Binder] -> [Atom] -> Env -> Env
addManyBindersToEnv so [] [] env = env
addManyBindersToEnv so (b : binders) (v : values) env = addManyBindersToEnv so binders values $ Map.insert (Id b) (so, v) env
addManyBindersToEnv so (b : binders) values env = addManyBindersToEnv so binders values $ Map.insert (Id b) (so, Unbinded (Id b)) env
addManyBindersToEnv so binders values _env = error $ "addManyBindersToEnv - length mismatch: " ++ show (so, [(Id b, binderType b, binderTypeSig b) | b <- binders], values)

lookupEnvSO :: HasCallStack => Env -> Binder -> M (StaticOrigin, Atom)
lookupEnvSO localEnv b = do
  env <- if binderTopLevel b
          then gets ssStaticGlobalEnv
          else pure localEnv
  case Map.lookup (Id b) env of
    Just a  -> pure a
    Nothing -> case binderUniqueName b of
      -- HINT: GHC.Prim module does not exist it's a wired in module
      "ghc-prim_GHC.Prim.void#"           -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.realWorld#"      -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.coercionToken#"  -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.proxy#"          -> pure (SO_Builtin, Void)
      "ghc-prim_GHC.Prim.(##)"            -> pure (SO_Builtin, Void)
      _ -> stgErrorM $ "unknown variable: " ++ show b

lookupEnv :: HasCallStack => Env -> Binder -> M Atom
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

lookupTVar :: HasCallStack => Int -> M TVarDescriptor
lookupTVar m = do
  IntMap.lookup m <$> gets ssTVars >>= \case
    Nothing -> stgErrorM $ "unknown TVar: " ++ show m
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
markExecuted i = pure () -- modify' $ \s@StgState{..} -> s {ssExecutedClosures = setInsert i ssExecutedClosures}

markExecutedId :: Id -> M ()
markExecutedId i = modify' $ \s@StgState{..} -> s {ssExecutedClosureIds = setInsert i ssExecutedClosureIds}

markPrimOp :: Name -> M ()
markPrimOp i = modify' $ \s@StgState{..} -> s {ssExecutedPrimOps = setInsert i ssExecutedPrimOps}

markFFI :: ForeignCall -> M ()
markFFI i = modify' $ \s@StgState{..} -> s {ssExecutedFFI = setInsert i ssExecutedFFI}

markPrimCall :: PrimCall -> M ()
markPrimCall i = modify' $ \s@StgState{..} -> s {ssExecutedPrimCalls = setInsert i ssExecutedPrimCalls}

-- call graph
-- HINT: build separate call graph for each region

addInterClosureCallGraphEdge :: StaticOrigin -> ProgramPoint -> ProgramPoint -> M ()
addInterClosureCallGraphEdge so from to = do
  let addEdge g@CallGraph{..} = g {cgInterClosureCallGraph = StrictMap.insertWith (+) (so, from, to) 1 cgInterClosureCallGraph}
      updateRegion = \case
        -- HINT: collect edges for regions on stack top only, the call graph will be merged for nested regions at close
        (i, a, regionCallGraph) : l -> (i, a, addEdge regionCallGraph) : l
        r -> r
  modify' $ \s@StgState{..} -> s
    { ssCallGraph   = addEdge ssCallGraph
    , ssRegionStack = fmap updateRegion ssRegionStack
    }

addIntraClosureCallGraphEdge :: ProgramPoint -> StaticOrigin -> ProgramPoint -> M ()
addIntraClosureCallGraphEdge from so to = do
  let addEdge g@CallGraph{..} = g {cgIntraClosureCallGraph = StrictMap.insertWith (+) (from, so, to) 1 cgIntraClosureCallGraph}
      updateRegion = \case
        -- HINT: collect edges for regions on stack top only, the call graph will be merged for nested regions at close
        (i, a, regionCallGraph) : l -> (i, a, addEdge regionCallGraph) : l
        r -> r
  modify' $ \s@StgState{..} -> s
    { ssCallGraph   = addEdge ssCallGraph
    , ssRegionStack = fmap updateRegion ssRegionStack
    }

setProgramPoint :: ProgramPoint -> M ()
setProgramPoint pp = modify' $ \s@StgState{..} -> s {ssCurrentProgramPoint = pp}

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
-- stm

promptM :: IO () -> M ()
promptM ioAction = do
  isQuiet <- gets ssIsQuiet
  tid <- gets ssCurrentThreadId
  pp <- gets ssCurrentProgramPoint
  cc <- gets ssCurrentClosure
  tsList <- gets $ IntMap.toList . ssThreads
  liftIO . unless isQuiet $ do
    now <- getCurrentTime
    putStrLn $ "  now           = " ++ show now
    putStrLn $ "  tid           = " ++ show tid ++ "    thread status list: " ++ show [(tid, tsStatus ts) | (tid, ts) <- tsList]
    putStrLn $ "  program point = " ++ show pp
    ioAction
    --putStrLn "[press enter]"
    --getLine
    pure ()

promptM_ :: M () -> M ()
promptM_ ioAction = do
  isQuiet <- gets ssIsQuiet
  unless isQuiet $ do
    ioAction
    --putStrLn "[press enter]"
    --getLine
    pure ()

data TLogEntry
  = TLogEntry
  { tleObservedGlobalValue  :: !Atom
  , tleCurrentLocalValue    :: !Atom
  }
  deriving (Show, Eq, Ord)

type TLog = IntMap TLogEntry

validateTLog :: TLog -> M Bool
validateTLog tlog = do
  isValid <- and <$> forM (IntMap.toList tlog)
    (\(tvar, TLogEntry{..}) -> ((tleObservedGlobalValue ==) . tvdValue) <$> lookupTVar tvar)
  promptM $ do
    putStrLn $ "[STM] tlog: " ++ show tlog
    putStrLn $ "[STM] validateTLog: " ++ show isValid
  pure isValid

subscribeTVarWaitQueues :: Int -> TLog -> M ()
subscribeTVarWaitQueues tid tlog = do
  -- subscribe to wait queues
  let subscribe tvd@TVarDescriptor{..} = tvd {tvdQueue = IntSet.insert tid tvdQueue}
  forM_ (IntMap.keys tlog) $ \tvar -> do
    modify' $ \s@StgState{..} -> s {ssTVars = IntMap.adjust subscribe tvar ssTVars}

unsubscribeTVarWaitQueues :: Int -> TLog -> M ()
unsubscribeTVarWaitQueues tid tlog = do
  -- unsubscribe from wait queues
  let unsubscribe tvd@TVarDescriptor{..} = tvd {tvdQueue = IntSet.delete tid tvdQueue}
  forM_ (IntMap.keys tlog) $ \tvar -> do
    modify' $ \s@StgState{..} -> s {ssTVars = IntMap.adjust unsubscribe tvar ssTVars}


---------------------------------------------
-- threading

data AsyncExceptionMask
  = NotBlocked
  | Blocked     {isInterruptible :: !Bool}
  deriving (Eq, Ord, Show)

data ThreadState
  = ThreadState
  { tsCurrentResult     :: [Atom] -- Q: do we need this? A: yes, i.e. MVar read primops can write this after unblocking the thread
  , tsStack             :: ![StackContinuation]
  , tsStatus            :: !ThreadStatus
  , tsBlockedExceptions :: [(Int, Atom)] -- ids of the threads waiting to send an async exception + exception
  , tsBlockExceptions   :: !Bool  -- block async exceptions
  , tsInterruptible     :: !Bool  -- interruptible blocking of async exception
--  , tsAsyncExMask     :: !AsyncExceptionMask
  , tsBound             :: !Bool
  , tsLocked            :: !Bool  -- NOTE: can the thread be moved across capabilities? this is related to multicore haskell's load balancing
  , tsCapability        :: !Int   -- NOTE: the thread is running on this capability ; Q: is this necessary?
  , tsLabel             :: !(Maybe ByteString)
  -- STM
  , tsActiveTLog        :: !(Maybe TLog) -- elems: (global value, local value)
  , tsTLogStack         :: ![TLog]
  }
  deriving (Eq, Ord, Show)

-- thread operations

createThread :: M (Int, ThreadState)
createThread = do
  let ts = ThreadState
        { tsCurrentResult     = []
        , tsStack             = []
        , tsStatus            = ThreadRunning
        , tsBlockedExceptions = []
        , tsBlockExceptions   = False
        , tsInterruptible     = False
        , tsBound             = False
        , tsLocked            = False
        , tsCapability        = 0 -- TODO: implement capability handling
        , tsLabel             = Nothing
        , tsActiveTLog        = Nothing
        , tsTLogStack         = []
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
  | BlockedOnBlackHole    Int       -- heap address
  | BlockedOnThrowAsyncEx Int       -- target thread id
  | BlockedOnSTM          TLog
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
  ctid <- gets ssCurrentThreadId
  liftIO $ putStrLn $ "thread Ids: " ++ show threadIds ++ " current tid: " ++ show ctid
  mapM_ reportThread threadIds

reportThread :: Int -> M ()
reportThread tid = do
  endTS <- getThreadState tid
  liftIO $ reportThreadIO tid endTS

reportThreadIO :: Int -> ThreadState -> IO ()
reportThreadIO tid endTS = do
    putStrLn ""
    putStrLn $ show ("tid", tid, "tsStatus", tsStatus endTS)
    Text.putStrLn $ pShowNoColor endTS
    putStrLn ""

showStackCont :: StackContinuation -> String
showStackCont = \case
  CaseOf clAddr clo _ b _ _ -> "CaseOf, closure name: " ++ show clo ++ ", addr: " ++ show clAddr ++ ", result var: " ++ show (b)
  c -> show c

-------------------------
-- GC

data RefSet
  = RefSet
  { rsHeap                :: !IntSet
  , rsWeakPointers        :: !IntSet
  , rsTVars               :: !IntSet
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
  , rsThreads             :: !IntSet
  }

emptyRefSet :: RefSet
emptyRefSet = RefSet
  { rsHeap                = IntSet.empty
  , rsWeakPointers        = IntSet.empty
  , rsTVars               = IntSet.empty
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
  , rsThreads             = IntSet.empty
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
  , asNextTVar              :: !Int
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
  , asNextTVar              = 0
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
  , asNextTVar              = ssNextTVar
  , asNextArray             = ssNextArray
  , asNextMutableArray      = ssNextMutableArray
  , asNextSmallArray        = ssNextSmallArray
  , asNextSmallMutableArray = ssNextSmallMutableArray
  , asNextArrayArray        = ssNextArrayArray
  , asNextMutableArrayArray = ssNextMutableArrayArray
  }

data Region
  = IRRegion
  { regionStart :: Name
  , regionEnd   :: Name
  }
  | EventRegion
  { regionName  :: Name
  }
  deriving (Eq, Ord, Show)

-- let-no-escape statistics
markLNE :: [Addr] -> M ()
markLNE lneAddrs = do
  let count = length lneAddrs
  modify' $ \s@StgState{..} -> s { ssTotalLNECount = count + ssTotalLNECount}

-- program point

data ProgramPoint
  = PP_Global
  | PP_Apply      Int ProgramPoint
  | PP_StgPoint   StgPoint
  deriving (Eq, Ord, Read, Show)

dumpStgState :: M ()
dumpStgState = do
  firstHeapAddress <- gets ssDynamicHeapStart
  stgState0 <- get
  let stgState1 = stgState0
        { ssHeap            = IntMap.withoutKeys (ssHeap stgState0) (IntSet.fromList [0..firstHeapAddress-1])
        , ssStaticGlobalEnv = mempty
        , ssOrigin          = mempty
        }
  liftIO $ do
    let fname = "estgi-state-dump.txt"
    putStrLn $ "dumping stg state to: " ++ fname
    Text.writeFile fname $ pShowNoColor stgState1

debugPrintAtom :: Atom -> M String
debugPrintAtom = \case
  a@HeapPtr{} -> do
    heapObjStr <- debugPrintHeapObject <$> readHeap a
    pure $ show a ++ " -> " ++ heapObjStr
  a -> pure $ show a

debugPrintHeapObject :: HeapObject -> String
debugPrintHeapObject  = \case
  Con{..}           -> "Con: " ++ show (dcUniqueName $ unDC hoCon) ++ " " ++ show hoConArgs
  Closure{..}       -> "Clo: " ++ show hoName ++ " args: " ++ show hoCloArgs ++ " env: " ++ show (Map.size hoEnv) ++ " missing: " ++ show hoCloMissing
  BlackHole{..}     -> "BlackHole: " ++ debugPrintHeapObject hoBHOriginalThunk
  ApStack{}         -> "ApStack"
  RaiseException ex -> "RaiseException: " ++ show ex

----------------

debugAsyncExceptions :: M ()
debugAsyncExceptions = do
  tid <- gets ssCurrentThreadId
  threads <- gets ssThreads
  when (any (\ts -> tsBlockedExceptions ts /= []) threads) $ do
    liftIO $ putStrLn $ "current thread: " ++ show tid
    reportThreads
    error "TODO: deliver async exception"

traceLog :: String -> M ()
traceLog msg = do
  gets ssTracingState >>= \case
    NoTracing     -> pure ()
    DoTracing{..} -> do
      tid <- gets ssCurrentThreadId
      ts <- getCurrentThreadState
      liftIO $ hPutStrLn thWholeProgramPath $ maybe "" BS8.unpack (tsLabel ts) ++ "\t" ++  show tid ++ "\t" ++ msg

mylog :: String -> M ()
mylog _ = pure ()
{-
mylog msg = do
  ctid <- gets ssCurrentThreadId
  pp <- gets ssCurrentProgramPoint
  liftIO $ do
    BS8.putStrLn . BS8.pack $ msg ++ " " ++ show pp ++ " " ++ show ctid
    hFlush stdout
-}

wakeupBlackHoleQueueThreads :: Int -> M ()
wakeupBlackHoleQueueThreads addr = readHeap (HeapPtr addr) >>= \case
  BlackHole{..} -> do
    -- wake up blocked threads
    forM_ hoBHWaitQueue $ \waitingTid -> do
      waitingTS <- getThreadState waitingTid
      case tsStatus waitingTS of
        ThreadBlocked (BlockedOnBlackHole dstAddr) -> do
          updateThreadState waitingTid (waitingTS {tsStatus = ThreadRunning})
        _ -> error $ "internal error - invalid thread status: " ++ show (tsStatus waitingTS)
  x -> error $ "internal error - expected BlackHole, got: " ++ show x
