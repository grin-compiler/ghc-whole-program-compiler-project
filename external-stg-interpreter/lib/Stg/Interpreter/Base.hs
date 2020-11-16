{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.Base where

import Data.Word
import Foreign.Ptr
import Control.Monad.State.Strict
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
  | Blackhole HeapObject
  deriving (Show, Eq, Ord)

data StackContinuation
  = CaseOf  Id AltType [Alt]  -- pattern match on the result
  | Update  Addr                  -- update Addr with the result heap object ; NOTE: maybe this is irrelevant as the closure interpreter will perform the update if necessary
  | Apply   [Atom]                -- apply args on the result heap object
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

data Atom     -- Q: should atom fit into a cpu register? A: yes
  = HeapPtr   !Addr
  | Literal   !Lit  -- TODO: remove this
  | Void
  | PtrAtom       !PtrOrigin !(Ptr Word8)
  | IntAtom       !Int
  | WordAtom      !Word
  | FloatAtom     !Float
  | DoubleAtom    !Double
  | StablePointer !Atom -- TODO: need proper implementation
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
  | WeakPointer       !Atom !Atom !(Maybe Atom) -- key, value, finalizer
  | ThreadId
  | LiftedUndefined
  deriving (Show, Eq, Ord)

type ReturnValue = [Atom]

type Addr   = Int
type Heap   = IntMap HeapObject
type Env    = Map Id Atom   -- NOTE: must contain only the defined local variables
type Stack  = [(Env, StackContinuation)]

{-
  Q: do we want homogeneous or heterogeneous Heap ; e.g. single intmap with mixed things or multiple intmaps/vector with multiple address spaces
-}

newtype PrintableMVar a = PrintableMVar {unPrintableMVar :: MVar a} deriving Eq
instance Show (PrintableMVar a) where
  show _ = "MVar"

data StgState
  = StgState
  { ssHeap      :: !Heap
  , ssEnv       :: !Env
--  , ssStack     :: [(Env, StackContinuation)] -- TODO: use reified stack instead of the host language stack
  , ssEvalStack :: [Id]
  , ssNextAddr  :: !Int

  -- string constants ; models the program memory's static constant region
  -- HINT: the value is a PtrAtom that points to the key BS's content
  , ssCStringConstants    :: Map ByteString Atom

  -- primop related

  , ssMutableByteArrays   :: IntMap ByteArrayDescriptor
  , ssMVars               :: IntMap (Maybe Atom)
  , ssMutVars             :: IntMap Atom
  , ssArrays              :: IntMap (Vector Atom)
  , ssMutableArrays       :: IntMap (Vector Atom)
  , ssSmallArrays         :: IntMap (Vector Atom)
  , ssSmallMutableArrays  :: IntMap (Vector Atom)
  , ssArrayArrays         :: IntMap (Vector Atom)
  , ssMutableArrayArrays  :: IntMap (Vector Atom)

  , ssExceptionHandlers   :: [(PrintableMVar Bool, Atom)]

  -- FFI related
  , ssCBitsMap            :: DL
  , ssStateStore          :: PrintableMVar StgState

  , ssRtsSupport          :: Rts

  -- debug
  , ssExecutedClosures    :: Set Int
  , ssExecutedPrimOps     :: Set Name
  , ssExecutedFFI         :: Set ForeignCall
  , ssWeakPointers        :: Set Atom
  , ssAddressAfterInit    :: Int
  }
  deriving (Show)

emptyStgState :: PrintableMVar StgState -> DL -> StgState
emptyStgState stateStore dl = StgState
  { ssHeap      = mempty
  , ssEnv       = mempty
  , ssEvalStack = []
  , ssNextAddr  = 0

  , ssCStringConstants    = mempty

  -- primop related

  , ssMutableByteArrays   = mempty
  , ssMVars               = mempty
  , ssMutVars             = mempty
  , ssArrays              = mempty
  , ssMutableArrays       = mempty
  , ssSmallArrays         = mempty
  , ssSmallMutableArrays  = mempty
  , ssArrayArrays         = mempty
  , ssMutableArrayArrays  = mempty

  , ssExceptionHandlers   = []

  -- FFI related
  , ssCBitsMap            = dl
  , ssStateStore          = stateStore

  , ssRtsSupport          = undefined
  , ssExecutedClosures    = Set.empty
  , ssExecutedPrimOps     = Set.empty
  , ssExecutedFFI         = Set.empty
  , ssWeakPointers        = Set.empty
  , ssAddressAfterInit    = 0
  }

{-
        base_GHCziTopHandler_runIO_closure
        base_GHCziTopHandler_runNonIO_closure

    - collect the necessary DataCons for these constructors
        rts_mkChar      Czh_con_info
        rts_mkInt       Izh_con_info
        rts_mkInt8      I8zh_con_info
        rts_mkInt16     I16zh_con_info
        rts_mkInt32     I32zh_con_info
        rts_mkInt64     I64zh_con_info
        rts_mkWord      Wzh_con_info
        rts_mkWord8     W8zh_con_info
        rts_mkWord16    W16zh_con_info
        rts_mkWord32    W32zh_con_info
        rts_mkWord64    W64zh_con_info
        rts_mkPtr       Ptr_con_info
        rts_mkFunPtr    FunPtr_con_info
        rts_mkFloat     Fzh_con_info
        rts_mkDouble    Dzh_con_info
        rts_mkStablePtr StablePtr_con_info
        rts_mkBool      True_closure, False_closure
        rts_mkString    unpackCString_closure
-}

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
  , rtsUnpackCString       :: Atom
  , rtsTopHandlerRunIO     :: Atom
  , rtsTopHandlerRunNonIO  :: Atom
  }
  deriving (Show)

type M = StateT StgState IO

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

addEnv :: HasCallStack => Binder -> Atom -> M ()
addEnv b a = modify' $ \s -> s {ssEnv = Map.insert (Id b) a (ssEnv s)}

stgErrorM :: String -> M a
stgErrorM msg = do
  es <- gets ssEvalStack
  error $ unlines $ msg : "eval stack:" : map (\x -> "  " ++ show x) es

lookupEnv :: HasCallStack => Binder -> M Atom
lookupEnv b
 | binderId b == BinderId (Unique '0' 21) -- void#
 = pure Void
 | binderId b == BinderId (Unique '0' 15) -- realWorld#
 = pure Void
 | otherwise
 = do
  env <- gets ssEnv
  case Map.lookup (Id b) env of
    Nothing -> stgErrorM $ "unknown variable: " ++ show b
    Just a  -> pure a

update :: HasCallStack => Atom -> [Atom] -> M [Atom]
update (HeapPtr dst) result@[src] = do
  o <- readHeap src
  store dst o
  pure result

{-# NOINLINE withEnv #-}
withEnv :: HasCallStack => Id -> Env -> M a -> M a
withEnv i env m = do
  save <- gets ssEnv
  saveES <- gets ssEvalStack
  modify' $ \s -> s {ssEnv = env, ssEvalStack = i : saveES}
  --res <- trace (printf "eval %-40s %s" (BS8.unpack . getModuleName . binderModule $ unId i) (show i)) $  m
  res <- m
  modify' $ \s -> s {ssEnv = save, ssEvalStack = saveES}
  pure res

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

-- primop related

type PrimOpEval = Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]

type BuiltinStgEval = Atom -> M [Atom]
type BuiltinStgApply = Atom -> [Atom] -> M [Atom]

lookupMutVar :: HasCallStack => Int -> M Atom
lookupMutVar m = do
  IntMap.lookup m <$> gets ssMutVars >>= \case
    Nothing -> stgErrorM $ "unknown MutVar: " ++ show m
    Just a  -> pure a

lookupMVar :: HasCallStack => Int -> M (Maybe Atom)
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
