{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.Debugger.Datalog (exportStgState) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import qualified Data.ByteString.Char8 as BS8
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Primitive.ByteArray as BA
import Data.Maybe
import Data.List (intercalate, nub)
import Text.Printf

import System.Directory
import System.FilePath
import System.IO

import qualified Stg.Interpreter.GC as GC

import Stg.Interpreter.Base
import Stg.Syntax


data Param
  = S   String
  | N   Name
  | I   Int
  | A   Atom
  | ID  Id

data DLExport
  = DLExport
  { dleHandleMap  :: Map Name Handle
  }

type DL = StateT DLExport IO

getHandle :: Name -> DL Handle
getHandle n = do
  DLExport{..} <- get
  case Map.lookup n dleHandleMap of
    Just h -> pure h
    Nothing -> error $ "missing handle for: " ++ show n

genParam :: Param -> DL String
genParam = \case
  S s -> pure s
  N n -> pure $ BS8.unpack n
  I i -> pure $ show i
  A a -> do
          emitAtomToRef a
          pure $ show a
  ID i -> do
          emitBinderInfo i
          pure $ show i

addFact :: Name -> [Param] -> DL ()
addFact rName args = do
  h <- getHandle rName
  values <- mapM genParam args
  liftIO $ hPutStrLn h $ intercalate "\t" values

emitBinderInfo :: Id -> DL ()
emitBinderInfo i@(Id Binder{..}) = do
  h <- getHandle "BinderInfo"
  let args =
        [ show i
        , BS8.unpack binderName
        , show binderId
        , show binderType
        , BS8.unpack binderTypeSig
        , show binderScope
        , show binderDetails
        , BS8.unpack binderInfo
        , show binderDefLoc
        , BS8.unpack $ getUnitId binderUnitId
        , BS8.unpack $ getModuleName binderModule
        , show binderTopLevel
        ]
  liftIO $ hPutStrLn h $ intercalate "\t" args

emitAtomToRef :: Atom -> DL ()
emitAtomToRef a = do
  h <- getHandle "AtomToRef"
  let add s = liftIO $ hPutStrLn h $ (show a) ++ "\t" ++ s
  case a of
    HeapPtr i               -> add $ printf "$R_HeapPtr\t%d" i
    MVar i                  -> add $ printf "$R_MVar\t%d" i
    MutVar i                -> add $ printf "$R_MutVar\t%d" i
    Array i                 -> add $ arrIdxToRef i
    MutableArray i          -> add $ arrIdxToRef i
    SmallArray i            -> add $ smallArrIdxToRef i
    SmallMutableArray i     -> add $ smallArrIdxToRef i
    ArrayArray i            -> add $ arrayArrIdxToRef i
    MutableArrayArray i     -> add $ arrayArrIdxToRef i
    ByteArray i             -> add $ printf "$R_MutableByteArray\t%d" (baId i)
    MutableByteArray i      -> add $ printf "$R_MutableByteArray\t%d" (baId i)
    WeakPointer i           -> add $ printf "$R_WeakPointer\t%d" i
    StableName i            -> add $ printf "$R_StableName\t%d" i
    PtrAtom (StablePtr i) _ -> add $ printf "$R_StablePointer\t%d" i
    ThreadId i              -> add $ printf "$R_ThreadId\t%d" i
    _                   -> pure ()

arrIdxToRef :: ArrIdx -> String
arrIdxToRef = \case
  MutArrIdx i -> printf "$R_MutableArray\t%d" i
  ArrIdx i    -> printf "$R_Array\t%d" i

smallArrIdxToRef :: SmallArrIdx -> String
smallArrIdxToRef = \case
  SmallMutArrIdx i  -> printf "$R_SmallMutableArray\t%d" i
  SmallArrIdx i     -> printf "$R_SmallArray\t%d" i

arrayArrIdxToRef :: ArrayArrIdx -> String
arrayArrIdxToRef = \case
  ArrayMutArrIdx i  -> printf "$R_MutableArrayArray\t%d" i
  ArrayArrIdx i     -> printf "$R_ArrayArray\t%d" i


exportStgStateM :: StgState -> DL ()
exportStgStateM stgState@StgState{..} = do
  -- array like resources

  exportArrayLike ssArrays              "Array"               "ArrayArg"
  exportArrayLike ssMutableArrays       "MutableArray"        "MutableArrayArg"

  exportArrayLike ssSmallArrays         "SmallArray"          "SmallArrayArg"
  exportArrayLike ssSmallMutableArrays  "SmallMutableArray"   "SmallMutableArrayArg"

  exportArrayLike ssArrayArrays         "ArrayArray"          "ArrayArrayArg"
  exportArrayLike ssMutableArrayArrays  "MutableArrayArray"   "MutableArrayArrayArg"

  -- simple resources
  forM_ (IntMap.toList ssMutVars) $ \(i, a) -> do
    addFact "MutVar" [I i, A a]

  forM_ (IntMap.toList ssStablePointers) $ \(i, a) -> do
    addFact "StablePointer" [I i, A a]

  forM_ (Map.toList ssStableNameMap) $ \(a, i) -> do
    addFact "StableName" [I i, A a]

  forM_ (IntMap.toList ssMutableByteArrays) $ \(i, ByteArrayDescriptor{..}) -> do
    addFact "MutableByteArray" [I i, S (show baaPinned), I baaAlignment, I $ BA.sizeofMutableByteArray baaMutableByteArray]

  -- mvars
  forM_ (IntMap.toList ssMVars) $ \(i, mv@MVarDescriptor{..}) -> do
    let values = maybeToList mvdValue
    addFact "MVar" [I i, I (length values), I (length mvdQueue), S (show mv)]

    forM_ (zip [0..] values) $ \(idx, a) -> do
      addFact "MVar_Value" [I i, I idx, A a]

    forM_ (zip [0..] mvdQueue) $ \(idx, tid) -> do
      addFact "MVar_Queue" [I i, I idx, I tid]

  -- weak pointers
  forM_ (IntMap.toList ssWeakPointers) $ \(i, wp@WeakPtrDescriptor{..}) -> do
    let values      = maybeToList wpdValue
        finalizers  = maybeToList wpdFinalizer
    addFact "WeakPointer" [I i, A wpdKey, I (length values), I (length finalizers), I (length wpdCFinalizers), S (show wp)]

    forM_ (zip [0..] values) $ \(idx, a) -> do
      addFact "WeakPointer_Value" [I i, I idx, A a]

    forM_ (zip [0..] finalizers) $ \(idx, a) -> do
      addFact "WeakPointer_Finalizer" [I i, I idx, A a]

    forM_ (zip [0..] wpdCFinalizers) $ \(idx, (fun, mEnv, dat)) -> do
      let envs = maybeToList mEnv
      addFact "WeakPointer_CFinalizer" [I i, I idx, A fun, A dat, I (length envs)]
      forM_ (zip [0..] envs) $ \(envIdx, a) -> do
        addFact "WeakPointer_CFinalizer_Env" [I i, I idx, I envIdx, A a]

  -- static global env
  forM_ (Map.toList ssStaticGlobalEnv) $ \(n, a) -> do
    addFact "StaticGlobalEnv" [ID n, A $ snd a]

  -- heap
  forM_ (IntMap.toList ssHeap) $ \(i, ho) -> case ho of
    Con{..} -> do
      addFact "Heap_Con" [I i, S (show hoIsLNE), N (dcUniqueName hoCon), I (length hoConArgs)]
      forM_ (zip [0..] hoConArgs) $ \(idx, a) -> do
        addFact "Heap_ConArg" [I i, I idx, A a]

    Closure{..} -> do
      addFact "Heap_Closure" [I i, S (show hoIsLNE), ID hoName, I (Map.size hoEnv), I (length hoCloArgs), I hoCloMissing]
      forM_ (zip [0..] hoCloArgs) $ \(idx, a) -> do
        addFact "Heap_ClosureArg" [I i, I idx, A a]
      forM_ (zip [0..] (Map.toList hoEnv)) $ \(idx, (n, a)) -> do
        addFact "Heap_ClosureEnv" [I i, I idx, ID n, A $ snd a]

    BlackHole o -> do
      addFact "Heap_BlackHole" [I i, S (GC.debugPrintHeapObject o)]

    ApStack{..} -> do
      let stackId = printf "$ApStackStack(%d)" i
      emitStack stackId hoStack
      addFact "Heap_ApStack" [I i, I (length hoResult), I (length hoStack), S stackId]
      forM_ (zip [0..] hoResult) $ \(idx, a) -> do
        addFact "Heap_ApStackResult" [I i, I idx, A a]

    RaiseException ex -> do
      addFact "Heap_RaiseException" [I i, A ex]

  -- threads
  forM_ (IntMap.toList ssThreads) $ \(i, ThreadState{..}) -> do
    let stackId = printf "$ThreadStack(%d)" i
    emitStack stackId tsStack
    addFact "ThreadState" [I i, S (show tsStatus), I (length tsCurrentResult), S stackId]
    forM_ (zip [0..] tsCurrentResult) $ \(idx, a) -> do
      addFact "Thread_CurrentResult" [I i, I idx, A a]

  -- origin
  forM_ (IntMap.toList ssOrigin) $ \(i, (closureId, closureAddr, tid)) -> do
    addFact "Origin" [I i, ID closureId, I closureAddr, I tid]

  -- current closure (for GC)
  addFact "CurrentClosureAddr" [I ssCurrentClosureAddr]

  -- trace events & markers
  forM_ (zip [0..] $ reverse ssTraceEvents) $ \(i, (n, a)) -> do
    forM_ (genAddressState a) $ \(ns, value) -> do
      addFact "TraceEvent" [S n, I i, S ns, I value]

  forM_ (zip [0..] $ reverse ssTraceMarkers) $ \(i, (n, a)) -> do
    forM_ (genAddressState a) $ \(ns, value) -> do
      addFact "TraceMarker" [S n, I i, S ns, I value]

  -- regions
  forM_ (Map.toList ssRegions) $ \(Region start_name end_name, (_, _curCallGraph, l)) -> do
    forM_ (zip [0..] (reverse l)) $ \(idx, (s, e)) -> do
      forM_ (zip (genAddressState s) (genAddressState e)) $ \((start_ns, start_value), (end_ns, end_value)) -> do
        addFact "Region" [N start_name, N end_name, I idx, S start_ns, I start_value, I end_value]

  -- ssHeapStartAddress
  addFact "HeapStartAddress" [I ssHeapStartAddress]

  -- current address state
  forM_ (genAddressState $ convertAddressState stgState) $ \(ns, value) -> do
    addFact "CurrentAddressState" [S ns, I value]

  -- GC marker
  forM_ (zip [0..] $ reverse ssGCMarkers) $ \(i, a) -> do
    forM_ (genAddressState a) $ \(ns, value) -> do
      addFact "GCMarker" [I i, S ns, I value]

genAddressState :: AddressState -> [(String, Int)]
genAddressState AddressState{..} =
  [ ("$R_HeapPtr",            asNextHeapAddr)
  , ("$R_Array",              asNextArray)
  , ("$R_MutableArray",       asNextMutableArray)
  , ("$R_ArrayArray",         asNextArrayArray)
  , ("$R_MutableArrayArray",  asNextMutableArrayArray)
  , ("$R_MutableByteArray",   asNextMutableByteArray)
  , ("$R_MutVar",             asNextMutVar)
  , ("$R_MVar",               asNextMVar)
  , ("$R_SmallArray",         asNextSmallArray)
  , ("$R_SmallMutableArray",  asNextSmallMutableArray)
  , ("$R_StableName",         asNextStableName)
  , ("$R_StablePointer",      asNextStablePointer)
  , ("$R_WeakPointer",        asNextWeakPointer)
  , ("$R_ThreadId",           asNextThreadId)
  ]

exportArrayLike :: IntMap (Vector Atom) -> Name -> Name -> DL ()
exportArrayLike im arrRelation argRelation = do
  forM_ (IntMap.toList im) $ \(i, arr) -> do
    addFact arrRelation [I i, I $ V.length arr]
    flip V.imapM_ arr $ \idx a -> do
      addFact argRelation [I i, I idx, A a]

emitStack :: String -> [StackContinuation] -> DL ()
emitStack stackId stack = do
  addFact "Stack" [S stackId, I (length stack)]
  forM_ (zip [0..] stack) $ \(frame, sc) -> do
    addFact "StackDesc" [S stackId, I frame, S (showStackCont sc)]
    case sc of
      Apply args -> do
        addFact "Stack_Apply" [S stackId, I frame, I (length args)]
        forM_ (zip [0..] args) $ \(argIdx, a) -> do
          addFact "Stack_ApplyArg" [S stackId, I frame, I argIdx, A a]

      CaseOf _ clName env result _ _ -> do
        addFact "Stack_CaseOf" [S stackId, I frame, ID clName, ID (Id result), I (Map.size env)]
        forM_ (zip [0..] (Map.toList env)) $ \(envIdx, (n, a)) -> do
          addFact "Stack_CaseOfEnv" [S stackId, I frame, I envIdx, ID n, A $ snd a]

      _ -> pure ()

allFactNames :: [Name]
allFactNames = nub
  [ "Array"
  , "ArrayArg"
  , "ArrayArray"
  , "ArrayArrayArg"
  , "AtomToRef"
  , "BinderInfo"
  , "CurrentAddressState"
  , "CurrentClosureAddr"
  , "GCMarker"
  , "Heap_ApStack"
  , "Heap_ApStackResult"
  , "Heap_BlackHole"
  , "Heap_Closure"
  , "Heap_ClosureArg"
  , "Heap_ClosureEnv"
  , "Heap_Con"
  , "Heap_ConArg"
  , "Heap_RaiseException"
  , "HeapStartAddress"
  , "MutableArray"
  , "MutableArrayArg"
  , "MutableArrayArray"
  , "MutableArrayArrayArg"
  , "MutableByteArray"
  , "MutVar"
  , "MVar"
  , "MVar_Queue"
  , "MVar_Value"
  , "Origin"
  , "Region"
  , "SmallArray"
  , "SmallArrayArg"
  , "SmallMutableArray"
  , "SmallMutableArrayArg"
  , "StableName"
  , "StablePointer"
  , "Stack"
  , "Stack_Apply"
  , "Stack_ApplyArg"
  , "Stack_CaseOf"
  , "Stack_CaseOfEnv"
  , "StackDesc"
  , "StaticGlobalEnv"
  , "Thread_CurrentResult"
  , "ThreadState"
  , "TraceEvent"
  , "TraceMarker"
  , "WeakPointer"
  , "WeakPointer_CFinalizer"
  , "WeakPointer_CFinalizer_Env"
  , "WeakPointer_Finalizer"
  , "WeakPointer_Value"
  ]

mkHandles :: FilePath -> [Name] -> IO (Map Name Handle)
mkHandles dir names = do
  l <- forM names $ \n -> do
    let fname = dir </> BS8.unpack n ++ ".facts"
    h <- openFile fname WriteMode
    pure (n, h)
  pure $ Map.fromList l

exportStgState :: FilePath -> StgState -> IO ()
exportStgState dbFolder s = do
  dir <- makeAbsolute dbFolder
  putStrLn $ "save StgState datalog facts to: " ++ dir
  createDirectoryIfMissing True dir
  hMap <- mkHandles dir allFactNames
  DLExport{..} <- execStateT (exportStgStateM s) $ DLExport
    { dleHandleMap  = hMap
    }
  mapM_ hClose $ Map.elems hMap
