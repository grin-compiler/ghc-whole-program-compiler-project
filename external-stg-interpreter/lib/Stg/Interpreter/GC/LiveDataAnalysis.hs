{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}
module Stg.Interpreter.GC.LiveDataAnalysis where

import GHC.Generics
import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set
import System.FilePath
import System.Directory
import Foreign.Ptr
import Text.Printf
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Language.Souffle.Compiled (SouffleM)
import qualified Language.Souffle.Compiled as Souffle

import Stg.Interpreter.Base
import Stg.Interpreter.GC.GCRef

-------- souffle program

data ExtStgGC = ExtStgGC

data GCRoot = GCRoot String
  deriving (Eq, Show, Generic)

data Reference = Reference String String
  deriving (Eq, Show, Generic)

data MaybeDeadlockingThread = MaybeDeadlockingThread String
  deriving (Eq, Show, Generic)

instance Souffle.Program ExtStgGC where
  type ProgramFacts ExtStgGC = [GCRoot, Reference, MaybeDeadlockingThread]
  programName = const "ext_stg_gc"

instance Souffle.Fact GCRoot where
  type FactDirection GCRoot = 'Souffle.Input
  factName = const "GCRoot"

instance Souffle.Fact Reference where
  type FactDirection Reference = 'Souffle.Input
  factName = const "Reference"

instance Souffle.Fact MaybeDeadlockingThread where
  type FactDirection MaybeDeadlockingThread = 'Souffle.Input
  factName = const "MaybeDeadlockingThread"

instance Souffle.Marshal GCRoot
instance Souffle.Marshal Reference
instance Souffle.Marshal MaybeDeadlockingThread

---------------------------
-- analysis api
---------------------------

runLiveDataAnalysis :: [Atom] -> StgState -> IO (RefSet, IntSet)
runLiveDataAnalysis extraGCRoots stgState = Souffle.runSouffle ExtStgGC $ \maybeProgram -> do  -- Initializes the Souffle program.
  case maybeProgram of
    Nothing -> liftIO $ fail "Failed to load ext-stg-gc datalog program."
    Just prog -> do
      -- populate input facts
      addGCRootFacts prog stgState extraGCRoots
      addReferenceFacts prog stgState
      addMaybeDeadlockingThreadFacts prog stgState
      -- run analysis
      Souffle.setNumThreads prog 8
      Souffle.run prog

      let factPath = if dsKeepGCFacts $ ssDebugSettings stgState
            then "./.gc-datalog-facts" </> printf "gc-cycle-%03i" (ssGCCounter stgState)
            else "./.gc-datalog-facts"
      absFactPath <- liftIO $ makeAbsolute factPath
      liftIO $ do
        createDirectoryIfMissing True absFactPath
        unless (ssIsQuiet stgState) $ do
          putStrLn $ "write gc facts to: " ++ absFactPath
      Souffle.writeFiles prog absFactPath
      unless (ssIsQuiet stgState) $ do
        liftIO $ putStrLn "Souffle.writeFiles done"

      -- read back result
      --readbackDeadData prog
      (,) <$> readbackLiveData factPath (ssIsQuiet stgState)
          <*> readbackDeadlockingThreadData factPath (ssIsQuiet stgState)

---------------------------
-- handle input facts
---------------------------

addGCRootFacts :: Souffle.Handle ExtStgGC -> StgState -> [Atom] -> SouffleM ()
addGCRootFacts prog stgState localGCRoots = withGCRootFacts stgState localGCRoots $ \_msg s -> do
  Souffle.addFact prog $ GCRoot $ BS8.unpack $ unGCSymbol s

addReferenceFacts :: Souffle.Handle ExtStgGC -> StgState -> SouffleM ()
addReferenceFacts prog stgState = withReferenceFacts stgState $ \from to -> do
  Souffle.addFact prog $ Reference (BS8.unpack $ unGCSymbol from) (BS8.unpack $ unGCSymbol to)

withGCRootFacts :: Monad m => StgState -> [Atom] -> (String -> GCSymbol -> m ()) -> m ()
withGCRootFacts StgState{..} localGCRoots addGCRoot = do

  -- HINT: the following can be GC roots
  {-
    gc roots:
      - stable pointer indices
      - threads state (result + stack)
      - cafs
      - local
      - current closure ; Q: is it an extra? A: yes, it belongs to the local roots
  -}

  -- local
  visitGCRef (addGCRoot "local") localGCRoots

  -- stable pointer values
  visitGCRef (addGCRoot "stable pointer") [PtrAtom (StablePtr idx) (intPtrToPtr $ IntPtr idx) | idx <- IntMap.keys ssStablePointers]

  -- CAFs
  visitGCRef (addGCRoot "CAF") $ map HeapPtr $ IntSet.toList ssCAFSet

  -- stack continuations of live threads
  forM_ (IntMap.toList ssThreads) $ \(tid, ts) -> case tsStatus ts of
    ThreadFinished  -> pure ()
    ThreadDied      -> pure ()
    ThreadRunning   -> addGCRoot "thread" $ encodeRef tid NS_Thread
    ThreadBlocked r -> case r of
      BlockedOnMVar{}         -> pure () -- will be referred by the mvar wait queue
      BlockedOnMVarRead{}     -> pure () -- will be referred by the mvar wait queue
      BlockedOnBlackHole{}    -> pure () -- will be referred by the BlockedOnBlackHole ADDR thread status
      BlockedOnThrowAsyncEx{} -> pure () -- will be referred by the target thread's blocked exceptions queue
      BlockedOnSTM{}          -> pure () -- will be referred by the tvar wait queue
      BlockedOnForeignCall{}  -> error "not implemented yet"
      BlockedOnRead{}         -> addGCRoot "thread" $ encodeRef tid NS_Thread
      BlockedOnWrite{}        -> addGCRoot "thread" $ encodeRef tid NS_Thread
      BlockedOnDelay{}        -> addGCRoot "thread" $ encodeRef tid NS_Thread

withReferenceFacts :: forall m . Monad m => StgState -> (GCSymbol -> GCSymbol -> m ()) -> m ()
withReferenceFacts StgState{..} addReference = do
  let addRefs :: (VisitGCRef a, Monad m) => IntMap a -> RefNamespace -> m ()
      addRefs im ns = do
        let l = IntMap.toList im
        forM_ l $ \(i, v) -> visitGCRef (addReference (encodeRef i ns)) v

  -- HINT: these types are tracked by GC
  let dynamicHeap = IntMap.filterWithKey (\a _ -> a >= ssDynamicHeapStart) ssHeap
      cafHeap     = IntMap.restrictKeys ssHeap ssCAFSet
  addRefs dynamicHeap           NS_HeapPtr
  addRefs cafHeap               NS_HeapPtr
  addRefs ssWeakPointers        NS_WeakPointer
  addRefs ssTVars               NS_TVar
  addRefs ssMVars               NS_MVar
  addRefs ssMutVars             NS_MutVar
  addRefs ssArrays              NS_Array
  addRefs ssMutableArrays       NS_MutableArray
  addRefs ssSmallArrays         NS_SmallArray
  addRefs ssSmallMutableArrays  NS_SmallMutableArray
  addRefs ssArrayArrays         NS_ArrayArray
  addRefs ssMutableArrayArrays  NS_MutableArrayArray
  addRefs ssStablePointers      NS_StablePointer
  addRefs ssThreads             NS_Thread

  -- references for backhole wait queues
  let blackholes = [ (tid, addr)
                   | (tid, ts) <- IntMap.toList ssThreads
                   , Update addr <- tsStack ts
                   ]
  forM_ blackholes $ \(tid, addr) -> case IntMap.lookup addr ssHeap of
    Just (BlackHole _ _ waitingThreads) -> do
      forM_ waitingThreads $ \waitingTid -> do
        addReference (encodeRef tid NS_Thread) (encodeRef waitingTid NS_Thread)
    ho -> error $ "internal error - expected Blackhole, got: " ++ show ho

  -- stable name references
  let stableNames = Map.toList ssStableNameMap
  forM_ stableNames $ \(v, i) -> visitGCRef (addReference (encodeRef i NS_StableName)) v

addMaybeDeadlockingThreadFacts :: Souffle.Handle ExtStgGC -> StgState -> SouffleM ()
addMaybeDeadlockingThreadFacts prog StgState{..} = do
  let addMaybeDeadlockingThread :: GCSymbol -> SouffleM ()
      addMaybeDeadlockingThread i = Souffle.addFact prog $ MaybeDeadlockingThread $ BS8.unpack $ unGCSymbol i

  -- potentially deadlocking threads to check
  forM_ (IntMap.toList ssThreads) $ \(tid, ts) -> case tsStatus ts of
    ThreadFinished  -> pure ()
    ThreadDied      -> pure ()
    ThreadRunning   -> pure ()
    ThreadBlocked r -> case r of
      BlockedOnMVar{}         -> addMaybeDeadlockingThread $ encodeRef tid NS_Thread
      BlockedOnMVarRead{}     -> addMaybeDeadlockingThread $ encodeRef tid NS_Thread
      BlockedOnBlackHole{}    -> addMaybeDeadlockingThread $ encodeRef tid NS_Thread
      BlockedOnThrowAsyncEx{} -> addMaybeDeadlockingThread $ encodeRef tid NS_Thread
      BlockedOnSTM{}          -> addMaybeDeadlockingThread $ encodeRef tid NS_Thread
      BlockedOnForeignCall{}  -> error "not implemented yet"
      BlockedOnRead{}         -> pure ()
      BlockedOnWrite{}        -> pure ()
      BlockedOnDelay{}        -> pure ()

---------------------------
-- handle output facts
---------------------------
{-
readbackDeadData :: Souffle.Handle ExtStgGC -> SouffleM RefSet
readbackDeadData prog = do
  dead :: [Dead] <- Souffle.getFacts prog
  foldM collectGCSymbol emptyRefSet dead
-}

collectGCSymbol :: RefSet -> GCSymbol -> SouffleM RefSet
collectGCSymbol dd@RefSet{..} sym = do
  -- HINT: decode datalog value
  let (namespace, idx) = decodeRef sym
  pure $ case namespace of
    NS_Array              -> dd {rsArrays             = IntSet.insert idx rsArrays}
    NS_ArrayArray         -> dd {rsArrayArrays        = IntSet.insert idx rsArrayArrays}
    NS_HeapPtr            -> dd {rsHeap               = IntSet.insert idx rsHeap}
    NS_MutableArray       -> dd {rsMutableArrays      = IntSet.insert idx rsMutableArrays}
    NS_MutableArrayArray  -> dd {rsMutableArrayArrays = IntSet.insert idx rsMutableArrayArrays}
    NS_MutableByteArray   -> dd {rsMutableByteArrays  = IntSet.insert idx rsMutableByteArrays}
    NS_MutVar             -> dd {rsMutVars            = IntSet.insert idx rsMutVars}
    NS_TVar               -> dd {rsTVars              = IntSet.insert idx rsTVars}
    NS_MVar               -> dd {rsMVars              = IntSet.insert idx rsMVars}
    NS_SmallArray         -> dd {rsSmallArrays        = IntSet.insert idx rsSmallArrays}
    NS_SmallMutableArray  -> dd {rsSmallMutableArrays = IntSet.insert idx rsSmallMutableArrays}
    NS_StableName         -> dd {rsStableNames        = IntSet.insert idx rsStableNames}
    NS_WeakPointer        -> dd {rsWeakPointers       = IntSet.insert idx rsWeakPointers}
    NS_StablePointer      -> dd {rsStablePointers     = IntSet.insert idx rsStablePointers}
    NS_Thread             -> dd {rsThreads            = IntSet.insert idx rsThreads}
    _                     -> error $ "invalid dead value: " ++ show namespace ++ " " ++ show idx

readbackLiveData :: FilePath -> Bool -> SouffleM RefSet
readbackLiveData factDir isQuiet = do
  liveSet <- liftIO $ loadStringSet isQuiet (factDir </> "Live.csv")
  foldM (\dd sym -> collectGCSymbol dd sym) emptyRefSet $ Set.toList liveSet

readbackDeadlockingThreadData :: FilePath -> Bool -> SouffleM IntSet
readbackDeadlockingThreadData factDir isQuiet = do
  deadlockingSet <- liftIO $ loadStringSet isQuiet (factDir </> "DeadlockingThread.csv")
  rsThreads <$> foldM (\dd sym -> collectGCSymbol dd sym) emptyRefSet (Set.toList deadlockingSet)

loadStringSet :: Bool -> String -> IO (Set GCSymbol)
loadStringSet isQuiet factPath = do
  absFactPath <- makeAbsolute factPath
  unless isQuiet $ do
    putStrLn $ "loading: " ++ show absFactPath
  Set.fromList . map GCSymbol . BS8.lines <$> BS8.readFile absFactPath
