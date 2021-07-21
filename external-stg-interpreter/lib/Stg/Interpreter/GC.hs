{-# LANGUAGE RecordWildCards, LambdaCase, FlexibleInstances #-}
module Stg.Interpreter.GC where

import Text.Printf
import Control.Monad.State
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Control.Concurrent

import Stg.Syntax
import Stg.Interpreter.Debug (exportCallGraph)
import Stg.Interpreter.Base
import Stg.Interpreter.GC.LiveDataAnalysis
import qualified Stg.Interpreter.PrimOp.WeakPointer as PrimWeakPointer

import Stg.Interpreter.GC.RetainerAnalysis

checkGC :: [Atom] -> M ()
checkGC localGCRoots = do
  tryPrune
  nextAddr <- gets ssNextHeapAddr
  lastGCAddr <- gets ssLastGCAddr
  gcIsRunning <- gets ssGCIsRunning
  when (not gcIsRunning && nextAddr - lastGCAddr > 3000000) $ do
    exportCallGraph -- HINT: export call graph in case the app does not terminate in the normal way
    a <- getAddressState
    modify' $ \s@StgState{..} -> s {ssLastGCAddr = nextAddr, ssGCIsRunning = True, ssGCMarkers = a : ssGCMarkers}
    runGC localGCRoots
    {-
      TODO:
        done - send the current state for live data analysis (async channel) if the GC condition triggers
        done - check if any liveness result has arrived, if so then prune the current state
      DESIGN:
        use MVars for communication
    -}


runGC :: [Atom] -> M ()
runGC = runGCSync

postGCReport :: M ()
postGCReport = do
  heap <- gets ssHeap
  heapStart <- gets ssHeapStartAddress
  let (staticHeap, dynHeap) = IntMap.split (heapStart - 1) heap
      dynHeapSize = IntMap.size dynHeap
      staticHeapSize = IntMap.size staticHeap
  liftIO $ do
    putStrLn $ "heap start:       " ++ show heapStart
    putStrLn $ "static heap size: " ++ show (staticHeapSize + if IntMap.member (heapStart - 1) staticHeap then 1 else 0)
    putStrLn $ "dyn heap size:    " ++ show dynHeapSize

-- async GC

analysisLoop :: MVar ([Atom], StgState) -> MVar RefSet -> IO ()
analysisLoop gcIn gcOut = do
  (localGCRoots, stgState) <- takeMVar gcIn
  rsData <- runLiveDataAnalysis localGCRoots stgState
  reportRemovedData stgState rsData
  reportAddressCounters stgState
  putMVar gcOut rsData
  analysisLoop gcIn gcOut

init :: IO (ThreadId, MVar ([Atom], StgState), MVar RefSet)
init = do
  gcIn <- newEmptyMVar
  gcOut <- newEmptyMVar
  gcTid <- forkIO $ analysisLoop gcIn gcOut
  pure (gcTid, gcIn, gcOut)

tryPrune :: M ()
tryPrune = do
  PrintableMVar gcOut <- gets ssGCOutput
  liftIO (tryTakeMVar gcOut) >>= \case
    Nothing -> pure ()
    Just rsData -> do
      finalizeDeadWeakPointers (rsWeakPointers rsData)
      stgState <- get
      -- remove dead data from stg state
      liftIO $ putStrLn " * done GC"
      put $ (pruneStgState stgState rsData) {ssGCIsRunning = False}
      loadRetanerDb
      postGCReport

runGCAsync :: [Atom] -> M ()
runGCAsync localGCRoots = do
  stgState <- get
  PrintableMVar gcIn <- gets ssGCInput
  liftIO $ do
    putStrLn " * start GC"
    putMVar gcIn (localGCRoots, stgState)

-- synchronous GC

runGCSync :: [Atom] -> M ()
runGCSync localGCRoots = do
  stgState <- get
  rsData <- liftIO $ runLiveDataAnalysis localGCRoots stgState
  put $ (pruneStgState stgState rsData) {ssGCIsRunning = False}
  finalizeDeadWeakPointers (rsWeakPointers rsData)
  loadRetanerDb
  liftIO $ do
    reportRemovedData stgState rsData
    reportAddressCounters stgState
  postGCReport

-- weak pointer handling

finalizeDeadWeakPointers :: IntSet -> M ()
finalizeDeadWeakPointers rsWeaks = do
  pure () -- TODO: check how the native GHC RTS calls weak pointer finalizers

-- utils

pruneStgState :: StgState -> RefSet -> StgState
pruneStgState = pruneStgStateLive

pruneStgStateDead :: StgState -> RefSet -> StgState
pruneStgStateDead stgState@StgState{..} RefSet{..} = stgState
  { ssHeap                = IntMap.withoutKeys ssHeap                rsHeap
{-
  -- TODO: run weak pointer finalizers
  , ssWeakPointers        = IntMap.withoutKeys ssWeakPointers        rsWeakPointers
-}
  , ssMVars               = IntMap.withoutKeys ssMVars               rsMVars
  , ssMutVars             = IntMap.withoutKeys ssMutVars             rsMutVars
  , ssArrays              = IntMap.withoutKeys ssArrays              rsArrays
  , ssMutableArrays       = IntMap.withoutKeys ssMutableArrays       rsMutableArrays
  , ssSmallArrays         = IntMap.withoutKeys ssSmallArrays         rsSmallArrays
  , ssSmallMutableArrays  = IntMap.withoutKeys ssSmallMutableArrays  rsSmallMutableArrays
  , ssArrayArrays         = IntMap.withoutKeys ssArrayArrays         rsArrayArrays
  , ssMutableArrayArrays  = IntMap.withoutKeys ssMutableArrayArrays  rsMutableArrayArrays
  , ssMutableByteArrays   = IntMap.withoutKeys ssMutableByteArrays   rsMutableByteArrays
  , ssStableNameMap       = Map.filter (`IntSet.notMember` rsStableNames) ssStableNameMap
--  , ssThreads             = IntMap.filter (isThreadLive . tsStatus)  ssThreads
  }

pruneStgStateLive :: StgState -> RefSet -> StgState
pruneStgStateLive stgState@StgState{..} RefSet{..} = stgState
  { ssHeap                = IntMap.restrictKeys ssHeap                rsHeap
  , ssOrigin              = IntMap.restrictKeys ssOrigin              rsHeap
{-
  -- TODO: run weak pointer finalizers
  , ssWeakPointers        = IntMap.restrictKeys ssWeakPointers        rsWeakPointers
-}
  , ssMVars               = IntMap.restrictKeys ssMVars               rsMVars
  , ssMutVars             = IntMap.restrictKeys ssMutVars             rsMutVars
  , ssArrays              = IntMap.restrictKeys ssArrays              rsArrays
  , ssMutableArrays       = IntMap.restrictKeys ssMutableArrays       rsMutableArrays
  , ssSmallArrays         = IntMap.restrictKeys ssSmallArrays         rsSmallArrays
  , ssSmallMutableArrays  = IntMap.restrictKeys ssSmallMutableArrays  rsSmallMutableArrays
  , ssArrayArrays         = IntMap.restrictKeys ssArrayArrays         rsArrayArrays
  , ssMutableArrayArrays  = IntMap.restrictKeys ssMutableArrayArrays  rsMutableArrayArrays
  , ssMutableByteArrays   = IntMap.restrictKeys ssMutableByteArrays   rsMutableByteArrays
  , ssStableNameMap       = Map.filter (`IntSet.member` rsStableNames) ssStableNameMap
--  , ssThreads             = IntMap.filter (isThreadLive . tsStatus)  ssThreads
  }

reportAddressCounters :: StgState -> IO ()
reportAddressCounters StgState{..} = do
  let reportI msg val = do
        printf "  %s %d\n" msg val
  putStrLn "resource address counters:"
  reportI "ssNextHeapAddr          " ssNextHeapAddr
  reportI "ssNextStableName        " ssNextStableName
  reportI "ssNextWeakPointer       " ssNextWeakPointer
  reportI "ssNextStablePointer     " ssNextStablePointer
  reportI "ssNextMutableByteArray  " ssNextMutableByteArray
  reportI "ssNextMVar              " ssNextMVar
  reportI "ssNextMutVar            " ssNextMutVar
  reportI "ssNextArray             " ssNextArray
  reportI "ssNextMutableArray      " ssNextMutableArray
  reportI "ssNextSmallArray        " ssNextSmallArray
  reportI "ssNextSmallMutableArray " ssNextSmallMutableArray
  reportI "ssNextArrayArray        " ssNextArrayArray
  reportI "ssNextMutableArrayArray " ssNextMutableArrayArray

reportRemovedData :: StgState -> RefSet -> IO ()
reportRemovedData StgState{..} RefSet{..} = do
  let report_ sizeFun msg m s = do
        let old   = sizeFun m
            new   = old - diff
            diff  = IntSet.size s
            ratio = if old == 0 then 0 else 100 * fromIntegral diff / fromIntegral old :: Float
        printf "  %s old: %-10d  new: %-10d  diff: %-10d  dead: %5.2f%%\n" msg old new diff ratio

      reportI = report_ IntMap.size
      reportM = report_ Map.size


  putStrLn "freed after GC:"
  reportI "ssHeap               " ssHeap rsHeap
  reportI "ssWeakPointers       " ssWeakPointers rsWeakPointers
  reportI "ssMVars              " ssMVars rsMVars
  reportI "ssMutVars            " ssMutVars rsMutVars
  reportI "ssArrays             " ssArrays rsArrays
  reportI "ssMutableArrays      " ssMutableArrays rsMutableArrays
  reportI "ssSmallArrays        " ssSmallArrays rsSmallArrays
  reportI "ssSmallMutableArrays " ssSmallMutableArrays rsSmallMutableArrays
  reportI "ssArrayArrays        " ssArrayArrays rsArrayArrays
  reportI "ssMutableArrayArrays " ssMutableArrayArrays rsMutableArrayArrays
  reportI "ssMutableByteArrays  " ssMutableByteArrays rsMutableByteArrays
  reportM "ssStableNameMap      " ssStableNameMap rsStableNames

  let threads = IntMap.elems ssThreads
  printf "live threads: %-6d  all threads: %d\n" (length $ [ts | ts <- threads, isThreadLive (tsStatus ts)]) (length threads)

{-
lifetimeAnalysis :: M ()
lifetimeAnalysis = do
  heap <- gets ssHeap
  heapStart <- gets ssHeapStartAddress
  let age = 600000 -- heapStart - 1
  let (_, dynHeap) = IntMap.split age heap
      n = 100
      some = take n $ IntMap.toList dynHeap
  liftIO $ do
    printf "the first %d younger than %d (age) non-static heap objects:\n" n age
    forM_ some $ \(i, o) -> printf "%-8d %3s  %s\n" i (ppLNE o) (debugPrintHeapObject o)
-}
debugPrintHeapObject :: HeapObject -> String
debugPrintHeapObject  = \case
  Con{..}           -> "Con: " ++ show (dcUniqueName hoCon) ++ " " ++ show hoConArgs
  Closure{..}       -> "Clo: " ++ show hoName ++ " args: " ++ show hoCloArgs ++ " env: " ++ show (Map.size hoEnv) ++ " missing: " ++ show hoCloMissing
  BlackHole o       -> "BlackHole - " ++ debugPrintHeapObject o
  ApStack{}         -> "ApStack"
  RaiseException ex -> "RaiseException: " ++ show ex

ppLNE :: HeapObject -> String
ppLNE = \case
  Con{..} | hoIsLNE     -> "LNE"
  Closure{..} | hoIsLNE -> "LNE"
  _ -> ""

{-
  TODO:
    skip - weak pointers finalizer
    done - print resource address counters after each GC
    done - switch to 5 bit datalog namespace int32 encoding ; Q: should we switch to 64 bit ints?
    - add stack namespace
    - add local root namespace
    - add only the stack and local to gcroot
    - add stack and local to reference relation
    - NOT-NOW: change namespace encoding to non-uniform way: heap ptr 0. bit = 0 [OPTIMIZATION / FUTURE WORK]
  with the Retain relation we could see the retainer origin

  show:
    - origin    addr + closure name + live/dead
    - retainer  addr + closure name

  new debug commands:
    - stop      ; pause execution and listen to commands
    - retainer  ; show retainer
    - origin    ; show value origin
    - info      ; show heap object + 'retainer' + 'origin'
    - trace-retainer  ; trace back values to GCRoots and print retainer objects their origins ; transitive closure of 'info'
    - trace-origin    ; trace back value origin to the oldest living object (show 'info' after each step)
    - report-state    ; report stack + current thread + next heap/array/etc. addresses
    - gc        ; run gc
    - lifetime age count   ; call lifetimeAnalysis with the age threshold and item count
-}
