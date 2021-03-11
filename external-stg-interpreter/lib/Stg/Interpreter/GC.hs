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
import Stg.Interpreter.Base
import Stg.Interpreter.GC.LiveDataAnalysis
import qualified Stg.Interpreter.PrimOp.WeakPointer as PrimWeakPointer

checkGC :: [Atom] -> M ()
checkGC localGCRoots = do
  tryPrune
  nextAddr <- gets ssNextHeapAddr
  lastGCAddr <- gets ssLastGCAddr
  gcIsRunning <- gets ssGCIsRunning
  when (not gcIsRunning && nextAddr - lastGCAddr > 300000) $ do
    modify' $ \s -> s {ssLastGCAddr = nextAddr, ssGCIsRunning = True}
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

analysisLoop :: MVar ([Atom], StgState) -> MVar DeadData -> IO ()
analysisLoop gcIn gcOut = do
  (localGCRoots, stgState) <- takeMVar gcIn
  deadData <- runLiveDataAnalysis localGCRoots stgState
  reportRemovedData stgState deadData
  reportAddressCounters stgState
  putMVar gcOut deadData
  analysisLoop gcIn gcOut

init :: IO (ThreadId, MVar ([Atom], StgState), MVar DeadData)
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
    Just deadData -> do
      finalizeDeadWeakPointers (deadWeakPointers deadData)
      stgState <- get
      -- remove dead data from stg state
      liftIO $ putStrLn " * done GC"
      put $ (pruneStgState stgState deadData) {ssGCIsRunning = False}
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
  deadData <- liftIO $ runLiveDataAnalysis localGCRoots stgState
  finalizeDeadWeakPointers (deadWeakPointers deadData)
  put $ (pruneStgState stgState deadData) {ssGCIsRunning = False}
  liftIO $ do
    reportRemovedData stgState deadData
    reportAddressCounters stgState
  postGCReport

-- weak pointer handling

finalizeDeadWeakPointers :: IntSet -> M ()
finalizeDeadWeakPointers deadWeaks = do
  pure () -- TODO: check how the native GHC RTS calls weak pointer finalizers

-- utils

pruneStgState :: StgState -> DeadData -> StgState
pruneStgState stgState@StgState{..} DeadData{..} = stgState
  { ssHeap                = IntMap.withoutKeys ssHeap                deadHeap
{-
  -- TODO: run weak pointer finalizers
  , ssWeakPointers        = IntMap.withoutKeys ssWeakPointers        deadWeakPointers
-}
  , ssMVars               = IntMap.withoutKeys ssMVars               deadMVars
  , ssMutVars             = IntMap.withoutKeys ssMutVars             deadMutVars
  , ssArrays              = IntMap.withoutKeys ssArrays              deadArrays
  , ssMutableArrays       = IntMap.withoutKeys ssMutableArrays       deadMutableArrays
  , ssSmallArrays         = IntMap.withoutKeys ssSmallArrays         deadSmallArrays
  , ssSmallMutableArrays  = IntMap.withoutKeys ssSmallMutableArrays  deadSmallMutableArrays
  , ssArrayArrays         = IntMap.withoutKeys ssArrayArrays         deadArrayArrays
  , ssMutableArrayArrays  = IntMap.withoutKeys ssMutableArrayArrays  deadMutableArrayArrays
  , ssMutableByteArrays   = IntMap.withoutKeys ssMutableByteArrays   deadMutableByteArrays
  , ssStableNameMap       = Map.filter (`IntSet.notMember` deadStableNames) ssStableNameMap
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

reportRemovedData :: StgState -> DeadData -> IO ()
reportRemovedData StgState{..} DeadData{..} = do
  let report_ sizeFun msg m s = do
        let old   = sizeFun m
            new   = old - diff
            diff  = IntSet.size s
            ratio = if old == 0 then 0 else 100 * fromIntegral diff / fromIntegral old :: Float
        printf "  %s old: %-10d  new: %-10d  diff: %-10d  dead: %5.2f%%\n" msg old new diff ratio

      reportI = report_ IntMap.size
      reportM = report_ Map.size


  putStrLn "freed after GC:"
  reportI "ssHeap               " ssHeap deadHeap
  reportI "ssWeakPointers       " ssWeakPointers deadWeakPointers
  reportI "ssMVars              " ssMVars deadMVars
  reportI "ssMutVars            " ssMutVars deadMutVars
  reportI "ssArrays             " ssArrays deadArrays
  reportI "ssMutableArrays      " ssMutableArrays deadMutableArrays
  reportI "ssSmallArrays        " ssSmallArrays deadSmallArrays
  reportI "ssSmallMutableArrays " ssSmallMutableArrays deadSmallMutableArrays
  reportI "ssArrayArrays        " ssArrayArrays deadArrayArrays
  reportI "ssMutableArrayArrays " ssMutableArrayArrays deadMutableArrayArrays
  reportI "ssMutableByteArrays  " ssMutableByteArrays deadMutableByteArrays
  reportM "ssStableNameMap      " ssStableNameMap deadStableNames

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
  Closure{..}       -> "Clo: " ++ show hoName ++ " args: " ++ show hoCloArgs ++ " missing: " ++ show hoCloMissing
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
