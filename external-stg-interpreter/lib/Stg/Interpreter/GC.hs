{-# LANGUAGE RecordWildCards, LambdaCase, FlexibleInstances #-}
module Stg.Interpreter.GC where

import Text.Printf
import Control.Monad.State
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Control.Concurrent

import Stg.Syntax
import Stg.Interpreter.Base
import Stg.Interpreter.GC.LiveDataAnalysis

checkGC :: [Atom] -> M ()
checkGC extraGCRoots = do
  tryPrune
  nextAddr <- gets ssNextAddr
  lastGCAddr <- gets ssLastGCAddr
  gcIsRunning <- gets ssGCIsRunning
  when (not gcIsRunning && nextAddr - lastGCAddr > 300000) $ do
    lifetimeAnalysis
    modify' $ \s -> s {ssLastGCAddr = nextAddr, ssGCIsRunning = True}
    runGC extraGCRoots
    {-
      TODO:
        done - send the current state for live data analysis (async channel) if the GC condition triggers
        done - check if any liveness result has arrived, if so then prune the current state
      DESIGN:
        use MVars for communication
    -}


runGC :: [Atom] -> M ()
runGC = runGCAsync

-- async GC

analysisLoop :: MVar ([Atom], StgState) -> MVar DeadData -> IO ()
analysisLoop gcIn gcOut = do
  (extraGCRoots, stgState) <- takeMVar gcIn
  deadData <- runLiveDataAnalysis extraGCRoots stgState
  reportRemovedData stgState deadData
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
      stgState <- get
      -- remove dead data from stg state
      liftIO $ putStrLn " * done GC"
      put $ (pruneStgState stgState deadData) {ssGCIsRunning = False}

runGCAsync :: [Atom] -> M ()
runGCAsync extraGCRoots = do
  stgState <- get
  PrintableMVar gcIn <- gets ssGCInput
  liftIO $ do
    putStrLn " * start GC"
    putMVar gcIn (extraGCRoots, stgState)

-- synchronous GC

runGCSync :: [Atom] -> M ()
runGCSync extraGCRoots = do
  stgState <- get
  deadData <- liftIO $ runLiveDataAnalysis extraGCRoots stgState
  put $ pruneStgState stgState deadData
  liftIO $ reportRemovedData stgState deadData

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
    - add stack namespace
    - add extra root namespace
    - add only the stack and extra to gcroot
    - add stack and extra to reference relation
  with the Retain relation we could see the retainer origin

  show:
    - origin    addr + closure name + live/dead
    - retainer  addr + closure name

  new debug commands:
    - stop      ; pause execution and listen to commands
    - retainer  ; show retainer
    - origin    ; show value origin
    - info      ; show heap object + 'retainer' + 'origin'
    - track-retainer  ; track back values to GCRoots and print retainer objects their origins ; transitive closure of 'info'
    - track-origin    ; track back value origin to the oldest living object (show 'info' after each step)
    - report-state    ; report stack + current thread + next heap/array/etc. addresses
    - gc        ; run gc
    - lifetime age count   ; call lifetimeAnalysis with the age threshold and item count
-}
