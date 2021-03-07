{-# LANGUAGE RecordWildCards, LambdaCase, FlexibleInstances #-}
module Stg.Interpreter.GC where

import Text.Printf
import Control.Monad.State
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Control.Concurrent

import Stg.Interpreter.Base
import Stg.Interpreter.GC.LiveDataAnalysis

checkGC :: [Atom] -> M ()
checkGC extraGCRoots = do
  tryPrune
  nextAddr <- gets ssNextAddr
  lastGCAddr <- gets ssLastGCAddr
  gcIsRunning <- gets ssGCIsRunning
  when (not gcIsRunning && nextAddr - lastGCAddr > 300000) $ do
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
  putMVar gcOut deadData
  --reportRemovedData stgState deadData
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
      --liftIO $ putStrLn " * done GC"
      put $ (pruneStgState stgState deadData) {ssGCIsRunning = False}

runGCAsync :: [Atom] -> M ()
runGCAsync extraGCRoots = do
  stgState <- get
  PrintableMVar gcIn <- gets ssGCInput
  liftIO $ do
    --putStrLn " * start GC"
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
