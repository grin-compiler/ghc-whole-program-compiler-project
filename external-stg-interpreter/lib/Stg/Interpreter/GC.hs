{-# LANGUAGE RecordWildCards, LambdaCase, FlexibleInstances #-}
module Stg.Interpreter.GC where

import Text.Printf
import Control.Monad.State
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import Stg.Interpreter.Base
import Stg.Interpreter.GC.LiveDataAnalysis

runGC :: [Atom] -> M ()
runGC extraGCRoots = do
  liftIO $ putStrLn " * runGC"
  stgState <- get
  liveData <- liftIO $ runLiveDataAnalysis extraGCRoots stgState

  liftIO $ reportRemovedData stgState liveData

  -- remove dead data from stg state
  put $ pruneStgState stgState liveData
  pure ()

pruneStgState :: StgState -> LiveData -> StgState
pruneStgState stgState@StgState{..} LiveData{..} = stgState
  { ssHeap                = IntMap.restrictKeys ssHeap                liveHeap
{-
  -- TODO: run weak pointer finalizers
  , ssWeakPointers        = IntMap.restrictKeys ssWeakPointers        liveWeakPointers
-}
  , ssMVars               = IntMap.restrictKeys ssMVars               liveMVars
  , ssMutVars             = IntMap.restrictKeys ssMutVars             liveMutVars
  , ssArrays              = IntMap.restrictKeys ssArrays              liveArrays
  , ssMutableArrays       = IntMap.restrictKeys ssMutableArrays       liveMutableArrays
  , ssSmallArrays         = IntMap.restrictKeys ssSmallArrays         liveSmallArrays
  , ssSmallMutableArrays  = IntMap.restrictKeys ssSmallMutableArrays  liveSmallMutableArrays
  , ssArrayArrays         = IntMap.restrictKeys ssArrayArrays         liveArrayArrays
  , ssMutableArrayArrays  = IntMap.restrictKeys ssMutableArrayArrays  liveMutableArrayArrays
  , ssMutableByteArrays   = IntMap.restrictKeys ssMutableByteArrays   liveMutableByteArrays
  , ssStableNameMap       = Map.filter (`IntSet.member` liveStableNames) ssStableNameMap
  }

reportRemovedData :: StgState -> LiveData -> IO ()
reportRemovedData StgState{..} LiveData{..} = do
  let report msg m s  = printf "  %s old: %-10d  new: %-10d  diff: %-10d\n" msg (IntMap.size m) (IntSet.size s) (IntMap.size m - IntSet.size s)
  let report' msg m s = printf "  %s old: %-10d  new: %-10d  diff: %-10d\n" msg (Map.size m) (IntSet.size s) (Map.size m - IntSet.size s)
  putStrLn "freed after GC:"

  report  "ssHeap               " ssHeap liveHeap
  report  "ssWeakPointers       " ssWeakPointers liveWeakPointers
  report  "ssMVars              " ssMVars liveMVars
  report  "ssMutVars            " ssMutVars liveMutVars
  report  "ssArrays             " ssArrays liveArrays
  report  "ssMutableArrays      " ssMutableArrays liveMutableArrays
  report  "ssSmallArrays        " ssSmallArrays liveSmallArrays
  report  "ssSmallMutableArrays " ssSmallMutableArrays liveSmallMutableArrays
  report  "ssArrayArrays        " ssArrayArrays liveArrayArrays
  report  "ssMutableArrayArrays " ssMutableArrayArrays liveMutableArrayArrays
  report  "ssMutableByteArrays  " ssMutableByteArrays liveMutableByteArrays
  report' "ssStableNameMap      " ssStableNameMap liveStableNames
