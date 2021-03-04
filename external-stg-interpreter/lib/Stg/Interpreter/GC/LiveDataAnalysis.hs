{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}
module Stg.Interpreter.GC.LiveDataAnalysis where

import Data.Int
import Data.Bits
import GHC.Generics
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Language.Souffle.Compiled (SouffleM)
import qualified Language.Souffle.Compiled as Souffle

import Stg.Interpreter.Base
import Stg.Interpreter.GC.GCRef

-------- souffle program

data ExtStgGC = ExtStgGC

data GCRoot = GCRoot Int32
  deriving (Eq, Show, Generic)

data Reference = Reference Int32 Int32
  deriving (Eq, Show, Generic)

data Live = Live Int32
  deriving (Eq, Show, Generic)

instance Souffle.Program ExtStgGC where
  type ProgramFacts ExtStgGC = [GCRoot, Reference, Live]
  programName = const "ext-stg-gc"

instance Souffle.Fact GCRoot where
  type FactDirection GCRoot = 'Souffle.Input
  factName = const "GCRoot"

instance Souffle.Fact Reference where
  type FactDirection Reference = 'Souffle.Input
  factName = const "Reference"

instance Souffle.Fact Live where
  type FactDirection Live = 'Souffle.Output
  factName = const "Live"

instance Souffle.Marshal GCRoot
instance Souffle.Marshal Reference
instance Souffle.Marshal Live

---------------------------
-- analysis api
---------------------------

runLiveDataAnalysis :: StgState -> IO LiveData
runLiveDataAnalysis stgState = Souffle.runSouffle ExtStgGC $ \maybeProgram -> do  -- Initializes the Souffle program.
  case maybeProgram of
    Nothing -> liftIO $ fail "Failed to load ext-stg-gc datalog program."
    Just prog -> do
      -- populate input facts
      addGCRootFacts prog stgState
      addReferenceFacts prog stgState
      -- run analysis
      Souffle.setNumThreads prog 4
      Souffle.run prog
      -- read back result
      readbackLiveData prog

---------------------------
-- handle input facts
---------------------------

addGCRootFacts :: Souffle.Handle ExtStgGC -> StgState -> SouffleM ()
addGCRootFacts prog StgState{..} = do
  let addGCRoot :: Atom -> SouffleM ()
      addGCRoot a = visitAtom a $ \i -> Souffle.addFact prog $ GCRoot i

  -- HINT: the following can be GC roots

  -- stable pointer values
  visitGCRef addGCRoot ssStablePointers

  -- static global env
  visitGCRef addGCRoot ssStaticGlobalEnv

  -- stack continuations of live threads
  forM_ ssThreads $ \ts -> case isThreadLive (tsStatus ts) of
    False -> pure ()
    True  -> visitGCRef addGCRoot ts

addReferenceFacts :: Souffle.Handle ExtStgGC -> StgState -> SouffleM ()
addReferenceFacts prog StgState{..} = do
  let addReference :: Int32 -> Atom -> SouffleM ()
      addReference from a = visitAtom a $ \i -> Souffle.addFact prog $ Reference from i

      addRefs :: VisitGCRef a => IntMap a -> RefNamespace -> SouffleM ()
      addRefs im ns = do
        let l = IntMap.toList im
        forM_ l $ \(i, v) -> visitGCRef (addReference (encodeRef i ns)) v

  -- HINT: these types are tracked by GC
  addRefs ssHeap                NS_HeapPtr
  addRefs ssWeakPointers        NS_WeakPointer
  addRefs ssMVars               NS_MVar
  addRefs ssMutVars             NS_MutVar
  addRefs ssArrays              NS_Array
  addRefs ssMutableArrays       NS_MutableArray
  addRefs ssSmallArrays         NS_SmallArray
  addRefs ssSmallMutableArrays  NS_SmallMutableArray
  addRefs ssArrayArrays         NS_ArrayArray
  addRefs ssMutableArrayArrays  NS_MutableArrayArray

  -- stable name references
  let stableNames = Map.toList ssStableNameMap
  forM_ stableNames $ \(v, i) -> visitGCRef (addReference (encodeRef i NS_StableName)) v

---------------------------
-- handle output facts
---------------------------

data LiveData
  = LiveData
  { liveHeap                :: !(Set Int)
  , liveWeakPointers        :: !(Set Int)
  , liveMVars               :: !(Set Int)
  , liveMutVars             :: !(Set Int)
  , liveArrays              :: !(Set Int)
  , liveMutableArrays       :: !(Set Int)
  , liveSmallArrays         :: !(Set Int)
  , liveSmallMutableArrays  :: !(Set Int)
  , liveArrayArrays         :: !(Set Int)
  , liveMutableArrayArrays  :: !(Set Int)
  , liveStableNames         :: !(Set Int)
  }

emptyLiveData :: LiveData
emptyLiveData = LiveData
  { liveHeap                = Set.empty
  , liveWeakPointers        = Set.empty
  , liveMVars               = Set.empty
  , liveMutVars             = Set.empty
  , liveArrays              = Set.empty
  , liveMutableArrays       = Set.empty
  , liveSmallArrays         = Set.empty
  , liveSmallMutableArrays  = Set.empty
  , liveArrayArrays         = Set.empty
  , liveMutableArrayArrays  = Set.empty
  , liveStableNames         = Set.empty
  }

readbackLiveData :: Souffle.Handle ExtStgGC -> SouffleM LiveData
readbackLiveData prog = do
  live :: [Live] <- Souffle.getFacts prog
  foldM addLive emptyLiveData live

addLive :: LiveData -> Live -> SouffleM LiveData
addLive ld@LiveData{..} (Live l) = do
  -- HINT: decode datalog value
  let namespace = toEnum $ fromIntegral (l .&. 0xf)
      idx       = shiftR (fromIntegral l) 4
  pure $ case namespace of
    NS_Array              -> ld {liveArrays             = Set.insert idx liveArrays}
    NS_ArrayArray         -> ld {liveArrayArrays        = Set.insert idx liveArrayArrays}
    NS_HeapPtr            -> ld {liveHeap               = Set.insert idx liveHeap}
    NS_MutableArray       -> ld {liveMutableArrays      = Set.insert idx liveMutableArrays}
    NS_MutableArrayArray  -> ld {liveMutableArrayArrays = Set.insert idx liveMutableArrayArrays}
    NS_MutVar             -> ld {liveMutVars            = Set.insert idx liveMutVars}
    NS_MVar               -> ld {liveMVars              = Set.insert idx liveMVars}
    NS_SmallArray         -> ld {liveSmallArrays        = Set.insert idx liveSmallArrays}
    NS_SmallMutableArray  -> ld {liveSmallMutableArrays = Set.insert idx liveSmallMutableArrays}
    NS_StableName         -> ld {liveStableNames        = Set.insert idx liveStableNames}
    NS_WeakPointer        -> ld {liveWeakPointers       = Set.insert idx liveWeakPointers}
