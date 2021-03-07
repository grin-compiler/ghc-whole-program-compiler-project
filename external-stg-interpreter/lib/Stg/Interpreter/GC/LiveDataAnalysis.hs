{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, DeriveGeneric #-}
module Stg.Interpreter.GC.LiveDataAnalysis where

import Data.Int
import Data.Bits
import GHC.Generics
import Control.Monad.State
import qualified Data.IntSet as IntSet
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

data Dead = Dead Int32
  deriving (Eq, Show, Generic)

instance Souffle.Program ExtStgGC where
  type ProgramFacts ExtStgGC = [GCRoot, Reference, Dead]
  programName = const "ext_stg_gc"

instance Souffle.Fact GCRoot where
  type FactDirection GCRoot = 'Souffle.Input
  factName = const "GCRoot"

instance Souffle.Fact Reference where
  type FactDirection Reference = 'Souffle.Input
  factName = const "Reference"

instance Souffle.Fact Dead where
  type FactDirection Dead = 'Souffle.Output
  factName = const "Dead"

instance Souffle.Marshal GCRoot
instance Souffle.Marshal Reference
instance Souffle.Marshal Dead

---------------------------
-- analysis api
---------------------------

runLiveDataAnalysis :: [Atom] -> StgState -> IO DeadData
runLiveDataAnalysis extraGCRoots stgState = Souffle.runSouffle ExtStgGC $ \maybeProgram -> do  -- Initializes the Souffle program.
  case maybeProgram of
    Nothing -> liftIO $ fail "Failed to load ext-stg-gc datalog program."
    Just prog -> do
      -- populate input facts
      addGCRootFacts prog stgState extraGCRoots
      addReferenceFacts prog stgState
      -- run analysis
      Souffle.setNumThreads prog 2
      Souffle.run prog
      -- read back result
      readbackDeadData prog

---------------------------
-- handle input facts
---------------------------

addGCRootFacts :: Souffle.Handle ExtStgGC -> StgState -> [Atom] -> SouffleM ()
addGCRootFacts prog StgState{..} extraGCRoots = do
  let addGCRoot :: Atom -> SouffleM ()
      addGCRoot a = visitAtom a $ \i -> Souffle.addFact prog $ GCRoot i

  -- HINT: the following can be GC roots

  -- utility
  visitGCRef addGCRoot extraGCRoots

  -- current closure
  addGCRoot $ HeapPtr ssCurrentClosureAddr

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

readbackDeadData :: Souffle.Handle ExtStgGC -> SouffleM DeadData
readbackDeadData prog = do
  dead :: [Dead] <- Souffle.getFacts prog
  foldM addDead emptyDeadData dead

addDead :: DeadData -> Dead -> SouffleM DeadData
addDead dd@DeadData{..} (Dead l) = do
  -- HINT: decode datalog value
  let namespace = toEnum $ fromIntegral (l .&. 0xf)
      idx       = shiftR (fromIntegral l) 4
  pure $ case namespace of
    NS_Array              -> dd {deadArrays             = IntSet.insert idx deadArrays}
    NS_ArrayArray         -> dd {deadArrayArrays        = IntSet.insert idx deadArrayArrays}
    NS_HeapPtr            -> dd {deadHeap               = IntSet.insert idx deadHeap}
    NS_MutableArray       -> dd {deadMutableArrays      = IntSet.insert idx deadMutableArrays}
    NS_MutableArrayArray  -> dd {deadMutableArrayArrays = IntSet.insert idx deadMutableArrayArrays}
    NS_MutableByteArray   -> dd {deadMutableByteArrays  = IntSet.insert idx deadMutableByteArrays}
    NS_MutVar             -> dd {deadMutVars            = IntSet.insert idx deadMutVars}
    NS_MVar               -> dd {deadMVars              = IntSet.insert idx deadMVars}
    NS_SmallArray         -> dd {deadSmallArrays        = IntSet.insert idx deadSmallArrays}
    NS_SmallMutableArray  -> dd {deadSmallMutableArrays = IntSet.insert idx deadSmallMutableArrays}
    NS_StableName         -> dd {deadStableNames        = IntSet.insert idx deadStableNames}
    NS_WeakPointer        -> dd {deadWeakPointers       = IntSet.insert idx deadWeakPointers}
