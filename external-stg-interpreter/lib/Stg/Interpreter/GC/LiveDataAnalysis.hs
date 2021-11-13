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
import System.Directory
import Foreign.Ptr

import Language.Souffle.Compiled (SouffleM)
import qualified Language.Souffle.Compiled as Souffle

import Stg.Interpreter.Base
import Stg.Interpreter.GC.GCRef

import qualified Stg.Interpreter.GC.RetainerAnalysis as Live


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

runLiveDataAnalysis :: [Atom] -> StgState -> IO RefSet
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

      let factPath = "./.gc-datalog-facts"
      absFactPath <- liftIO $ makeAbsolute factPath
      liftIO $ do
        createDirectoryIfMissing True absFactPath
        putStrLn $ "write gc facts to: " ++ absFactPath
      Souffle.writeFiles prog absFactPath
      liftIO $ putStrLn "Souffle.writeFiles done"

      -- read back result
      --readbackDeadData prog
      readbackLiveData

---------------------------
-- handle input facts
---------------------------

addGCRootFacts :: Souffle.Handle ExtStgGC -> StgState -> [Atom] -> SouffleM ()
addGCRootFacts prog StgState{..} localGCRoots = do
  let addGCRoot :: Atom -> SouffleM ()
      addGCRoot a = visitAtom a $ \i -> Souffle.addFact prog $ GCRoot i

  -- HINT: the following can be GC roots
  {-
    gc roots:
      - stable pointer indices
      - stack indices
      - static env
      - local
      - current closure ; Q: is it an extra? A: yes, it belongs to the local roots
  -}

  -- local
  visitGCRef addGCRoot localGCRoots

  -- stable pointer values
  visitGCRef addGCRoot [PtrAtom (StablePtr idx) (intPtrToPtr $ IntPtr idx) | idx <- IntMap.keys ssStablePointers]

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
  addRefs ssStablePointers      NS_StablePointer

  -- stable name references
  let stableNames = Map.toList ssStableNameMap
  forM_ stableNames $ \(v, i) -> visitGCRef (addReference (encodeRef i NS_StableName)) v

---------------------------
-- handle output facts
---------------------------

readbackDeadData :: Souffle.Handle ExtStgGC -> SouffleM RefSet
readbackDeadData prog = do
  dead :: [Dead] <- Souffle.getFacts prog
  foldM collectDead emptyRefSet dead

collectDead :: RefSet -> Dead -> SouffleM RefSet
collectDead dd@RefSet{..} (Dead l) = do
  -- HINT: decode datalog value
  let namespace = toEnum $ fromIntegral (l .&. 0xf)
      idx       = shiftR (fromIntegral l) 4
  pure $ case namespace of
    NS_Array              -> dd {rsArrays             = IntSet.insert idx rsArrays}
    NS_ArrayArray         -> dd {rsArrayArrays        = IntSet.insert idx rsArrayArrays}
    NS_HeapPtr            -> dd {rsHeap               = IntSet.insert idx rsHeap}
    NS_MutableArray       -> dd {rsMutableArrays      = IntSet.insert idx rsMutableArrays}
    NS_MutableArrayArray  -> dd {rsMutableArrayArrays = IntSet.insert idx rsMutableArrayArrays}
    NS_MutableByteArray   -> dd {rsMutableByteArrays  = IntSet.insert idx rsMutableByteArrays}
    NS_MutVar             -> dd {rsMutVars            = IntSet.insert idx rsMutVars}
    NS_MVar               -> dd {rsMVars              = IntSet.insert idx rsMVars}
    NS_SmallArray         -> dd {rsSmallArrays        = IntSet.insert idx rsSmallArrays}
    NS_SmallMutableArray  -> dd {rsSmallMutableArrays = IntSet.insert idx rsSmallMutableArrays}
    NS_StableName         -> dd {rsStableNames        = IntSet.insert idx rsStableNames}
    NS_WeakPointer        -> dd {rsWeakPointers       = IntSet.insert idx rsWeakPointers}
    NS_StablePointer      -> dd {rsStablePointers     = IntSet.insert idx rsStablePointers}
    _                     -> error $ "invalid dead value: " ++ show namespace ++ " " ++ show idx

readbackLiveData :: SouffleM RefSet
readbackLiveData = do
  liveSet <- liftIO $ Live.loadSet "Live.csv"
  foldM (\dd i -> collectDead dd $ Dead i) emptyRefSet $ map fromIntegral $ IntSet.toList liveSet
