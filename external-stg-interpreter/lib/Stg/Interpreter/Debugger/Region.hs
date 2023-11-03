{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger.Region where

import Text.Printf
import Control.Monad.State
import Data.Maybe
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.ByteString.Char8 as BS8
import System.Console.Pretty

import Stg.Interpreter.Base
import Stg.Interpreter.Debug
import Stg.Syntax

import qualified Stg.Interpreter.GC as GC
import qualified Stg.Interpreter.GC.GCRef as GC

evalRegionCommand :: String -> M ()
evalRegionCommand cmd = do
  tid <- gets ssCurrentThreadId
  case words cmd of
    ["estgi.debug.region.start", name] -> startRegion tid . EventRegion $ BS8.pack name
    ["estgi.debug.region.end",   name] -> endRegion tid . EventRegion $ BS8.pack name
    _ -> pure ()

dumpHeapObject :: Int -> HeapObject -> String
dumpHeapObject i o = printf "%-8d %3s  %s" i (GC.ppLNE o) (debugPrintHeapObject o)

dumpOriginM :: Int -> M String
dumpOriginM i = do
  origin <- gets ssOrigin
  case IntMap.lookup i origin of
    Nothing -> pure ""
    Just (oId,oAddr,_) -> pure $ (color White $ style Bold "  ORIGIN: ") ++ (color Green $ show oId) ++ " " ++ show oAddr

dumpHeapM :: Heap -> M ()
dumpHeapM h = do
  liftIO $ putStrLn $ "object count: " ++ show (IntMap.size h)
  forM_ (IntMap.toList h) $ \(i, o) -> do
    rootSet <- gets ssGCRootSet
    let dlRef = GC.encodeRef i GC.NS_HeapPtr
        markGCRoot s = if Set.member dlRef rootSet
                          then color Yellow $ s ++ "  * GC-Root *"
                          else s
    originStr <- dumpOriginM i
    liftIO $ putStrLn $ markGCRoot (dumpHeapObject i o) ++ originStr

getRegionHeap :: Int -> Int -> M Heap
getRegionHeap start end = do
  heap <- gets ssHeap
  let ltEnd   = fst $ IntMap.split end heap
      geStart = snd $ IntMap.split (start-1) ltEnd
  pure geStart

showRegion :: Bool -> String -> String -> M ()
showRegion doHeapDump start end = do
  instances <- gets ssRegionInstances
  let r = IRRegion (BS8.pack start) (BS8.pack end)
      printDelimiter = when doHeapDump $ liftIO $ putStrLn "\n==============================================================================\n"
  case Map.lookup r instances of
    Nothing       -> pure ()
    Just l -> do
      liftIO $ putStrLn $ "region data count: " ++ show (IntMap.size l)
      liftIO $ putStrLn $ "order:  OLD -> NEW"
      forM_ (IntMap.elems l) $ \(s, e) -> do
        printDelimiter
        let sAddr = asNextHeapAddr s
            eAddr = asNextHeapAddr e
        rHeap <- getRegionHeap sAddr eAddr
        liftIO $ printf "heap start: %-10d  end: %-10d  object count: %d\n" sAddr eAddr (IntMap.size rHeap)
        when doHeapDump $ do
          liftIO $ putStrLn ""
          dumpHeapM rHeap
          liftIO $ putStrLn ""
      printDelimiter

addRegion :: String -> String -> M ()
addRegion start end = do
  regions <- gets ssRegionCounter
  let s = BS8.pack start
      e = BS8.pack end
      r = IRRegion s e
  unless (Map.member r regions) $ do
    addMarker s r
    addMarker e r

delRegion :: String -> String -> M ()
delRegion start end = do
  regions <- gets ssRegionCounter
  let s = BS8.pack start
      e = BS8.pack end
      r = IRRegion s e
  when (Map.member r regions) $ do
    delMarker s r
    delMarker e r

addMarker :: Name -> Region -> M ()
addMarker m r = do
  modify $ \s@StgState{..} -> s {ssMarkers = Map.insertWith mappend m (Set.singleton r) ssMarkers}

delMarker :: Name -> Region -> M ()
delMarker m r = do
  let del s = let s' = Set.delete r s in if Set.null s' then Nothing else Just s'
  modify $ \s@StgState{..} -> s {ssMarkers = Map.update del m ssMarkers}

checkRegion :: Name -> M ()
checkRegion markerName = do
  tid <- gets ssCurrentThreadId
  markers <- gets ssMarkers
  case Map.lookup markerName markers of
    Nothing -> pure ()
    Just rl -> do
      forM_ rl $ \r@(IRRegion s e) -> case r of
        _ | markerName == s && markerName == e -> endRegion tid r >> startRegion tid r
        _ | markerName == s -> startRegion tid r
        _ | markerName == e -> endRegion tid r

nextRegionIndex :: Region -> M Int
nextRegionIndex r = do
  idx <- fromMaybe 0 <$> gets (Map.lookup r . ssRegionCounter)
  modify' $ \s@StgState{..} -> s {ssRegionCounter = Map.insert r (succ idx) ssRegionCounter}
  pure idx

startRegion :: Int -> Region -> M ()
startRegion threadId r = do
  idx <- nextRegionIndex r
  startAddr <- getAddressState
  modify $ \s@StgState{..} -> s {ssRegionStack = Map.insertWith (++) (threadId, r) [(idx, startAddr, emptyCallGraph)] ssRegionStack}

endRegion :: Int -> Region -> M ()
endRegion threadId r = do
  -- pop region
  gets (Map.lookup (threadId, r) . ssRegionStack) >>= \case
    Just ((idx, startAddr, callGraph) : stackTail) -> do
      exportRegionCallGraph idx r callGraph
      endAddr <- getAddressState
      modify $ \s@StgState{..} -> s { ssRegionInstances = Map.insertWith IntMap.union r (IntMap.singleton idx (startAddr, endAddr)) ssRegionInstances }
      case stackTail of
        [] -> do
          -- HINT: keep ssRegionStack small, to make call graph update fast
          modify $ \s@StgState{..} -> s { ssRegionStack = Map.delete (threadId, r) ssRegionStack }
        (o, a, cg) : l  -> do
          let mergedStackTail = (o, a, joinCallGraph cg callGraph) : l -- HINT: merge callgraphs for nested regions
          modify $ \s@StgState{..} -> s { ssRegionStack = Map.insert (threadId, r) mergedStackTail ssRegionStack}
    _ -> pure ()
