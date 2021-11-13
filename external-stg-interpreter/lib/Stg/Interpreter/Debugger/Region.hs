{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger.Region where

import Text.Printf
import Control.Monad.State
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

dumpHeapObject :: Int -> HeapObject -> String
dumpHeapObject i o = printf "%-8d %3s  %s" i (GC.ppLNE o) (GC.debugPrintHeapObject o)

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
    let dlRef = fromIntegral $ GC.encodeRef i GC.NS_HeapPtr
        markGCRoot s = if IntSet.member dlRef rootSet
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
  regions <- gets ssRegions
  let r = Region (BS8.pack start) (BS8.pack end)
      printDelimiter = when doHeapDump $ liftIO $ putStrLn "\n==============================================================================\n"
  case Map.lookup r regions of
    Nothing       -> pure ()
    Just (cur, _curCallGraph, l) -> do
      liftIO $ putStrLn $ "region data count: " ++ show (length l)
      liftIO $ putStrLn $ "order:  OLD -> NEW"
      forM_ (reverse l) $ \(s, e) -> do
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
  regions <- gets ssRegions
  let s = BS8.pack start
      e = BS8.pack end
      r = Region s e
  unless (Map.member r regions) $ do
    modify $ \s@StgState{..} -> s {ssRegions = Map.insert r (Nothing, emptyCallGraph, []) ssRegions}
    addMarker s r
    addMarker e r

delRegion :: String -> String -> M ()
delRegion start end = do
  regions <- gets ssRegions
  let s = BS8.pack start
      e = BS8.pack end
      r = Region s e
  when (Map.member r regions) $ do
    modify $ \s@StgState{..} -> s {ssRegions = Map.delete r ssRegions}
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
  markers <- gets ssMarkers
  case Map.lookup markerName markers of
    Nothing -> pure ()
    Just rl -> do
      forM_ rl $ \r@(Region s e) -> case r of
        _ | markerName == s && markerName == e -> startEndRegion r
        _ | markerName == s -> startRegion r
        _ | markerName == e -> endRegion r

startRegion :: Region -> M ()
startRegion r = do
  a <- getAddressState
  let start (Nothing, _, l) = (Just a, emptyCallGraph, l)
      start x = x -- HINT: multiple start is allowed to support more flexible debugging
  modify $ \s@StgState{..} -> s {ssRegions = Map.adjust start r ssRegions}

endRegion :: Region -> M ()
endRegion r = do
  exportRegionCallGraph r
  a <- getAddressState
  let end (Just s, _, l) = (Nothing, emptyCallGraph, (s, a) : l)
      end x = x -- HINT: if the region was not started then there is nothing to do
  modify $ \s@StgState{..} -> s {ssRegions = Map.adjust end r ssRegions}

startEndRegion :: Region -> M ()
startEndRegion r = do
  exportRegionCallGraph r
  a <- getAddressState
  let fun (Nothing, _, l) = (Just a, emptyCallGraph, l)
      fun (Just s, _, l)  = (Just a, emptyCallGraph, (s, a) : l)
  modify $ \s@StgState{..} -> s {ssRegions = Map.adjust fun r ssRegions}
