{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, TupleSections #-}
module Stg.Interpreter.Debugger.Internal where

import Text.Printf
import qualified Text.Read as Text
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.ByteString.Char8 as BS8
import Data.Tree
import System.Console.Pretty

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi

import Stg.Interpreter.Base
import Stg.Syntax

import qualified Stg.Interpreter.GC as GC
import qualified Stg.Interpreter.GC.GCRef as GC
import Stg.Interpreter.Debugger.Region
import Stg.Interpreter.GC.RetainerAnalysis
import Stg.Interpreter.Debugger.Datalog

showOriginTrace :: Int -> M ()
showOriginTrace i = do
  origin <- gets ssOrigin
  let go o s = unless (IntSet.member o s) $ do
        let dlRef = fromIntegral $ GC.encodeRef o GC.NS_HeapPtr
        str <- decodeAndShow dlRef
        case IntMap.lookup o origin of
          Just (oId, oAddr) -> do
                            liftIO $ putStrLn $ str ++ "  " ++ show oId
                            go oAddr (IntSet.insert o s)
          _ -> liftIO $ putStrLn str
  go i IntSet.empty

reportState :: M ()
reportState = do
  (_, dbgOut) <- getDebuggerChan <$> gets ssDebuggerChan
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  currentClosure <- gets ssCurrentClosure >>= \case
    Nothing -> pure ""
    Just (Id c) -> pure $ binderUniqueName c
  currentClosureAddr <- gets ssCurrentClosureAddr
  liftIO $ Unagi.writeChan dbgOut $ DbgOutThreadReport tid ts currentClosure currentClosureAddr

showRetainer :: Int -> M ()
showRetainer i = do
  heap <- gets ssHeap
  rMap <- gets ssRetainerMap
  rootSet <- gets ssGCRootSet

  let dlRef = fromIntegral $ GC.encodeRef i GC.NS_HeapPtr
  liftIO $ do
    putStrLn $ "retianers of addr: " ++ show i ++ "   dl-ref: " ++ show dlRef ++ if IntSet.member dlRef rootSet then "  * GC-Root *" else ""
  case IntMap.lookup dlRef rMap of
    Nothing   -> liftIO $ putStrLn $ "no retainer for: " ++ show i ++ "   dl-ref: " ++ show dlRef
    Just rSet -> do
      forM_ (IntSet.toList rSet) $ \o -> case GC.decodeRef $ fromIntegral o of
        (GC.NS_HeapPtr, r)
          | Just ho <- IntMap.lookup r heap -> liftIO $ putStrLn $ dumpHeapObject r ho
        x -> liftIO $ print x

getRetainers :: Int -> M [Int]
getRetainers dlRef = do
  rootSet <- gets ssGCRootSet
  rMap <- gets ssRetainerMap
  case IntMap.lookup dlRef rMap of
    Just rSet
      | IntSet.notMember dlRef rootSet
      -> pure $ IntSet.toList rSet
    _ -> pure []

decodeAndShow :: Int -> M String
decodeAndShow dlRef = do
  heap <- gets ssHeap
  origin <- gets ssOrigin
  rootSet <- gets ssGCRootSet
  let showOrigin = \case
        Nothing -> ""
        Just (oId,oAddr) -> (color White $ style Bold "  ORIGIN: ") ++ (color Green $ show oId) ++ " " ++ show oAddr
      showHeapObj = \case
        Nothing -> ""
        Just ho -> " " ++ GC.debugPrintHeapObject ho
      str = case GC.decodeRef $ fromIntegral dlRef of
              x@(GC.NS_HeapPtr, r)  -> markGCRoot (show x ++ showHeapObj (IntMap.lookup r heap)) ++ showOrigin (IntMap.lookup r origin)
              x                     -> markGCRoot (show x)
      markGCRoot s = if IntSet.member dlRef rootSet
                        then color Yellow $ s ++ "  * GC-Root *"
                        else s
  pure str

getRetainerTree :: Int -> M (Tree String)
getRetainerTree i = do
  let dlRef = fromIntegral $ GC.encodeRef i GC.NS_HeapPtr
      go x = (x,) <$> getRetainers x
  tree <- unfoldTreeM go dlRef
  mapM decodeAndShow tree

showRetainerTree :: Int -> M ()
showRetainerTree i = do
  tree <- getRetainerTree i
  liftIO $ putStrLn $ drawTree tree

dbgCommands :: [([String], String, [String] -> M ())]
dbgCommands =
  [ ( ["gc"]
    , "run sync. garbage collector"
    , \_ -> do
        curClosureAddr <- gets ssCurrentClosureAddr
        GC.runGCSync [HeapPtr curClosureAddr]
    )
  , ( ["cleardb"]
    , "clear retainer db"
    , \_ -> clearRetanerDb
    )

  , ( ["loaddb"]
    , "load retainer db"
    , \_ -> loadRetanerDb2
    )

  , ( ["?"]
    , "show debuggers' all internal commands"
    , \_ -> printHelp
    )

  , ( ["report"]
    , "report some internal data"
    , \_ -> do
        heapStart <- gets ssHeapStartAddress
        liftIO $ do
          putStrLn $ "heap start address: " ++ show heapStart
    )

  , ( ["query", "??"]
    , "queries a given list of NAME_PATTERNs in static global env as substring"
    , \patterns -> do
        env <- gets ssStaticGlobalEnv
        let filterPattern pat resultList = [n | n <- resultList, List.isInfixOf pat n]
            matches = foldr filterPattern (map show $ Map.keys env) patterns
        liftIO $ putStrLn $ unlines matches
    )

  , ( ["?b"]
    , "list breakpoints"
    , \_ -> do
        bks <- Map.toList <$> gets ssBreakpoints
        liftIO $ putStrLn $ unlines [printf "%-40s  %d [fuel]" (BS8.unpack name) fuel | (name, fuel) <- bks]
    )

  , ( ["?r"]
    , "[START] [END] list a given region or all regions if the arguments are omitted"
    , \case
      [] -> do
        regions <- Map.keys <$> gets ssRegions
        liftIO $ putStrLn $ unlines $ map show regions
      [start]       -> showRegion False start start
      [start, end]  -> showRegion False start end
      _ -> pure ()
    )

  , ( ["?r-dump"]
    , "[START] [END] dump all heap object from the given region"
    , \case
      [start]       -> showRegion True start start
      [start, end]  -> showRegion True start end
      _ -> pure ()
    )

  , ( ["+r"]
    , "add region: +r START_CLOSURE_NAME [END_CLOSURE_NAME] ; if only the start is provided then it will be the end marker also"
    , \case
        [start]       -> addRegion start start
        [start, end]  -> addRegion start end
        _             -> pure ()
    )

  , ( ["-r"]
    , "del region: -r START_CLOSURE_NAME [END_CLOSURE_NAME] ; if only the start is provided then it will be the end marker also"
    , \case
        [start]       -> delRegion start start
        [start, end]  -> delRegion start end
        _             -> pure ()
    )

  , ( ["peek-range", "pr"]
    , "ADDR_START ADDR_END [COUNT] - list all heap objects in the given heap address region, optionally show only the first (COUNT) elements"
    , \case
      [start, end]
        | Just s <- Text.readMaybe start
        , Just e <- Text.readMaybe end
        -> do
            rHeap <- getRegionHeap s e
            dumpHeapM rHeap
      [start, end, count]
        | Just s <- Text.readMaybe start
        , Just e <- Text.readMaybe end
        , Just c <- Text.readMaybe count
        -> do
            rHeap <- getRegionHeap s e
            dumpHeapM $ IntMap.fromList $ take c $ IntMap.toList rHeap
      _ -> pure ()
    )

  , ( ["count-range", "cr"]
    , "ADDR_START ADDR_END - count heap objects in the given heap address region"
    , \case
      [start, end]
        | Just s <- Text.readMaybe start
        , Just e <- Text.readMaybe end
        -> do
            rHeap <- getRegionHeap s e
            liftIO $ putStrLn $ "object count: " ++ show (IntMap.size rHeap)
      _ -> pure ()
    )

  , ( ["retainer", "ret"]
    , "ADDR - show the retainer objects (heap objects that refer to the queried object"
    , \case
        [addrS]
          | Just addr <- Text.readMaybe addrS
          -> showRetainer addr
        _ -> pure ()
    )

  , ( ["ret-tree", "rt"]
    , "ADDR - show the retainer tree of an object"
    , \case
        [addrS]
          | Just addr <- Text.readMaybe addrS
          -> showRetainerTree addr
        _ -> pure ()
    )

  , ( ["trace-origin", "to"]
    , "ADDR - traces back heap object origin until the first dead object"
    , \case
        [addrS]
          | Just addr <- Text.readMaybe addrS
          -> showOriginTrace addr
        _ -> pure ()
    )

  , ( ["?e"]
    , "list all trace events and heap address state"
    , \_-> do
        events <- gets ssTraceEvents
        forM_ (reverse events) $ \(msg, AddressState{..}) -> liftIO $ printf "%-10d  %s\n" asNextHeapAddr (show msg)
    )

  , ( ["?e-dump"]
    , "list all trace events and the whole address state"
    , \_-> do
        events <- gets ssTraceEvents
        forM_ (reverse events) $ \(msg, a) -> liftIO $ do
          print msg
          print a
    )

  , ( ["?m"]
    , "list all trace markers and heap address state"
    , \_-> do
        markers <- gets ssTraceMarkers
        forM_ (reverse markers) $ \(msg, AddressState{..}) -> liftIO $ printf "%-10d  %s\n" asNextHeapAddr (show msg)
    )

  , ( ["?m-dump"]
    , "list all trace markers and the whole address state"
    , \_-> do
        markers <- gets ssTraceMarkers
        forM_ (reverse markers) $ \(msg, a) -> liftIO $ do
          print msg
          print a
    )

  , ( ["save-state"]
    , "DIR_NAME - save stg state as datalog facts to the given directory"
    , \case
        [dirName] -> do
          s <- get
          liftIO $ do
            exportStgState dirName s
            putStrLn "done."
        _ -> pure ()
    )
  ]


flatCommands :: [(String, String, [String] -> M ())]
flatCommands = [(cmd, desc, action) | (tokens, desc, action) <- dbgCommands, cmd <- tokens]

-- HINT: design to support help and descriptions

printHelp :: M ()
printHelp = do
  let maxLen = maximum $ map length [c | (c, _, _) <- flatCommands]
  liftIO $ putStrLn "internal debugger commands:"
  forM_ flatCommands $ \(cmd, desc, _) -> do
    liftIO $ printf ("  %-" ++ show maxLen ++ "s - %s\n") cmd desc
  liftIO $ putStrLn ""

runInternalCommand :: String -> M ()
runInternalCommand cmd = do
  case words cmd of
    c : args
      | [action] <- [a | (n, _, a) <- flatCommands, n == c]
      -> action args

    _ -> liftIO $ putStrLn "unknown command"
