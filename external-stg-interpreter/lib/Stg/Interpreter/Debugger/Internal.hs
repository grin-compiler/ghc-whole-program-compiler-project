{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger.Internal where

import Text.Printf
import Text.Read
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.ByteString.Char8 as BS8

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi

import Stg.Interpreter.Base
import Stg.Syntax

import qualified Stg.Interpreter.GC as GC
import qualified Stg.Interpreter.GC.GCRef as GC
import Stg.Interpreter.Debugger.Region

reportState :: M ()
reportState = do
  (_, dbgOut) <- getDebuggerChan <$> gets ssDebuggerChan
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  Id currentClosure <- gets ssCurrentClosure
  currentClosureAddr <- gets ssCurrentClosureAddr
  liftIO $ Unagi.writeChan dbgOut $ DbgOutThreadReport tid ts (binderUniqueName currentClosure) currentClosureAddr

showRetainer :: Int -> M ()
showRetainer i = do
  heap <- gets ssHeap
  rMap <- gets ssRetainerMap
  let dlRef = fromIntegral $ GC.encodeRef i GC.NS_HeapPtr
  case IntMap.lookup dlRef rMap of
    Nothing   -> liftIO $ putStrLn $ "no retainer for: " ++ show i ++ ", dl-ref: " ++ show dlRef
    Just rSet -> do
      forM_ (IntSet.toList rSet) $ \o -> case GC.decodeRef $ fromIntegral o of
        (GC.NS_HeapPtr, r)
          | Just ho <- IntMap.lookup r heap -> liftIO $ dumpHeapObject r ho
        x -> liftIO $ print x

dbgCommands :: [([String], String, [String] -> M ())]
dbgCommands =
  [ ( ["gc"]
    , "run sync. garbage collector"
    , \_ -> do
        curClosureAddr <- gets ssCurrentClosureAddr
        GC.runGCSync [HeapPtr curClosureAddr]
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

  , ( ["peek-range"]
    , "ADDR_START ADDR_END [COUNT] - list all heap objects in the given heap address region, optionally show only the first (COUNT) elements"
    , \case
      [start, end]
        | Just s <- readMaybe start
        , Just e <- readMaybe end
        -> do
            rHeap <- getRegionHeap s e
            liftIO $ dumpHeapIO rHeap
      [start, end, count]
        | Just s <- readMaybe start
        , Just e <- readMaybe end
        , Just c <- readMaybe count
        -> do
            rHeap <- getRegionHeap s e
            liftIO $ dumpHeapIO $ IntMap.fromList $ take c $ IntMap.toList rHeap
      _ -> pure ()
    )

  , ( ["retainer"]
    , "ADDR - show the retainer objects (heap objects that refer to the quieried object"
    , \case
        [addrS]
          | Just addr <- readMaybe addrS
          -> showRetainer addr
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
