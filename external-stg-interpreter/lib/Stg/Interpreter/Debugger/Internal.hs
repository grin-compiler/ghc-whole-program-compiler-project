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
import Control.Concurrent (myThreadId)
import Control.Concurrent.MVar

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
        let dlRef = GC.encodeRef o GC.NS_HeapPtr
        str <- decodeAndShow dlRef
        case IntMap.lookup o origin of
          Just (oId, oAddr, oTid) -> do
                            liftIO $ putStrLn $ str ++ "  " ++ show oId ++ " tid: " ++ show oTid
                            go oAddr (IntSet.insert o s)
          _ -> liftIO $ putStrLn str
  go i IntSet.empty

reportStgStateSync :: M ()
reportStgStateSync = do
  DebuggerChan{..} <- gets ssDebuggerChan
  stgState <- get
  liftIO . putMVar dbgSyncResponse $ DbgOutStgState stgState

reportStateSync :: M ()
reportStateSync = do
  DebuggerChan{..} <- gets ssDebuggerChan
  msg <- getThreadReport
  liftIO $ putMVar dbgSyncResponse msg

getThreadReport :: M DebugOutput
getThreadReport = do
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  currentClosure <- gets ssCurrentClosure >>= \case
    Nothing -> pure ""
    Just (Id c) -> pure $ binderUniqueName c
  currentClosureAddr <- gets ssCurrentClosureAddr
  ntid <- liftIO $ myThreadId
  pure $ DbgOutThreadReport tid ts currentClosure currentClosureAddr (show ntid)

showRetainer :: Int -> M ()
showRetainer i = do
  heap <- gets ssHeap
  rMap <- gets ssRetainerMap
  rootSet <- gets ssGCRootSet

  let dlRef = GC.encodeRef i GC.NS_HeapPtr
  liftIO $ do
    putStrLn $ "retianers of addr: " ++ show i ++ "   dl-ref: " ++ show dlRef ++ if Set.member dlRef rootSet then "  * GC-Root *" else ""
  case Map.lookup dlRef rMap of
    Nothing   -> liftIO $ putStrLn $ "no retainer for: " ++ show i ++ "   dl-ref: " ++ show dlRef
    Just rSet -> do
      forM_ (Set.toList rSet) $ \o -> case GC.decodeRef o of
        (GC.NS_HeapPtr, r)
          | Just ho <- IntMap.lookup r heap -> liftIO $ putStrLn $ dumpHeapObject r ho
        x -> liftIO $ print x

getRetainers :: GCSymbol -> M [GCSymbol]
getRetainers dlRef = do
  rootSet <- gets ssGCRootSet
  rMap <- gets ssRetainerMap
  case Map.lookup dlRef rMap of
    Just rSet
      | Set.notMember dlRef rootSet
      -> pure $ Set.toList rSet
    _ -> pure []

decodeAndShow :: GCSymbol -> M String
decodeAndShow dlRef = do
  heap <- gets ssHeap
  origin <- gets ssOrigin
  rootSet <- gets ssGCRootSet
  let showOrigin = \case
        Nothing -> ""
        Just (oId,oAddr,_) -> (color White $ style Bold "  ORIGIN: ") ++ (color Green $ show oId) ++ " " ++ show oAddr
      showHeapObj = \case
        Nothing -> ""
        Just ho -> " " ++ debugPrintHeapObject ho
      str = case GC.decodeRef dlRef of
              x@(GC.NS_HeapPtr, r)  -> markGCRoot (show x ++ showHeapObj (IntMap.lookup r heap)) ++ showOrigin (IntMap.lookup r origin)
              x                     -> markGCRoot (show x)
      markGCRoot s = if Set.member dlRef rootSet
                        then color Yellow $ s ++ "  * GC-Root *"
                        else s
  pure str

getRetainerTree :: Int -> M (Tree String)
getRetainerTree i = do
  let dlRef = GC.encodeRef i GC.NS_HeapPtr
      go x = (x,) <$> getRetainers x
  tree <- unfoldTreeM go dlRef
  mapM decodeAndShow tree

showRetainerTree :: Int -> M ()
showRetainerTree i = do
  tree <- getRetainerTree i
  liftIO $ putStrLn $ drawTree tree

wrapWithDbgOut :: ([String] -> M ()) -> [String] -> M ()
wrapWithDbgOut cmdM args = do
  cmdM args
  DebuggerChan{..} <- gets ssDebuggerChan
  liftIO $ putMVar dbgSyncResponse DbgOut

dbgCommands :: [([String], String, [String] -> M ())]
dbgCommands =
  [ ( ["gc"]
    , "run sync. garbage collector"
    , wrapWithDbgOut $ \_ -> do
        localEnv <- gets ssLocalEnv
        GC.runGCSync localEnv
    )
  , ( ["cleardb"]
    , "clear retainer db"
    , wrapWithDbgOut $ \_ -> clearRetanerDb
    )

  , ( ["loaddb"]
    , "load retainer db"
    , wrapWithDbgOut $ \_ -> loadRetainerDb2
    )

  , ( ["?"]
    , "show debuggers' all internal commands"
    , wrapWithDbgOut $ \_ -> printHelp
    )

  , ( ["report"]
    , "report some internal data"
    , wrapWithDbgOut $ \_ -> do
        heapStart <- gets ssDynamicHeapStart
        liftIO $ do
          putStrLn $ "heap start address: " ++ show heapStart
    )

  , ( ["query", "??"]
    , "queries a given list of NAME_PATTERNs in static global env as substring"
    , wrapWithDbgOut $ \patterns -> do
        env <- gets ssStaticGlobalEnv
        let filterPattern pat resultList = [n | n <- resultList, List.isInfixOf pat n]
            matches = foldr filterPattern (map show $ Map.keys env) patterns
        liftIO $ putStrLn $ unlines matches
    )

  , ( ["?b"]
    , "list breakpoints"
    , wrapWithDbgOut $ \_ -> do
        bks <- Map.toList <$> gets ssBreakpoints
        liftIO $ putStrLn $ unlines [printf "%-40s  %d [fuel]" (show name) fuel | (name, fuel) <- bks]
    )

  , ( ["?r"]
    , "[START] [END] list a given region or all regions if the arguments are omitted"
    , wrapWithDbgOut $ \case
      [] -> do
        regions <- Map.keys <$> gets ssRegionStack
        liftIO $ putStrLn $ unlines $ map show regions
      [start]       -> showRegion False start start
      [start, end]  -> showRegion False start end
      _ -> pure ()
    )

  , ( ["?r-dump"]
    , "[START] [END] dump all heap object from the given region"
    , wrapWithDbgOut $ \case
      [start]       -> showRegion True start start
      [start, end]  -> showRegion True start end
      _ -> pure ()
    )

  , ( ["+r"]
    , "add region: +r START_CLOSURE_NAME [END_CLOSURE_NAME] ; if only the start is provided then it will be the end marker also"
    , wrapWithDbgOut $ \case
        [start]       -> addRegion start start
        [start, end]  -> addRegion start end
        _             -> pure ()
    )

  , ( ["-r"]
    , "del region: -r START_CLOSURE_NAME [END_CLOSURE_NAME] ; if only the start is provided then it will be the end marker also"
    , wrapWithDbgOut $ \case
        [start]       -> delRegion start start
        [start, end]  -> delRegion start end
        _             -> pure ()
    )

  , ( ["peek-range", "pr"]
    , "ADDR_START ADDR_END [COUNT] - list all heap objects in the given heap address region, optionally show only the first (COUNT) elements"
    , wrapWithDbgOut $ \case
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
    , wrapWithDbgOut $ \case
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
    , wrapWithDbgOut $ \case
        [addrS]
          | Just addr <- Text.readMaybe addrS
          -> showRetainer addr
        _ -> pure ()
    )

  , ( ["ret-tree", "rt"]
    , "ADDR - show the retainer tree of an object"
    , wrapWithDbgOut $ \case
        [addrS]
          | Just addr <- Text.readMaybe addrS
          -> showRetainerTree addr
        _ -> pure ()
    )

  , ( ["trace-origin", "to"]
    , "ADDR - traces back heap object origin until the first dead object"
    , wrapWithDbgOut $ \case
        [addrS]
          | Just addr <- Text.readMaybe addrS
          -> showOriginTrace addr
        _ -> pure ()
    )

  , ( ["?e"]
    , "list all trace events and heap address state"
    , wrapWithDbgOut $ \_-> do
        events <- gets ssTraceEvents
        forM_ (reverse events) $ \(msg, AddressState{..}) -> liftIO $ printf "%-10d  %s\n" asNextHeapAddr (show msg)
    )

  , ( ["?e-dump"]
    , "list all trace events and the whole address state"
    , wrapWithDbgOut $ \_-> do
        events <- gets ssTraceEvents
        forM_ (reverse events) $ \(msg, a) -> liftIO $ do
          print msg
          print a
    )

  , ( ["?m"]
    , "list all trace markers and heap address state"
    , wrapWithDbgOut $ \_-> do
        markers <- gets ssTraceMarkers
        forM_ (reverse markers) $ \(msg, _tid, AddressState{..}) -> liftIO $ printf "%-10d  %s\n" asNextHeapAddr (show msg)
    )

  , ( ["?m-dump"]
    , "list all trace markers and the whole address state"
    , wrapWithDbgOut $ \_-> do
        markers <- gets ssTraceMarkers
        forM_ (reverse markers) $ \(msg, _tid, a) -> liftIO $ do
          print msg
          print a
    )

  , ( ["save-state"]
    , "DIR_NAME - save stg state as datalog facts to the given directory"
    , wrapWithDbgOut $ \case
        [dirName] -> do
          s <- get
          liftIO $ do
            exportStgState dirName s
            putStrLn "done."
        _ -> pure ()
    )

  , ( ["fuel"]
    , "STEP-COUNT - make multiple steps ; 'fuel -' - turn off step count check"
    , wrapWithDbgOut $ \case
      ["-"]
        -> modify' $ \s@StgState{..} -> s {ssDebugFuel = Nothing}
      [countS]
        | Just stepCount <- Text.readMaybe countS
        -> modify' $ \s@StgState{..} -> s {ssDebugFuel = Just stepCount}
      _ -> pure ()
    )

  , ( ["ret-tree", "rt"]
    , "ADDR - show the retainer tree of an object"
    , wrapWithDbgOut $ \case
        [addrS]
          | Just addr <- Text.readMaybe addrS
          -> showRetainerTree addr
        _ -> pure ()
    )

  , ( ["get-current-thread-state"]
    , "reports the currently running thread state"
    , \_ -> reportStateSync
    )

  , ( ["get-stg-state"]
    , "reports the stg state"
    , \_ -> reportStgStateSync
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
