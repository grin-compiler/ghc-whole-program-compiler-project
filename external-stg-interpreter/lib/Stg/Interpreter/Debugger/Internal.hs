{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Debugger.Internal where

import Text.Printf
import Control.Monad.State
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi

import Stg.Interpreter.Base
import Stg.Syntax

import qualified Stg.Interpreter.GC as GC

reportState :: M ()
reportState = do
  (_, dbgOut) <- getDebuggerChan <$> gets ssDebuggerChan
  tid <- gets ssCurrentThreadId
  ts <- getThreadState tid
  Id currentClosure <- gets ssCurrentClosure
  currentClosureAddr <- gets ssCurrentClosureAddr
  liftIO $ Unagi.writeChan dbgOut $ DbgOutThreadReport tid ts (binderUniqueName currentClosure) currentClosureAddr

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
