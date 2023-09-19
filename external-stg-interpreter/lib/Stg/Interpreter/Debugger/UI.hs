{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Interpreter.Debugger.UI where

import System.Exit
import System.Posix.Process
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import qualified Data.List as List
import qualified Data.Map as Map
import Text.Printf
import Text.Read
import Data.Maybe

import Stg.Interpreter.Base
import Stg.Interpreter
import Stg.Syntax
import Stg.IRLocation

ppSrcSpan :: SrcSpan -> String
ppSrcSpan = \case
  UnhelpfulSpan s             -> show s
  RealSrcSpan RealSrcSpan'{..} _
    | srcSpanSLine == srcSpanELine
    -> printf "%s:%d:%d-%d" (BS8.unpack srcSpanFile) srcSpanSLine srcSpanSCol srcSpanECol
    | otherwise
    -> printf "%s:%d:%d-%d:%d" (BS8.unpack srcSpanFile) srcSpanSLine srcSpanSCol srcSpanELine srcSpanECol

debugProgram :: Bool -> [Char] -> [String] -> DebuggerChan -> Maybe String -> DebugSettings -> IO ()
debugProgram switchCWD appPath appArgs dbgChan dbgScript debugSettings = do
  case dbgScript of
    Just fname -> do
      dbgScriptLines <- lines <$> readFile fname
      forkIO $ do
        runDebugScript dbgChan dbgScriptLines
        -- HINT: start REPL when the script is finished
        startDebuggerReplUI dbgChan
      pure ()

    Nothing -> do
      -- start debug REPL UI
      startDebuggerReplUI dbgChan


  putStrLn $ "loading " ++ appPath
  loadAndRunProgram False switchCWD appPath appArgs dbgChan DbgStepByStep True debugSettings
  putStrLn "program finshed"

startDebuggerReplUI :: DebuggerChan -> IO ()
startDebuggerReplUI dbgChan@DebuggerChan{..} = do
  putStrLn "simple debugger"
  printHelp
  putMVar dbgSyncRequest (CmdInternal "?") -- HINT: print internal debug commands at start

  forkIO $ do
    printDebugOutputLoop dbgChan

  forkIO $ do
    debugger dbgChan

  pure ()

printEnv :: Env -> IO ()
printEnv env = do
  let unBinderId (BinderId u) = u
      l = maximum . map (\(Id Binder{..}) -> sum [BS8.length $ getModuleName binderModule, 2, BS8.length binderName, 1, length $ show $ unBinderId binderId]) $ Map.keys env

      showItem (n@(Id Binder{..}), v) = printf ("  %-" ++ show l ++ "s  =  %s") (mod ++ "  " ++ name) (show v)
        where
          BinderId u  = binderId
          name        = BS8.unpack binderName ++ ('_' : show u)
          mod         = BS8.unpack $ getModuleName binderModule
      str = List.sort $ map showItem $ Map.toList env
  putStrLn $ unlines str

printDebugOutputLoop :: DebuggerChan -> IO ()
printDebugOutputLoop dbgChan@DebuggerChan{..} = do
  takeMVar dbgSyncResponse >>= printDebugOutput
  printDebugOutputLoop dbgChan

printDebugOutput :: DebugOutput -> IO ()
printDebugOutput = \case
  DbgOutResult result -> do
    putStrLn $ "result: " ++ show result

  DbgOutClosureList closureNames -> do
    mapM_ BS8.putStrLn closureNames

  DbgOutCurrentClosure nameM addr env -> do
    print nameM
    putStrLn $ "addr:   " ++ show addr
    printEnv env
    case nameM of
      Nothing   -> pure ()
      Just name -> putStrLn $ "source location: " ++ (ppSrcSpan . binderDefLoc . unId $ name)

  DbgOutThreadReport tid ts currentClosureName currentClosureAddr ntid -> do
    reportThreadIO tid ts
    putStrLn $ " * printDebugOutput myThreadId = " ++ show ntid
    putStrLn $ " * breakpoint, thread id: " ++ show tid ++ ", current closure: " ++ show currentClosureName ++ ", addr: " ++ show currentClosureAddr

  DbgOutHeapObject addr heapObj -> do
    putStrLn $ "addr: " ++ show addr
    printHeapObject heapObj

  DbgOutString msg -> putStrLn msg

  DbgOutByteString msg -> BS8.putStrLn msg

  DbgOutStgState stgState -> do
    putStrLn $ "stg state: TODO"
    pure ()

  DbgOut -> pure ()

printHeapObject :: HeapObject -> IO ()
printHeapObject = \case
  Con{..} -> do
    let DataCon{..} = unDC hoCon
    putStrLn $ BS8.unpack (getModuleName dcModule) ++ "." ++ BS8.unpack dcName ++ " " ++ show hoConArgs

  Closure{..} -> do
    putStrLn $ "closure: " ++ show hoName
    putStrLn $ "args:    " ++ show hoCloArgs
    putStrLn $ "missing: " ++ show hoCloMissing
    putStrLn "closure local env:"
    printEnv hoEnv
    putStrLn $ "source location: " ++ (ppSrcSpan . binderDefLoc . unId $ hoName)

  BlackHole{..} -> do
    putStrLn "BlackHole:"
    printHeapObject hoBHOriginalThunk
    putStrLn ""

  ApStack{} -> do
    putStrLn "ApStack"

  RaiseException ex -> do
    putStrLn $ "RaiseException: " ++ show ex

printHelp :: IO ()
printHelp = do
  putStrLn "commands:"
  putStrLn " quit                     - exit debugger and program"
  putStrLn " list                     - list visited closures"
  putStrLn " clear                    - clear visited closure list"
  putStrLn " +b QUALIFIED_CLOSURE_NAME  [FUEL]  - add breakpoint with optional FUEL counter that will be decreased until 0"
  putStrLn "                                      the breakpoint will trigger at 0, the default FUEL value is 0"
  putStrLn " -b QUALIFIED_CLOSURE_NAME          - remove breakpoint"
  putStrLn " step 's'                 - step into the next closure"
  putStrLn " continue 'c'             - continue until the next breakpoint"
  putStrLn " k                        - report current closure name"
  putStrLn " e                        - report current closure + step"
  putStrLn " stop                     - stop program execution, ('continue' or 'step' will resume execution)"
  putStrLn " peek HEAP_ADDR           - print heap object from the given address, short cmd alias: 'p'"
  putStrLn " help                     - print reified debug commands"
  putStrLn " ?                        - print internal debug commands"

debugger :: DebuggerChan -> IO ()
debugger dbgChan = do
  line <- getLine
  parseDebugCommand line dbgChan
  debugger dbgChan

parseDebugCommand :: String -> DebuggerChan -> IO ()
parseDebugCommand line dbgChan@DebuggerChan{..} = do
  case words line of
    ["help"]      -> printHelp >> putMVar dbgSyncRequest (CmdInternal "?")
    --["+b", name]        -> putMVar dbgSyncRequest $ CmdAddBreakpoint (BkpStgPoint . SP_RhsClosureExpr $ BS8.pack name) 0
    --["+b", name, fuel]  -> putMVar dbgSyncRequest $ CmdAddBreakpoint (BkpStgPoint . SP_RhsClosureExpr $ BS8.pack name) (fromMaybe 0 $ readMaybe fuel)
    --["-b", name]  -> putMVar dbgSyncRequest $ CmdRemoveBreakpoint $ BkpStgPoint . SP_RhsClosureExpr $ BS8.pack name
    ["list"]      -> putMVar dbgSyncRequest $ CmdListClosures
    ["clear"]     -> putMVar dbgSyncRequest $ CmdClearClosureList
    ["step"]      -> putMVar dbgSyncRequest $ CmdStep
    ["s"]         -> putMVar dbgSyncRequest $ CmdStep
    ["continue"]  -> putMVar dbgSyncRequest $ CmdContinue
    ["c"]         -> putMVar dbgSyncRequest $ CmdContinue
    ["k"]         -> putMVar dbgSyncRequest $ CmdCurrentClosure
    ["e"]         -> do
      putMVar dbgSyncRequest $ CmdCurrentClosure
      putMVar dbgSyncRequest $ CmdStep
    ["quit"]        -> exitImmediately ExitSuccess
    ["stop"]        -> putMVar dbgSyncRequest CmdStop
    ["peek", addr]  -> putMVar dbgSyncRequest $ CmdPeekHeap $ read addr
    ["p",    addr]  -> putMVar dbgSyncRequest $ CmdPeekHeap $ read addr
    [] -> pure ()

    _ -> putMVar dbgSyncRequest $ CmdInternal line

runDebugScript :: DebuggerChan -> [String] -> IO ()
runDebugScript dbgChan@DebuggerChan{..} lines = do
  let waitBreakpoint = do
        msg <- Unagi.readChan dbgAsyncEventOut
        print msg
        case msg of
          DbgEventHitBreakpoint{} -> putMVar dbgSyncRequest $ CmdInternal "get-current-thread-state"
          _                       -> waitBreakpoint

  forM_ lines $ \cmd -> do
    putStrLn cmd
    case words cmd of
      ["wait-b"] -> waitBreakpoint
      _ -> parseDebugCommand cmd dbgChan
