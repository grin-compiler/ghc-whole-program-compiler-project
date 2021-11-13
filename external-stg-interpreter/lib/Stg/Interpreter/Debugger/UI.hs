{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Interpreter.Debugger.UI where

import System.Exit
import System.Posix.Process
import Control.Concurrent
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

ppSrcSpan :: SrcSpan -> String
ppSrcSpan = \case
  UnhelpfulSpan s             -> BS8.unpack s
  RealSrcSpan RealSrcSpan'{..} _
    | srcSpanSLine == srcSpanELine
    -> printf "%s:%d:%d-%d" (BS8.unpack srcSpanFile) srcSpanSLine srcSpanSCol srcSpanECol
    | otherwise
    -> printf "%s:%d:%d-%d:%d" (BS8.unpack srcSpanFile) srcSpanSLine srcSpanSCol srcSpanELine srcSpanECol

debugProgram :: Bool -> [Char] -> [String] -> DebuggerChan -> Unagi.InChan DebugCommand -> Unagi.OutChan DebugOutput -> Maybe String -> IO ()
debugProgram switchCWD appPath appArgs dbgChan dbgCmdI dbgOutO dbgScript = do
  case dbgScript of
    Just fname -> do
      dbgScriptLines <- lines <$> readFile fname
      forkIO $ do
        runDebugScript dbgCmdI dbgOutO dbgScriptLines
        -- HINT: start REPL when the script is finished
        startDebuggerReplUI dbgCmdI dbgOutO
      pure ()

    Nothing -> do
      -- start debug REPL UI
      startDebuggerReplUI dbgCmdI dbgOutO


  putStrLn $ "loading " ++ appPath
  loadAndRunProgram switchCWD appPath appArgs dbgChan DbgStepByStep True
  putStrLn "program finshed"

startDebuggerReplUI :: Unagi.InChan DebugCommand -> Unagi.OutChan DebugOutput -> IO ()
startDebuggerReplUI dbgCmdI dbgOutO = do
  putStrLn "simple debugger"
  printHelp
  Unagi.writeChan dbgCmdI (CmdInternal "?") -- HINT: print internal debug commands at start

  forkIO $ do
    printDebugOutputLoop dbgOutO

  forkIO $ do
    debugger dbgCmdI

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

printDebugOutputLoop :: Unagi.OutChan DebugOutput -> IO ()
printDebugOutputLoop dbgOutO = do
  Unagi.readChan dbgOutO >>= printDebugOutput
  printDebugOutputLoop dbgOutO

printDebugOutput :: DebugOutput -> IO ()
printDebugOutput = \case
  DbgOutClosureList closureNames -> do
    mapM_ BS8.putStrLn closureNames

  DbgOutCurrentClosure nameM addr env -> do
    print nameM
    putStrLn $ "addr:   " ++ show addr
    printEnv env
    case nameM of
      Nothing   -> pure ()
      Just name -> putStrLn $ "source location: " ++ (ppSrcSpan . binderDefLoc . unId $ name)

  DbgOutThreadReport tid ts currentClosureName currentClosureAddr -> do
    reportThreadIO tid ts
    putStrLn $ " * breakpoint, thread id: " ++ show tid ++ ", current closure: " ++ show currentClosureName ++ ", addr: " ++ show currentClosureAddr

  DbgOutHeapObject addr heapObj -> do
    putStrLn $ "addr: " ++ show addr
    printHeapObject heapObj

printHeapObject :: HeapObject -> IO ()
printHeapObject = \case
  Con{..} -> do
    let DataCon{..} = hoCon
    putStrLn $ BS8.unpack (getModuleName dcModule) ++ "." ++ BS8.unpack dcName ++ " " ++ show hoConArgs

  Closure{..} -> do
    putStrLn $ "closure: " ++ show hoName
    putStrLn $ "args:    " ++ show hoCloArgs
    putStrLn $ "missing: " ++ show hoCloMissing
    putStrLn "closure local env:"
    printEnv hoEnv
    putStrLn $ "source location: " ++ (ppSrcSpan . binderDefLoc . unId $ hoName)

  BlackHole ho -> do
    putStrLn "BlackHole:"
    printHeapObject ho
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

debugger :: Unagi.InChan DebugCommand -> IO ()
debugger dbgCmdI = do
  line <- getLine
  parseDebugCommand line dbgCmdI
  debugger dbgCmdI

parseDebugCommand :: String -> Unagi.InChan DebugCommand -> IO ()
parseDebugCommand line dbgCmdI = do
  case words line of
    ["help"]      -> printHelp >> Unagi.writeChan dbgCmdI (CmdInternal "?")
    ["+b", name]        -> Unagi.writeChan dbgCmdI $ CmdAddBreakpoint (BS8.pack name) 0
    ["+b", name, fuel]  -> Unagi.writeChan dbgCmdI $ CmdAddBreakpoint (BS8.pack name) (fromMaybe 0 $ readMaybe fuel)
    ["-b", name]  -> Unagi.writeChan dbgCmdI $ CmdRemoveBreakpoint $ BS8.pack name
    ["list"]      -> Unagi.writeChan dbgCmdI $ CmdListClosures
    ["clear"]     -> Unagi.writeChan dbgCmdI $ CmdClearClosureList
    ["step"]      -> Unagi.writeChan dbgCmdI $ CmdStep
    ["s"]         -> Unagi.writeChan dbgCmdI $ CmdStep
    ["continue"]  -> Unagi.writeChan dbgCmdI $ CmdContinue
    ["c"]         -> Unagi.writeChan dbgCmdI $ CmdContinue
    ["k"]         -> Unagi.writeChan dbgCmdI $ CmdCurrentClosure
    ["e"]         -> do
      Unagi.writeChan dbgCmdI $ CmdCurrentClosure
      Unagi.writeChan dbgCmdI $ CmdStep
    ["quit"]        -> exitImmediately ExitSuccess
    ["stop"]        -> Unagi.writeChan dbgCmdI CmdStop
    ["peek", addr]  -> Unagi.writeChan dbgCmdI $ CmdPeekHeap $ read addr
    ["p",    addr]  -> Unagi.writeChan dbgCmdI $ CmdPeekHeap $ read addr
    [] -> pure ()

    _ -> Unagi.writeChan dbgCmdI $ CmdInternal line

runDebugScript :: Unagi.InChan DebugCommand -> Unagi.OutChan DebugOutput -> [String] -> IO ()
runDebugScript dbgCmdI dbgOutO lines = do
  let waitBreakpoint = do
        msg <- Unagi.readChan dbgOutO
        case msg of
          DbgOutThreadReport{}  -> printDebugOutput msg
          _                     -> printDebugOutput msg >> waitBreakpoint

  forM_ lines $ \cmd -> do
    putStrLn cmd
    case words cmd of
      ["wait-b"] -> waitBreakpoint
      _ -> parseDebugCommand cmd dbgCmdI
