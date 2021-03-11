{-# LANGUAGE LambdaCase, RecordWildCards #-}
import System.IO
import System.Exit
import System.Environment (getArgs)
import Control.Concurrent
import System.Posix.Process
import Control.Monad
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as List
import Text.Printf
import Text.Read
import Data.Maybe

import Options.Applicative
import Data.Semigroup ((<>))

import Stg.Interpreter.Base
import Stg.Interpreter
import Stg.Syntax

data StgIOpts
  = StgIOpts
  { switchCWD   :: Bool
  , runDebugger :: Bool
  , doTracing   :: Bool
  , appArgs1    :: String
  , appArgs2    :: [String]
  , appPath     :: FilePath
  , appArgs3    :: [String]
  }

stgi :: Parser StgIOpts
stgi = StgIOpts
  <$> switch (long "cwd" <> help "Changes the working directory to where the APPFILE is located")
  <*> switch (short 'd' <> long "debug" <> help "Enable simple debugger")
  <*> switch (short 't' <> long "trace" <> help "Enable tracing")
  <*> strOption (long "args" <> value "" <> help "Space separated APPARGS")
  <*> many (strOption (short 'a' <> help "Single APPARG"))
  <*> argument str (metavar "APPFILE" <> help "The .ghc_stgapp or .fullpak file to run")
  <*> many (argument str (metavar "APPARG..."))

main :: IO ()
main = do
  let opts = info (stgi <**> helper) mempty
  StgIOpts{..}  <- execParser opts

  let appArgs = words appArgs1 ++ appArgs2 ++ appArgs3

  (dbgCmdI, dbgCmdO) <- Unagi.newChan 100
  (dbgOutI, dbgOutO) <- Unagi.newChan 100
  let dbgChan = DebuggerChan (dbgCmdO, dbgOutI)

  case runDebugger of
    True  -> debugProgram switchCWD appPath appArgs dbgChan dbgCmdI dbgOutO
    False -> loadAndRunProgram switchCWD appPath appArgs dbgChan DbgRunProgram doTracing

debugProgram :: Bool -> [Char] -> [String] -> DebuggerChan -> Unagi.InChan DebugCommand -> Unagi.OutChan DebugOutput -> IO ()
debugProgram switchCWD appPath appArgs dbgChan dbgCmdI dbgOutO = do

  putStrLn "simple debugger"
  printHelp
  Unagi.writeChan dbgCmdI (CmdInternal "?") -- HINT: print internal debug commands at start

  forkIO $ do
    printDebugOutput dbgOutO

  forkIO $ do
    debugger dbgCmdI

  putStrLn $ "loading " ++ appPath
  loadAndRunProgram switchCWD appPath appArgs dbgChan DbgStepByStep True
  putStrLn "program finshed"

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

printDebugOutput :: Unagi.OutChan DebugOutput -> IO ()
printDebugOutput dbgOutO = do
  Unagi.readChan dbgOutO >>= \case
    DbgOutClosureList closureNames -> do
      mapM_ BS8.putStrLn closureNames

    DbgOutCurrentClosure name addr env -> do
      BS8.putStrLn name
      putStrLn $ "addr: " ++ show addr
      printEnv env

    DbgOutThreadReport tid ts currentClosureName currentClosureAddr -> do
      reportThreadIO tid ts
      putStrLn $ " * breakpoint, thread id: " ++ show tid ++ ", current closure: " ++ show currentClosureName ++ ", addr: " ++ show currentClosureAddr

    DbgOutHeapObject addr heapObj -> do
      putStrLn $ "addr: " ++ show addr
      printHeapObject heapObj

  printDebugOutput dbgOutO

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
  putStrLn " peek HEAP_ADDR           - print heap object from the given address"
  putStrLn " help                     - print reified debug commands"
  putStrLn " ?                        - print internal debug commands"



debugger :: Unagi.InChan DebugCommand -> IO ()
debugger dbgCmdI = do
  line <- getLine
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
    [] -> pure ()

    _ -> Unagi.writeChan dbgCmdI $ CmdInternal line
  debugger dbgCmdI
