{-# LANGUAGE LambdaCase, RecordWildCards #-}
import System.IO
import System.Exit
import System.Environment (getArgs)
import Control.Concurrent
import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8

import Options.Applicative
import Data.Semigroup ((<>))

import Stg.Interpreter.Base
import Stg.Interpreter

data StgIOpts
  = StgIOpts
  { switchCWD   :: Bool
  , runDebugger :: Bool
  , appArgs1    :: String
  , appArgs2    :: [String]
  , appPath     :: FilePath
  , appArgs3    :: [String]
  }

stgi :: Parser StgIOpts
stgi = StgIOpts
  <$> switch (long "cwd" <> help "Changes the working directory to where the APPFILE is located")
  <*> switch (short 'd' <> long "debug" <> help "Enable simple debugger")
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
    False -> runProgram   switchCWD appPath appArgs dbgChan

debugProgram switchCWD appPath appArgs dbgChan dbgCmdI dbgOutO = do

  putStrLn "simple debugger"
  printHelp

  forkIO $ do
    printDebugOutput dbgOutO

  forkIO $ do
    runProgram switchCWD appPath appArgs dbgChan
    putStrLn "program finshed"
    exitSuccess

  debugger dbgCmdI

printHelp = do
  putStrLn "commands:"
  putStrLn " quit                     - exit debugger"
  putStrLn " list                     - list closures"
  putStrLn " +QUALIFIED_CLOSURE_NAME  - add breakpoint"
  putStrLn " -QUALIFIED_CLOSURE_NAME  - remove breakpoint"
  putStrLn " step                     - step into the next closure"
  putStrLn " continue                 - continue until the next breakpoint"

printDebugOutput dbgOutO = do
  Unagi.readChan dbgOutO >>= \case
    DbgOutClosureList closureNames  -> mapM_ BS8.putStrLn closureNames
    DbgOutCurrentClosure name       -> BS8.putStrLn name
  printDebugOutput dbgOutO

debugger dbgCmdI = do
  line <- getLine
  case line of
    '+':name    -> Unagi.writeChan dbgCmdI $ CmdAddBreakpoint $ BS8.pack name
    '-':name    -> Unagi.writeChan dbgCmdI $ CmdRemoveBreakpoint $ BS8.pack name
    "list"      -> Unagi.writeChan dbgCmdI $ CmdListClosures
    "clear"     -> Unagi.writeChan dbgCmdI $ CmdClearClosureList
    "step"      -> Unagi.writeChan dbgCmdI $ CmdStep
    "s"         -> Unagi.writeChan dbgCmdI $ CmdStep
    "continue"  -> Unagi.writeChan dbgCmdI $ CmdContinue
    "c"         -> Unagi.writeChan dbgCmdI $ CmdContinue
    "k"         -> Unagi.writeChan dbgCmdI $ CmdCurrentClosure
    "quit"      -> exitSuccess
    "" -> pure ()
    _ -> putStrLn ("unknown command: " ++ line) >> printHelp
  debugger dbgCmdI
