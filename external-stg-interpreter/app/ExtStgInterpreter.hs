{-# LANGUAGE LambdaCase, RecordWildCards #-}

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi
import Control.Concurrent.MVar

import Options.Applicative
import Data.Semigroup ((<>))
import qualified ShellWords

import Stg.Interpreter.Debugger.UI
import Stg.Interpreter.Base
import Stg.Interpreter

data StgIOpts
  = StgIOpts
  { switchCWD     :: Bool
  , runDebugger   :: Bool
  , doTracing     :: Bool
  , isQuiet       :: Bool
  , ignoreRtsArgs :: Bool
  , dbgScript     :: Maybe FilePath
  , appArgsFile   :: Maybe FilePath
  , appArgs1      :: String
  , appArgs2      :: [String]
  , appPath       :: FilePath
  , appArgs3      :: [String]
  , keepGCFacts   :: Bool
  }

stgi :: Parser StgIOpts
stgi = StgIOpts
  <$> switch (long "cwd" <> help "Changes the working directory to where the APPFILE is located")
  <*> switch (short 'd' <> long "debug" <> help "Enable simple debugger")
  <*> switch (short 't' <> long "trace" <> help "Enable tracing")
  <*> switch (short 'q' <> long "quiet" <> help "disable debug messages")
  <*> switch (long "ignore-rts-args" <> help "ignore arguments between +RTS and -RTS")
  <*> (optional $ strOption (long "debug-script" <> metavar "FILENAME" <> help "Run debug commands from file"))
  <*> (optional $ strOption (long "args-file" <> metavar "FILENAME" <> help "Get app arguments from file"))
  <*> strOption (long "args" <> value "" <> help "Space separated APPARGS")
  <*> many (strOption (short 'a' <> help "Single APPARG"))
  <*> argument str (metavar "APPFILE" <> help "The .ghc_stgapp or .fullpak file to run")
  <*> many (argument str (metavar "APPARG..."))
  <*> switch (long "keep-gc-facts" <> help "Keep GC datalog facts in separate folder for each GC cycle")

main :: IO ()
main = do
  let opts = info (stgi <**> helper) mempty
  StgIOpts{..}  <- execParser opts

  argsFromFile <- case appArgsFile of
    Nothing -> pure []
    Just fname -> do
      str <- readFile fname
      case ShellWords.parse str of
        Left err  -> error err
        Right l   -> pure l
  let appArgs0  = argsFromFile ++ words appArgs1 ++ appArgs2 ++ appArgs3
      appArgs   = if ignoreRtsArgs then dropRtsOpts appArgs0 else appArgs0

      debugSettings = defaultDebugSettings
        { dsKeepGCFacts = keepGCFacts
        }

  (dbgAsyncI, dbgAsyncO) <- Unagi.newChan 100
  dbgRequestMVar <- newEmptyMVar
  dbgResponseMVar <- newEmptyMVar
  let dbgChan = DebuggerChan
        { dbgSyncRequest    = dbgRequestMVar
        , dbgSyncResponse   = dbgResponseMVar
        , dbgAsyncEventIn   = dbgAsyncI
        , dbgAsyncEventOut  = dbgAsyncO
        }

  case runDebugger of
    True  -> debugProgram switchCWD appPath appArgs dbgChan dbgScript debugSettings
    False -> loadAndRunProgram isQuiet switchCWD appPath appArgs dbgChan DbgRunProgram doTracing debugSettings

dropRtsOpts :: [String] -> [String]
dropRtsOpts [] = []
dropRtsOpts ("+RTS" : args) = dropRtsOpts $ dropWhile (/= "-RTS") args
dropRtsOpts ("-RTS" : args) = dropRtsOpts args
dropRtsOpts (a : args) = a : dropRtsOpts args
