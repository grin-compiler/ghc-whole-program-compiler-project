{-# LANGUAGE LambdaCase, RecordWildCards #-}

import qualified Control.Concurrent.Chan.Unagi.Bounded as Unagi

import Options.Applicative
import Data.Semigroup ((<>))

--import Stg.Interpreter.Debugger.UI
import Stg.Interpreter.Base
import Stg.Interpreter

data StgIOpts
  = StgIOpts
  { switchCWD   :: Bool
  , runDebugger :: Bool
  , doTracing   :: Bool
  , dbgScript   :: Maybe String
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
  <*> (optional $ strOption (long "debug-script" <> metavar "FILENAME" <> help "Run debug commands from file"))
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
    --True  -> debugProgram switchCWD appPath appArgs dbgChan dbgCmdI dbgOutO dbgScript
    False -> loadAndRunProgram switchCWD appPath appArgs dbgChan DbgRunProgram doTracing
