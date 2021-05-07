{-# LANGUAGE LambdaCase, RecordWildCards #-}

import Control.Monad.IO.Class
import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))

import Data.List (isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map as Map

import System.Directory
import System.FilePath

import qualified Data.ByteString.Char8 as BS8

import Codec.Archive.Zip

import qualified Stg.Program as Stg

data MkFactsOpts
  = MkFactsOpts
  { lampakPath  :: FilePath
  }

appOpts :: Parser MkFactsOpts
appOpts = MkFactsOpts
  <$> argument str (metavar "LAMPAKFILE" <> help "The .lampak file to collect IR datalog facts from")

main :: IO ()
main = do
  let opts = info (appOpts <**> helper) mempty
  MkFactsOpts{..} <- execParser opts

  let irFactsPath0 = takeBaseName lampakPath -<.> ".ir-datalog-facts"
  irFactsPath <- makeAbsolute irFactsPath0

  putStrLn $ "linking lambda IR datalog facts into: " ++ irFactsPath

  -- HINT: cleanup old content
  removePathForcibly irFactsPath
  createDirectoryIfMissing True irFactsPath

  withArchive lampakPath $ do
    -- get list of modules
    appInfoEntry <- mkEntrySelector "app.info"
    content <- lines . BS8.unpack <$> getEntry appInfoEntry
    let mods      = Stg.parseSection content "modules:"
        dlDirSet  = Set.fromList [m </> "datalog" | m <- mods]

    entries <- Map.keys <$> getEntries
    forM_ entries $ \e -> do
      let entryPath = unEntrySelector e
          entryDir  = takeDirectory entryPath
      when (Set.member entryDir dlDirSet) $ do
        factData <- getEntry e
        let entryFileName = takeFileName entryPath
        liftIO $ BS8.appendFile (irFactsPath </> entryFileName) factData

  -- InitialReachable.facts
  writeFile (irFactsPath </> "InitialReachable.facts") $ "ghc_rts_abstract_model\n"
