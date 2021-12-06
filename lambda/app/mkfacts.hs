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
  , clusterPath :: Maybe FilePath
  }

appOpts :: Parser MkFactsOpts
appOpts = MkFactsOpts
  <$> argument str (metavar "LAMPAKFILE" <> help "The .lampak file to collect IR datalog facts from")
  <*> optional (strOption (short 'c' <> long "cluster" <> metavar "FILENAME" <> help "List of modules that defines the code cluster of the exported IR"))

main :: IO ()
main = do
  let opts = info (appOpts <**> helper) mempty
  MkFactsOpts{..} <- execParser opts

  let irFactsPath0    = lampakPath -<.> ".ir-datalog-facts"
      outerFactsPath0 = lampakPath -<.> ".outer-ir-datalog-facts"
  irFactsPath <- makeAbsolute irFactsPath0
  outerFactsPath <- makeAbsolute outerFactsPath0

  putStrLn "linking lambda IR datalog facts into:"
  putStrLn irFactsPath
  putStrLn outerFactsPath

  -- HINT: cleanup old content
  removePathForcibly irFactsPath
  createDirectoryIfMissing True irFactsPath

  removePathForcibly outerFactsPath
  createDirectoryIfMissing True outerFactsPath

  withArchive lampakPath $ do
    -- get list of modules
    appInfoEntry <- mkEntrySelector "app.info"
    content <- lines . BS8.unpack <$> getEntry appInfoEntry
    let mods      = Stg.parseSection content "modules:"
        dlDirSet  = Set.fromList [m </> "datalog" | m <- mods]

    clusterDirSet <- case clusterPath of
      Nothing -> pure dlDirSet -- HINT: cluster = whole program
      Just fn -> do
        content <- lines . BS8.unpack <$> liftIO (BS8.readFile fn)
        let mods      = Stg.parseSection content "cluster-modules:"
            dlDirSet  = Set.fromList [m </> "datalog" | m <- mods]
        pure dlDirSet

    entries <- Map.keys <$> getEntries
    forM_ entries $ \e -> do
      let entryPath = unEntrySelector e
          entryDir  = takeDirectory entryPath
      when (Set.member entryDir dlDirSet) $ do
        factData <- getEntry e
        let entryFileName = takeFileName entryPath
            factsPath = if Set.member entryDir clusterDirSet
              then irFactsPath
              else outerFactsPath
        liftIO $ BS8.appendFile (factsPath </> entryFileName) factData

  -- InitialReachable.facts
  -- HINT: the ghc_rts_abstract_model is in the Main modules
  writeFile (irFactsPath </> "InitialReachable.facts") $ "ghc_rts_abstract_model\n"

{-
  TODO:
    when cluster is specified then:
      export the clusre outer code into a separate folder
-}
