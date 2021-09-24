{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

import Control.Monad.IO.Class
import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))

import Data.List (isSuffixOf, foldl')
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.FilePath

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Binary (encode)

import Codec.Archive.Zip
import Codec.Archive.Zip.Unix

import qualified Stg.Syntax   as Stg
import qualified Stg.Program  as Stg
import qualified Stg.IO       as Stg
import qualified Stg.Reconstruct as Stg
import qualified Stg.Deconstruct as Stg
import Lambda.Stg.StripDeadCode

data StrippedFullpakOpts
  = StrippedFullpakOpts
  { fullpakPath :: FilePath
  }

appOpts :: Parser StrippedFullpakOpts
appOpts = StrippedFullpakOpts
  <$> argument str (metavar "FULLPAK" <> help "The .fullpak file name to strip")

readNameSet :: FilePath -> IO (Set Stg.Name)
readNameSet f = Set.fromList . BS8.lines <$> BS8.readFile f

main :: IO ()
main = do
  let opts = info (appOpts <**> helper) mempty
  StrippedFullpakOpts{..} <- execParser opts

  let lampakPath          = fullpakPath -<.> ".lampak"
      strippedFullpakName = fullpakPath -<.> ".stripped.fullpak"
      outputFactsPath     = fullpakPath -<.> ".ir-datalog-facts" </> "out"

  livenessFacts <- LivenessFacts
    <$> readNameSet (outputFactsPath </> "ReachableCode.csv")
    <*> readNameSet (outputFactsPath </> "LiveStaticData.csv")
    <*> readNameSet (outputFactsPath </> "LiveConstructor.csv")
    <*> readNameSet (outputFactsPath </> "LiveConGroup.csv")

  putStrLn $ "creating " ++ strippedFullpakName
  putStrLn "modules:"

  createArchive strippedFullpakName $ do

    appInfo <- liftIO $ Stg.readModpakS fullpakPath "app.info" id

    let content = lines . BS8.unpack $ appInfo
        mods    = Stg.parseSection content "modules:"

    addCFAResult outputFactsPath

    liveModules <- forM (zip [1..] mods) $ \(idx, modName) -> do
      let modStgbinName = modName </> Stg.modpakStgbinPath
      stgMod <- liftIO $ do
        putStrLn $ "  " ++ modName
        Stg.readModpakL fullpakPath modStgbinName Stg.decodeStgbin

      modNameMap <- liftIO $ Stg.readModpakS lampakPath (modName </> "name-map.info") readNameMap

      -- export stripped .stgbin and stats
      mmod <- liftIO $ stripDeadCode livenessFacts modNameMap stgMod
      result <- case mmod of
        Nothing -> pure Nothing
        Just (strippedMod, stripStat) -> do
          addZstdEntry (modName </> "strip.info") $ ppStripStat stripStat

          let stgBin = BSL.toStrict . encode . Stg.deconModule . Stg.reconModule . Stg.deconModule $ strippedMod
          addZstdEntry (modName </> "module.stgbin") stgBin
          pure $ Just modName

      -- write out data
      --when (idx `mod` 50 == 0) commit
      commit
      pure result

    -- top level info
    let content = BS8.pack $ unlines
          [ "modules:", Stg.printSection $ catMaybes liveModules
          ]

    addZstdEntry "app.info" content

    -- copy original app.ghc_stgapp file
    ghcstgapp <- liftIO $ Stg.readModpakS fullpakPath "app.ghc_stgapp" id
    addZstdEntry "app.ghc_stgapp" ghcstgapp

addZstdEntry :: FilePath -> BS8.ByteString -> ZipArchive ()
addZstdEntry path content = do
  e <- mkEntrySelector path
  addEntry Zstd content e
  setExternalFileAttrs (fromFileMode 0o0644) e

{-
  fullpak to stripped-fullpak conversion:
    - input: .lampak name ; NOTE: .fullpak and facts path is calculated from the .lampak path
    - read reachable code as Set
    - get modules list from .lampak including name-map
    - copy app.info to .stripped-fullpak archive
    - foreach module:
      + read single module from .fullpak as Module
      + strip Module using name-map and reachableCodeSet
      + add Module to the new .stripped-fullpak archive
-}

readNameMap :: BS8.ByteString -> (Map BS8.ByteString BS8.ByteString, Map BS8.ByteString [BS8.ByteString])
readNameMap content = foldl' go mempty . map BS8.words $ BS8.lines content where
  go (bMap, aMap) = \case
    [] -> (bMap, aMap)
    ["b", stgName, lambdaName]  -> (Map.insert stgName lambdaName bMap, aMap)
    "a" : stgName : lambdaNames -> (bMap, Map.insert stgName lambdaNames aMap)

ppStripStat :: StripStat -> BS8.ByteString
ppStripStat StripStat{..} = BS8.unlines $
  [ "deleted def: " <> n | n <- ssDeletedDefs] ++
  [ "deleted data: " <> n | n <- ssDeletedData] ++
  [ "deleted alt: " <> n | n <- ssDeletedAlts] ++
  [ "deleted con: " <> n | n <- ssDeletedCons] ++
  [ "deleted tycon: " <> n | n <- ssDeletedTyCons] ++
  [ "deleted ref: " <> n | n <- ssDeletedRefs]

addCFAResult :: FilePath -> ZipArchive ()
addCFAResult dir = do
  cfaFiles <- liftIO $ listDirectory dir

  forM_ cfaFiles $ \fname -> do
    let factPath = dir </> fname
    factEntry <- mkEntrySelector (".cfa-result" </> fname)
    loadEntry Zstd factEntry factPath
    setExternalFileAttrs (fromFileMode 0o0644) factEntry
