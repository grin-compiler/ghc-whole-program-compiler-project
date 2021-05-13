{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad
import Control.Concurrent.Async.Pool

import System.FilePath
import System.Directory
import System.Environment

import Codec.Archive.Zip
import Codec.Archive.Zip.Unix

import Stg.IO
import Stg.Syntax
import Stg.Program
import Stg.Reconstruct
import Stg.Deconstruct

import Stg.DeadFunctionElimination.Facts
import Stg.DeadFunctionElimination.Analysis
import Stg.DeadFunctionElimination.StripModule

import Data.Maybe
import Data.Binary

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL

genProgramDfeFacts :: FilePath -> FilePath -> [String] -> IO ()
genProgramDfeFacts dceFactPath fullpakPath modNameList = timeItNamed "fact collection run time" $ do
  putStrLn "generate datalog facts for whole stg program dead function elimination"
  withTaskGroup 4 $ \g -> do
    mapTasks g  [ readModpakL fullpakPath (modName </> modpakStgbinPath) decodeStgbin >>= writeDfeFacts (dceFactPath </> modName ++ ".stgbin")
                | modName <- modNameList
                ]
  pure ()

main :: IO ()
main = do
  [fullpakPath] <- getArgs

  appInfo <- readModpakS fullpakPath "app.info" id
  let content     = lines . BS8.unpack $ appInfo
      mods        = parseSection content "modules:"
      dceFactPath = fullpakPath -<.> "simple-dce-facts"

  -- HINT: cleanup old content
  removePathForcibly dceFactPath
  createDirectoryIfMissing True dceFactPath

  -- geneate facts
  genProgramDfeFacts dceFactPath fullpakPath mods

  -- run analysis
  livenessAnalysisLogM [dceFactPath </> m ++ ".stgbin" | m <- mods]

  let dcefullpakName  = fullpakPath -<.> ".dce.fullpak"
  putStrLn $ "creating " ++ dcefullpakName
  putStrLn "modules:"

  createArchive dcefullpakName $ do

    liveMods <- forM mods $ \modName -> do
      strippedMod <- liftIO $ do
        putStrLn $ "  " ++ modName
        let modStgbinName = modName </> modpakStgbinPath
        stgMod <- readModpakL fullpakPath modStgbinName decodeStgbin

        tryStripDeadParts {-modpakName-}"." stgMod -- TODO: fix liveness input name

      case isEmptyModule strippedMod of
        True  -> pure Nothing
        False -> do
          let stgBin = BSL.toStrict . encode . deconModule . reconModule . deconModule $ strippedMod
          addZstdEntry (modName </> "module.stgbin") stgBin
          pure $ Just modName

    -- top level info
    let content = BS8.pack $ unlines
          [ "modules:", printSection $ catMaybes liveMods
          ]

    addZstdEntry "app.info" content

addZstdEntry :: FilePath -> BS8.ByteString -> ZipArchive ()
addZstdEntry path content = do
  e <- mkEntrySelector path
  addEntry Zstd content e
  setExternalFileAttrs (fromFileMode 0o0644) e

isEmptyModule :: Module -> Bool
isEmptyModule Module{..} =
  null [tc | (u, ml) <- moduleTyCons, u == moduleUnitId, (m, tcl) <- ml, m == moduleName, tc <- tcl] &&
  null moduleTopBindings &&
  null moduleForeignFiles &&
  NoStubs /= moduleForeignStubs
