{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Fullpak
  ( mkFullpak
  ) where

import Control.Monad
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import System.Directory
import Codec.Archive.Zip
import Codec.Archive.Zip.Unix
import Text.Printf

import qualified Data.Map as Map
import qualified Data.Yaml as Y

import Stg.Program
import Stg.Foreign.Linker
import qualified Stg.GHC.Symbols as GHCSymbols

getModuleList :: [StgModuleInfo] -> IO [FilePath]
getModuleList modinfoList = do
  putStrLn $ "all modules: " ++ show (length modinfoList)
  forM modinfoList $ \StgModuleInfo{..} -> do
    printf "%-60s %s\n" modPackageName modModuleName
    pure modModpakPath

add :: FilePath -> FilePath -> ZipArchive ()
add zipPath srcPath = do
  entry <- mkEntrySelector zipPath
  loadEntry Zstd entry srcPath
  setExternalFileAttrs (fromFileMode 0o0644) entry

mkFullpak :: FilePath -> Bool -> Bool -> FilePath -> IO ()
mkFullpak ghcstgappPath stgbinsOnly includeAll fullpakName = do
  -- mk .fullpak
  modinfoList <- getAppModuleMapping ghcstgappPath
  appModpaks <- if includeAll
    then getModuleList modinfoList
    else collectProgramModules (map modModpakPath modinfoList) "main" "Main" GHCSymbols.liveSymbols

  let modpakMap       = Map.fromList [(modModpakPath m , m) | m <- modinfoList]
      fullpakModules  = [modpakMap Map.! m | m <- appModpaks]

  -- collect license info
  StgAppLicenseInfo{..} <- getAppLicenseInfo ghcstgappPath

  -- collect cbits sources
  cbitsSourceInfos <- getAppForeignFiles ghcstgappPath

  -- link cbits.so
  workDir <- getExtStgWorkDirectory ghcstgappPath
  let soName = workDir </> "cbits.so"
  doesFileExist soName >>= \case
    True  -> do
      putStrLn "using existing cbits.so"
    False -> do
      putStrLn "linking cbits.so"
      linkForeignCbitsSharedLib ghcstgappPath

  putStrLn $ "creating " ++ fullpakName
  createArchive fullpakName $ do
    -- top level info
    let content = Y.encode $
          AppInfo
          { aiLiveCode =
              [ CodeInfo
                { ciPackageName = modPackageName
                , ciUnitId      = modUnitId
                , ciModuleName  = modModuleName
                }
              | StgModuleInfo{..} <- fullpakModules
              ]
          }
    appinfo <- mkEntrySelector "app.info"
    addEntry Deflate content appinfo
    setExternalFileAttrs (fromFileMode 0o0644) appinfo

    -- add .ghc_stgapp to .fullpak
    app_ghcstgapp <- mkEntrySelector "app.ghc_stgapp"
    loadEntry Deflate app_ghcstgapp ghcstgappPath
    setExternalFileAttrs (fromFileMode 0o0644) app_ghcstgapp

    -- copy license info
    forM_ stgappUnitConfs $ \unitConf -> do
      add (".package-db-and-license-info" </> takeFileName unitConf) unitConf

    -- copy cbits sources
    forM_ cbitsSourceInfos $ \StgAppForeignSourceInfo{..} -> do
      add ("cbits-source" </> stgForeignUnitId </> stgForeignSourceRelPath) stgForeignSourceAbsPath

    -- copy cbits.so and related files
    add "cbits/cbits.so" soName
    add "cbits/cbits.so.sh" (soName ++ ".sh")
    add "cbits/stub.c" (workDir </> "stub.c")

    -- copy module content
    forM_ fullpakModules $ \StgModuleInfo{..} -> do
      let files =
            [ "module.stgbin"
            ] ++ if stgbinsOnly then [] else
            [ "module.ghcstg"
            , "module.fullcore-hi"
            , "module.ghccore"
            , "module.hs"
            , "module.cmm"
            , "module.s"
            , "module.info"
            , "module_stub.h"
            , "module_stub.c"
            , "module_capi_stub.o"
            ]
      existingFiles <- withArchive modModpakPath $ mapM mkEntrySelector files >>= filterM doesEntryExist
      forM_ existingFiles $ \src -> do
        dst <- mkEntrySelector ("haskell" </> modPackageName </> modModuleName </> unEntrySelector src)
        copyEntry modModpakPath src dst
        setExternalFileAttrs (fromFileMode 0o0644) dst
