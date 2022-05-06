{-# LANGUAGE RecordWildCards, LambdaCase #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import System.Directory
import Codec.Archive.Zip
import Codec.Archive.Zip.Unix
import Text.Printf

import qualified Data.Map as Map

import Stg.Program
import Stg.Foreign.Linker
import qualified Stg.GHC.Symbols as GHCSymbols

data Fullpak
  = Fullpak
  { ghcstgappPath :: FilePath
  , stgbinsOnly   :: Bool
  , includeAll    :: Bool
  }

fullpak :: Parser Fullpak
fullpak = Fullpak
  <$> argument str (metavar "FILE" <> help "The .ghc_stgapp file that will be packed")
  <*> switch (short 's' <> long "stgbins-only" <> help "Packs the module.stgbin files only")
  <*> switch (short 'a' <> long "include-all" <> help "Includes all progam and library modules (without dead module elimination)")

getModuleList :: [StgModuleInfo] -> IO [FilePath]
getModuleList modinfoList = do
  putStrLn $ "all modules: " ++ show (length modinfoList)
  forM modinfoList $ \StgModuleInfo{..} -> do
    printf "%-60s %s\n" modPackageName modModuleName
    pure modModpakPath

main :: IO ()
main = do
  let opts = info (fullpak <**> helper) mempty
  Fullpak{..}  <- execParser opts

  -- mk .fullpak
  modinfoList <- getAppModuleMapping ghcstgappPath
  appModpaks <- if includeAll
    then getModuleList modinfoList
    else collectProgramModules (map modModpakPath modinfoList) "main" "Main" GHCSymbols.liveSymbols

  let modpakMap       = Map.fromList [(modModpakPath m , m) | m <- modinfoList]
      fullpakModules  = [modpakMap Map.! m | m <- appModpaks]
      fullpakName     = ghcstgappPath -<.> ".fullpak"

  -- collect license info
  StgAppLicenseInfo{..} <- getAppLicenseInfo ghcstgappPath


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
    let content = BS8.pack $ unlines
          [ "modules:", printSection $ map modModuleName fullpakModules
          ]
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
            , "module.ghccore"
            , "module.hs"
            , "module.cmm"
            , "module.s"
            , "module.info"
            , "module_stub.h"
            , "module_stub.c"
            ]
      existingFiles <- withArchive modModpakPath $ mapM mkEntrySelector files >>= filterM doesEntryExist
      forM_ existingFiles $ \src -> do
        dst <- mkEntrySelector (modModuleName </> unEntrySelector src)
        copyEntry modModpakPath src dst
        setExternalFileAttrs (fromFileMode 0o0644) dst

add :: FilePath -> FilePath -> ZipArchive ()
add zipPath srcPath = do
  entry <- mkEntrySelector zipPath
  loadEntry Zstd entry srcPath
  setExternalFileAttrs (fromFileMode 0o0644) entry
