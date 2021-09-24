{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Catch
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import Codec.Archive.Zip
import Codec.Archive.Zip.Unix
import Text.Printf

import qualified Data.Map as Map

import Stg.Program
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
            ]
      forM_ files $ \fn -> do
        (src, dst) <- liftIO $ do
          (,) <$> mkEntrySelector fn <*> mkEntrySelector (modModuleName </> fn)
        flip catch (\EntryDoesNotExist{} -> pure ()) $ do
          copyEntry modModpakPath src dst
          setExternalFileAttrs (fromFileMode 0o0644) dst
