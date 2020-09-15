{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Control.Monad.IO.Class
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import Codec.Archive.Zip
import Codec.Archive.Zip.Unix

import qualified Data.Map as Map
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)

import Stg.Program
import qualified Stg.GHC.Symbols as GHCSymbols

data Fullpak
  = Fullpak
  { ghcstgappPath :: FilePath
  , stgbinsOnly   :: Bool
  }

fullpak :: Parser Fullpak
fullpak = Fullpak
  <$> argument str (metavar "FILE" <> help "The .ghc_stgapp file that will be packed")
  <*> switch (short 's' <> long "stgbins-only" <> help "Packs the module.stgbin files only")

main :: IO ()
main = do
  let opts = info (fullpak <**> helper) mempty
  Fullpak{..}  <- execParser opts

  -- mk .fullpak
  modinfoList <- getAppModuleMapping ghcstgappPath
  appModpaks <- collectProgramModules (map modModpakPath modinfoList) "main" "Main" GHCSymbols.liveSymbols
  let modpakMap       = Map.fromList [(modModpakPath m , m) | m <- modinfoList]
      fullpakModules  = [modpakMap Map.! m | m <- appModpaks]

      ppSection l     = unlines ["- " ++ x | x <- nubOrd $ map show l]

      fullpakName     = ghcstgappPath -<.> ".fullpak"

  putStrLn $ "creating " ++ fullpakName
  createArchive fullpakName $ do
    -- top level info
    let content = BS8.pack $ unlines
          [ "modules:", ppSection . sort $ map modModuleName fullpakModules
          ]
    appinfo <- mkEntrySelector "app.info"
    addEntry Deflate content appinfo
    setExternalFileAttrs (fromFileMode 0o0644) appinfo

    -- copy module content
    forM_ fullpakModules $ \StgModuleInfo{..} -> do
      let files =
            [ "module.stgbin"
            ] ++ if stgbinsOnly then [] else
            [ "module.ghcstg"
            , "module.hs"
            ]
      forM_ files $ \fn -> do
        (src, dst) <- liftIO $ do
          (,) <$> mkEntrySelector fn <*> mkEntrySelector (modModuleName </> fn)
        copyEntry modModpakPath src dst
        setExternalFileAttrs (fromFileMode 0o0644) dst
