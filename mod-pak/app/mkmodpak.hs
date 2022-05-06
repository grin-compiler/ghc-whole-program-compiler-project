{-# LANGUAGE RecordWildCards #-}

import Control.Monad.IO.Class
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.ByteString as BS
import System.FilePath
import Codec.Archive.Zip
import Codec.Archive.Zip.Unix

data Modpak
  = Modpak
  { modpakName  :: String
  , stgbinPath  :: FilePath
  , ghcstgPath  :: FilePath
  , cmmPath     :: FilePath
  , asmPath     :: FilePath
  , infoPath    :: FilePath
  , hsSrcPath   :: Maybe FilePath
  , ghccorePath :: Maybe FilePath
  , stubhPath   :: Maybe FilePath
  , stubcPath   :: Maybe FilePath
  }

modpak :: Parser Modpak
modpak = Modpak
  <$> strOption (long "modpakname" <> metavar "FILENAME" <> help "The name of the Modpak archive to be created")
  <*> strOption (long "stgbin" <> metavar "FILENAME" <> help "Stgbin file to be added to the archive")
  <*> strOption (long "ghcstg" <> metavar "FILENAME" <> help "Pretty printed GHC Stg file to be added to the archive")
  <*> strOption (long "cmm" <> metavar "FILENAME" <> help "Cmm source file to be added to the archive")
  <*> strOption (long "asm" <> metavar "FILENAME" <> help "Assembly source file to be added to the archive")
  <*> strOption (long "info" <> metavar "FILENAME" <> help "Compilation info file to be added to the archive")
  <*> optional (strOption (long "hssrc" <> metavar "FILENAME" <> help "Haskell source file to be added to the archive"))
  <*> optional (strOption (long "ghccore" <> metavar "FILENAME" <> help "Pretty printed GHC Core file to be added to the archive"))
  <*> optional (strOption (long "stubh" <> metavar "FILENAME" <> help "FFI stub C header file to be added to the archive"))
  <*> optional (strOption (long "stubc" <> metavar "FILENAME" <> help "FFI stub C source file to be added to the archive"))

add :: FilePath -> FilePath -> ZipArchive ()
add zipPath srcPath = do
  entry <- mkEntrySelector zipPath
  loadEntry Zstd entry srcPath
  setExternalFileAttrs (fromFileMode 0o0644) entry

maybe_add :: FilePath -> Maybe FilePath -> ZipArchive ()
maybe_add zipPath = mapM_ (add zipPath)

main :: IO ()
main = do
  let opts = info (modpak <**> helper) mempty
  Modpak{..} <- execParser opts

  createArchive modpakName $ do
    add "module.stgbin" stgbinPath
    add "module.ghcstg" ghcstgPath
    add "module.cmm"    cmmPath
    add "module.s"      asmPath
    add "module.info"   infoPath

    maybe_add "module.hs"       hsSrcPath
    maybe_add "module.ghccore"  ghccorePath
    maybe_add "module_stub.h"   stubhPath
    maybe_add "module_stub.c"   stubcPath
