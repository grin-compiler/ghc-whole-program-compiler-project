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

main :: IO ()
main = do
  let opts = info (modpak <**> helper) mempty
  Modpak{..}  <- execParser opts

  stgbinData  <- BS.readFile stgbinPath
  stgbinEntry <- mkEntrySelector "module.stgbin"

  ghcstgData  <- BS.readFile ghcstgPath
  ghcstgEntry <- mkEntrySelector "module.ghcstg"

  cmmData  <- BS.readFile cmmPath
  cmmEntry <- mkEntrySelector "module.cmm"

  asmData  <- BS.readFile asmPath
  asmEntry <- mkEntrySelector "module.s"

  infoData  <- BS.readFile infoPath
  infoEntry <- mkEntrySelector "module.info"

  ghccoreEntry <- mkEntrySelector "module.ghccore"
  hsEntry <- mkEntrySelector "module.hs"
  createArchive modpakName $ do
    addEntry Zstd stgbinData stgbinEntry
    setExternalFileAttrs (fromFileMode 0o0644) stgbinEntry

    addEntry Zstd ghcstgData ghcstgEntry
    setExternalFileAttrs (fromFileMode 0o0644) ghcstgEntry

    addEntry Zstd cmmData cmmEntry
    setExternalFileAttrs (fromFileMode 0o0644) cmmEntry

    addEntry Zstd asmData asmEntry
    setExternalFileAttrs (fromFileMode 0o0644) asmEntry

    addEntry Zstd infoData infoEntry
    setExternalFileAttrs (fromFileMode 0o0644) infoEntry

    case hsSrcPath of
      Nothing -> pure ()
      Just hs -> do
        hsData <- liftIO $ BS.readFile hs
        addEntry Zstd hsData hsEntry
        setExternalFileAttrs (fromFileMode 0o0644) hsEntry
    case ghccorePath of
      Nothing -> pure ()
      Just c -> do
        coreData <- liftIO $ BS.readFile c
        addEntry Zstd coreData ghccoreEntry
        setExternalFileAttrs (fromFileMode 0o0644) ghccoreEntry
