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
  , hsSrcPath   :: Maybe FilePath
  }

modpak :: Parser Modpak
modpak = Modpak
  <$> strOption (long "modpakname" <> metavar "FILENAME" <> help "The name of the Modpak archive to be created")
  <*> strOption (long "stgbin" <> metavar "FILENAME" <> help "Stgbin file to be added to the archive")
  <*> strOption (long "ghcstg" <> metavar "FILENAME" <> help "Pretty printed GHC Stg file to be added to the archive")
  <*> optional (strOption (long "hssrc" <> metavar "FILENAME" <> help "Haskell source file to be added to the archive"))

main :: IO ()
main = do
  let opts = info (modpak <**> helper) mempty
  Modpak{..}  <- execParser opts
  stgbinData  <- BS.readFile stgbinPath
  stgbinEntry <- mkEntrySelector "module.stgbin"
  ghcstgData  <- BS.readFile ghcstgPath
  ghcstgEntry <- mkEntrySelector "module.ghcstg"
  hsEntry <- mkEntrySelector "module.hs"
  createArchive modpakName $ do
    addEntry Store stgbinData stgbinEntry
    setExternalFileAttrs (fromFileMode 0o0644) stgbinEntry
    addEntry Store ghcstgData ghcstgEntry
    setExternalFileAttrs (fromFileMode 0o0644) ghcstgEntry
    case hsSrcPath of
      Nothing -> pure ()
      Just hs -> do
        hsData <- liftIO $ BS.readFile hs
        addEntry Store hsData hsEntry
        setExternalFileAttrs (fromFileMode 0o0644) hsEntry
