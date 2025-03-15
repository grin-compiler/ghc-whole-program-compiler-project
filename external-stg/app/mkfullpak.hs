import           Data.Bool           (Bool)
import           Data.Monoid         (Monoid (..))
import           Data.Semigroup      ((<>))

import           Options.Applicative

import           Stg.Fullpak

import           System.FilePath
import           System.IO           (IO)

data FullpakOptions
  = FullpakOptions
  { ghcstgappPath :: FilePath
  , stgbinsOnly   :: Bool
  , includeAll    :: Bool
  }

fullpak :: Parser FullpakOptions
fullpak = FullpakOptions
  <$> argument str (metavar "FILE" <> help "The .ghc_stgapp file that will be packed")
  <*> switch (short 's' <> long "stgbins-only" <> help "Packs the module.stgbin files only")
  <*> switch (short 'a' <> long "include-all" <> help "Includes all progam and library modules (without dead module elimination)")

main :: IO ()
main = do
  let opts = info (fullpak <**> helper) mempty
  FullpakOptions{..} <- execParser opts
  let fullpakName = ghcstgappPath -<.> ".fullpak"

  mkFullpak ghcstgappPath stgbinsOnly includeAll fullpakName
