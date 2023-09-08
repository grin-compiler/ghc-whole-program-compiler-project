import Control.Monad
import Data.List

import Options.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.IO as T

import Stg.Pretty
import Stg.IO

modes :: Parser (IO ())
modes = subparser
    (  mode "show" showMode (progDesc "print Stg")
    )
  where
    mode :: String -> Parser a -> InfoMod a -> Mod CommandFields a
    mode name f opts = command name (info (helper <*> f) opts)

    modpakFile :: Parser FilePath
    modpakFile = argument str (metavar "MODPAK_OR_STGBIN" <> help "pretty prints external stg from .modpak or .stgbin file")

    showMode :: Parser (IO ())
    showMode =
        run <$> modpakFile <*> switch (long "hide-tickish" <> help "do not print STG IR Tickish annotation")
      where
        run fname hideTickish = do
            dump <- case () of
              _ | isSuffixOf "modpak" fname -> Stg.IO.readModpakL fname modpakStgbinPath decodeStgbin
              _ | isSuffixOf "stgbin" fname -> decodeStgbin <$> BSL.readFile fname
              _ -> fail "unknown file format"
            let cfg = Config
                  { cfgPrintTickish = not hideTickish
                  }
            T.putStrLn . fst . pShowWithConfig cfg $ pprModule dump

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
