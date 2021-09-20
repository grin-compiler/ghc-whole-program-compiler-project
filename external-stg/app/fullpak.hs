{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List (isSuffixOf)
import Data.Monoid
import Data.Ord

import Options.Applicative

import Stg.Syntax
import Stg.Analysis.Closure
import Stg.Program

modes :: Parser (IO ())
modes = subparser
    (  mode "stats" statsMode (progDesc "print fullpak statistics")
    )
  where
    mode :: String -> Parser a -> InfoMod a -> Mod CommandFields a
    mode name f opts = command name (info (helper <*> f) opts)

    fullpakFile :: Parser FilePath
    fullpakFile = argument str (metavar "FULLPAK" <> help "GHC-WPC .fullpak or .ghc_stgapp file")

    statsMode :: Parser (IO ())
    statsMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- case () of
              _ | isSuffixOf "fullpak" fname    -> Stg.Program.getFullpakModules fname
              _ | isSuffixOf "ghc_stgapp" fname -> Stg.Program.getGhcStgAppModules fname
              _ | isSuffixOf "json" fname       -> Stg.Program.getJSONModules fname
              _ -> fail "unknown file format"
            putStrLn $ "total number of closures: " ++ show (length $ concatMap getAllClosures moduleList)

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
