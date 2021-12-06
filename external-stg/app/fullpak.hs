{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}

import Control.Monad
import Data.Maybe
import Data.List (isSuffixOf)
import Data.Monoid
import Data.Ord
import Data.Semigroup ((<>))
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import System.IO

import Options.Applicative

import Stg.Syntax
import Stg.Analysis.Closure
import Stg.Program

loadModules :: FilePath -> IO [Module]
loadModules fname = case () of
  _ | isSuffixOf "fullpak" fname    -> Stg.Program.getFullpakModules fname
  _ | isSuffixOf "ghc_stgapp" fname -> Stg.Program.getGhcStgAppModules fname
  _ | isSuffixOf "json" fname       -> Stg.Program.getJSONModules fname
  _ -> fail "unknown file format"

modes :: Parser (IO ())
modes = subparser
    (  mode "stats" statsMode (progDesc "print fullpak statistics")
    <> mode "deps"  depsMode  (progDesc "write out module dependency facts")
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
            moduleList <- loadModules fname
            putStrLn $ "total number of closures: " ++ show (length $ concatMap getAllClosures moduleList)

    depsMode :: Parser (IO ())
    depsMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- loadModules fname
            withFile (fname -<.> ".deps.tsv") WriteMode $ \h -> do
              hPutStrLn h "Source\tTarget"
              forM_ moduleList $ \Module{..} -> do
                let modName = getUnitId moduleUnitId <> "_" <> getModuleName moduleName
                    deps    = [ modName <> "\t" <> depName
                              | (uid, mods) <- moduleDependency
                              , mod <- mods
                              , let depName = getUnitId uid <> "_" <> getModuleName mod
                              , depName /= modName
                              ]
                BS8.hPutStr h $ BS8.unlines deps
            pure ()

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
