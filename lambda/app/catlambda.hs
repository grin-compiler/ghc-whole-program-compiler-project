{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

import Control.Monad.IO.Class
import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))

import Data.List (isPrefixOf)
import qualified Data.Set as Set
import qualified Data.Map as Map

import System.Directory
import System.FilePath

import qualified Data.ByteString.Char8 as BS8

import Codec.Archive.Zip

import qualified Stg.Program as Stg

data CatLambdaOpts
  = CatLambdaOpts
  { lampakPath  :: FilePath
  }

appOpts :: Parser CatLambdaOpts
appOpts = CatLambdaOpts
  <$> argument str (metavar "LAMPAKFILE" <> help "The .lampak file to collect .lambda IR from")

main :: IO ()
main = do
  let opts = info (appOpts <**> helper) mempty
  CatLambdaOpts{..} <- execParser opts

  let lambdaPath0 = lampakPath -<.> ".lambda"
  lambdaPath <- makeAbsolute lambdaPath0

  putStrLn $ "linking lambda IR to: " ++ lambdaPath
  BS8.writeFile lambdaPath ""

  withArchive lampakPath $ do
    -- get list of modules
    appInfoEntry <- mkEntrySelector "app.info"
    content <- lines . BS8.unpack <$> getEntry appInfoEntry
    let mods = Stg.parseSection content "modules:"

    forM_ mods $ \m -> do
      e <- mkEntrySelector $ m </> "module.lambda"
      src <- getEntry e
      liftIO $ BS8.appendFile lambdaPath $ BS8.pack ("\nmodule " ++ m ++ "\n\n") <> src
