{-# LANGUAGE LambdaCase, RecordWildCards #-}

import Control.Monad.IO.Class
import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))

import Data.List (isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set

import System.Directory
import System.FilePath
import System.IO.Temp

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Binary (encode)
import Text.PrettyPrint.ANSI.Leijen (plain, pretty)

import Codec.Archive.Zip
import Codec.Archive.Zip.Unix

import qualified Stg.Syntax   as Stg
import qualified Stg.Program  as Stg

import Lambda.Syntax
import Lambda.Pretty
import Lambda.Stg.FromStg
import Lambda.Datalog.ToDatalog

data MkLamPakOpts
  = MkLamPakOpts
  { appPath     :: FilePath
  }

appOpts :: Parser MkLamPakOpts
appOpts = MkLamPakOpts
  <$> argument str (metavar "APPFILE" <> help "The .ghc_stgapp or .fullpak file to convert")

main :: IO ()
main = do
  let opts = info (appOpts <**> helper) mempty
  MkLamPakOpts{..} <- execParser opts
  mods <- case takeExtension appPath of
    ".fullpak"                          -> Stg.getFullpakModules appPath
    ".json"                             -> Stg.getJSONModules appPath
    ext | isSuffixOf "_ghc_stgapp" ext  -> Stg.getGhcStgAppModules appPath
    _                                   -> error "unknown input file format"

  let lampakName  = appPath -<.> ".lampak"

  putStrLn $ "creating " ++ lampakName
  putStrLn "modules:"

  -- HINT: delete tmp directory in the end
  tmpSys <- getCanonicalTemporaryDirectory
  withTempDirectory tmpSys "lambda-datalog" $ \tmpDir -> do
    createArchive lampakName $ do
      -- top level info
      let content = BS8.pack $ unlines
            [ "modules:", Stg.printSection $ map (Stg.getModuleName . Stg.moduleName) mods
            ]

      addZstdEntry "app.info" content

      forM_ (zip [1..] mods) $ \(idx, stgMod) -> do
        let modName = BS8.unpack . Stg.getModuleName . Stg.moduleName $ stgMod
        (prg, CGStat{..}) <- liftIO $ do
          putStrLn $ "  " ++ modName
          codegenLambda stgMod

        -- add stats if not empty
        unless (null cgMessages) $ do
          addZstdEntry (modName </> "messages.info") . BS8.pack $ unlines cgMessages

        unless (null cgWarnings) $ do
          addZstdEntry (modName </> "warnings.info") . BS8.pack $ unlines cgWarnings

        unless (null cgErrors) $ do
          addZstdEntry (modName </> "errors.info") . BS8.pack $ unlines cgErrors

        addZstdEntry (modName </> "name-map.info") . BS8.pack $ unlines cgNameMap

        -- export .lambdabin
        let lambdaBin = BSL.toStrict $ encode prg
        addZstdEntry (modName </> "module.lambdabin") lambdaBin

        -- export .lambda pretty print
        let lambdaSrc = BS8.pack . showWidth 200 . plain $ pretty prg
        addZstdEntry (modName </> "module.lambda") lambdaSrc

        -- export datalog facts
        exportDatalogFacts tmpDir modName prg

        -- write out data
        --when (idx `mod` 50 == 0) commit

addZstdEntry :: FilePath -> BS8.ByteString -> ZipArchive ()
addZstdEntry path content = do
  e <- mkEntrySelector path
  addEntry Zstd content e
  setExternalFileAttrs (fromFileMode 0o0644) e

exportDatalogFacts :: FilePath -> String -> Program -> ZipArchive ()
exportDatalogFacts tmpDir modName prg = do
  let tmpDL = tmpDir </> modName
  factFiles <- liftIO $ do
    createDirectoryIfMissing False tmpDL
    programToFactsM False tmpDL prg
    listDirectory tmpDL

  forM_ factFiles $ \factName -> do
    let factPath = tmpDL </> factName
    factEntry <- mkEntrySelector (modName </> "datalog" </> factName)
    loadEntry Zstd factEntry factPath
    setExternalFileAttrs (fromFileMode 0o0644) factEntry
