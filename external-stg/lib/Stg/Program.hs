{-# LANGUAGE TupleSections, LambdaCase, RecordWildCards, OverloadedStrings #-}
module Stg.Program where

import Control.Monad.IO.Class
import Control.Monad
import Text.Printf

import Data.Maybe
import Data.List (isPrefixOf, groupBy, sortBy, foldl')
import Data.Containers.ListUtils (nubOrd)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson

import System.Directory
import System.FilePath
import System.FilePath.Find
import Codec.Archive.Zip

import qualified Data.Yaml as Y
import Data.Yaml (FromJSON(..), (.:), (.:?), (.!=))

import Stg.Syntax
import Stg.IO
import Stg.JSON ()
import Stg.Reconstruct (reconModule)
import qualified Stg.GHC.Symbols as GHCSymbols

moduleToModpak :: String -> String -> FilePath
moduleToModpak modpakExt moduleName = replaceEq '.' '/' moduleName ++ modpakExt
  where
    replaceEq :: Eq a => a -> a -> [a] -> [a]
    replaceEq from to = map (\cur -> if cur == from then to else cur)

parseSection :: [String] -> String -> [String]
parseSection content n = map (read . tail) . takeWhile (isPrefixOf "-") . tail . dropWhile (not . isPrefixOf n) $ content

printSection :: Show a => [a] -> String
printSection l = unlines ["- " ++ x | x <- nubOrd $ map show l]

data StgModuleInfo
  = StgModuleInfo
  { modModuleName           :: String
  , modModpakPath           :: FilePath
  , modPackageName          :: String
  , modPackageVersion       :: String
  }
  deriving (Eq, Ord, Show)

data UnitLinkerInfo =
  UnitLinkerInfo
  { unitName            :: String
  , unitVersion         :: String
  , unitId              :: String
  , unitImportDirs      :: [FilePath]
  , unitLibraries       :: [String]
  , unitLibDirs         :: [FilePath]
  , unitExtraLibs       :: [String]
  , unitLdOptions       :: [String]
  , unitExposedModules  :: [String]
  , unitHiddenModules   :: [String]
  } deriving (Eq, Show)

instance FromJSON UnitLinkerInfo where
  parseJSON (Y.Object v) =
    UnitLinkerInfo
      <$> v .: "name"
      <*> v .: "version"
      <*> v .: "id"
      <*> v .:? "unit-import-dirs" .!= []
      <*> v .:? "unit-libraries" .!= []
      <*> v .:? "library-dirs" .!= []
      <*> v .:? "extra-libraries" .!= []
      <*> v .:? "ld-options" .!= []
      <*> v .:? "exposed-modules" .!= []
      <*> v .:? "hidden-modules" .!= []
  parseJSON _ = fail "Expected Object for UnitLinkerInfo value"

data GhcStgApp
  = GhcStgApp
  { appWays           :: [String]
  , appObjSuffix      :: String
  , appNoHsMain       :: Bool
  , appPlatformOS     :: String
  , appUnitDbPaths    :: [FilePath]
  , appObjFiles       :: [FilePath]
  , appExtraLdInputs  :: [FilePath]
  , appExtraLibs      :: [String]
  , appExtraLibDirs   :: [FilePath]
  , appLdOptions      :: [String]
  , appLibDeps        :: [UnitLinkerInfo]
  } deriving (Eq, Show)

instance FromJSON GhcStgApp where
  parseJSON (Y.Object v) =
    GhcStgApp
      <$> v .:? "ways" .!= []
      <*> v .: "o_suffix"
      <*> v .: "no_hs_main"
      <*> v .: "platform_os"
      <*> v .:? "unit_db_paths" .!= []
      <*> v .:? "o_files" .!= []
      <*> v .:? "extra_ld_inputs" .!= []
      <*> v .:? "extra-libraries" .!= []
      <*> v .:? "library-dirs" .!= []
      <*> v .:? "ld-options" .!= []
      <*> v .:? "app-deps" .!= []
  parseJSON _ = fail "Expected Object for UnitLinkerInfo value"

getAppModuleMapping :: FilePath -> IO [StgModuleInfo]
getAppModuleMapping ghcStgAppFname = do
  let showLog = False -- TODO: use RIO ???
  let packageName = "exe:" ++ takeBaseName ghcStgAppFname -- TODO: save package to .ghc_stgapp
  GhcStgApp{..} <- Y.decodeFileThrow ghcStgAppFname
  let modpakExt = "." ++ appObjSuffix ++ "_modpak"
      check f = do
        exist <- doesFileExist f
        unless exist $ when showLog $ do
          putStrLn $ "modpak does not exist: " ++ f
        pure exist
  libModules <- filterM (check . modModpakPath) $
        [ StgModuleInfo
          { modModuleName     = mod
          , modModpakPath     = dir </> moduleToModpak modpakExt mod
          , modPackageName    = unitName
          , modPackageVersion = unitVersion
          }
        | UnitLinkerInfo{..} <- appLibDeps
        , dir <- unitImportDirs
        , mod <- unitExposedModules ++ unitHiddenModules

        -- TODO: make this better somehow
        -- HINT: this module does not exist, it's a GHC builtin for primops
        , let builtin = mod == "GHC.Prim" && unitName == "ghc-prim"
        , not builtin

        ]

  extraAppModpaks <- filterM check [oPath ++ "_modpak" | oPath <- appExtraLdInputs]
  let appModpaks = [oPath ++ "_modpak" | oPath <- appObjFiles]
  -- load app module names from stgbins
  appModules <- forM (appModpaks ++ extraAppModpaks) $ \modpak -> do
    (_phase, _unitId, moduleName, _srcPath) <- readModpakL modpak modpakStgbinPath decodeStgbinModuleName
    pure StgModuleInfo
          { modModuleName     = BS8.unpack $ getModuleName moduleName
          , modModpakPath     = modpak
          , modPackageName    = packageName
          , modPackageVersion = ""
          }
  pure $ appModules ++ libModules

getAppModpaks :: FilePath -> IO [FilePath]
getAppModpaks ghcStgAppFname = map modModpakPath <$> getAppModuleMapping ghcStgAppFname

collectProgramModules' :: Bool -> [FilePath] -> String -> String -> [(String, String, String)] -> IO [FilePath]
collectProgramModules' showLog modpakFileNames unitId mod liveSymbols = do
  -- filter dependenies only
  (fexportedList, depList) <- fmap unzip . forM modpakFileNames $ \fname -> do
    (_, u, m, _, _, hasForeignExport, deps) <- readModpakL fname modpakStgbinPath decodeStgbinInfo
    let fexport = if hasForeignExport then Just (u, m) else Nothing
    pure (fexport, ((u, m), [(du, dm) | (du, dl) <- deps, dm <- dl]))
  let fnameMap  = Map.fromList $ zip (map fst depList) modpakFileNames
      mnameMap  = Map.fromList $ zip modpakFileNames (map fst depList)
      depMap    = Map.fromList depList
      calcDep s n
        | Set.member n s = s
        | Just l <- Map.lookup n depMap = foldl' calcDep (Set.insert n s) l
        | otherwise = Set.insert n s -- error $ printf "missing module: %s" . show $ getModuleName n

      keyMain = (UnitId $ BS8.pack unitId, ModuleName $ BS8.pack mod)
      prunedDeps = catMaybes [Map.lookup m fnameMap | m <- Set.toList $ foldl calcDep mempty $ keyMain : rtsDeps ++ catMaybes fexportedList]
      rtsDeps = [(UnitId $ BS8.pack u, ModuleName $ BS8.pack m) | (u, m, _) <- liveSymbols]

  when showLog $ do
    putStrLn $ "all modules: " ++ show (length modpakFileNames)
    putStrLn $ "app modules: " ++ show (length prunedDeps)
    putStrLn $ "app dependencies:\n"
    forM_ [mnameMap Map.! fname | fname <- prunedDeps] $ \(UnitId uid, ModuleName mod) -> do
      printf "%-60s %s\n" (BS8.unpack uid) (BS8.unpack mod)
  pure prunedDeps

collectProgramModules :: [FilePath] -> String -> String -> [(String, String, String)] -> IO [FilePath]
collectProgramModules = collectProgramModules' True

-- .fullpak
getFullpakModules :: FilePath -> IO [Module]
getFullpakModules fullpakPath = do
  withArchive fullpakPath $ do
    appinfoSelector <- liftIO $ mkEntrySelector fullpakAppInfoPath
    content <- lines . BS8.unpack <$> getEntry appinfoSelector
    let modules = parseSection content "modules:"
    forM modules $ \m -> do
      s <- liftIO $ mkEntrySelector $ m </> modpakStgbinPath
      decodeStgbin . BSL.fromStrict <$> getEntry s

-- .ghc_stgapp
getGhcStgAppModules :: FilePath -> IO [Module]
getGhcStgAppModules ghcstgappPath = do
  modinfoList <- getAppModuleMapping ghcstgappPath
  appModpaks <- collectProgramModules' False (map modModpakPath modinfoList) "main" "Main" GHCSymbols.liveSymbols
  forM appModpaks $ \modpakName -> do
    readModpakL modpakName modpakStgbinPath decodeStgbin

-- .json
getJSONModules :: FilePath -> IO [Module]
getJSONModules filePath = do
  res <- Aeson.eitherDecodeFileStrict' filePath
  case res of
    Left  err     -> error err
    Right smodule -> pure [reconModule smodule]

data StgLibLinkerInfo
  = StgLibLinkerInfo
  { stglibName            :: String
  , stglibCbitsPaths      :: [FilePath]
  , stglibCapiStubsPaths  :: [FilePath]
  , stglibAllStubsPaths   :: [FilePath]
  , stglibExtraLibs       :: [String]
  , stglibExtraLibDirs    :: [FilePath]
  , stglibLdOptions       :: [String]
  }
  deriving (Eq, Ord, Show)

data StgAppLinkerInfo
  = StgAppLinkerInfo
  { stgappCObjects      :: [FilePath]
  , stgappExtraLibs     :: [String]
  , stgappExtraLibDirs  :: [FilePath]
  , stgappLdOptions     :: [String]
  , stgappPlatformOS    :: String
  , stgappNoHsMain      :: Bool
  }
  deriving (Eq, Ord, Show)

getAppLinkerInfo :: FilePath -> IO (StgAppLinkerInfo, [StgLibLinkerInfo])
getAppLinkerInfo ghcStgAppFname = do
  GhcStgApp{..} <- Y.decodeFileThrow ghcStgAppFname

  -- app info
  appCbits <- flip filterM (appExtraLdInputs ++ appObjFiles) $ \objName -> do
    -- ASSUMPTION: if there is no .modpak file then it must be a C like object file
    let modpakFile = objName ++ "_modpak"
    not <$> doesFileExist modpakFile
  let appInfo = StgAppLinkerInfo
        { stgappCObjects      = appCbits
        , stgappExtraLibs     = appExtraLibs
        , stgappExtraLibDirs  = appExtraLibDirs
        , stgappLdOptions     = appLdOptions
        , stgappPlatformOS    = appPlatformOS
        , stgappNoHsMain      = appNoHsMain
        }

  -- lib info
  let forceDynamic = True
      arExt n = if forceDynamic
        then "-ghc9.0.2.1000.dyn_o" ++ n ++ ".a"
        else "." ++ appObjSuffix ++ n ++ ".a"
  libInfoList <- forM appLibDeps $ \UnitLinkerInfo{..} -> do

    let pathList = [path </> libName | path <- unitLibDirs, lib <- unitLibraries, let libName = "lib" ++ lib]

    cbitsPathList <- forM pathList $ \path -> do
      -- FIXME: make cbits and stubs name handling robust, this is a HACK!
      let cbitsPath = path ++ arExt "_cbits"
      doesFileExist cbitsPath >>= \case
        True  -> pure $ Just cbitsPath
        False -> pure Nothing

    stubsPathList <- forM pathList $ \path -> do
      -- FIXME: make cbits and stubs name handling robust, this is a HACK!
      let stubsPath = path ++ arExt "_capi_stubs"
      doesFileExist stubsPath >>= \case
        True  -> pure $ Just stubsPath
        False -> pure Nothing

    allStubsPathList <- forM pathList $ \path -> do
      -- FIXME: make cbits and stubs name handling robust, this is a HACK!
      let stubsPath = path ++ arExt "_all_stubs"
      doesFileExist stubsPath >>= \case
        True  -> pure $ Just stubsPath
        False -> pure Nothing

    pure $ StgLibLinkerInfo
      { stglibName            = unitId
      , stglibCbitsPaths      = catMaybes cbitsPathList
      , stglibCapiStubsPaths  = catMaybes stubsPathList
      , stglibAllStubsPaths   = catMaybes allStubsPathList
      , stglibExtraLibs       = unitExtraLibs
      , stglibExtraLibDirs    = unitLibDirs
      , stglibLdOptions       = unitLdOptions
      }
  pure (appInfo, libInfoList)

data StgAppLicenseInfo
  = StgAppLicenseInfo
  { stgappUnitConfs :: [FilePath]
  }
  deriving (Eq, Ord, Show)

getAppLicenseInfo :: FilePath -> IO StgAppLicenseInfo
getAppLicenseInfo ghcStgAppFname = do
  GhcStgApp{..} <- Y.decodeFileThrow ghcStgAppFname
  confList <- forM appLibDeps $ \UnitLinkerInfo{..} -> do
    let possibleUnitConfs = nubOrd
          [ confPath </> (confName ++ ".conf")
          | confName <- unitId : [libName | 'H':'S':libName <- unitLibraries]
          , confPath <- appUnitDbPaths
          ]
    unitConfs <- forM possibleUnitConfs $ \p -> do
      doesFileExist p >>= \case
        True  -> pure $ Just p
        False -> pure Nothing
    -- NOTE: report errors, but never fail.
    case catMaybes unitConfs of
      c : _ -> pure $ Just c -- HINT: pick the first match
      [] -> do
        putStrLn $ "no .conf file was found for " ++ unitId
        pure Nothing
  pure $ StgAppLicenseInfo
    { stgappUnitConfs = catMaybes confList
    }

-- API for external-stg-compiler

data StgAppInfo
  = StgAppInfo
  { _appIncludePaths   :: [String]
  , _appLibPaths       :: [String]
  , _appLdOptions      :: [String]
  , _appCLikeObjFiles  :: [String]
  , _appNoHsMain       :: Bool
  }
  deriving (Eq, Ord, Show)

getAppInfo :: FilePath -> IO StgAppInfo
getAppInfo ghcStgAppFname = do
  pure undefined
