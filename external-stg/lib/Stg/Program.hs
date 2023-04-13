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
  , modUnitId               :: String
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
  , appGhcName        :: String
  , appGhcVersion     :: String
  , appPlatformOS     :: String
  , appUnitDbPaths    :: [FilePath]
  , appObjectDir      :: FilePath
  , appUnitId         :: String
  , appModules        :: [String]
  , appMainModuleName :: String
  , appMainModuleObj  :: FilePath
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
      <*> v .: "o-suffix"
      <*> v .: "no-hs-main"
      <*> v .: "ghc-name"
      <*> v .: "ghc-version"
      <*> v .: "platform-os"
      <*> v .:? "unit-db-paths" .!= []
      <*> v .: "object-dir"
      <*> v .: "app-unit-id"
      <*> v .: "app-modules"
      <*> v .: "app-main-module-name"
      <*> v .:? "app-main-module-object" .!= ""
      <*> v .:? "extra-ld-inputs" .!= []
      <*> v .:? "extra-libraries" .!= []
      <*> v .:? "library-dirs" .!= []
      <*> v .:? "ld-options" .!= []
      <*> v .:? "app-deps" .!= []
  parseJSON _ = fail "Expected Object for UnitLinkerInfo value"

wpcModpaksPath, wpcCbitsPath, wpcCapiStubsPath, wpcCbitsSourcePath :: String
wpcModpaksPath    = "extra-compilation-artifacts" </> "wpc-plugin" </> "modpaks"
wpcCbitsPath      = "extra-compilation-artifacts" </> "wpc-plugin" </> "cbits"
wpcCapiStubsPath  = "extra-compilation-artifacts" </> "wpc-plugin" </> "capi-stubs"
wpcWrapperStubsPath = "extra-compilation-artifacts" </> "wpc-plugin" </> "wrapper-stubs"
wpcCbitsSourcePath  = "extra-compilation-artifacts" </> "wpc-plugin" </> "cbits-source"

readGhcStgApp :: FilePath -> IO GhcStgApp
readGhcStgApp = Y.decodeFileThrow

getAppModuleMapping :: FilePath -> IO [StgModuleInfo]
getAppModuleMapping ghcStgAppFname = do
  let showLog = False -- TODO: use RIO ???
  let packageName = "exe:" ++ takeBaseName ghcStgAppFname -- TODO: save package to .ghc_stgapp
  GhcStgApp{..} <- readGhcStgApp ghcStgAppFname
  let modpakExt = "." ++ appObjSuffix ++ "_modpak"
      check f = do
        --putStrLn $ "check: " ++ f
        exist <- doesFileExist f
        unless exist $ when showLog $ do
          putStrLn $ "modpak does not exist: " ++ f
        pure exist

  libModules <- filterM (check . modModpakPath) $
        [ StgModuleInfo
          { modModuleName     = mod
          , modModpakPath     = dir </> wpcModpaksPath </> moduleToModpak modpakExt mod
          , modPackageName    = unitName
          , modPackageVersion = unitVersion
          , modUnitId         = unitId
          }
        | UnitLinkerInfo{..} <- appLibDeps
        , dir <- unitImportDirs
        , mod <- unitExposedModules ++ unitHiddenModules

        -- TODO: make this better somehow
        -- HINT: this module does not exist, it's a GHC builtin for primops
        , let builtin = mod == "GHC.Prim" && unitName == "ghc-prim"
        , not builtin

        ]

  appMods <- filterM (check . modModpakPath) $
      [ StgModuleInfo
        { modModuleName     = mod
        , modModpakPath     = appObjectDir </> wpcModpaksPath </> modpakRelPath
        , modPackageName    = appUnitId
        , modPackageVersion = ""
        , modUnitId         = appUnitId
        }
      | mod <- appModules
      , let modpakRelPath = if mod == appMainModuleName
                              then appMainModuleObj -<.> modpakExt
                              else moduleToModpak modpakExt mod
      ]
  pure $ appMods ++ libModules

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

findIfExists :: RecursionPredicate -> FilterPredicate -> FilePath -> IO [FilePath]
findIfExists rp fp path = do
  exists <- doesDirectoryExist path
  if exists
    then find rp fp path
    else pure []

getAppLinkerInfo :: FilePath -> IO (StgAppLinkerInfo, [StgLibLinkerInfo])
getAppLinkerInfo ghcStgAppFname = do
  GhcStgApp{..} <- readGhcStgApp ghcStgAppFname

  -- app info
  -- TODO: use .dyn_o
  appCbits <- findIfExists always (extension ==? ".o") $ appObjectDir </> wpcCbitsPath
  appCapiStubs <- findIfExists always (extension ==? ".o") $ appObjectDir </> wpcCapiStubsPath
  let appInfo = StgAppLinkerInfo
        { stgappCObjects      = appCbits ++ appCapiStubs
        , stgappExtraLibs     = appExtraLibs
        , stgappExtraLibDirs  = appExtraLibDirs
        , stgappLdOptions     = appLdOptions
        , stgappPlatformOS    = appPlatformOS
        , stgappNoHsMain      = appNoHsMain
        }

  -- lib info
  let forceDynamic = True
      {-
      arExt n = if forceDynamic
        then "-" ++ appGhcName ++ appGhcVersion ++ ".dyn_o" ++ n ++ ".a"
        else "." ++ appObjSuffix ++ n ++ ".a"
      -}
  libInfoList <- forM appLibDeps $ \UnitLinkerInfo{..} -> do

    cbitsPathList <- forM unitLibDirs $ \path -> do
      findIfExists always (extension ==? ".dyn_o") $ path </> wpcCbitsPath

    capiStubsPathList <- forM unitLibDirs $ \path -> do
      findIfExists always (extension ==? ".dyn_o") $ path </> wpcCapiStubsPath

    pure $ StgLibLinkerInfo
      { stglibName            = unitName
      , stglibCbitsPaths      = concat cbitsPathList
      , stglibCapiStubsPaths  = concat capiStubsPathList
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
  GhcStgApp{..} <- readGhcStgApp ghcStgAppFname
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

-- observation of foreign cbits source files

data StgAppForeignSourceInfo
  = StgAppForeignSourceInfo
  { stgForeignSourceAbsPath :: FilePath
  , stgForeignSourceRelPath :: FilePath
  , stgForeignUnitId        :: String
  }
  deriving (Eq, Ord, Show)

getAppForeignFiles :: FilePath -> IO [StgAppForeignSourceInfo]
getAppForeignFiles ghcStgAppFname = do
  GhcStgApp{..} <- readGhcStgApp ghcStgAppFname
  let appSrcDir = appObjectDir </> wpcCbitsSourcePath
  appCbitsSources <- findIfExists always (fileType ==? RegularFile) appSrcDir
  let appCbitsInfos =
        [ StgAppForeignSourceInfo
          { stgForeignSourceAbsPath = p
          , stgForeignSourceRelPath = makeRelative appSrcDir p
          , stgForeignUnitId        = appUnitId
          }
        | p <- appCbitsSources
        ]
  libsCbitsInfos <- forM appLibDeps $ \UnitLinkerInfo{..} -> do
    forM unitLibDirs $ \path -> do
      let libSrcDir = path </> wpcCbitsSourcePath
      libCbitsSources <- findIfExists always (fileType ==? RegularFile) libSrcDir
      pure
        [ StgAppForeignSourceInfo
          { stgForeignSourceAbsPath = p
          , stgForeignSourceRelPath = makeRelative libSrcDir p
          , stgForeignUnitId        = unitId
          }
        | p <- libCbitsSources
        ]

  pure $ appCbitsInfos ++ (concat $ concat libsCbitsInfos)
