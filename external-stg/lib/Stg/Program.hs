module Stg.Program where

import           Codec.Archive.Zip         (getEntry, mkEntrySelector, withArchive)

import           Control.Applicative       (Applicative (..), (<$>))
import           Control.Monad             (Functor (..), Monad (..), MonadFail (..), filterM, forM, forM_, unless,
                                            when)
import           Control.Monad.IO.Class    (MonadIO (..))

import qualified Data.Aeson                as Aeson
import           Data.Bool                 (Bool (..), not, otherwise, (&&))
import qualified Data.ByteString.Char8     as BS8
import qualified Data.ByteString.Lazy      as BSL
import           Data.Containers.ListUtils (nubOrd)
import           Data.Either               (Either (..))
import           Data.Eq                   (Eq ((==)))
import           Data.Function             (($), (.))
import           Data.List                 (concat, drop, dropWhile, foldl, foldl', isPrefixOf, length, takeWhile,
                                            unlines, unzip, zip, (++))
import qualified Data.Map                  as Map
import           Data.Maybe                (Maybe (..), catMaybes, listToMaybe, maybeToList)
import           Data.Monoid               (Monoid (..))
import           Data.Ord                  (Ord)
import qualified Data.Set                  as Set
import           Data.String               (IsString (..), String)
import           Data.Text                 (Text, replace, unpack)
import           Data.Tuple                (fst)
import           Data.Yaml                 (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Yaml                 as Y

import           GHC.Err                   (error, undefined)

import qualified Stg.GHC.Symbols           as GHCSymbols
import           Stg.IO                    (decodeStgbin, decodeStgbinInfo, fullpakAppInfoPath, modpakStgbinPath,
                                            readModpakL)
import           Stg.JSON                  ()
import           Stg.Reconstruct           (reconModule)
import           Stg.Syntax                (Module, ModuleName (..), UnitId (..))

import           System.Directory          (createDirectoryIfMissing, doesDirectoryExist, doesFileExist,
                                            getHomeDirectory)
import           System.FilePath           (FilePath, makeRelative, splitFileName, (-<.>), (<.>), (</>))
import           System.FilePath.Find      (FileType (..), FilterPredicate, RecursionPredicate, always, extension,
                                            fileType, find, (==?))
import           System.IO                 (IO, putStrLn)
import           System.Process            (callCommand)

import           Text.Printf               (printf)
import           Text.Read                 (read)
import           Text.Show                 (Show (..))

moduleToModpak :: String -> String -> FilePath
moduleToModpak modpakExt moduleName = replaceEq '.' '/' moduleName ++ modpakExt
  where
    replaceEq :: Eq a => a -> a -> [a] -> [a]
    replaceEq from to = fmap (\cur -> if cur == from then to else cur)

parseSection :: [String] -> String -> [String]
parseSection content n = fmap (read . drop 1) . takeWhile (isPrefixOf "-") . drop 1 . dropWhile (not . isPrefixOf n) $ content

printSection :: Show a => [a] -> String
printSection l = unlines ["- " ++ x | x <- nubOrd $ fmap show l]

data StgModuleInfo
  = StgModuleInfo
  { modModuleName     :: String
  , modModpakPath     :: FilePath
  , modPackageName    :: String
  , modPackageVersion :: String
  , modUnitId         :: String
  }
  deriving stock (Eq, Ord, Show)

data UnitLinkerInfo =
  UnitLinkerInfo
  { unitName           :: String
  , unitVersion        :: String
  , unitId             :: String
  , unitImportDirs     :: [FilePath]
  , unitLibraries      :: [String]
  , unitLibDirs        :: [FilePath]
  , unitExtraLibs      :: [String]
  , unitLdOptions      :: [String]
  , unitExposedModules :: [String]
  , unitHiddenModules  :: [String]
  , unitArtifactsDir   :: Maybe FilePath
  } deriving stock (Eq, Show)

instance FromJSON UnitLinkerInfo where
  parseJSON :: Y.Value -> Y.Parser UnitLinkerInfo
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
      <*> pure Nothing
  parseJSON _ = fail "Expected Object for UnitLinkerInfo value"

data GhcStgApp
  = GhcStgApp
  { appWays           :: [String]
  , appObjSuffix      :: String
  , appNoHsMain       :: Bool
  , appGhcName        :: String
  , appGhcVersion     :: String
  , appTargetPlatform :: String
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
  } deriving stock (Eq, Show)

instance FromJSON GhcStgApp where
  parseJSON :: Y.Value -> Y.Parser GhcStgApp
  parseJSON (Y.Object v) =
    GhcStgApp
      <$> v .:? "ways" .!= []
      <*> v .: "o-suffix"
      <*> v .: "no-hs-main"
      <*> v .: "ghc-name"
      <*> v .: "ghc-version"
      <*> v .: "target-platform"
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
-- wpcWrapperStubsPath = "extra-compilation-artifacts" </> "wpc-plugin" </> "wrapper-stubs"
wpcCbitsSourcePath  = "extra-compilation-artifacts" </> "wpc-plugin" </> "cbits-source"

getAppModuleMapping :: FilePath -> IO [StgModuleInfo]
getAppModuleMapping ghcStgAppFname = do
  let showLog = False -- TODO: use RIO ???
  -- let packageName = "exe:" ++ takeBaseName ghcStgAppFname -- TODO: save package to .ghc_stgapp
  GhcStgApp{..} <- readGhcStgApp ghcStgAppFname
  let modpakExt = "." ++ appObjSuffix ++ "_modpak"
      check f = do
        --putStrLn $ "check: " ++ f
        exist <- doesFileExist f
        unless exist $ when showLog $ putStrLn $ "modpak does not exist: " ++ f
        pure exist

  libModules <- filterM (check . modModpakPath) $
        [ StgModuleInfo
          { modModuleName     = mod
          , modModpakPath     = newDir </> wpcModpaksPath </> moduleToModpak modpakExt mod
          , modPackageName    = unitName
          , modPackageVersion = unitVersion
          , modUnitId         = unitId
          }
        | UnitLinkerInfo{..} <- appLibDeps
        , Just dir <- [unitArtifactsDir]
        , mod <- unitExposedModules ++ unitHiddenModules

        -- TODO: make this better somehow
        -- HINT: this module does not exist, it's a GHC builtin for primops
        , let builtin = mod == "GHC.Prim" && unitName == "ghc-prim"
              newDir = unpack $ replace "bindist" "foundation-pak" $ fromString @Text dir
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
getAppModpaks ghcStgAppFname = fmap modModpakPath <$> getAppModuleMapping ghcStgAppFname

collectProgramModules' :: Bool -> [FilePath] -> String -> String -> [(String, String, String)] -> IO [FilePath]
collectProgramModules' showLog modpakFileNames unitId mod liveSymbols = do
  -- filter dependenies only
  (fexportedList, depList) <- fmap unzip . forM modpakFileNames $ \fname -> do
    (_, unitId', module', _, _, hasForeignExport, deps) <- readModpakL fname modpakStgbinPath decodeStgbinInfo
    let fexport = if hasForeignExport then Just (unitId', module') else Nothing
    pure
      ( fexport,
      ( (unitId', module'),
        [(depUnitId, depModule)
        | (depUnitId, depModuleNames) <- deps
        , depModule <- depModuleNames
        ]
      )
      )
  let fnameMap  = Map.fromList $ zip (fmap fst depList) modpakFileNames
      mnameMap  = Map.fromList $ zip modpakFileNames (fmap fst depList)
      depMap    = Map.fromList depList
      calcDep s n
        | Set.member n s = s
        | Just l <- Map.lookup n depMap = foldl' calcDep (Set.insert n s) l
        | otherwise = Set.insert n s -- error $ printf "missing module: %s" . show $ snd n

      keyMain = (UnitId $ BS8.pack unitId, ModuleName $ BS8.pack mod)
      rtsDeps = [(UnitId $ BS8.pack u, ModuleName $ BS8.pack m) | (u, m, _) <- liveSymbols]

      prunedDeps = catMaybes [Map.lookup m fnameMap | m <- Set.toList $ foldl calcDep mempty $ keyMain : rtsDeps ++ catMaybes fexportedList]

  when showLog $ do
    putStrLn $ "all modules: " ++ show (length modpakFileNames)
    putStrLn $ "app modules: " ++ show (length prunedDeps)
    putStrLn "app dependencies:\n"
    forM_ [mnameMap Map.! fname | fname <- prunedDeps] $ \(UnitId uid, ModuleName mod') -> printf "%-60s %s\n" (BS8.unpack uid) (BS8.unpack mod')
  pure prunedDeps

collectProgramModules :: [FilePath] -> String -> String -> [(String, String, String)] -> IO [FilePath]
collectProgramModules = collectProgramModules' True

-- .fullpak
getFullpakModules :: FilePath -> IO [Module]
getFullpakModules fullpakPath = withArchive fullpakPath $ do
  appinfoSelector <- liftIO $ mkEntrySelector fullpakAppInfoPath
  AppInfo{..} <- getEntry appinfoSelector >>= Y.decodeThrow
  forM aiLiveCode $ \CodeInfo{..} -> do
    s <- liftIO $ mkEntrySelector $ "haskell" </> ciPackageName </> ciModuleName </> modpakStgbinPath
    decodeStgbin . BSL.fromStrict <$> getEntry s

-- .ghc_stgapp
getGhcStgAppModules :: FilePath -> IO [Module]
getGhcStgAppModules ghcstgappPath = do
  modinfoList <- getAppModuleMapping ghcstgappPath
  appModpaks <- collectProgramModules' False (fmap modModpakPath modinfoList) "main" "Main" GHCSymbols.liveSymbols
  forM appModpaks $ \modpakName -> readModpakL modpakName modpakStgbinPath decodeStgbin

-- .json
getJSONModules :: FilePath -> IO [Module]
getJSONModules path = do
  res <- Aeson.eitherDecodeFileStrict' path
  case res of
    Left  err     -> error err
    Right smodule -> pure [reconModule smodule]

data StgLibLinkerInfo
  = StgLibLinkerInfo
  { stglibName           :: String
  , stglibCbitsPaths     :: [FilePath]
  , stglibCapiStubsPaths :: [FilePath]
  , stglibExtraLibs      :: [String]
  , stglibExtraLibDirs   :: [FilePath]
  , stglibLdOptions      :: [String]
  }
  deriving stock (Eq, Ord, Show)

data StgAppLinkerInfo
  = StgAppLinkerInfo
  { stgappCObjects     :: [FilePath]
  , stgappExtraLibs    :: [String]
  , stgappExtraLibDirs :: [FilePath]
  , stgappLdOptions    :: [String]
  , stgappPlatformOS   :: String
  , stgappNoHsMain     :: Bool
  }
  deriving stock (Eq, Ord, Show)

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
  -- let forceDynamic = True
      {-
      arExt n = if forceDynamic
        then "-" ++ appGhcName ++ appGhcVersion ++ ".dyn_o" ++ n ++ ".a"
        else "." ++ appObjSuffix ++ n ++ ".a"
      -}
  libInfoList <- forM appLibDeps $ \UnitLinkerInfo{..} -> do

    cbitsPathList <- forM (maybeToList unitArtifactsDir) $ \path -> findIfExists always (extension ==? ".dyn_o") $ path </> wpcCbitsPath

    capiStubsPathList <- forM (maybeToList unitArtifactsDir) $ \path -> findIfExists always (extension ==? ".dyn_o") $ path </> wpcCapiStubsPath

    pure $ StgLibLinkerInfo
      { stglibName            = unitName
      , stglibCbitsPaths      = concat cbitsPathList
      , stglibCapiStubsPaths  = concat capiStubsPathList
      , stglibExtraLibs       = unitExtraLibs
      , stglibExtraLibDirs    = unitLibDirs
      , stglibLdOptions       = unitLdOptions
      }
  pure (appInfo, libInfoList)

newtype StgAppLicenseInfo
  = StgAppLicenseInfo
  { stgappUnitConfs :: [FilePath]
  }
  deriving stock (Eq, Ord, Show)

getAppLicenseInfo :: FilePath -> IO StgAppLicenseInfo
getAppLicenseInfo ghcStgAppFname = do
  GhcStgApp{..} <- readGhcStgApp ghcStgAppFname
  confList <- forM appLibDeps $ \UnitLinkerInfo{..} -> do
    let possibleUnitConfs = nubOrd
          [ confPath </> (confName ++ ".conf")
          | confName <- unitId : [libName | 'H':'S':libName <- unitLibraries]
          , confPath <- appUnitDbPaths
          ]
    unitConfs <- forM possibleUnitConfs $ \p -> doesFileExist p >>= \case
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
  { _appIncludePaths  :: [String]
  , _appLibPaths      :: [String]
  , _appLdOptions     :: [String]
  , _appCLikeObjFiles :: [String]
  , _appNoHsMain      :: Bool
  }
  deriving stock (Eq, Ord, Show)

getAppInfo :: FilePath -> IO StgAppInfo
getAppInfo _ghcStgAppFname = pure undefined

-- observation of foreign cbits source files

data StgAppForeignSourceInfo
  = StgAppForeignSourceInfo
  { stgForeignSourceAbsPath :: FilePath
  , stgForeignSourceRelPath :: FilePath
  , stgForeignUnitId        :: String
  }
  deriving stock (Eq, Ord, Show)

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
  libsCbitsInfos <- forM appLibDeps $ \UnitLinkerInfo{..} -> forM (maybeToList unitArtifactsDir) $ \path -> do
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

  pure $ appCbitsInfos ++ concat (concat libsCbitsInfos)

readGhcStgApp :: FilePath -> IO GhcStgApp
readGhcStgApp fname = do
  ghcStgApp <- Y.decodeFileThrow fname
  PakYaml{..} <- getFoundationPakForGhcStgApp fname
  let foundationUnitMap = Map.fromList
        [ (pakUnitId, pakUnitDir)
        | PakUnitInfo{..} <- pakyamlPackages
        ]

      wiredInUnitIds = Set.fromList
        [ "base"
        , "ghc"
        , "ghc-bignum"
        , "ghc-prim"
        , "rts"
        , "template-haskell"
        ]

      -- HINT: create versioned unit-id
      calculateUnitId UnitLinkerInfo{..}
        -- handle wired-in units
        | Set.member unitId wiredInUnitIds
        = unitName ++ "-" ++ unitVersion

        | otherwise
        = unitId

  pure $ ghcStgApp
    { appLibDeps =
        [ uli
          { unitArtifactsDir = case Map.lookup (calculateUnitId uli) foundationUnitMap of
            Just dir -> Just dir
            _        -> listToMaybe unitImportDirs
          }
        | uli@UnitLinkerInfo{..} <- appLibDeps ghcStgApp
        ]
    }

-- foundation-pak handling

foundationPakCachePath :: IO FilePath
foundationPakCachePath = do
  home <- getHomeDirectory
  pure $ home </> ".estg/foundation-pak"


getFoundationPakForGhcStgApp :: FilePath -> IO PakYaml
getFoundationPakForGhcStgApp ghcstgapp = do
  GhcStgApp{..} <- Y.decodeFileThrow ghcstgapp
  root <- foundationPakCachePath
  -- foundation-pak name schema: `ghc-name`-`ghc-version`-`target-platform`.pak.zip
  let foundationPakName = printf "%s-%s-%s" appGhcName appGhcVersion appTargetPlatform
      foundationPakPath = root </> foundationPakName
      ghcVersionString  = printf "%s-%s" appGhcName appGhcVersion

  {-
      check foundationPakPath locally
      if not present, create URL and download it to foundationPakPath
      load the content from foundationPakPath
  -}
  downloadFoundationPakIfMissing ghcVersionString foundationPakPath

  let prefixPakUnitDirs :: FilePath -> PakYaml -> PakYaml
      prefixPakUnitDirs dir p@PakYaml{..} = p
        { pakyamlPackages =
            [ u {pakUnitDir = dir </> pakUnitDir}
            | u@PakUnitInfo{..} <- pakyamlPackages
            ]
        }

  pakYaml <- Y.decodeFileThrow (foundationPakPath </> "pak.yaml")
  pure $ prefixPakUnitDirs foundationPakPath pakYaml

data PakUnitInfo
  = PakUnitInfo
  { pakUnitName    :: String
  , pakUnitVersion :: String
  , pakUnitId      :: String
  , pakUnitDir     :: FilePath
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON PakUnitInfo where
  parseJSON :: Y.Value -> Y.Parser PakUnitInfo
  parseJSON (Y.Object v) =
    PakUnitInfo
      <$> v .: "name"
      <*> v .: "version"
      <*> v .: "id"
      <*> v .: "dir"
  parseJSON _ = fail "Expected Object for PakUnitInfo value"

data PakYaml
  = PakYaml
  { pakyamlPathPrefix :: FilePath
  , pakyamlPackages   :: [PakUnitInfo]
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON PakYaml where
  parseJSON :: Y.Value -> Y.Parser PakYaml
  parseJSON (Y.Object v) =
    PakYaml
      <$> v .: "path-prefix"
      <*> v .: "packages"
  parseJSON _ = fail "Expected Object for PakYaml value"

-- example URL: https://github.com/grin-compiler/foundation-pak/releases/download/ghc-9.2.7/ghc-9.2.7-x86_64-unknown-linux.pak.tar.xz
foundationPakURL :: String
foundationPakURL = "https://github.com/grin-compiler/foundation-pak/releases/download/"

downloadFoundationPakIfMissing :: String -> FilePath -> IO ()
downloadFoundationPakIfMissing ghcVersionString foundationPakPath = do
  exists <- doesDirectoryExist foundationPakPath
  unless exists $ do
    let (pakDir, pakName) = splitFileName foundationPakPath
        pakFileName       = pakName <.> ".pak.tar.zst"
        pakURL            = foundationPakURL </> ghcVersionString </> pakFileName
    printf "downloading %s into %s\n" pakURL pakDir
    createDirectoryIfMissing True pakDir
    callCommand $
      printf "(cd %s ; curl -L %s -o %s)"                        pakDir pakURL pakFileName
    callCommand $
      printf "(cd %s ; tar --use-compress-program=unzstd xf %s)" pakDir        pakFileName
    callCommand $
      printf "(cd %s ; rm %s)"                                   pakDir        pakFileName

-- fullpak related

data CodeInfo
  = CodeInfo
  { ciPackageName :: String
  , ciUnitId      :: String
  , ciModuleName  :: String
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON CodeInfo where
  parseJSON :: Y.Value -> Y.Parser CodeInfo
  parseJSON (Y.Object v) =
    CodeInfo
      <$> v .: "package-name"
      <*> v .: "unit-id"
      <*> v .: "module-name"
  parseJSON _ = fail "Expected Object for CodeInfo value"

instance ToJSON CodeInfo where
  toJSON :: CodeInfo -> Y.Value
  toJSON CodeInfo{..} =
    object
      [ "package-name"  .= ciPackageName
      , "unit-id"       .= ciUnitId
      , "module-name"   .= ciModuleName
      ]

newtype AppInfo
  = AppInfo
  { aiLiveCode :: [CodeInfo]
  }
  deriving stock (Eq, Ord, Show)

instance FromJSON AppInfo where
  parseJSON :: Y.Value -> Y.Parser AppInfo
  parseJSON (Y.Object v) =
    AppInfo
      <$> v .: "live-code"
  parseJSON _ = fail "Expected Object for AppInfo value"

instance ToJSON AppInfo where
  toJSON :: AppInfo -> Y.Value
  toJSON AppInfo{..} =
    object
      [ "live-code"  .= aiLiveCode
      ]
