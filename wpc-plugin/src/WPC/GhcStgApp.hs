module WPC.GhcStgApp where

import           Control.Applicative       (Applicative (..))

import           Data.Bool                 (Bool (..), not, otherwise)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Eq                   (Eq (..))
import           Data.Function             (($), (.))
import           Data.Functor              (Functor (..))
import           Data.List                 (isPrefixOf, (++))
import qualified Data.Set                  as Set
import           Data.String               (String)
import           Data.Version              (showVersion)

import           GHC.Data.Maybe            (Maybe (..), fromMaybe, maybe, maybeToList)
import qualified GHC.Data.ShortText        as ST
import           GHC.Driver.Ppr            (showSDoc)
import           GHC.Driver.Session        (DynFlags (..), GeneralFlag (..), GhcNameVersion (..), Option (..),
                                            PlatformMisc (..), gopt, objectSuf, ways)
import           GHC.Linker.Static.Utils   (exeFileName)
import           GHC.Linker.Types          (linkableObjs)
import           GHC.Platform              (Platform (..), platformOS)
import           GHC.Platform.ArchOS       (stringEncodeOS)
import           GHC.Unit.Env              (UnitEnv (..), preloadUnitsInfo', ue_unit_dbs)
import           GHC.Unit.Home.ModInfo     (HomeModInfo (..), HomeModLinkable (..), HomePackageTable, eltsHpt)
import           GHC.Unit.Info             (GenericUnitInfo (..))
import           GHC.Unit.Module.Deps      (Dependencies (..))
import           GHC.Unit.Module.ModIface  (ModIface_ (..))
import           GHC.Unit.State            (UnitDatabase (..), mayThrowUnitErr)
import           GHC.Unit.Types            (GenModule (..), wiredInUnitIds)
import           GHC.Utils.Json            (JsonDoc (..))
import           GHC.Utils.Outputable      (Outputable (..))

import           System.Directory          (getCurrentDirectory)
import           System.FilePath           (FilePath, isAbsolute, makeRelative, (</>))
import           System.IO                 (IO, putStrLn, writeFile)

import           Text.Show                 (Show (..))

import           WPC.Yaml                  (renderYAML)

{-
TODO:
  list app modules
  list app cbits
-}

writeGhcStgApp :: DynFlags -> UnitEnv -> HomePackageTable -> IO ()
writeGhcStgApp dflags unit_env hpt = do
  let home_mod_infos = eltsHpt hpt

      -- the packages we depend on
      dep_units = Set.toList
                    $ Set.unions
                    $ fmap (dep_direct_pkgs . mi_deps . hm_iface)
                    $ home_mod_infos

      platform    = targetPlatform dflags
      arch_os     = platformArchOS platform
      staticLink  = False
      output_fn   = exeFileName arch_os staticLink (outputFile_ dflags)

  root <- getCurrentDirectory
  dep_unit_infos <- mayThrowUnitErr (preloadUnitsInfo' unit_env dep_units)
  let pp :: Outputable a => a -> String
      pp = showSDoc dflags . ppr

      toAbsPath :: FilePath -> FilePath
      toAbsPath p
        | isAbsolute p  = p
        | otherwise     = root </> p

      arrOfAbsPathST = arrOfAbsPath . fmap ST.unpack
      arrOfAbsPath = JSArray . fmap JSString . nubOrd . fmap toAbsPath

      app_deps = JSArray
        [ JSObject
          [ ("name",              JSString $ pp unitPackageName)
          , ("version",           JSString (showVersion unitPackageVersion))
          , ("id",                JSString $ pp unitId)
          , ("unit-import-dirs",  arrOfAbsPathST unitImportDirs)
          , ("unit-libraries",    JSArray $ fmap (JSString . ST.unpack) unitLibraries)
          , ("library-dirs",      arrOfAbsPathST unitLibraryDirs)
          , ("extra-libraries",   JSArray $ fmap (JSString . ST.unpack) unitExtDepLibsSys)
          , ("framework-dirs",    arrOfAbsPathST unitExtDepFrameworkDirs)
          , ("extra-frameworks",  JSArray $ fmap (JSString . ST.unpack) unitExtDepFrameworks)
          , ("ld-options",        JSArray $ fmap (JSString . ST.unpack) unitLinkerOptions)
          , ("exposed-modules",   JSArray $ fmap (JSString . pp) [mod' | (mod', Nothing) <- unitExposedModules])
          , ("hidden-modules",    JSArray $ fmap (JSString . pp) unitHiddenModules)
          , ("depends",           JSArray $ fmap (JSString . pp) unitDepends)
          ]
        | GenericUnitInfo{..} <- dep_unit_infos
        ]

  let arrOfStr     = JSArray . fmap JSString . nubOrd
      appLdOptions = [ o
                     | Option o <- ldInputs dflags
                     , not $ isPrefixOf "-l" o
                     ]
      odir        = fromMaybe "." $ objectDir dflags
      mainModName = mainModuleNameIs dflags
      mainModObjs = [ makeRelative odir o
                    | HomeModInfo{..} <- home_mod_infos
                    , mainModName == moduleName (mi_module hm_iface)
                    , l <- maybeToList $ homeMod_object hm_linkable
                    , o <- linkableObjs l
                    ]
  mainModObj <- case mainModObjs of
    [o] -> pure $ JSString o
    l   -> do
            putStrLn $ "expected a single object file for the main module, got: " ++ show l
            pure JSNull
  writeFile (output_fn ++ "." ++ objectSuf dflags ++ "_ghc_stgapp") $ showSDoc dflags $ renderYAML $ JSObject
    [ ("ghc-name",          JSString . ghcNameVersion_programName $ ue_namever unit_env)
    , ("ghc-version",       JSString . ghcNameVersion_projectVersion $ ue_namever unit_env)
    , ("target-platform",   JSString . platformMisc_targetPlatformString $ platformMisc dflags)
    , ("platform-os",       JSString . stringEncodeOS . platformOS $ targetPlatform dflags)
    , ("no-hs-main",        JSBool $ gopt Opt_NoHsMain dflags)
    , ("o-suffix",          JSString $ objectSuf dflags)
    , ("ways",              arrOfStr $ fmap show . Set.toList $ ways dflags)
    , ("object-dir",        JSString . toAbsPath $ fromMaybe root (objectDir dflags))
    , ("app-unit-id",       JSString . pp $ ue_current_unit unit_env)
    , ("app-modules",       JSArray $ fmap (JSString . pp) [moduleName . mi_module $ hm_iface | HomeModInfo{..} <- home_mod_infos])
    , ("app-main-module-name",    JSString $ pp mainModName)
    , ("app-main-module-object",  mainModObj)
    , ("extra-ld-inputs",   arrOfAbsPath [f | FileOption _ f <- ldInputs dflags])
    , ("library-dirs",      arrOfAbsPath $ libraryPaths dflags)
    , ("extra-libraries",   arrOfStr [lib | Option ('-':'l':lib) <- ldInputs dflags])
    , ("framework-dirs",    arrOfAbsPath $ frameworkPaths dflags)
    , ("extra-frameworks",  JSArray $ fmap JSString $ cmdlineFrameworks dflags)
    , ("ld-options",        arrOfStr appLdOptions)
    , ("unit-db-paths",     arrOfAbsPath $ maybe [] (fmap unitDatabasePath) $ ue_unit_dbs unit_env)
    , ("wired-in-unit-ids", JSArray $ fmap (JSString . pp) wiredInUnitIds)
    , ("app-deps",          app_deps)
    ]

{-
eltsUDFM :: UniqDFM key elt -> [elt]
eltsHpt :: HomePackageTable -> [HomeModInfo]
type HomePackageTable = DModuleNameEnv HomeModInfo

data HomeModInfo = HomeModInfo
   { hm_iface    :: !ModIface
   , hm_details  :: ModDetails
   , hm_linkable :: !HomeModLinkable
   }

type ModIface = ModIface_ 'ModIfaceFinal
        mi_module     :: !Module,             -- ^ Name of the module we are for

data GenModule unit = Module
   { moduleUnit :: !unit       -- ^ Unit the module belongs to
   , moduleName :: !ModuleName -- ^ Module name (e.g. A.B.C)
   }
   deriving (Eq,Ord,Data,Functor)

-- | A Module is a pair of a 'Unit' and a 'ModuleName'.
type Module = GenModule Unit

moduleUnitId :: Module -> UnitId
moduleUnitId = toUnitId . moduleUnit
-}
