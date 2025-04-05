module WPC.GhcStgApp where

import           Data.Containers.ListUtils (nubOrd)
import           Data.List                 (isPrefixOf)
import qualified Data.Set                  as Set
import           Data.Version

import           GHC.Data.Maybe
import qualified GHC.Data.ShortText        as ST
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Linker.Static.Utils
import           GHC.Linker.Types
import           GHC.Platform
import           GHC.Platform.ArchOS
import           GHC.Prelude
import           GHC.Unit.Env
import           GHC.Unit.Home.ModInfo
import           GHC.Unit.Info
import           GHC.Unit.Module.Deps
import           GHC.Unit.Module.ModIface
import           GHC.Unit.State
import           GHC.Unit.Types
import           GHC.Utils.Json
import           GHC.Utils.Outputable

import           System.Directory
import           System.FilePath

import           WPC.Yaml

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

  let arrOfStr      = JSArray . fmap JSString . nubOrd
      appLdOptions  = [ o
                      | Option o <- ldInputs dflags
                      , not $ isPrefixOf "-l" o
                      ]
      odir          = fromMaybe "." (objectDir dflags)
      mainModName   = mainModuleNameIs dflags
      mainModObjs   = [ makeRelative odir o
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
