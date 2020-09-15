module Stg.Program where

import Control.Monad.IO.Class
import Control.Monad
import Text.Printf

import Data.Maybe
import Data.List (isPrefixOf, groupBy, sortBy, foldl')

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL

import System.Directory
import System.FilePath
import System.FilePath.Find
import Codec.Archive.Zip

import Stg.Syntax
import Stg.IO

moduleToModpak :: String -> String -> FilePath
moduleToModpak modpakExt moduleName = replaceEq '.' '/' moduleName ++ modpakExt
  where
    replaceEq :: Eq a => a -> a -> [a] -> [a]
    replaceEq from to = map (\cur -> if cur == from then to else cur)

parseSection :: [String] -> String -> [String]
parseSection content n = map (read . tail) . takeWhile (isPrefixOf "-") . tail . dropWhile (not . isPrefixOf n) $ content

data StgAppInfo
  = StgAppInfo
  { appIncludePaths   :: [String]
  , appLibPaths       :: [String]
  , appLdOptions      :: [String]
  , appCLikeObjFiles  :: [String]
  }
  deriving (Eq, Ord, Show)

getAppInfo :: FilePath -> IO StgAppInfo
getAppInfo ghcStgAppFname = do
  content <- lines <$> readFile ghcStgAppFname
  let libPaths  = parseSection content "pkg_lib_paths:"
      incPaths  = parseSection content "pkg_include_paths:"
      [root]    = parseSection content "root:"
      cObjFiles = parseSection content "extra_ld_inputs:"
      ldFlags   = parseSection content "other_flags:"
      extraLibs = parseSection content "extra_libs:"
      pkgLibs   = parseSection content "package_hs_libs:"
      cLibs     = filter (isPrefixOf "-lC") pkgLibs
  archives <- Set.fromList . map takeFileName . concat <$> mapM (find always (extension ==? ".a")) libPaths
  let cbits   = [o ++ ".cbits" | o <- pkgLibs, Set.member ("lib" ++ drop 2 o ++ ".cbits.a") archives]
      stubs   = [o ++ ".stubs" | o <- pkgLibs, Set.member ("lib" ++ drop 2 o ++ ".stubs.a") archives]
      ldOpts  = concat [stubs, cbits, cLibs, ldFlags, extraLibs]
  pure StgAppInfo
    { appIncludePaths   = incPaths
    , appLibPaths       = libPaths
    , appLdOptions      = ldOpts
    , appCLikeObjFiles  = [root </> o | o <- cObjFiles]
    }

data StgModuleInfo
  = StgModuleInfo
  { modModuleName   :: String
  , modModpakPath   :: FilePath
  , modPackageName  :: String
  }
  deriving (Eq, Ord, Show)

getLibModuleMapping :: FilePath -> IO [StgModuleInfo]
getLibModuleMapping stglibPath = do
  let packageName = takeFileName $ dropTrailingPathSeparator stglibPath
  fname <- head <$> find always (extension ==? ".stglib") stglibPath
  content <- lines <$> readFile fname
  let modules   = parseSection content "modules:"
      modpakExt = takeExtension . head $ parseSection content "modpaks:"
  pure
    [ StgModuleInfo
        { modModuleName   = m
        , modModpakPath   = stglibPath </> moduleToModpak modpakExt m
        , modPackageName  = packageName
        }
    | m <- modules
    ]

getAppModuleMapping :: FilePath -> IO [StgModuleInfo]
getAppModuleMapping ghcStgAppFname = do
  let packageName = "exe:" ++ takeBaseName ghcStgAppFname -- TODO: save package to .stgapp and .stglib
  content <- lines <$> readFile ghcStgAppFname
  let paths     = parseSection content "pkg_lib_paths:"
      [root]    = parseSection content "root:"
      o_files   = parseSection content "o_files:"
  libModpaks <- mapM getLibModuleMapping paths

  -- load app module names from stgbins
  appModules <- forM o_files $ \o -> do
    let modpak = root </> o ++ "_modpak"
    (_phase, _unitId, moduleName) <- readModpakL modpak modpakStgbinPath decodeStgbinModuleName
    pure StgModuleInfo
          { modModuleName   = BS8.unpack $ getModuleName moduleName
          , modModpakPath   = modpak
          , modPackageName  = packageName
          }

  pure $ appModules ++ concat libModpaks

getAppModpaks :: FilePath -> IO [FilePath]
getAppModpaks ghcStgAppFname = map modModpakPath <$> getAppModuleMapping ghcStgAppFname

collectProgramModules :: [FilePath] -> String -> String -> [(String, String, String)] -> IO [FilePath]
collectProgramModules modpakFileNames unitId mod liveSymbols = do
  -- filter dependenies only
  (fexportedList, depList) <- fmap unzip . forM modpakFileNames $ \fname -> do
    (_, u, m, _, hasForeignExport, deps) <- readModpakL fname modpakStgbinPath decodeStgbinInfo
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

  putStrLn $ "all modules: " ++ show (length modpakFileNames)
  putStrLn $ "app modules: " ++ show (length prunedDeps)
  putStrLn $ "app dependencies:\n"
  forM_ [mnameMap Map.! fname | fname <- prunedDeps] $ \(UnitId uid, ModuleName mod) -> do
    printf "%-60s %s\n" (BS8.unpack uid) (BS8.unpack mod)
  pure prunedDeps

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
