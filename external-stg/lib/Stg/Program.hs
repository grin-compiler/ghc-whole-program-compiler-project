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

printSection :: Show a => [a] -> String
printSection l = unlines ["- " ++ x | x <- nubOrd $ map show l]

parseSection :: [String] -> String -> [String]
parseSection content n = map (read . tail) . takeWhile (isPrefixOf "-") . tail . dropWhile (not . isPrefixOf n) $ content

parsePathSection :: FilePath -> [String] -> String -> [String]
parsePathSection root content n = map (\p -> if isAbsolute p then p else root </> p) $ parseSection content n

data StgAppInfo
  = StgAppInfo
  { appIncludePaths   :: [String]
  , appLibPaths       :: [String]
  , appLdOptions      :: [String]
  , appCLikeObjFiles  :: [String]
  , appNoHsMain       :: Bool
  }
  deriving (Eq, Ord, Show)

getAppInfo :: FilePath -> IO StgAppInfo
getAppInfo ghcStgAppFname = do
  readFile ghcStgAppFname >>= getAppInfoFromString

getAppInfoFromString :: String -> IO StgAppInfo
getAppInfoFromString rawContent = do
  let content     = lines rawContent
      [root]      = parseSection content "root:"
      [osuf]      = parseSection content "o_suffix:"
      libPaths    = parsePathSection root content "pkg_lib_paths:"
      incPaths    = parsePathSection root content "pkg_include_paths:"
      extra_input = parsePathSection root content "extra_ld_inputs:"
      ldFlags     = parseSection content "other_flags:"
      extraLibs   = parseSection content "extra_libs:"
      pkgLibs     = parseSection content "package_hs_libs:"
      nonHsLibs   = filter (not . isPrefixOf "-lHS") pkgLibs
      [noHsMain]  = parseSection content "no_hs_main:"

  -- HINT: if there is no corresponding modpak then it is a C like obj
  cObjFiles <- filterM (\n -> not <$> doesFileExist (n ++ "_modpak")) extra_input

  archiveList <- concat <$> mapM (find always (extension ==? ".a")) libPaths
  let archiveMap        = Map.fromList [(takeFileName n, takeDirectory n) | n <- archiveList]
      libArName n kind  = "lib" ++ drop 2 n ++ "." ++ osuf ++ kind ++".a"
      libLdOpt n kind   = n ++ "." ++ osuf ++ kind
  let cbits   = [ (libLdOpt o "_cbits", arPath)
                | o <- pkgLibs
                , let arName = libArName o "_cbits"
                , arPath <- maybeToList $ Map.lookup arName archiveMap
                ]
      stubs   = [ (libLdOpt o "_stubs", arPath)
                | o <- pkgLibs
                , let arName = libArName o "_stubs"
                , arPath <- maybeToList $ Map.lookup arName archiveMap
                ]
      ldOpts  = concat [map fst stubs, map fst cbits, nonHsLibs, ldFlags, extraLibs]

  pure StgAppInfo
    { appIncludePaths   = incPaths
    , appLibPaths       = libPaths ++ map snd stubs ++ map snd cbits
    , appLdOptions      = ldOpts
    , appCLikeObjFiles  = cObjFiles
    , appNoHsMain       = "True" == noHsMain
    }

data StgModuleInfo
  = StgModuleInfo
  { modModuleName   :: String
  , modModpakPath   :: FilePath
  , modPackageName  :: String
  }
  deriving (Eq, Ord, Show)

getLibModuleMapping :: FilePath -> IO [StgModuleInfo]
getLibModuleMapping stglibFname = do
  -- HINT: the folder containing the .stglib is named after the package name
  let packageName = takeFileName $ dropTrailingPathSeparator $ dropFileName stglibFname
      stglibPath  = dropFileName stglibFname
  content <- lines <$> readFile stglibFname
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
  let [root]      = parseSection content "root:"
      [osuf]      = parseSection content "o_suffix:"
      ways        = parseSection content "ways:"
      libPaths    = parsePathSection root content "pkg_lib_paths:"
      o_files     = parsePathSection root content "o_files:"
      extra_input = parsePathSection root content "extra_ld_inputs:"
      pkgLibs     = parseSection content "package_hs_libs:"
      pkgHSLibs   = [drop 4 n | n <- pkgLibs, isPrefixOf "-lHS" n]
  stglibs <- mapM (findStglibForHSLib ways osuf libPaths) pkgHSLibs
  libModpaks <- mapM getLibModuleMapping stglibs

  extraAppModpaks <- filterM doesFileExist $ map (++ "_modpak") extra_input
  let appModpaks  = map (++ "_modpak") o_files
  -- load app module names from stgbins
  appModules <- forM (appModpaks ++ extraAppModpaks) $ \modpak -> do
    (_phase, _unitId, moduleName, _srcPath) <- readModpakL modpak modpakStgbinPath decodeStgbinModuleName
    pure StgModuleInfo
          { modModuleName   = BS8.unpack $ getModuleName moduleName
          , modModpakPath   = modpak
          , modPackageName  = packageName
          }
  pure $ appModules ++ concat libModpaks

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

-- .stglib utility
findStglibForHSLib :: [String] -> String -> [FilePath] -> String -> IO FilePath
findStglibForHSLib ways osuf hsLibPaths libName = do
  -- HINT: e.g. libHSghc-boot-8.11.0.20201112-ghc8.11.0.20201112.dyn_o_stglib
  let waySet = Set.fromList ways
      stglibName
        | isPrefixOf "rts-1.0" libName  -- HACK!!
        = "libHSrts-1.0.o_stglib"

        | Set.member "WayDyn" waySet
        = "libHS" ++ libName ++ ".dyn_o_stglib"

        | otherwise
        = "libHS" ++ libName ++ ".o_stglib"

  filesFound <- forM hsLibPaths $ \path -> do
    find always (fileName ==? stglibName) path
  case concat filesFound of
    [stglibPath]  -> pure stglibPath
    []            -> error $ "stglib not found: " ++ stglibName
    l             -> error $ "multiple stglib found: " ++ stglibName ++ "\n" ++ show l
