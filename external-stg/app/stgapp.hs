{-# LANGUAGE ScopedTypeVariables, RecordWildCards, OverloadedStrings #-}

import Control.Monad
import Data.Maybe
import Data.List (isSuffixOf)
import Data.Monoid
import Data.Ord
import Data.Semigroup ((<>))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import System.IO
import System.Posix.DynamicLinker
import Text.Printf
import Foreign

import Options.Applicative

import Stg.Syntax
import Stg.Analysis.Closure
import Stg.Analysis.ForeignInfo
import Stg.Program
import Stg.GHC.Symbols
import Stg.Foreign.Linker

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
    <> mode "foreign-info" foreignInfoMode  (progDesc "print list of referred foreign calls and foreign symbols")
    <> mode "foreign-symbols" foreignSymbolsMode  (progDesc "print list of referred foreign symbols")
    <> mode "primops" primopsMode  (progDesc "print list of used primops")
    <> mode "literals" literalsMode  (progDesc "print list of used literals")
    <> mode "strings" stringsMode  (progDesc "print list of used strings")
    <> mode "undef" undefMode  (progDesc "print list of undefined foreign symbols")
    <> mode "link" linkMode  (progDesc "link cbits.so for the applications with used foreign functions")
    <> mode "hi-list"  hiListMode  (progDesc "whole program interface file list")
    <> mode "srcpaths"  srcpathMode  (progDesc "print module source filepaths")
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

    foreignInfoMode :: Parser (IO ())
    foreignInfoMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- loadModules fname
            let ForeignInfo{..} = getForeignInfos moduleList

            printf "foreign literal symbols: %d\n" (Map.size fiLitLabels)
            mapM_ (printf "  %s\n" . show ) $ Map.keys fiLitLabels

            printf "\nforeign prim calls: %d\n" (Map.size fiPrimCalls)
            mapM_ (printf "  %s\n" . show) $ Map.keys fiPrimCalls

            printf "\nforeign calls: %d\n" (Map.size fiForeignCalls)
            mapM_ (printf "  %s\n" . show) $ Map.keys fiForeignCalls

    foreignSymbolsMode :: Parser (IO ())
    foreignSymbolsMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- loadModules fname
            let ForeignInfo{..} = getForeignInfos moduleList
                symbols = Set.fromList $
                  [n | LitLabel n _ <- Map.keys fiLitLabels] ++
                  [n | PrimCall n _ <- Map.keys fiPrimCalls] ++
                  [n | StaticTarget _ n _ _ <- map foreignCTarget $ Map.keys fiForeignCalls]

            printf "foreign symbols: %d\n" (Set.size symbols)
            mapM_ BS8.putStrLn $ Set.toList symbols

    primopsMode :: Parser (IO ())
    primopsMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- loadModules fname
            let ForeignInfo{..} = getForeignInfos moduleList
            printf "used primops: %d\n" (Map.size fiPrimOps)
            mapM_ BS8.putStrLn $ Map.keys fiPrimOps

    literalsMode :: Parser (IO ())
    literalsMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- loadModules fname
            let ForeignInfo{..} = getForeignInfos moduleList
            printf "used literals: %d\n" (Map.size fiLiterals)
            forM_ (Map.toList fiLiterals) $ \(lit, count) -> do
              printf "%-10d %s\n" count (show lit)

    stringsMode :: Parser (IO ())
    stringsMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- loadModules fname
            let ForeignInfo{..} = getForeignInfos moduleList
            printf "used top level strings: %d\n" (Map.size fiTopStrings)
            forM_ (Map.toList fiTopStrings) $ \(str, count) -> do
              printf "%-10d %s\n" count (show str)

    undefMode :: Parser (IO ())
    undefMode =
        run <$> fullpakFile
      where
        run fname = do
            -- get foreign symbols
            moduleList <- loadModules fname
            let ignoredSymbols = Set.fromList $
                  [ n
                  | ForeignStubs _ _ _ _ l <- map moduleForeignStubs moduleList
                  , StubDeclImport _ (Just (StubImplImportCWrapper n _ _ _ _)) <- l
                  -- HINT: these symbols are used only by "createAdjustor" that does not dereference the symbol
                  ] ++ map BS8.pack handledRTSSymbols
            let ForeignInfo{..} = getForeignInfos moduleList
                symbols = Set.fromList $
                  [n | LitLabel n _ <- Map.keys fiLitLabels] ++
                  [n | PrimCall n _ <- Map.keys fiPrimCalls] ++
                  [n | StaticTarget _ n _ _ <- map foreignCTarget $ Map.keys fiForeignCalls]

            -- get known symbols from .cbits.so
            soName <- makeAbsolute (fname -<.> ".cbits.so")
            dl <- dlopen soName [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]

            undefList <- flip filterM (Set.toList symbols) $ \sym -> do
              BS8.useAsCString sym $ \s -> do
                symPtr <- c_dlsym (packDL dl) s
                pure (symPtr == nullFunPtr)

            let undefSet = Set.fromList undefList Set.\\ ignoredSymbols
            printf "undefined symbols: %d\n" (Set.size undefSet)
            mapM_ BS8.putStrLn $ Set.toList undefSet

            let nonRTSUndefSet = undefSet Set.\\ Set.fromList (map (BS8.pack . getSymbolName) rtsSymbols)
            printf "\nnon RTS undefined symbols: %d\n" (Set.size nonRTSUndefSet)
            mapM_ BS8.putStrLn $ Set.toList nonRTSUndefSet


    linkMode :: Parser (IO ())
    linkMode =
        run <$> fullpakFile
      where
        run fname = case isSuffixOf "ghc_stgapp" fname of
          False -> do
            printf "linking is not supported for %s\n" . show $ takeExtension fname
          True  -> do
            workDir <- getExtStgWorkDirectory fname
            createDirectoryIfMissing True workDir
            let soName = workDir </> "cbits.so"
            printf "linking %s\n" soName
            linkForeignCbitsSharedLib fname

    hiListMode :: Parser (IO ())
    hiListMode =
        run <$> fullpakFile
      where
        run fname = do
          moduleInfoList <- getAppModuleMapping fname

          printf "GHC core interface files: %d\n" (length moduleInfoList)
          forM_ moduleInfoList $ \StgModuleInfo{..} -> do
            let hiName = take (length modModpakPath - 8) modModpakPath ++ "hi"
            ok <- doesFileExist hiName
            case ok of
              True  -> printf "OK: %s\n" modModuleName
              False -> printf "MISSING: %s\n" hiName

    srcpathMode :: Parser (IO ())
    srcpathMode =
        run <$> fullpakFile
      where
        run fname = do
          moduleList <- loadModules fname
          -- print source filepaths
          forM_ moduleList $ \m -> case moduleSourceFilePath m of
            Nothing       -> pure ()
            Just srcPath  -> printf "source filepath: %s %s %s\n"
                              (BS8.unpack $ getUnitId $ moduleUnitId m)
                              (BS8.unpack $ getModuleName $ moduleName m)
                              (BS8.unpack srcPath)

          -- report empty moduleSourceFilePath
          forM_ [m | m <- moduleList, isNothing $ moduleSourceFilePath m] $ \m -> do
            printf "missing source filepath for: %s %s\n" (BS8.unpack $ getUnitId $ moduleUnitId m) (BS8.unpack $ getModuleName $ moduleName m)

          -- report ambiguous moduleSourceFilePath
          let moduleMaps  = [Map.singleton srcPath (1, [m]) | m <- moduleList, srcPath <- maybeToList $ moduleSourceFilePath m]
              duplicates  = Map.filter (\(n, _) -> n > 1) $ Map.unionsWith (\(n1, l1) (n2, l2) -> (n1 + n2, l1 ++ l2)) moduleMaps
          forM_ (Map.toList duplicates) $ \(srcPath, (_, mods)) -> forM_ mods $ \m -> do
            printf "duplicate source filepath: %s %s %s\n"
              (BS8.unpack $ getUnitId $ moduleUnitId m)
              (BS8.unpack $ getModuleName $ moduleName m)
              (BS8.unpack srcPath)

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
