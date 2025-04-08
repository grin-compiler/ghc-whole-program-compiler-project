import           Control.Applicative         (Applicative (..), (<$>))
import           Control.Monad               (Functor (..), MonadFail (..), filterM, forM_, join, mapM_)

import           Data.Bool                   (Bool (..))
import qualified Data.ByteString.Char8       as BS8
import           Data.Eq                     (Eq (..))
import           Data.Function               (flip, ($), (.))
import           Data.List                   (concatMap, isSuffixOf, length, take, (++))
import qualified Data.Map                    as Map
import           Data.Maybe                  (Maybe (..), isNothing, maybeToList)
import           Data.Monoid                 (Monoid (..), (<>))
import           Data.Ord                    (Ord (..))
import qualified Data.Set                    as Set
import           Data.String                 (String)

import           Foreign                     (nullFunPtr)

import           GHC.Num                     (Integer, Num (..))

import           Options.Applicative         (CommandFields, InfoMod, Mod, Parser)
import           Options.Applicative.Builder (argument, command, help, info, metavar, progDesc, str, subparser)
import           Options.Applicative.Extra   (execParser, helper)

import           Stg.Analysis.Closure        (getAllClosures)
import           Stg.Analysis.ForeignInfo    (ForeignInfo (..), getForeignInfos)
import           Stg.Foreign.Linker          (getExtStgWorkDirectory, linkForeignCbitsSharedLib)
import           Stg.GHC.Symbols             (getSymbolName, handledRTSSymbols, rtsSymbols)
import           Stg.Program                 (StgModuleInfo (..), getAppModuleMapping, getFullpakModules,
                                              getGhcStgAppModules, getJSONModules)
import           Stg.Syntax                  (CCallTarget (..), ForeignCall (..), ForeignStubs' (..), Lit (..), Module,
                                              Module' (..), PrimCall (..), StubDecl' (..), StubImpl (..), getModuleName,
                                              getUnitId)

import           System.Directory            (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import           System.FilePath             (FilePath, takeExtension, (-<.>), (</>))
import           System.IO                   (IO, IOMode (..), hPutStrLn, putStrLn, withFile)
import           System.Posix.DynamicLinker  (RTLDFlags (..), c_dlsym, dlopen, packDL)

import           Text.Printf                 (printf)
import           Text.Show                   (Show (..))

loadModules :: FilePath -> IO [Module]
loadModules fname = case () of
  _ | "fullpak" `isSuffixOf` fname    -> getFullpakModules fname
  _ | "ghc_stgapp" `isSuffixOf` fname -> getGhcStgAppModules fname
  _ | "json" `isSuffixOf` fname       -> getJSONModules fname
  _                                   -> fail "unknown file format"

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
                  [n | StaticTarget _ n _ _ <- foreignCTarget <$> Map.keys fiForeignCalls]

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
            forM_ (Map.toList fiLiterals) $ \(lit, count) -> printf "%-10d %s\n" count (show lit)

    stringsMode :: Parser (IO ())
    stringsMode =
        run <$> fullpakFile
      where
        run fname = do
            moduleList <- loadModules fname
            let ForeignInfo{..} = getForeignInfos moduleList
            printf "used top level strings: %d\n" (Map.size fiTopStrings)
            forM_ (Map.toList fiTopStrings) $ \(str', count) -> printf "%-10d %s\n" count (show str')

    undefMode :: Parser (IO ())
    undefMode =
        run <$> fullpakFile
      where
        run fname = do
            -- get foreign symbols
            moduleList <- loadModules fname
            let ignoredSymbols = Set.fromList $
                  [ n
                  | ForeignStubs _ _ _ _ l <- fmap moduleForeignStubs moduleList
                  , StubDeclImport _ (Just (StubImplImportCWrapper n _ _ _ _)) <- l
                  -- HINT: these symbols are used only by "createAdjustor" that does not dereference the symbol
                  ] ++ fmap BS8.pack handledRTSSymbols
            let ForeignInfo{..} = getForeignInfos moduleList
                symbols = Set.fromList $
                  [n | LitLabel n _ <- Map.keys fiLitLabels] ++
                  [n | PrimCall n _ <- Map.keys fiPrimCalls] ++
                  [n | StaticTarget _ n _ _ <- foreignCTarget <$> Map.keys fiForeignCalls]

            -- get known symbols from .cbits.so
            soName <- makeAbsolute (fname -<.> ".cbits.so")
            dl <- dlopen soName [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]

            undefList <- flip filterM (Set.toList symbols) $ \sym -> BS8.useAsCString sym $ \s -> do
              symPtr <- c_dlsym (packDL dl) s
              pure (symPtr == nullFunPtr)

            let undefSet = Set.fromList undefList Set.\\ ignoredSymbols
            printf "undefined symbols: %d\n" (Set.size undefSet)
            mapM_ BS8.putStrLn $ Set.toList undefSet

            let nonRTSUndefSet = undefSet Set.\\ Set.fromList (fmap (BS8.pack . getSymbolName) rtsSymbols)
            printf "\nnon RTS undefined symbols: %d\n" (Set.size nonRTSUndefSet)
            mapM_ BS8.putStrLn $ Set.toList nonRTSUndefSet


    linkMode :: Parser (IO ())
    linkMode =
        run <$> fullpakFile
      where
        run fname = if "ghc_stgapp" `isSuffixOf` fname then (do
          workDir <- getExtStgWorkDirectory fname
          createDirectoryIfMissing True workDir
          let soName = workDir </> "cbits.so"
          printf "linking %s\n" soName
          linkForeignCbitsSharedLib fname) else (do
          printf "linking is not supported for %s\n" . show $ takeExtension fname)

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
            (if ok then printf "OK: %s\n" modModuleName else printf "MISSING: %s\n" hiName)

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
          forM_ [m | m <- moduleList, isNothing $ moduleSourceFilePath m] $ \m -> printf "missing source filepath for: %s %s\n" (BS8.unpack $ getUnitId $ moduleUnitId m) (BS8.unpack $ getModuleName $ moduleName m)

          -- report ambiguous moduleSourceFilePath
          let moduleMaps  = [Map.singleton srcPath (1 :: Integer, [m]) | m <- moduleList, srcPath <- maybeToList $ moduleSourceFilePath m]
              duplicates  = Map.filter (\(n, _) -> n > 1) $ Map.unionsWith (\(n1, l1) (n2, l2) -> (n1 + n2, l1 ++ l2)) moduleMaps
          forM_ (Map.toList duplicates) $ \(srcPath, (_, mods)) -> forM_ mods $ \m -> printf "duplicate source filepath: %s %s %s\n"
            (BS8.unpack $ getUnitId $ moduleUnitId m)
            (BS8.unpack $ getModuleName $ moduleName m)
            (BS8.unpack srcPath)

main :: IO ()
main = join $ execParser $ info (helper <*> modes) mempty
