{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List.Split
import Control.Monad
import Control.Concurrent.Async.Pool

import System.FilePath
import System.Directory
import System.Environment
import System.Process

import Stg.DeadFunctionElimination.Analysis (timeItNamed)

import Stg.GHC.Backend
import Stg.IO
import Stg.Program

import qualified GHC.Driver.Types as GHC
import qualified GHC.Utils.Outputable as GHC

import qualified Data.ByteString.Char8 as BS8

main :: IO ()
main = do
  [fullpakPath] <- getArgs

  appInfo <- readModpakS fullpakPath "app.info" id
  let content = lines . BS8.unpack $ appInfo
      mods    = parseSection content "modules:"
      objectOutputPath = fullpakPath -<.> ".o"

  removePathForcibly objectOutputPath
  createDirectoryIfMissing True objectOutputPath

  putStrLn "compile STG modules"

  let oStg = [objectOutputPath </> m ++ ".o" | m <- mods]

  timeItNamed "program objects codegen time" $ do
    withTaskGroup 4 $ \g -> do
      mapTasks g [callProcess "gen-obj2" (fullpakPath : f) | f <- chunksOf 40 mods]

  putStrLn $ "linking exe"

  {-
  = StgAppInfo
  { appIncludePaths   :: [String]
  , appLibPaths       :: [String]
  , appLdOptions      :: [String]
  , appCLikeObjFiles  :: [String]
  , appNoHsMain       :: Bool
  }
  -}
  -- HINT: `readModpakS` reads from zip files, so it works for .fullpak also
  ghcstgappContent <- readModpakS fullpakPath "app.ghc_stgapp" BS8.unpack
  StgAppInfo{..} <- getAppInfoFromString ghcstgappContent

  putStrLn $ unlines $ "appIncludePaths:" : appIncludePaths
  putStrLn $ unlines $ "appLibPaths:" : appLibPaths
  putStrLn $ unlines $ "appLdOptions:" : appLdOptions

  let cg = NCG

  print $ "appCLikeObjFiles: " ++ show appCLikeObjFiles
  appCLikeObjFiles' <- forM appCLikeObjFiles $ \fname -> do
    o <- BS8.readFile fname
    let newObjName = fname ++ ".o"
    BS8.writeFile newObjName o
    pure newObjName
  print $ "appCLikeObjFiles: " ++ show appCLikeObjFiles'

  compileProgram cg appNoHsMain appIncludePaths appLibPaths appLdOptions (appCLikeObjFiles' ++ oStg) GHC.NoStubs [] []
