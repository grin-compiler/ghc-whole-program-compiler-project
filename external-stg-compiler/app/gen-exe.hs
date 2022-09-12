{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List
import Data.List.Split
import Control.Monad
import Control.Concurrent.Async.Pool

import System.Directory
import System.Environment
import System.Process
import GHC.Conc (getNumProcessors)

import qualified Stg.GHC.Symbols as GHCSymbols
import Stg.GHC.Backend
import Stg.IO
import Stg.Syntax
import Stg.Program

import Stg.DeadFunctionElimination.Facts
import Stg.DeadFunctionElimination.Analysis

import Data.Binary

import qualified GHC.Driver.Types as GHC
import qualified GHC.Utils.Outputable as GHC

import qualified Data.ByteString as BS

{-
  TODO: LTO-DFE
    done - app module pruning
    done - ext stg name collector pass
    skip - cli tool to generate facts

    done - collect facts + run LTO-DFE + export results
    done - call gen-obj + import liveness result + prune top level bindings
-}


genProgramDfeFacts :: [FilePath] -> IO ()
genProgramDfeFacts modpakFileNames = timeItNamed "fact collection run time" $ do
  putStrLn "generate datalog facts for whole stg program dead function elimination"
  cpuCount <- getNumProcessors
  withTaskGroup cpuCount $ \g -> do
    mapTasks g [readModpakL f modpakStgbinPath decodeStgbin >>= writeDfeFacts f | f <- modpakFileNames]
  pure ()

main :: IO ()
main = do
  [stgAppFname] <- getArgs

  putStrLn "compile STG modules"
  modpaks <- getAppModpaks stgAppFname

  appModpaks <- collectProgramModules modpaks "main" "Main" GHCSymbols.liveSymbols

  -- DFE pass
  genProgramDfeFacts appModpaks
  livenessAnalysisLogM appModpaks

  let oStg = [s ++ ".o" | s <- appModpaks]

  forM_ oStg $ \obj -> do
    fileExists <- doesFileExist obj
    when fileExists $ removeFile obj

  cpuCount <- getNumProcessors
  timeItNamed "program objects codegen time" $ do
    withTaskGroup cpuCount $ \g -> do
      mapTasks g [callProcess "gen-obj" f | f <- chunksOf 1 appModpaks]

  putStrLn $ "linking exe"

  (StgAppLinkerInfo{..}, libInfos) <- getAppLinkerInfo stgAppFname
  let linkerInfoList = filter (\StgLibLinkerInfo{..} -> stglibName /= "rts") libInfos

      cbitsOpts = concat
          [ concat
            [ [ "-L" ++ dir | dir <- stglibExtraLibDirs]
            , stglibLdOptions
            , [ "-l" ++ lib | lib <- stglibExtraLibs]
            ]
          | StgLibLinkerInfo{..} <- linkerInfoList
          ]

      cbitsArs = concatMap stglibCbitsPaths linkerInfoList

      stubArs = concatMap stglibAllStubsPaths linkerInfoList


      appOpts = concat
        [ [ "-L" ++ dir | dir <- stgappExtraLibDirs]
        , stgappLdOptions
        , [ "-l" ++ lib | lib <- stgappExtraLibs]
        ]

  let cLikeObjFiles = stgappCObjects
      includePaths  = []
      libPaths      = stgappExtraLibDirs ++ concatMap stglibExtraLibDirs linkerInfoList
      ldOptions     = stgappLdOptions ++ concatMap stglibLdOptions linkerInfoList ++ appOpts ++ cbitsOpts ++ cbitsArs ++ stubArs

  putStrLn "libPaths"
  putStrLn $ unlines libPaths
  putStrLn "ldOptions"
  putStrLn $ unlines ldOptions

  let cg = NCG

  print $ "appCLikeObjFiles: " ++ show cLikeObjFiles
  appCLikeObjFiles' <- forM cLikeObjFiles $ \fname -> do
    o <- BS.readFile fname
    let newObjName = fname ++ ".o"
    BS.writeFile newObjName o
    pure newObjName
  print $ "appCLikeObjFiles: " ++ show appCLikeObjFiles'

  let objs = appCLikeObjFiles' ++ oStg
  putStrLn "objs"
  putStrLn $ unlines objs

  compileProgram cg stgappNoHsMain includePaths libPaths ldOptions objs GHC.NoStubs [] []
