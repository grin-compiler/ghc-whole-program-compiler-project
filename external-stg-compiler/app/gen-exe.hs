{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List.Split
import Control.Monad
import Control.Concurrent.Async.Pool

import System.Directory
import System.Environment
import System.Process

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
  withTaskGroup 4 $ \g -> do
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

  timeItNamed "program objects codegen time" $ do
    withTaskGroup 4 $ \g -> do
      mapTasks g [callProcess "gen-obj" f | f <- chunksOf 1 appModpaks]

  putStrLn $ "linking exe"

  StgAppInfo{..} <- getAppInfo stgAppFname

  let cg = NCG

  print $ "appCLikeObjFiles: " ++ show appCLikeObjFiles
  appCLikeObjFiles' <- forM appCLikeObjFiles $ \fname -> do
    o <- BS.readFile fname
    let newObjName = fname ++ ".o"
    BS.writeFile newObjName o
    pure newObjName
  print $ "appCLikeObjFiles: " ++ show appCLikeObjFiles'

  compileProgram cg appNoHsMain appIncludePaths appLibPaths appLdOptions (appCLikeObjFiles' ++ oStg) GHC.NoStubs [] []
