{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, LambdaCase, TupleSections, OverloadedStrings #-}
module Stg.DeadFunctionElimination.Analysis where

import Control.Monad.IO.Class
import qualified Language.Souffle.Compiled (SouffleM)
import qualified Language.Souffle.Compiled as Souffle

import Control.Monad
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import System.Directory
import System.FilePath
import System.Process
import System.IO
import System.IO.Temp
import Data.Time.Clock

import qualified Stg.GHC.Symbols as GHCSymbols

{-
  TODO:
    done - collect module facts
    done - run analysis
    done - write module liveness
-}

timeItNamed :: String -> IO m -> IO m
timeItNamed title m = do
  preTime <- getCurrentTime
  result <- m
  postTime <- getCurrentTime
  putStrLn $ title ++ ": " ++ show (diffUTCTime postTime preTime)
  pure result

livenessAnalysisM :: [FilePath] -> IO ()
livenessAnalysisM = livenessAnalysisImplM False

livenessAnalysisLogM :: [FilePath] -> IO ()
livenessAnalysisLogM = livenessAnalysisImplM True

livenessAnalysisImplM :: Bool -> [FilePath] -> IO ()
livenessAnalysisImplM log stgBins = do

  tmpSys <- getCanonicalTemporaryDirectory
  tmpDir <- createTempDirectory tmpSys "ext-stg-liveness"

  when log $ do
    putStrLn "livenessAnalysisM:"
    putStrLn $ "export facts to:"
    putStrLn tmpDir

  -- prepare input facts

  let fixes =
        [ "ghc-prim_GHC.Types.True"
        ]

  mergeInputFacts stgBins tmpDir
  -- add GHC exported symbols to LiveSources
  -- WORKAROUND: add "main_Main.main" keep "main_:Main.main" alive because it lives in the Main module
  appendFile (tmpDir </> "LiveSource.facts") $ unlines $ "main_Main.main" : GHCSymbols.liveQualifiedSymbols ++ fixes

  -- run liveness analysis

  when log $ putStrLn "run: ext-stg-liveness"
  -- exec as a separate process
  --callProcess "ext-stg-liveness" ["--output=" ++ tmpDir, "--facts=" ++ tmpDir, "--jobs=4"]

  -- exec as embedded .cpp
  (if log
    then timeItNamed "liveness analysis run time"
    else id) $ execLiveness tmpDir tmpDir 4

  when log $ putStrLn "read back result"
  copyFile (tmpDir </> "LiveFunName.csv") "LiveFunName.csv"
  copyFile (tmpDir </> "LiveDataConName.csv") "LiveDataConName.csv"

dfeInputFacts :: [String]
dfeInputFacts =
  [ "LiveSource.facts"
  , "TyCon.facts"
  , "TyConReference.facts"
  , "DataConReference.facts"
  , "FunReference.facts"
  ]

mergeInputFacts :: [FilePath] -> FilePath -> IO ()
mergeInputFacts stgBins tmpDir = do
  forM_ dfeInputFacts $ \factName -> do
    let factFile = tmpDir </> factName
    forM_ stgBins $ \stgFname -> do
      BS.readFile (stgFname -<.> factName) >>= BS.appendFile factFile

{-
// input fatcs
.decl TyCon(tycon:Name, datacon:Name)
.input TyCon

.decl TyConReference(fun:Name, tycon:Name)
.input TyConReference

.decl DataConReference(fun:Name, datacon:Name)
.input DataConReference

.decl FunReference(fun:Name, funref:Name)
.input FunReference

.decl LiveSource(fun:Name)
.input LiveSource

// output fatcs
.decl LiveFunName(fun:Name)
.output LiveFunName

.decl LiveTyConName(tycon:Name)
.output LiveTyConName

.decl LiveDataConName(datacon:Name)
.output LiveDataConName
-}

data Liveness = Liveness

instance Souffle.Program Liveness where
  type ProgramFacts Liveness = '[]
  programName = const "ext_stg_liveness"

execLiveness :: FilePath -> FilePath -> Word64 -> IO ()
execLiveness inputDir outputDir threadCount = Souffle.runSouffle Liveness $ \maybeProgram -> do
  case maybeProgram of
    Nothing -> liftIO $ putStrLn "Failed to load program."
    Just prog -> do
      Souffle.setNumThreads prog threadCount
      Souffle.loadFiles prog inputDir
      Souffle.run prog
      Souffle.writeFiles prog outputDir
