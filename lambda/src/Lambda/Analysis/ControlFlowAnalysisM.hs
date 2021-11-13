{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Lambda.Analysis.ControlFlowAnalysisM where

-- NOTE: only when the whole program is available

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Map (Map)
import qualified Data.Map as Map

import System.Directory
import System.FilePath
import System.Process
import System.IO
import System.IO.Temp

import Lambda.Syntax
import Lambda.Datalog.ToDatalog

controlFlowAnalysisM :: [String] -> [String] -> Program -> IO (Map String [[Text]])
controlFlowAnalysisM = controlFlowAnalysisImplM False

controlFlowAnalysisLogM :: [String] -> [String] -> Program -> IO (Map String [[Text]])
controlFlowAnalysisLogM = controlFlowAnalysisImplM True

controlFlowAnalysisImplM :: Bool -> [String] -> [String] -> Program -> IO (Map String [[Text]])
controlFlowAnalysisImplM log calledByOuterCode initialReachable prg = do

  tmpSys <- getCanonicalTemporaryDirectory
  tmpCfa <- createTempDirectory tmpSys "lambda-cfa"

  when log $ do
    putStrLn "controlFlowAnalysisM:"
    putStrLn $ "export facts to:"
    putStrLn tmpCfa

  programToFactsM log tmpCfa prg

  -- HINT: main function, that does not take closures or constructors as arguments, only C-land values
  let srcFile = tmpCfa </> "InitialReachable.facts"
  when log $ putStrLn srcFile
  writeFile srcFile $ unlines initialReachable

  -- HINT: these functions can receive closures and constructors as arguments
  let outerFile = tmpCfa </> "CalledByOuterCode.facts"
  when log $ putStrLn outerFile
  writeFile outerFile $ unlines calledByOuterCode

  when log $ putStrLn "run: lambda-cfa"
  callProcess "lambda-cfa" ["--output=" ++ tmpCfa, "--facts=" ++ tmpCfa, "--jobs=4"]

  when log $ putStrLn "read back result"
  result <- filter (\n -> takeExtension n == ".csv") <$> listDirectory tmpCfa
  Map.fromList <$> forM result
    (\fname -> do
        row <- map (Text.splitOn "\t") . Text.lines <$> Text.readFile (tmpCfa </> fname)
        pure (takeBaseName fname, row)
    )
