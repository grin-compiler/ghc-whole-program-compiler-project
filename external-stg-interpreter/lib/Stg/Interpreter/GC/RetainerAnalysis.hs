{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.GC.RetainerAnalysis where

import Control.Monad.State
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import System.Directory

import Stg.Interpreter.Base

loadMap :: String -> IO (IntMap IntSet)
loadMap fname = do
  let factPath = "./.gc-datalog-facts/" ++ fname
  absFactPath <- makeAbsolute factPath
  putStrLn $ "loading: " ++ show absFactPath
  refs <- map (map read . words) . lines <$> readFile absFactPath
  pure $ IntMap.fromListWith IntSet.union [(to, IntSet.singleton from) | [to, from] <- refs]

loadSet :: Bool -> String -> IO IntSet
loadSet isQuiet fname = do
  let factPath = "./.gc-datalog-facts/" ++ fname
  absFactPath <- makeAbsolute factPath
  unless isQuiet $ do
    putStrLn $ "loading: " ++ show absFactPath
  IntSet.fromList . map read . lines <$> readFile absFactPath

loadRetanerDb :: M ()
loadRetanerDb = pure ()

loadRetanerDb2 :: M ()
loadRetanerDb2 = do
  refMap <- liftIO $ loadMap "Reference.csv"
  retMap <- liftIO $ loadMap "LiveReferredBy.csv"
  isQuiet <- gets ssIsQuiet
  gcRootSet <- liftIO $ loadSet isQuiet "GCRoot.csv"
  modify' $ \s -> s {
      ssReferenceMap  = refMap
    , ssRetainerMap   = retMap
    , ssGCRootSet     = gcRootSet
    }

clearRetanerDb :: M ()
clearRetanerDb = do
  modify' $ \s -> s {
      ssReferenceMap  = mempty
    , ssRetainerMap   = mempty
    , ssGCRootSet     = IntSet.empty
    }
