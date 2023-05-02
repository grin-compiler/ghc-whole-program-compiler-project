{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.GC.RetainerAnalysis where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import System.Directory
import System.FilePath
import Text.Printf

import Stg.Interpreter.Base
{-
loadMap :: String -> IO (IntMap IntSet)
loadMap factPath = do
  absFactPath <- makeAbsolute factPath
  putStrLn $ "loading: " ++ show absFactPath
  refs <- map (map read . words) . lines <$> readFile absFactPath
  pure $ IntMap.fromListWith IntSet.union [(to, IntSet.singleton from) | [to, from] <- refs]
-}

loadStringSet :: Bool -> String -> IO (Set String)
loadStringSet isQuiet factPath = do
  absFactPath <- makeAbsolute factPath
  unless isQuiet $ do
    putStrLn $ "loading: " ++ show absFactPath
  Set.fromList . lines <$> readFile absFactPath

loadRetainerDb :: M ()
loadRetainerDb = pure ()

loadRetainerDb2 :: M ()
loadRetainerDb2 = error "TODO"
{-
loadRetainerDb2 = do
  gcCycle <- gets ssGCCounter
  let factDir = "./.gc-datalog-facts" </> printf "gc-cycle-%03i" gcCycle
  refMap <- liftIO $ loadMap $ factDir </> "Reference.csv"
  retMap <- liftIO $ loadMap $ factDir </> "LiveReferredBy.csv"
  isQuiet <- gets ssIsQuiet
  gcRootSet <- liftIO $ loadStringSet isQuiet $ factDir </> "GCRoot.csv"
  modify' $ \s -> s {
      ssReferenceMap  = refMap
    , ssRetainerMap   = retMap
    , ssGCRootSet     = gcRootSet
    }
-}
clearRetanerDb :: M ()
clearRetanerDb = do
  modify' $ \s -> s {
      ssReferenceMap  = mempty
    , ssRetainerMap   = mempty
    , ssGCRootSet     = Set.empty
    }
