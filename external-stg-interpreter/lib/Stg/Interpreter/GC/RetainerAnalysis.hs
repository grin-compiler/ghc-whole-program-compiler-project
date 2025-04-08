module Stg.Interpreter.GC.RetainerAnalysis where

import           Control.Applicative   (Applicative (..), (<$>))
import           Control.Monad         (Functor (..), unless)
import           Control.Monad.State   (MonadIO (..), gets, modify')

import           Data.Bool             (Bool)
import qualified Data.ByteString.Char8 as BS8
import           Data.Function         (($), (.))
import           Data.List             ((++))
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Monoid           (Monoid (..))
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.String           (String)

import           Stg.Interpreter.Base  (GCSymbol (..), M, StgState (..))

import           System.Directory      (makeAbsolute)
import           System.FilePath       ((</>))
import           System.IO             (IO, putStrLn)

import           Text.Printf           (printf)
import           Text.Show             (Show (..))

loadMap :: String -> IO (Map GCSymbol (Set GCSymbol))
loadMap factPath = do
  absFactPath <- makeAbsolute factPath
  putStrLn $ "loading: " ++ show absFactPath
  refs <- fmap BS8.words . BS8.lines <$> BS8.readFile absFactPath
  pure $ Map.fromListWith Set.union [(GCSymbol to, Set.singleton $ GCSymbol from) | [to, from] <- refs]


loadStringSet :: Bool -> String -> IO (Set GCSymbol)
loadStringSet isQuiet factPath = do
  absFactPath <- makeAbsolute factPath
  unless isQuiet $ do
    putStrLn $ "loading: " ++ show absFactPath
  Set.fromList . fmap GCSymbol . BS8.lines <$> BS8.readFile absFactPath

loadRetainerDb :: M ()
loadRetainerDb = pure ()

loadRetainerDb2 :: M ()
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

clearRetanerDb :: M ()
clearRetanerDb = do
  modify' $ \s -> s {
      ssReferenceMap  = mempty
    , ssRetainerMap   = mempty
    , ssGCRootSet     = Set.empty
    }
