module Stg.Interpreter.Debugger.Retainer
 ( exportRetainerGraph
-- , exportRetainerDominatorTree
 ) where

import           Control.Applicative                    (Applicative (..))
import           Control.Monad                          (forM_, when)
import           Control.Monad.State                    (MonadState (..), StateT, evalStateT, execStateT, gets, modify')
import           Control.Monad.Writer                   (MonadIO (..), MonadWriter (..), execWriter)

import           Data.Bimap                             (Bimap)
import qualified Data.Bimap                             as Bimap
import           Data.Bool                              (Bool (..), not, otherwise)
import qualified Data.ByteString.Char8                  as BS8
import           Data.Function                          (flip, id, ($), (.))
import           Data.Int                               (Int)
import           Data.IntMap.Strict                     (IntMap)
import qualified Data.IntMap.Strict                     as IntMap
import           Data.IntSet                            (IntSet)
import qualified Data.IntSet                            as IntSet
import           Data.List                              (drop, (++))
import           Data.Map                               (Map)
import qualified Data.Map                               as Map
import           Data.Maybe                             (Maybe (..), mapMaybe, maybe)
import           Data.Monoid                            (Monoid (..))
import           Data.Set                               (Set)
import qualified Data.Set                               as Set
import           Data.String                            (String)

import           Stg.Interpreter.Base                   (GCSymbol (..), StgState (..))
import           Stg.Interpreter.Debugger.TraverseState (getHeapObjectCategory, getHeapObjectSummary)
import           Stg.Interpreter.GC.GCRef               (RefNamespace (..), decodeRef)
import           Stg.Interpreter.GC.LiveDataAnalysis    (withGCRootFacts, withReferenceFacts)

import           System.IO                              (FilePath, Handle, IO, IOMode (..), hPutStr, print, withFile)

import           Text.Show                              (Show (..))



data RetainerState
  = RetainerState
  { rsGraph   :: IntMap IntSet
  , rsNodeMap :: Bimap GCSymbol Int
  }

type RetainerM = StateT RetainerState IO

addNode :: GCSymbol -> RetainerM Int
addNode n = do
  nodeMap <- gets rsNodeMap
  case Bimap.lookup n nodeMap of
    Just i  -> pure i
    Nothing -> do
      let i = Bimap.size nodeMap
      modify' $ \s@RetainerState{..} -> s {rsNodeMap = Bimap.insert n i rsNodeMap}
      pure i

addEdge :: GCSymbol -> GCSymbol -> RetainerM ()
addEdge from to = do
  fromId <- addNode from
  toId <- addNode to
  modify' $ \s@RetainerState{..} -> s {rsGraph = IntMap.insertWith IntSet.union fromId (IntSet.singleton toId) rsGraph}

exportRetainerGraph :: FilePath -> FilePath -> StgState -> GCSymbol -> IO ()
exportRetainerGraph nodesFname edgesFname stgState root = do
  {-
    done - calculate retainer graph
    done - traverse graph
  -}
  -- HINT: retainer = inverse reference
  RetainerState{..} <- flip execStateT (RetainerState mempty Bimap.empty) . withReferenceFacts stgState $ \from to -> addEdge to from
  let gcRootSet :: Map GCSymbol String
      gcRootSet = execWriter $ withGCRootFacts stgState (ssLocalEnv stgState) $ \msg s -> tell $ Map.singleton s msg

  withFile edgesFname WriteMode $ \hEdge -> withFile nodesFname WriteMode $ \hNode -> do
    BS8.hPutStrLn hNode $ BS8.intercalate "\t"
      [ "Id"
      , "Label"
      , "partition2"
      ]
    BS8.hPutStrLn hEdge $ BS8.intercalate "\t"
      [ "Source"
      , "Target"
      , "partition2"
      ]
    flip evalStateT Set.empty . addEdgesFrom hNode hEdge stgState gcRootSet root True $ \case
      source
        | Just i <- Bimap.lookup source rsNodeMap
        , Just edges <- IntMap.lookup i rsGraph
        -> (mapMaybe (`Bimap.lookupR` rsNodeMap) (IntSet.toList edges))
        | otherwise
        -> []

  pure ()

type ExportM = StateT (Set GCSymbol) IO

mark :: GCSymbol -> ExportM Bool
mark symbol = state $ \visitedSet ->
  let wasVisited = Set.member symbol visitedSet
  in (not wasVisited, if wasVisited then visitedSet else Set.insert symbol visitedSet)

addEdgesFrom :: Handle -> Handle -> StgState -> Map GCSymbol String -> GCSymbol -> Bool -> (GCSymbol -> [GCSymbol]) -> ExportM ()
addEdgesFrom hNode hEdge stgState@StgState{..} gcRootSet source isRoot getEdges = do
  firstTimeVisit <- mark source
  when firstTimeVisit $ do
    liftIO $ print source

    let (ns, idx) = decodeRef source
        (nodeLabel, nodeCategory) = case ns of
          NS_HeapPtr
            | Just ho <- IntMap.lookup idx ssHeap
            -> (getHeapObjectSummary ho, getHeapObjectCategory ho)
          _ -> (drop 3 $ show ns, drop 3 $ show ns)

    -- HINT: write line to node .tsv
    liftIO $ do
      BS8.hPut hNode $ unGCSymbol source
      BS8.hPut hNode "\t"
      hPutStr hNode $
        (if isRoot then ("Root " ++) else id) $
        maybe id (\msg str -> "GCRoot " ++ msg ++ " " ++ str) (Map.lookup source gcRootSet) nodeLabel
      BS8.hPut hNode "\t"
      hPutStr hNode nodeCategory
      BS8.hPut hNode "\n"

    -- TODO: generate Source node attributes ; or get
    forM_ (getEdges source) $ \target -> do
      -- HINT: write line to edge .tsv
      liftIO $ do
        BS8.hPut hEdge $ unGCSymbol source
        BS8.hPut hEdge "\t"
        BS8.hPut hEdge $ unGCSymbol target
        BS8.hPut hEdge "\t"
        BS8.hPut hEdge "green"
        BS8.hPut hEdge "\n"
      addEdgesFrom hNode hEdge stgState gcRootSet target False getEdges

{-
exportRetainerDominatorTree :: FilePath -> FilePath -> StgState -> GCSymbol -> IO ()
exportRetainerDominatorTree nodesFname edgesFname stgState root = do
  -- HINT: retainer = inverse reference
  RetainerState{..} <- flip execStateT (RetainerState mempty Bimap.empty) . withReferenceFacts stgState $ \from to -> addEdge to from
  let gcRootSet :: Set GCSymbol
      gcRootSet = execWriter $ withGCRootFacts stgState (ssLocalEnv stgState) (tell . Set.singleton)

  withFile edgesFname WriteMode $ \hEdge -> do
    withFile nodesFname WriteMode $ \hNode -> do
      BS8.hPutStrLn hNode $ BS8.intercalate "\t"
        [ "Id"
        , "Label"
        , "partition2"
        ]
      BS8.hPutStrLn hEdge $ BS8.intercalate "\t"
        [ "Source"
        , "Target"
        , "partition2"
        ]
-}
