{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.Debugger.TraverseState
 ( exportReachableGraph
 , exportHeapGraph
 , getHeapObjectSummary
 , getHeapObjectCategory
 ) where

import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as BS8
import System.IO
import Stg.Interpreter.Base
import Stg.Interpreter.GC.GCRef

{-
  export GCSymbol's reachability graph as gephi compatible .tsv file
-}

type ExportM = StateT (Set GCSymbol) IO

exportReachableGraph :: FilePath -> FilePath -> StgState -> GCSymbol -> IO ()
exportReachableGraph nodesFname edgesFname stgState root = do
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
      evalStateT (addEdgesFrom hNode hEdge stgState root True) Set.empty
  {-
    special case: if gcsymbol has no children then emit one node only
    otherwise:    leaves will always be included

    OR:
      the graph should be empty if the object has no internal structure
  -}

mark :: GCSymbol -> ExportM Bool
mark symbol = state $ \visitedSet ->
  let wasVisited = Set.member symbol visitedSet
  in (not wasVisited, if wasVisited then visitedSet else Set.insert symbol visitedSet)

addEdgesFrom :: Handle -> Handle -> StgState -> GCSymbol -> Bool -> ExportM ()
addEdgesFrom hNode hEdge stgState@StgState{..} source isRoot = do
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
      hPutStr hNode $ if isRoot then "Root " ++ nodeLabel else nodeLabel
      BS8.hPut hNode "\t"
      hPutStr hNode nodeCategory
      BS8.hPut hNode "\n"

    -- TODO: generate Source node attributes ; or get
    let emitEdge :: VisitGCRef a => Maybe a -> ExportM ()
        emitEdge Nothing  = error $ "missing StgState item: " ++ show (ns, idx)
        emitEdge obj      = flip visitGCRef obj $ \target -> do
          -- HINT: write line to edge .tsv
          liftIO $ do
            BS8.hPut hEdge $ unGCSymbol source
            BS8.hPut hEdge "\t"
            BS8.hPut hEdge $ unGCSymbol target
            BS8.hPut hEdge "\t"
            BS8.hPut hEdge "green"
            BS8.hPut hEdge "\n"
          addEdgesFrom hNode hEdge stgState target False

    case ns of
      NS_Array              -> emitEdge $ IntMap.lookup idx ssArrays
      NS_ArrayArray         -> emitEdge $ IntMap.lookup idx ssArrayArrays
      NS_HeapPtr            -> emitEdge $ IntMap.lookup idx ssHeap
      NS_MutableArray       -> emitEdge $ IntMap.lookup idx ssMutableArrays
      NS_MutableArrayArray  -> emitEdge $ IntMap.lookup idx ssMutableArrayArrays
      NS_MutableByteArray   -> pure () -- IntMap.lookup idx ssMutableByteArrays
      NS_MutVar             -> emitEdge $ IntMap.lookup idx ssMutVars
      NS_TVar               -> emitEdge $ IntMap.lookup idx ssTVars
      NS_MVar               -> emitEdge $ IntMap.lookup idx ssMVars
      NS_SmallArray         -> emitEdge $ IntMap.lookup idx ssSmallArrays
      NS_SmallMutableArray  -> emitEdge $ IntMap.lookup idx ssSmallMutableArrays
{-
      NS_StableName
        | Just obj <- IntMap.lookup idx  -- TODO
-}
      NS_StablePointer      -> emitEdge $ IntMap.lookup idx ssStablePointers
      NS_WeakPointer        -> emitEdge $ IntMap.lookup idx ssWeakPointers
      NS_Thread             -> emitEdge $ IntMap.lookup idx ssThreads

      _ -> error $ "unknown StgState item: " ++ show (ns, idx)

getHeapObjectSummary :: HeapObject -> String
getHeapObjectSummary = \case
  Con{..} -> "Con: " ++ show hoCon
  Closure{..} -> if hoCloMissing == 0
    then "Thunk: " ++ show hoName
    else "Closure: " ++ show hoName
  BlackHole{} -> "BlackHole"
  ApStack{} -> "ApStack"
  RaiseException{} -> "RaiseException"

getHeapObjectCategory :: HeapObject -> String
getHeapObjectCategory = \case
  Con{}             -> "Constructor"
  Closure{..}       -> if hoCloMissing == 0 then "Thunk" else "Closure"
  BlackHole{}       -> "BlackHole"
  ApStack{}         -> "ApStack"
  RaiseException{}  -> "Exception"

exportHeapGraph :: FilePath -> FilePath -> Heap -> IO ()
exportHeapGraph nodesFname edgesFname heap = do
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
      flip evalStateT Set.empty $ do
        forM_ (IntMap.toList heap) $ \(addr, obj) -> do
          let source = encodeRef addr NS_HeapPtr

              genNode node = do
                firstTimeVisit <- mark node
                when firstTimeVisit $ do
                  let (ns, idx) = decodeRef node
                      (nodeLabel, nodeCategory) = case ns of
                        NS_HeapPtr
                          | Just ho <- IntMap.lookup idx heap
                          -> (getHeapObjectSummary ho, getHeapObjectCategory ho)
                        _ -> (drop 3 $ show ns, drop 3 $ show ns)
                  -- HINT: write line to node .tsv
                  liftIO $ do
                    BS8.hPut hNode $ unGCSymbol node
                    BS8.hPut hNode "\t"
                    hPutStr hNode nodeLabel
                    BS8.hPut hNode "\t"
                    hPutStr hNode nodeCategory
                    BS8.hPut hNode "\n"

          genNode source
          flip visitGCRef obj $ \target -> do
            genNode target
            -- HINT: write line to edge .tsv
            liftIO $ do
              BS8.hPut hEdge $ unGCSymbol source
              BS8.hPut hEdge "\t"
              BS8.hPut hEdge $ unGCSymbol target
              BS8.hPut hEdge "\t"
              BS8.hPut hEdge "green"
              BS8.hPut hEdge "\n"
