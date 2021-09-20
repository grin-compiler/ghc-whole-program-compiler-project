{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.Debug where

import qualified GHC.Exts as Exts
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.Primitive.ByteArray as BA
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.Map.Strict as StrictMap
import Data.List (intercalate, foldl', sortOn)
import System.IO
import System.Directory
import System.FilePath
import Text.Printf

import Control.Monad.State.Strict

import Stg.Syntax
import Stg.Interpreter.Base

showCons :: Int -> M ()
showCons addr = do
  h <- gets ssHeap
  liftIO $ mapM_ print [(i, dcUniqueName dc, args) | x@(i, c@(Con _ dc args)) <- IntMap.toAscList h, i >= addr]

{-
  | Closure
    { hoName        :: Id
    , hoCloBody     :: StgRhsClosure
    , hoEnv         :: Env    -- local environment ; with live variables only, everything else is pruned
    , hoCloArgs     :: [Atom]
    , hoCloMissing  :: Int    -- HINT: this is a Thunk if 0 arg is missing ; if all is missing then Fun ; Pap is some arg is provided
    }

  = Binder
    { binderName      :: !Name
    , binderId        :: !BinderId
    , binderType      :: !Type
    , binderTypeSig   :: !Name
    , binderScope     :: !Scope
    , binderDetails   :: !IdDetails
    , binderInfo      :: !IdInfo
    , binderDefLoc    :: !SrcSpan
    , binderUnitId    :: !UnitId
    , binderModule    :: !ModuleName
    , binderTopLevel  :: !Bool
    }

-}
showClosures :: Int -> M ()
showClosures addr = do
  h <- gets ssHeap
  executed <- gets ssExecutedClosures
  let thunks = [x | x@(i, Closure{..}) <- IntMap.toAscList h, i >= addr, Set.notMember i executed, hoCloMissing == 0]
  liftIO $ mapM_ print [(i, getUnitId binderUnitId, getModuleName binderModule, hoName, hoCloMissing, hoCloArgs) | x@(i, c@Closure{..}) <- thunks, let Id Binder{..} = hoName]
  {-
  forM_ thunks $ \(i, _) -> do
    liftIO $ do
      putStrLn "\n-------------------------------------------\n"
      putStrLn $ "will force: " ++ show i
      pure ()
    result <- builtinStgEval (HeapPtr i)
    liftIO $ putStrLn $ "forced: " ++ show i ++ " got: " ++ show result
  -}
{-
showWeakPointers :: BuiltinStgApply -> M ()
showWeakPointers builtinStgApply = do
  wl <- gets ssWeakPointers
  liftIO $ mapM_ print wl
  forM_ [finalizer | WeakPointer _ _ (Just finalizer) <- Set.toList wl] $ \f -> do
    result <- builtinStgApply f [Void]
    liftIO $ putStrLn $ "run finalizer: " ++ show f ++ " got: " ++ show result
-}
showByteArrays :: M ()
showByteArrays = do
  arrs <- gets ssMutableByteArrays
  liftIO $ forM_ (IntMap.toList arrs) $ \(i, ByteArrayDescriptor{..}) -> do
    arr <- map BS.w2c . Exts.toList <$> BA.unsafeFreezeByteArray baaMutableByteArray
    print (i, arr)
    putStrLn "\n-------------------------------------------\n"
{-
  = ByteArrayDescriptor
  { baaMutableByteArray :: !(BA.MutableByteArray RealWorld)
  , baaByteArray        :: !(Maybe BA.ByteArray)  -- HINT: ByteArray can only be created via unsafeFreeze from a MutableByteArray
  , baaPinned           :: !Bool
  , baaAlignment        :: !Int
  }
-}

showMarked :: M ()
showMarked = do
  ops <- Set.toList <$> gets ssExecutedPrimOps
  ffis <- Set.toList <$> gets ssExecutedFFI
  prims <- Set.toList <$> gets ssExecutedPrimCalls
  liftIO $ do
    mapM_ print ops
    mapM_ print ffis
    mapM_ print prims

showDebug :: EvalOnNewThread -> M ()
showDebug evalOnNewThread = do
  limit <- gets ssHeapStartAddress
  liftIO $ putStrLn "\n-------------------------------------------\n"
  liftIO $ putStrLn "Used primops and foreign functions:"
  liftIO $ putStrLn "\n-------------------------------------------\n"
  showMarked

  liftIO $ putStrLn "\n-------------------------------------------\n"
  liftIO $ putStrLn "Data Constructors:"
  liftIO $ putStrLn "\n-------------------------------------------\n"
  --showCons limit

  liftIO $ putStrLn "\n-------------------------------------------\n"
  liftIO $ putStrLn "Closures:"
  liftIO $ putStrLn "\n-------------------------------------------\n"
  --showClosures limit

  liftIO $ putStrLn "\n-------------------------------------------\n"
  liftIO $ putStrLn "ByteArrays:"
  liftIO $ putStrLn "\n-------------------------------------------\n"
  --showByteArrays

  liftIO $ putStrLn "\n-------------------------------------------\n"
  liftIO $ putStrLn "WeakPointers:"
  liftIO $ putStrLn "\n-------------------------------------------\n"
  --showWeakPointers builtinStgApply

exportCallGraph :: M ()
exportCallGraph = do
  Rts{..} <- gets ssRtsSupport
  globalCallGraph <- gets ssCallGraph
  liftIO $ do
    writeCallGraph (rtsProgName ++ "-call-graph.tsv") globalCallGraph
    writeCallGraphSummary (rtsProgName ++ "-call-graph-summary") globalCallGraph

exportRegionCallGraph :: Region -> M ()
exportRegionCallGraph r@Region{..} = do
  regions <- gets ssRegions
  case Map.lookup r regions of
    Just (Just{}, callGraph, l) -> do
      Rts{..} <- gets ssRtsSupport
      let regionName  = BS8.unpack regionStart ++ "-" ++ BS8.unpack regionEnd
          dirName     = "." ++ rtsProgName ++ "-call-graph" </> regionName
          idx         = length l
      liftIO $ do
        regionPath <- makeAbsolute dirName
        createDirectoryIfMissing True regionPath
        putStrLn $ "save call graphs to: " ++ regionPath
        writeCallGraph (regionPath </> printf "%04d" idx ++ ".tsv") callGraph
        writeCallGraphSummary (regionPath </> printf "%04d" idx ++ "-summary") callGraph

    _ -> pure () -- HINT: ignore missing regions or non-open regions

writeCallGraph :: FilePath -> CallGraph -> IO ()
writeCallGraph fname CallGraph{..} = do
  let showSO = \case
        SO_CloArg         -> "unknown"
        SO_Let            -> "known"
        SO_Scrut          -> "unknown"
        SO_AltArg         -> "unknown"
        SO_TopLevel       -> "known"
        SO_Builtin        -> "known"
        SO_ClosureResult  -> "unknown"
  withFile fname WriteMode $ \h -> do
    hPutStrLn h "count\tSource\tTarget\tstatic-origin\tcall-site-type"
    forM_ (sortOn (negate . snd) $ StrictMap.toList cgInterClosureCallGraph) $ \((so, from, to), count) -> do
      hPutStrLn h $ intercalate "\t" [show count, show from, show to, show so, showSO so]
    forM_ (sortOn (negate . snd) $ StrictMap.toList cgIntraClosureCallGraph) $ \((from, so, to), count) -> do
      hPutStrLn h $ intercalate "\t" [show count, show from, show to, "direct", "direct"]

writeCallGraphSummary :: FilePath -> CallGraph -> IO ()
writeCallGraphSummary fname CallGraph{..} = do
  let cgFrequencyList = StrictMap.elems cgInterClosureCallGraph ++ StrictMap.elems cgIntraClosureCallGraph
      cgMin   = minimum cgFrequencyList
      cgMax   = maximum cgFrequencyList
      cgSum   = sum cgFrequencyList
      cgSize  = StrictMap.size cgInterClosureCallGraph + StrictMap.size cgIntraClosureCallGraph
  withFile fname WriteMode $ \h -> do
    hPutStrLn h $ unlines
      [ printf "min: %d" cgMin
      , printf "max: %d" cgMax
      , printf "sum: %d" cgSum
      , printf "size (edge count): %d" cgSize
      ]
