{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Stg.Interpreter.GC.GCRef where

import           Control.Applicative   (Applicative (..))
import           Control.Monad         (Monad (..), forM_, mapM_)

import qualified Data.ByteString.Char8 as BS8
import           Data.Eq               (Eq)
import           Data.Foldable         (Foldable)
import           Data.Function         (($), (.))
import           Data.Int              (Int)
import qualified Data.IntSet           as IntSet
import           Data.Ord              (Ord)

import           Stg.Interpreter.Base  (ArrIdx (..), ArrayArrIdx (..), Atom (..), ByteArrayIdx (..), GCSymbol (..),
                                        HeapObject (..), MVarDescriptor (..), PtrOrigin (..), SmallArrIdx (..),
                                        StackContinuation (..), TVarDescriptor (..), ThreadState (..),
                                        WeakPtrDescriptor (..))

import           Text.Read             (Read, read)
import           Text.Show             (Show (..))

-- HINT: populate datalog database during a traversal

class VisitGCRef a where
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> a -> m ()

instance VisitGCRef Atom where
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> Atom -> m ()
  visitGCRef action a = visitAtom a action

instance (Foldable t, VisitGCRef a) => VisitGCRef (t a) where
  visitGCRef :: (Monad m) => (GCSymbol -> m ()) -> t a -> m ()
  visitGCRef action = mapM_ (visitGCRef action)

instance VisitGCRef HeapObject where
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> HeapObject -> m ()
  visitGCRef action = \case
    Con{..}           -> visitGCRef action hoConArgs
    Closure{..}       -> visitGCRef action hoCloArgs >> visitGCRef action hoEnv
    BlackHole {}      -> pure () -- HINT: the blackhole wait queue is handled separately
    ApStack{..}       -> visitGCRef action hoResult >> visitGCRef action hoStack
    RaiseException ex -> visitGCRef action ex

instance VisitGCRef StackContinuation where
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> StackContinuation -> m ()
  visitGCRef action = \case
    CaseOf _ _ env _ _ _    -> visitGCRef action env
    Update{}                -> pure () -- HINT: the thunk is under evaluation, its closure is referred from the thread stack
                                       --       the blackhole wait queue is handled separately
    Apply args              -> visitGCRef action args
    Catch handler _ _       -> visitGCRef action handler
    CatchRetry stm alt _ _  -> visitGCRef action stm >> visitGCRef action alt
    CatchSTM stm handler    -> visitGCRef action stm >> visitGCRef action handler
    RestoreExMask{}         -> pure ()
    RunScheduler{}          -> pure ()
    Atomically stmAction    -> visitGCRef action stmAction
    AtomicallyOp stmAction  -> visitGCRef action stmAction
    DataToTagOp{}           -> pure ()
    RaiseOp ex              -> visitGCRef action ex
    KeepAlive value         -> visitGCRef action value
    DebugFrame{}            -> pure ()

instance VisitGCRef ThreadState where
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> ThreadState -> m ()
  visitGCRef action ThreadState{..} = do
    visitGCRef action tsCurrentResult
    visitGCRef action tsStack
    --visitGCRef action tsActiveTLog  -- not GC related
    --visitGCRef action tsTLogStack   -- not GC related
    --visitGCRef action tsStatus      -- not GC related
    forM_ tsBlockedExceptions $ \(throwerTid, ex) -> do
      action $ encodeRef throwerTid NS_Thread
      visitGCRef action ex
{-
instance VisitGCRef ThreadStatus where
  visitGCRef action = \case
    ThreadRunning   -> pure ()
    ThreadFinished  -> pure ()
    ThreadDied      -> pure ()
    ThreadBlocked r -> case r of
      BlockedOnMVar mvarId mAtom      -> visitGCRef action $ MVar mvarId : maybeToList mAtom
      BlockedOnMVarRead mvarId        -> visitGCRef action $ MVar mvarId
      BlockedOnBlackHole              -> pure ()
      BlockedOnThrowAsyncEx targetTid -> visitGCRef action $ ThreadId targetTid
      BlockedOnSTM{}                  -> pure ()
      BlockedOnForeignCall            -> pure ()
      BlockedOnRead _fd               -> pure ()
      BlockedOnWrite _fd              -> pure ()
      BlockedOnDelay _targetTime      -> pure ()
      e -> error $ show e
    e -> error $ show e

instance VisitGCRef TLogEntry where
  visitGCRef action TLogEntry{..} = do
    action tleObservedGlobalValue
    action tleCurrentLocalValue
-}
instance VisitGCRef WeakPtrDescriptor where
  -- NOTE: the value is not tracked by the GC
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> WeakPtrDescriptor -> m ()
  visitGCRef action WeakPtrDescriptor{..} = do
    ----------- temporarly track the value -- FIXME
    visitGCRef action wpdValue
    -----------
    visitGCRef action wpdKey
    visitGCRef action wpdFinalizer
    forM_ wpdCFinalizers $ \(a1, ma2, a3) -> do
      visitGCRef action a1
      visitGCRef action a3
      visitGCRef action ma2

instance VisitGCRef MVarDescriptor where
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> MVarDescriptor -> m ()
  visitGCRef action MVarDescriptor{..} = do
    visitGCRef action mvdValue
    forM_ mvdQueue $ \tid -> action $ encodeRef tid NS_Thread

instance VisitGCRef TVarDescriptor where
  visitGCRef :: Monad m => (GCSymbol -> m ()) -> TVarDescriptor -> m ()
  visitGCRef action TVarDescriptor{..} = do
    visitGCRef action tvdValue
    forM_ (IntSet.toList tvdQueue) $ \tid -> action $ encodeRef tid NS_Thread

-- datalog ref value encoding:
data RefNamespace
  = NS_Array
  | NS_ArrayArray
  | NS_HeapPtr
  | NS_MutableArray
  | NS_MutableArrayArray
  | NS_MutableByteArray
  | NS_MutVar
  | NS_TVar
  | NS_MVar
  | NS_SmallArray
  | NS_SmallMutableArray
  | NS_StableName
  | NS_StablePointer
  | NS_WeakPointer
  | NS_Thread
  deriving stock (Eq, Ord, Show, Read)

encodeRef :: Int -> RefNamespace -> GCSymbol
encodeRef i ns = GCSymbol $ BS8.pack $ show (ns, i)

decodeRef :: GCSymbol -> (RefNamespace, Int)
decodeRef = read . BS8.unpack . unGCSymbol

visitAtom :: Monad m => Atom -> (GCSymbol -> m ()) -> m ()
visitAtom atom action = case atom of
  HeapPtr i               -> action $ encodeRef i NS_HeapPtr
  Literal{}               -> pure ()
  Void                    -> pure ()
  PtrAtom (StablePtr i) _ -> action $ encodeRef i NS_StablePointer -- HINT: for debug purposes (track usage) keep this reference
  PtrAtom{}               -> pure ()
  IntAtom{}               -> pure ()
  WordAtom{}              -> pure ()
  FloatAtom{}             -> pure ()
  DoubleAtom{}            -> pure ()
  MVar i                  -> action $ encodeRef i NS_MVar
  MutVar i                -> action $ encodeRef i NS_MutVar
  TVar i                  -> action $ encodeRef i NS_TVar
  Array i                 -> action $ arrIdxToRef i
  MutableArray i          -> action $ arrIdxToRef i
  SmallArray i            -> action $ smallArrIdxToRef i
  SmallMutableArray i     -> action $ smallArrIdxToRef i
  ArrayArray i            -> action $ arrayArrIdxToRef i
  MutableArrayArray i     -> action $ arrayArrIdxToRef i
  ByteArray i             -> action $ encodeRef (baId i) NS_MutableByteArray
  MutableByteArray i      -> action $ encodeRef (baId i) NS_MutableByteArray
  WeakPointer i           -> action $ encodeRef i NS_WeakPointer
  StableName i            -> action $ encodeRef i NS_StableName
  ThreadId i              -> action $ encodeRef i NS_Thread -- NOTE: in GHC the ThreadId# prim type is a strong pointer to TSO (thread state oject)
  LiftedUndefined{}       -> pure ()
  Rubbish{}               -> pure ()
  Unbinded{}              -> pure ()
  -- _                   -> error $ "internal error - incomplete pattern: " ++ show atom

arrIdxToRef :: ArrIdx -> GCSymbol
arrIdxToRef = \case
  MutArrIdx i -> encodeRef i NS_MutableArray
  ArrIdx i    -> encodeRef i NS_Array

smallArrIdxToRef :: SmallArrIdx -> GCSymbol
smallArrIdxToRef = \case
  SmallMutArrIdx i  -> encodeRef i NS_SmallMutableArray
  SmallArrIdx i     -> encodeRef i NS_SmallArray

arrayArrIdxToRef :: ArrayArrIdx -> GCSymbol
arrayArrIdxToRef = \case
  ArrayMutArrIdx i  -> encodeRef i NS_MutableArrayArray
  ArrayArrIdx i     -> encodeRef i NS_ArrayArray
