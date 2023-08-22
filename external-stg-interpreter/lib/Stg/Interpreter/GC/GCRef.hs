{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, FlexibleInstances #-}
module Stg.Interpreter.GC.GCRef where

import Data.Maybe
import Control.Monad.State
import Foreign.Ptr

import Language.Souffle.Compiled (SouffleM)

import Stg.Interpreter.Base

-- HINT: populate datalog database during a traversal

class VisitGCRef a where
  visitGCRef :: (Atom -> SouffleM ()) -> a -> SouffleM ()

instance VisitGCRef Atom where
  visitGCRef action a = action a

instance (Foldable t, VisitGCRef a) => VisitGCRef (t a) where
  visitGCRef action a = mapM_ (visitGCRef action) a

instance VisitGCRef HeapObject where
  visitGCRef action = \case
    Con{..}           -> visitGCRef action hoConArgs
    Closure{..}       -> visitGCRef action hoCloArgs >> visitGCRef action hoEnv
    BlackHole _ o     -> pure ()
    ApStack{..}       -> visitGCRef action hoResult >> visitGCRef action hoStack
    RaiseException ex -> action ex

instance VisitGCRef StackContinuation where
  visitGCRef action = \case
    CaseOf _ _ env _ _ _    -> visitGCRef action env
    Update addr             -> pure () -- action $ HeapPtr addr -- TODO/FIXME: this is not a GC root!
    Apply args              -> visitGCRef action args
    Catch handler _ _       -> action handler
    CatchRetry stm alt _ _  -> action stm >> action alt
    CatchSTM stm handler    -> action stm >> action handler
    RestoreExMask{}         -> pure ()
    RunScheduler{}          -> pure ()
    Atomically stmAction    -> action stmAction
    AtomicallyOp stmAction  -> action stmAction
    DataToTagOp{}           -> pure ()
    RaiseOp ex              -> action ex
    KeepAlive value         -> action value
    DebugFrame{}            -> pure ()

instance VisitGCRef ThreadState where
  visitGCRef action ThreadState{..} = do
    visitGCRef action tsCurrentResult
    visitGCRef action tsStack
    visitGCRef action tsActiveTLog
    visitGCRef action tsTLogStack
    visitGCRef action tsStatus
    visitGCRef action $ map snd tsBlockedExceptions

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

instance VisitGCRef WeakPtrDescriptor where
  -- NOTE: the value is not tracked by the GC
  visitGCRef action WeakPtrDescriptor{..} = do
    ----------- temporarly track the value -- FIXME
    visitGCRef action wpdValue
    -----------
    action wpdKey
    visitGCRef action wpdFinalizer
    forM_ wpdCFinalizers $ \(a1, ma2, a3) -> do
      action a1
      action a3
      visitGCRef action ma2

instance VisitGCRef MVarDescriptor where
  visitGCRef action MVarDescriptor{..} = visitGCRef action mvdValue

instance VisitGCRef TVarDescriptor where
  visitGCRef action TVarDescriptor{..} = visitGCRef action tvdValue

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
  deriving (Show, Read)

encodeRef :: Int -> RefNamespace -> GCSymbol
encodeRef i ns = GCSymbol $ show (ns, i)

decodeRef :: GCSymbol -> (RefNamespace, Int)
decodeRef = read . unGCSymbol

visitAtom :: Atom -> (GCSymbol -> SouffleM ()) -> SouffleM ()
visitAtom atom action = case atom of
  HeapPtr i           -> action $ encodeRef i NS_HeapPtr
  MVar i              -> action $ encodeRef i NS_MVar
  MutVar i            -> action $ encodeRef i NS_MutVar
  TVar i              -> action $ encodeRef i NS_TVar
  Array i             -> action $ arrIdxToRef i
  MutableArray i      -> action $ arrIdxToRef i
  SmallArray i        -> action $ smallArrIdxToRef i
  SmallMutableArray i -> action $ smallArrIdxToRef i
  ArrayArray i        -> action $ arrayArrIdxToRef i
  MutableArrayArray i -> action $ arrayArrIdxToRef i
  ByteArray i         -> action $ encodeRef (baId i) NS_MutableByteArray
  MutableByteArray i  -> action $ encodeRef (baId i) NS_MutableByteArray
  WeakPointer i       -> action $ encodeRef i NS_WeakPointer
  StableName i        -> action $ encodeRef i NS_StableName
  PtrAtom (StablePtr i) _ -> action $ encodeRef i NS_StablePointer -- HINT: for debug purposes (track usage) keep this reference
  _                   -> pure ()

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
