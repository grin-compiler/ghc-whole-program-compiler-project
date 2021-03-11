{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, FlexibleInstances #-}
module Stg.Interpreter.GC.GCRef where

import Data.Int
import Data.Bits
import Control.Monad.State

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
    BlackHole o       -> pure ()
    ApStack{..}       -> visitGCRef action hoResult >> visitGCRef action hoStack
    RaiseException ex -> action ex

instance VisitGCRef StackContinuation where
  visitGCRef action = \case
    CaseOf _ _ env _ _ _  -> visitGCRef action env
    Update addr           -> action $ HeapPtr addr
    Apply args            -> visitGCRef action args
    Catch handler _ _     -> action handler
    _                     -> pure ()

instance VisitGCRef ThreadState where
  visitGCRef action ThreadState{..} = do
    visitGCRef action tsCurrentResult
    visitGCRef action tsStack

instance VisitGCRef WeakPtrDescriptor where
  -- NOTE: the value is not tracked by the GC
  visitGCRef action WeakPtrDescriptor{..} = do
    ----------- temporarly track the value -- FIXME
    visitGCRef action wpdVale
    -----------
    action wpdKey
    visitGCRef action wpdFinalizer
    forM_ wpdCFinalizers $ \(a1, ma2, a3) -> do
      action a1
      action a3
      visitGCRef action ma2

instance VisitGCRef MVarDescriptor where
  visitGCRef action MVarDescriptor{..} = visitGCRef action mvdValue

-- datalog ref value encoding:
--  28 bit index value + 4 bit namespace tag ; max 16 namespaces
data RefNamespace
  = NS_Array
  | NS_ArrayArray
  | NS_HeapPtr
  | NS_MutableArray
  | NS_MutableArrayArray
  | NS_MutableByteArray
  | NS_MutVar
  | NS_MVar
  | NS_SmallArray
  | NS_SmallMutableArray
  | NS_StableName
  | NS_StablePointer
  | NS_WeakPointer
  deriving (Show, Enum)

encodeRef :: Int -> RefNamespace -> Int32
encodeRef i ns = shiftL (fromIntegral i) 4 .|. (fromIntegral $ fromEnum ns)

visitAtom :: Atom -> (Int32 -> SouffleM ()) -> SouffleM ()
visitAtom atom action = case atom of
  HeapPtr i           -> action $ encodeRef i NS_HeapPtr
  MVar i              -> action $ encodeRef i NS_MVar
  MutVar i            -> action $ encodeRef i NS_MutVar
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

arrIdxToRef :: ArrIdx -> Int32
arrIdxToRef = \case
  MutArrIdx i -> encodeRef i NS_MutableArray
  ArrIdx i    -> encodeRef i NS_Array

smallArrIdxToRef :: SmallArrIdx -> Int32
smallArrIdxToRef = \case
  SmallMutArrIdx i  -> encodeRef i NS_SmallMutableArray
  SmallArrIdx i     -> encodeRef i NS_SmallArray

arrayArrIdxToRef :: ArrayArrIdx -> Int32
arrayArrIdxToRef = \case
  ArrayMutArrIdx i  -> encodeRef i NS_MutableArrayArray
  ArrayArrIdx i     -> encodeRef i NS_ArrayArray
