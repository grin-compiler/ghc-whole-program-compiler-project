{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.WeakPointer where

import Control.Monad.State
import qualified Data.IntMap as IntMap
import Data.Maybe
import Foreign.Ptr

import Stg.Syntax
import Stg.Interpreter.Base
import qualified Stg.Interpreter.FFI as FFI

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

newWeakPointer :: Atom -> Atom -> Maybe Atom -> M Int
newWeakPointer key value finalizer = do
  weakPointers <- gets ssWeakPointers
  next <- gets ssNextWeakPointer
  let desc = WeakPtrDescriptor
        { wpdKey          = key
        , wpdValue        = Just value
        , wpdFinalizer    = finalizer
        , wpdCFinalizers  = []
        }

  modify' $ \s -> s {ssWeakPointers = IntMap.insert next desc weakPointers, ssNextWeakPointer = succ next}
  pure next

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- mkWeak# :: o -> b -> (State# RealWorld -> (# State# RealWorld, c #)) -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeak#", [key, value, finalizer, _w]) -> do
    wpId <- newWeakPointer key value (Just finalizer)
    pure [WeakPointer wpId]

  -- mkWeakNoFinalizer# :: o -> b -> State# RealWorld -> (# State# RealWorld, Weak# b #)
  ( "mkWeakNoFinalizer#", [key, value, _w]) -> do
    wpId <- newWeakPointer key value Nothing
    pure [WeakPointer wpId]

  -- addCFinalizerToWeak# :: Addr# -> Addr# -> Int# -> Addr# -> Weak# b -> State# RealWorld -> (# State# RealWorld, Int# #)
  ( "addCFinalizerToWeak#", [fun, dataPtr, IntV hasEnv, envPtr, WeakPointer wpId, _w]) -> do
    wpd@WeakPtrDescriptor{..} <- lookupWeakPointerDescriptor wpId
    let desc = wpd {wpdCFinalizers = (fun, if hasEnv == 0 then Nothing else Just envPtr, dataPtr) : wpdCFinalizers}
    modify' $ \s@StgState{..} -> s {ssWeakPointers = IntMap.insert wpId desc ssWeakPointers}
    pure [IntV $ if wpdValue == Nothing then 0 else 1]

  -- deRefWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, a #)
  ( "deRefWeak#", [WeakPointer wpId, _w]) -> do
    WeakPtrDescriptor{..} <- lookupWeakPointerDescriptor wpId
    case wpdValue of
      Just v  -> pure [IntV 1, v]
      Nothing -> pure [IntV 0, LiftedUndefined]

  -- finalizeWeak# :: Weak# a -> State# RealWorld -> (# State# RealWorld, Int#, (State# RealWorld -> (# State# RealWorld, b #) ) #)
  ( "finalizeWeak#", [WeakPointer wpId, _w]) -> do
    finalizeWeak wpId

  -- touch# :: o -> State# RealWorld -> State# RealWorld
  ( "touch#", [_o, _s]) -> do
    -- see more about 'touch#': https://gitlab.haskell.org/ghc/ghc/-/wikis/hidden-dangers-of-touch
    pure []

  _ -> fallback op args t tc

finalizeWeak :: Int -> M [Atom]
finalizeWeak wpId = do
  wpd@WeakPtrDescriptor{..} <- lookupWeakPointerDescriptor wpId
  case wpdValue of
    Nothing -> pure [IntV 0, LiftedUndefined]
    Just v  -> do
      let finalizedWpd = wpd {wpdValue = Nothing}
      modify' $ \s@StgState{..} -> s {ssWeakPointers = IntMap.insert wpId finalizedWpd ssWeakPointers}
      mapM_ runCFinalizer wpdCFinalizers
      case wpdFinalizer of
        Nothing -> pure [IntV 0, LiftedUndefined]
        Just f  -> pure [IntV 1, f]

runCFinalizer :: (Atom, Maybe Atom, Atom) -> M ()
runCFinalizer (PtrAtom _ cFunPtr, mCEnv, cData) = do
  cArgs <- catMaybes <$> mapM FFI.mkFFIArg (maybeToList mCEnv ++ [cData])
  liftIOAndBorrowStgState $ do
    let cRetType = UnboxedTuple []
    FFI.evalForeignCall (castPtrToFunPtr cFunPtr) cArgs cRetType
  pure ()
runCFinalizer f = error $ "unsupported weakptr c finalizer: " ++ show f
