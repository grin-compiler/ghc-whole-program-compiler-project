module Stg.Interpreter.PrimOp.WeakPointer where

import           Control.Applicative  (Applicative (..), (<$>))
import           Control.Monad        (mapM, mapM_, void)
import           Control.Monad.State  (gets, modify')

import           Data.Eq              (Eq (..))
import           Data.Function        (($))
import           Data.Int             (Int)
import qualified Data.IntMap          as IntMap
import           Data.List            ((++))
import           Data.Maybe           (Maybe (..), catMaybes, isNothing, maybeToList)

import           Foreign.Ptr          (castPtrToFunPtr)

import           GHC.Err              (error)

import           Prelude              (Enum (..))

import           Stg.Interpreter.Base
import qualified Stg.Interpreter.FFI  as FFI
import           Stg.Syntax           (Name, TyCon, Type (..))

import           Text.Show            (Show (..))

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
    pure [IntV $ if isNothing wpdValue then 0 else 1]

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
    Just _v -> do
      let finalizedWpd = wpd {wpdValue = Nothing}
      modify' $ \s@StgState{..} -> s {ssWeakPointers = IntMap.insert wpId finalizedWpd ssWeakPointers}
      mapM_ runCFinalizer wpdCFinalizers
      case wpdFinalizer of
        Nothing -> pure [IntV 0, LiftedUndefined]
        Just f  -> pure [IntV 1, f]

runCFinalizer :: (Atom, Maybe Atom, Atom) -> M ()
runCFinalizer (PtrAtom _ cFunPtr, mCEnv, cData) = do
  cArgs <- catMaybes <$> mapM FFI.mkFFIArg (maybeToList mCEnv ++ [cData])
  void $ liftIOAndBorrowStgState $ do
    let cRetType = UnboxedTuple []
    FFI.evalForeignCall (castPtrToFunPtr cFunPtr) cArgs cRetType
runCFinalizer f = error $ "unsupported weakptr c finalizer: " ++ show f
