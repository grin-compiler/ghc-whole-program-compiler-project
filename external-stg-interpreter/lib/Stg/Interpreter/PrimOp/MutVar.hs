{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, ConstraintKinds, DataKinds, TypeApplications, FlexibleContexts #-}
module Stg.Interpreter.PrimOp.MutVar where

import Control.Carrier.State.Strict
import Control.Effect.State
import Control.Effect.Labelled
import qualified Control.Effect.State.Labelled as L
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

type P sig m =
  ( M sig m
  , HasLabelled "MutVar" (State MutVarState) sig m
  )

data MutVarState
  = MutVarState
  { ssMutVars    :: IntMap AtomAddr  -- abs-int: IntMap (Set ...) | lattice
  , ssNextMutVar :: !Int
  }
  deriving (Show)

emptyMutVarState :: MutVarState
emptyMutVarState = MutVarState
  { ssMutVars    = mempty
  , ssNextMutVar = 0
  }

run m   = runState  emptyMutVarState (runLabelled @"MutVar" m)
eval m  = evalState emptyMutVarState (runLabelled @"MutVar" m)

lookupMutVar :: P sig m => Int -> m AtomAddr
lookupMutVar m = do
  IntMap.lookup m <$> L.gets @"MutVar" ssMutVars >>= \case
    Nothing -> stgErrorM $ "unknown MutVar: " ++ show m
    Just a  -> pure a

evalPrimOp :: P sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args, argsAddr) of

  -- newMutVar# :: a -> State# s -> (# State# s, MutVar# s a #)
  ( "newMutVar#", [_a, _s], [a, _]) -> allocAtoms =<< do
    L.state @"MutVar"  (\s@MutVarState{..} ->
      let next  = ssNextMutVar
      in (s {ssMutVars = IntMap.insert next a ssMutVars, ssNextMutVar = succ next}, [MutVar next]))

  -- readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)
  ( "readMutVar#", [MutVar m, _s], _) -> do
    a <- lookupMutVar m
    pure [a]

  -- writeMutVar# :: MutVar# s a -> a -> State# s -> State# s
  ( "writeMutVar#", [MutVar m, _a, _s], [_, a, _]) -> do
    _ <- lookupMutVar m -- check existence
    L.modify @"MutVar" $ \s@MutVarState{..} -> s {ssMutVars = IntMap.insert m a ssMutVars}
    pure []

  -- sameMutVar# :: MutVar# s a -> MutVar# s a -> Int#
  ( "sameMutVar#", [MutVar a, MutVar b], _) -> do
    allocAtoms [IntV $ if a == b then 1 else 0]

  -- atomicModifyMutVar2# :: MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
  ( "atomicModifyMutVar2#", [MutVar m, _fun, _s], [_, fun, _]) -> do
    Rts{..} <- gets ssRtsSupport
    -- NOTE: CPU atomic
    old <- lookupMutVar m

    -- transform through fun, get a pair result
    apFun <- readHeapClosure =<< getAtom rtsApplyFun1Arg
    lazyNewTup2Value <- HeapPtr <$> allocAndStore (apFun {hoCloArgs = [fun, old], hoCloMissing = 0}) >>= storeNewAtom

    -- get the first value of the pair
    tup2Prj0 <- readHeapClosure =<< getAtom rtsTuple2Proj0
    lazyNewMutVarValue <- HeapPtr <$> allocAndStore (tup2Prj0 {hoCloArgs = [lazyNewTup2Value], hoCloMissing = 0}) >>= storeNewAtom

    -- update mutvar
    L.modify @"MutVar" $ \s@MutVarState{..} -> s {ssMutVars = IntMap.insert m lazyNewMutVarValue ssMutVars}
    pure [old, lazyNewTup2Value]

  -- atomicModifyMutVar_# :: MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
  ( "atomicModifyMutVar_#", [MutVar m, _fun, _s], [_, fun, _]) -> do
    Rts{..} <- gets ssRtsSupport
    -- NOTE: CPU atomic
    old <- lookupMutVar m

    -- transform through fun, get the new value
    apFun <- readHeapClosure =<< getAtom rtsApplyFun1Arg
    lazyNewMutVarValue <- HeapPtr <$> allocAndStore (apFun {hoCloArgs = [fun, old], hoCloMissing = 0}) >>= storeNewAtom

    -- update mutvar
    L.modify @"MutVar" $ \s@MutVarState{..} -> s {ssMutVars = IntMap.insert m lazyNewMutVarValue ssMutVars}
    pure [old, lazyNewMutVarValue]

  -- casMutVar# :: MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)
  ( "casMutVar#", [MutVar m, old, new, _s], [_, oldAddr, newAddr, _]) -> do
  -- NOTE: CPU atomic
    current <- lookupMutVar m >>= getAtom
    if current == old
      then do
        L.modify @"MutVar" $ \s@MutVarState{..} -> s {ssMutVars = IntMap.insert m newAddr ssMutVars}
        (:) <$> storeNewAtom (IntV 0) <*> pure [newAddr]
      else do
        (:) <$> storeNewAtom (IntV 1) <*> pure [oldAddr]

  _ -> fallback op argsAddr t tc
