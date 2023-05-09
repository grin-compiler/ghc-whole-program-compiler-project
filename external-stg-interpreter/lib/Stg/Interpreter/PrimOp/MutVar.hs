{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.MutVar where

import Control.Monad.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newMutVar# :: a -> State# s -> (# State# s, MutVar# s a #)
  ( "newMutVar#", [a, _s]) -> do
    mutVars <- gets ssMutVars
    next <- gets ssNextMutVar
    modify' $ \s -> s {ssMutVars = IntMap.insert next a mutVars, ssNextMutVar = succ next}
    pure [MutVar next]

  -- readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)
  ( "readMutVar#", [MutVar m, _s]) -> do
    a <- lookupMutVar m
    pure [a]

  -- writeMutVar# :: MutVar# s a -> a -> State# s -> State# s
  ( "writeMutVar#", [MutVar m, a, _s]) -> do
    _ <- lookupMutVar m -- check existence
    modify' $ \s@StgState{..} -> s {ssMutVars = IntMap.insert m a ssMutVars}
    pure []

  -- atomicModifyMutVar2# :: MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
  ( "atomicModifyMutVar2#", [MutVar m, fun, _s]) -> do
    Rts{..} <- gets ssRtsSupport
    -- NOTE: CPU atomic
    old <- lookupMutVar m

    -- transform through fun, get a pair result
    apFun <- readHeapClosure rtsApplyFun1Arg
    lazyNewTup2Value <- HeapPtr <$> allocAndStore (apFun {hoCloArgs = [fun, old], hoCloMissing = 0})

    -- get the first value of the pair
    tup2Prj0 <- readHeapClosure rtsTuple2Proj0
    lazyNewMutVarValue <- HeapPtr <$> allocAndStore (tup2Prj0 {hoCloArgs = [lazyNewTup2Value], hoCloMissing = 0})

    -- update mutvar
    modify' $ \s@StgState{..} -> s {ssMutVars = IntMap.insert m lazyNewMutVarValue ssMutVars}
    pure [old, lazyNewTup2Value]

  -- atomicModifyMutVar_# :: MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
  ( "atomicModifyMutVar_#", [MutVar m, fun, _s]) -> do
    Rts{..} <- gets ssRtsSupport
    -- NOTE: CPU atomic
    old <- lookupMutVar m

    -- transform through fun, get the new value
    apFun <- readHeapClosure rtsApplyFun1Arg
    lazyNewMutVarValue <- HeapPtr <$> allocAndStore (apFun {hoCloArgs = [fun, old], hoCloMissing = 0})

    -- update mutvar
    modify' $ \s@StgState{..} -> s {ssMutVars = IntMap.insert m lazyNewMutVarValue ssMutVars}
    pure [old, lazyNewMutVarValue]

  -- casMutVar# :: MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)
  ( "casMutVar#", [MutVar m, old, new, _s]) -> do
  -- NOTE: CPU atomic
    current <- lookupMutVar m
    if current == old
      then do
        modify' $ \s@StgState{..} -> s {ssMutVars = IntMap.insert m new ssMutVars}
        pure [IntV 0, new]
      else do
        pure [IntV 1, current]

  -- OBSOLETE from GHC 9.4
  -- sameMutVar# :: MutVar# s a -> MutVar# s a -> Int#
  ( "sameMutVar#", [MutVar a, MutVar b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  _ -> fallback op args t tc
