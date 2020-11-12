{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.MVar where

import Control.Monad.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newMVar# :: State# s -> (# State# s, MVar# s a #)
  ("newMVar#", [_s]) -> do
    state (\s@StgState{..} ->
      let next = IntMap.size ssMVars
      in ([MVar next], s {ssMVars = IntMap.insert next Nothing ssMVars}))

  -- TODO takeMVar# :: MVar# s a -> State# s -> (# State# s, a #)
  ("takeMVar#", [MVar m, _s]) -> do
    lookupMVar m >>= \case
      Nothing -> stgErrorM $ "TODO: blocking takeMVar# for mvar " ++ show m
      Just a  -> state $ \s@StgState{..} -> ([a], s {ssMVars = IntMap.insert m Nothing ssMVars})

  -- tryTakeMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
  ("tryTakeMVar#", [MVar m, _s]) -> do
    lookupMVar m >>= \case
      Nothing -> do
        pure [IntV 0, LiftedUndefined]
      Just a -> do
        modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m Nothing ssMVars }
        pure [IntV 1, a]

  -- TODO putMVar# :: MVar# s a -> a -> State# s -> State# s
  ("putMVar#", [MVar m, a, _s]) -> do
    lookupMVar m >>= \case
      Just{}  -> stgErrorM $ "TODO: blocking putMVar# for mvar " ++ show m
      Nothing -> modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m (Just a) ssMVars}
    pure []

  -- tryPutMVar# :: MVar# s a -> a -> State# s -> (# State# s, Int# #)
  ("tryPutMVar#", [MVar m, a, _s]) -> do
    lookupMVar m >>= \case
      Nothing -> do
        modify' $ \s@StgState{..} -> s { ssMVars = IntMap.insert m (Just a) ssMVars }
        pure [IntV 1]
      Just _  -> do
        pure [IntV 0]

  -- TODO readMVar# :: MVar# s a -> State# s -> (# State# s, a #)
  ("readMVar#", [MVar m, _s]) -> do
    lookupMVar m >>= \case
      Nothing -> stgErrorM $ "TODO: blocking readMVar for mvar " ++ show m
      Just a  -> pure [a]

  -- tryReadMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
  ("tryReadMVar#", [MVar m, _s]) -> do
    lookupMVar m >>= \case
      Nothing -> pure [IntV 0, LiftedUndefined]
      Just a  -> pure [IntV 1, a]

  -- sameMVar# :: MVar# s a -> MVar# s a -> Int#
  ("sameMVar#", [MVar a, MVar b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  -- isEmptyMVar# :: MVar# s a -> State# s -> (# State# s, Int# #)
  ("isEmptyMVar#", [MVar a, _s]) -> do
    v <- lookupMVar a
    pure $ case v of
      Nothing -> [IntV 1]
      Just _  -> [IntV 0]

  _ -> fallback op args t tc
