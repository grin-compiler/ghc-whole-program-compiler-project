{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.MVar where

import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newMVar# :: State# s -> (# State# s, MVar# s a #)
  ("newMVar#", [_s]) -> do
    state (\s@StgState{..} -> let next = IntMap.size ssMVars in ([MVar next], s {ssMVars = IntMap.insert next Nothing ssMVars}))

  -- takeMVar# :: MVar# s a -> State# s -> (# State# s, a #)
  ("takeMVar#", [MVar m, _s]) -> do
    lookupMVar m >>= \case
      Nothing -> stgErrorM $ "TODO: blocking takeMVar# for mvar " ++ show m
      Just a  -> state $ \s@StgState{..} -> ([a], s {ssMVars = IntMap.insert m Nothing ssMVars})

  -- TODO: tryTakeMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)

  -- putMVar# :: MVar# s a -> a -> State# s -> State# s
  ("putMVar#", [MVar m, a, _s]) -> do
    lookupMVar m >>= \case
      Just{}  -> stgErrorM $ "TODO: blocking putMVar# for mvar " ++ show m
      Nothing -> modify' $ \s@StgState{..} -> s {ssMVars = IntMap.insert m (Just a) ssMVars}
    pure []

  _ -> fallback op args t tc

  -- TODO: tryPutMVar# :: MVar# s a -> a -> State# s -> (# State# s, Int# #)
  -- TODO: readMVar# :: MVar# s a -> State# s -> (# State# s, a #)
  -- TODO: tryReadMVar# :: MVar# s a -> State# s -> (# State# s, Int#, a #)
  -- TODO: sameMVar# :: MVar# s a -> MVar# s a -> Int#
  -- TODO: isEmptyMVar# :: MVar# s a -> State# s -> (# State# s, Int# #)


{-
------------------------------------------------------------------------
section "Synchronized Mutable Variables"
        {Operations on {\tt MVar\#}s. }
------------------------------------------------------------------------

primtype MVar# s a
        { A shared mutable variable ({\it not} the same as a {\tt MutVar\#}!).
        (Note: in a non-concurrent implementation, {\tt (MVar\# a)} can be
        represented by {\tt (MutVar\# (Maybe a))}.) }

primop  NewMVarOp "newMVar#"  GenPrimOp
   State# s -> (# State# s, MVar# s a #)
   {Create new {\tt MVar\#}; initially empty.}
   with
   out_of_line = True
   has_side_effects = True

primop  TakeMVarOp "takeMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, a #)
   {If {\tt MVar\#} is empty, block until it becomes full.
   Then remove and return its contents, and set it empty.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryTakeMVarOp "tryTakeMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int#, a #)
   {If {\tt MVar\#} is empty, immediately return with integer 0 and value undefined.
   Otherwise, return with integer 1 and contents of {\tt MVar\#}, and set {\tt MVar\#} empty.}
   with
   out_of_line      = True
   has_side_effects = True

primop  PutMVarOp "putMVar#" GenPrimOp
   MVar# s a -> a -> State# s -> State# s
   {If {\tt MVar\#} is full, block until it becomes empty.
   Then store value arg as its new contents.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryPutMVarOp "tryPutMVar#" GenPrimOp
   MVar# s a -> a -> State# s -> (# State# s, Int# #)
   {If {\tt MVar\#} is full, immediately return with integer 0.
    Otherwise, store value arg as {\tt MVar\#}'s new contents, and return with integer 1.}
   with
   out_of_line      = True
   has_side_effects = True

primop  ReadMVarOp "readMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, a #)
   {If {\tt MVar\#} is empty, block until it becomes full.
   Then read its contents without modifying the MVar, without possibility
   of intervention from other threads.}
   with
   out_of_line      = True
   has_side_effects = True

primop  TryReadMVarOp "tryReadMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int#, a #)
   {If {\tt MVar\#} is empty, immediately return with integer 0 and value undefined.
   Otherwise, return with integer 1 and contents of {\tt MVar\#}.}
   with
   out_of_line      = True
   has_side_effects = True

primop  SameMVarOp "sameMVar#" GenPrimOp
   MVar# s a -> MVar# s a -> Int#

primop  IsEmptyMVarOp "isEmptyMVar#" GenPrimOp
   MVar# s a -> State# s -> (# State# s, Int# #)
   {Return 1 if {\tt MVar\#} is empty; 0 otherwise.}
   with
   out_of_line = True
   has_side_effects = True
-}