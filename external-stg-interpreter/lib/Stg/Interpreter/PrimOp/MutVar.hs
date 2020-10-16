{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.MutVar where

import Control.Monad.State
import qualified Data.IntMap as IntMap

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i = Literal (LitNumber LitNumInt i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newMutVar# :: a -> State# s -> (# State# s, MutVar# s a #)
  ("newMutVar#", [a, _s]) -> do
    mutVars <- gets ssMutVars
    let next = IntMap.size mutVars
    modify' $ \s -> s {ssMutVars = IntMap.insert next a mutVars}
    pure [MutVar next]

  -- readMutVar# :: MutVar# s a -> State# s -> (# State# s, a #)
  ("readMutVar#", [MutVar m, _s]) -> do
    a <- lookupMutVar m
    pure [a]

  -- writeMutVar# :: MutVar# s a -> a -> State# s -> State# s
  ("writeMutVar#", [MutVar m, a, _s]) -> do
    _ <- lookupMutVar m -- check existence
    modify' $ \s@StgState{..} -> s {ssMutVars = IntMap.insert m a ssMutVars}
    pure []

  -- sameMutVar# :: MutVar# s a -> MutVar# s a -> Int#
  ("sameMutVar#", [MutVar a, MutVar b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  -- TODO: atomicModifyMutVar2# :: MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
  -- TODO: atomicModifyMutVar_# :: MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
  -- TODO: casMutVar# :: MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)

  _ -> fallback op args t tc

{-
-- Note [Why not an unboxed tuple in atomicModifyMutVar2#?]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Looking at the type of atomicModifyMutVar2#, one might wonder why
-- it doesn't return an unboxed tuple. e.g.,
--
--   MutVar# s a -> (a -> (# a, b #)) -> State# s -> (# State# s, a, (# a, b #) #)
--
-- The reason is that atomicModifyMutVar2# relies on laziness for its atomicity.
-- Given a MutVar# containing x, atomicModifyMutVar2# merely replaces
-- its contents with a thunk of the form (fst (f x)). This can be done using an
-- atomic compare-and-swap as it is merely replacing a pointer.

primop  AtomicModifyMutVar2Op "atomicModifyMutVar2#" GenPrimOp
   MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
   { Modify the contents of a {\tt MutVar\#}, returning the previous
     contents and the result of applying the given function to the
     previous contents. Note that this isn't strictly
     speaking the correct type for this function; it should really be
     {\tt MutVar\# s a -> (a -> (a,b)) -> State\# s -> (\# State\# s, a, (a, b) \#)},
     but we don't know about pairs here. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  AtomicModifyMutVar_Op "atomicModifyMutVar_#" GenPrimOp
   MutVar# s a -> (a -> a) -> State# s -> (# State# s, a, a #)
   { Modify the contents of a {\tt MutVar\#}, returning the previous
     contents and the result of applying the given function to the
     previous contents. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

primop  CasMutVarOp "casMutVar#" GenPrimOp
  MutVar# s a -> a -> a -> State# s -> (# State# s, Int#, a #)
   with
   out_of_line = True
   has_side_effects = True
-}
