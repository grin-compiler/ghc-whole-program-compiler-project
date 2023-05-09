{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Array where

import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

lookupArrIdx :: ArrIdx -> M (V.Vector Atom)
lookupArrIdx = \case
  MutArrIdx i -> lookupMutableArray i
  ArrIdx    i -> lookupArray i

updateArrIdx :: ArrIdx -> V.Vector Atom -> M ()
updateArrIdx m v = do
  modify' $ \s@StgState{..} -> case m of
    MutArrIdx n -> s { ssMutableArrays = IntMap.insert n v ssMutableArrays }
    ArrIdx    n -> s { ssArrays        = IntMap.insert n v ssArrays }

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newArray# :: Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
  ( "newArray#", [IntV i, a, _s]) -> do
    mutableArrays <- gets ssMutableArrays
    next <- gets ssNextMutableArray
    let v = V.replicate i a
    modify' $ \s -> s {ssMutableArrays = IntMap.insert next v mutableArrays, ssNextMutableArray = succ next}
    pure [MutableArray $ MutArrIdx next]

  -- readArray# :: MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
  ( "readArray#", [MutableArray a, IntV i, _s]) -> do
    v <- lookupArrIdx a
    pure [v V.! i]

  -- writeArray# :: MutableArray# s a -> Int# -> a -> State# s -> State# s
  ( "writeArray#", [MutableArray m, IntV i, a, _s]) -> do
    v <- lookupArrIdx m
    updateArrIdx m (v V.// [(i, a)])
    pure []

  -- sizeofArray# :: Array# a -> Int#
  ( "sizeofArray#", [Array a]) -> do
    v <- lookupArrIdx a
    pure [IntV $ V.length v]

  -- sizeofMutableArray# :: MutableArray# s a -> Int#
  ( "sizeofMutableArray#", [MutableArray a]) -> do
    v <- lookupArrIdx a
    pure [IntV $ V.length v]

  -- indexArray# :: Array# a -> Int# -> (# a #)
  ( "indexArray#", [Array a, IntV i]) -> do
    v <- lookupArrIdx a
    pure [v V.! i]

  -- unsafeFreezeArray# :: MutableArray# s a -> State# s -> (# State# s, Array# a #)
  ( "unsafeFreezeArray#", [MutableArray v, _s]) -> do
    pure [Array v]

  -- unsafeThawArray# :: Array# a -> State# s -> (# State# s, MutableArray# s a #)
  ( "unsafeThawArray#", [Array v, _s]) -> do
    pure [MutableArray v]

  -- copyArray# :: Array# a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  ( "copyArray#", [Array src, IntV os, MutableArray dst, IntV od, IntV n, _s]) -> do
    vsrc <- lookupArrIdx src
    vdst <- lookupArrIdx dst
    let vdst' = vdst V.// [ (di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! si
                          ]
    updateArrIdx dst vdst'
    pure []

  -- copyMutableArray# :: MutableArray# s a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  ( "copyMutableArray#", [ MutableArray src, IntV os, MutableArray dst, IntV od, IntV n, _s]) -> do
    vsrc <- lookupArrIdx src
    vdst <- lookupArrIdx dst
    let vdst' = vdst V.// [ (di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! si
                          ]
    updateArrIdx dst vdst'
    pure []

  -- cloneArray# :: Array# a -> Int# -> Int# -> Array# a
  ( "cloneArray#", [Array src, IntV o, IntV n]) -> do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextArray
      in ([Array $ ArrIdx next], s' {ssArrays = IntMap.insert next vdst ssArrays, ssNextArray = succ next})

  -- cloneMutableArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  ( "cloneMutableArray#", [ MutableArray src, IntV o, IntV n, _s]) -> do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextMutableArray
      in ([MutableArray $ MutArrIdx next], s' {ssMutableArrays = IntMap.insert next vdst ssMutableArrays, ssNextMutableArray = succ next})

  -- freezeArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, Array# a #)
  ( "freezeArray#", [MutableArray src, IntV o, IntV n, _s]) -> do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextArray
      in ([Array $ ArrIdx next], s' {ssArrays = IntMap.insert next vdst ssArrays, ssNextArray = succ next})

  -- thawArray# :: Array# a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  ( "thawArray#", [Array src, IntV o, IntV n, _s]) -> do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextMutableArray
      in ([MutableArray $ MutArrIdx next], s' {ssMutableArrays = IntMap.insert next vdst ssMutableArrays, ssNextMutableArray = succ next})

  -- casArray# :: MutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
  -- NOTE: CPU atomic
  ( "casArray#", [MutableArray src, IntV o, old, new, _s]) -> do
    vsrc <- lookupArrIdx src
    let current = vsrc V.! o
    if current == old
      then do
        updateArrIdx src (vsrc V.// [(o, new)])
        pure [IntV 0, new]
      else do
        pure [IntV 1, current]

  -- OBSOLETE from GHC 9.4
  -- sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Int#
  ( "sameMutableArray#", [MutableArray a, MutableArray b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  _ -> fallback op args t tc
