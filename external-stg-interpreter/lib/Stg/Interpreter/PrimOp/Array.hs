{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Array where

import qualified Data.IntMap as IntMap
import qualified Data.Vector as V

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)

lookupArrIdx :: M sig m => ArrIdx -> m (V.Vector AtomAddr)
lookupArrIdx = \case
  MutArrIdx i -> lookupMutableArray i
  ArrIdx    i -> lookupArray i

updateArrIdx :: M sig m => ArrIdx -> V.Vector AtomAddr -> m ()
updateArrIdx m v = do
  modify $ \s@StgState{..} -> case m of
    MutArrIdx n -> s { ssMutableArrays = IntMap.insert n v ssMutableArrays }
    ArrIdx    n -> s { ssArrays        = IntMap.insert n v ssArrays }

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args, argsAddr) of

  -- newArray# :: Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
  ( "newArray#", [IntV i, _a, _s], [_, a, _]) -> do
    next <- freshMutableArrayAddress
    let v = V.replicate i a
    mutableArrays <- gets ssMutableArrays
    modify $ \s -> s {ssMutableArrays = IntMap.insert next v mutableArrays}
    allocAtoms [MutableArray $ MutArrIdx next]

  -- sameMutableArray# :: MutableArray# s a -> MutableArray# s a -> Int#
  ( "sameMutableArray#", [MutableArray a, MutableArray b], _) -> do
    allocAtoms [IntV $ if a == b then 1 else 0]

  -- readArray# :: MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
  ( "readArray#", [MutableArray a, IntV i, _s], _) -> do
    v <- lookupArrIdx a
    pure [v V.! i]

  -- writeArray# :: MutableArray# s a -> Int# -> a -> State# s -> State# s
  ( "writeArray#", [MutableArray m, IntV i, _a, _s], [_, _, a, _]) -> do
    v <- lookupArrIdx m
    updateArrIdx m (v V.// [(i, a)])
    pure []

  -- sizeofArray# :: Array# a -> Int#
  ( "sizeofArray#", [Array a], _) -> do
    v <- lookupArrIdx a
    allocAtoms [IntV $ V.length v]

  -- sizeofMutableArray# :: MutableArray# s a -> Int#
  ( "sizeofMutableArray#", [MutableArray a], _) -> do
    v <- lookupArrIdx a
    allocAtoms [IntV $ V.length v]

  -- indexArray# :: Array# a -> Int# -> (# a #)
  ( "indexArray#", [Array a, IntV i], _) -> do
    v <- lookupArrIdx a
    pure [v V.! i]

  -- unsafeFreezeArray# :: MutableArray# s a -> State# s -> (# State# s, Array# a #)
  ( "unsafeFreezeArray#", [MutableArray v, _s], _) -> do
    allocAtoms [Array v]

  -- unsafeThawArray# :: Array# a -> State# s -> (# State# s, MutableArray# s a #)
  ( "unsafeThawArray#", [Array v, _s], _) -> do
    allocAtoms [MutableArray v]

  -- copyArray# :: Array# a -> Int# -> MutableArray# s a -> Int# -> Int# -> State# s -> State# s
  ( "copyArray#", [Array src, IntV os, MutableArray dst, IntV od, IntV n, _s], _) -> do
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
  ( "copyMutableArray#", [ MutableArray src, IntV os, MutableArray dst, IntV od, IntV n, _s], _) -> do
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
  ( "cloneArray#", [Array src, IntV o, IntV n], _) -> allocAtoms =<< do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    next <- freshArrayAddress
    state $ \s'@StgState{..} ->
      ( s' {ssArrays = IntMap.insert next vdst ssArrays}
      , [Array $ ArrIdx next]
      )

  -- cloneMutableArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  ( "cloneMutableArray#", [MutableArray src, IntV o, IntV n, _s], _) -> allocAtoms =<< do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    next <- freshMutableArrayAddress
    state $ \s'@StgState{..} ->
      ( s' {ssMutableArrays = IntMap.insert next vdst ssMutableArrays}
      , [MutableArray $ MutArrIdx next]
      )

  -- freezeArray# :: MutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, Array# a #)
  ( "freezeArray#", [MutableArray src, IntV o, IntV n, _s], _) -> allocAtoms =<< do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    next <- freshArrayAddress
    state $ \s'@StgState{..} ->
      ( s' {ssArrays = IntMap.insert next vdst ssArrays}
      , [Array $ ArrIdx next]
      )

  -- thawArray# :: Array# a -> Int# -> Int# -> State# s -> (# State# s, MutableArray# s a #)
  ( "thawArray#", [Array src, IntV o, IntV n, _s], _) -> allocAtoms =<< do
    vsrc <- lookupArrIdx src
    let vdst = V.slice o n vsrc
    next <- freshMutableArrayAddress
    state $ \s'@StgState{..} ->
      ( s' {ssMutableArrays = IntMap.insert next vdst ssMutableArrays}
      , [MutableArray $ MutArrIdx next]
      )

  -- casArray# :: MutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
  -- NOTE: CPU atomic
  ( "casArray#", [MutableArray src, IntV o, old, new, _s], [_, _, oldAddr, newAddr, _]) -> do
    vsrc <- lookupArrIdx src
    current <- getAtom (vsrc V.! o)
    if current == old
      then do
        updateArrIdx src (vsrc V.// [(o, newAddr)])
        (:) <$> storeNewAtom (IntV 0) <*> pure [newAddr]
      else do
        (:) <$> storeNewAtom (IntV 1) <*> pure [oldAddr]

  _ -> fallback op argsAddr t tc
