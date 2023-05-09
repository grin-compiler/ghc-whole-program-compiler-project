{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.SmallArray where

import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

lookupSmallArrIdx :: SmallArrIdx -> M (V.Vector Atom)
lookupSmallArrIdx = \case
  SmallMutArrIdx i -> lookupSmallMutableArray i
  SmallArrIdx    i -> lookupSmallArray i

updateSmallArrIdx :: SmallArrIdx -> V.Vector Atom -> M ()
updateSmallArrIdx m v = do
  modify' $ \s@StgState{..} -> case m of
    SmallMutArrIdx n -> s { ssSmallMutableArrays = IntMap.insert n v ssSmallMutableArrays }
    SmallArrIdx    n -> s { ssSmallArrays        = IntMap.insert n v ssSmallArrays }

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newSmallArray# :: Int# -> a -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "newSmallArray#", [IntV i, a, _s]) -> do
    smallMutableArrays <- gets ssSmallMutableArrays
    next <- gets ssNextSmallMutableArray
    let v = V.replicate (fromIntegral i) a
    modify' $ \s@StgState{..} -> s {ssSmallMutableArrays = IntMap.insert next v ssSmallMutableArrays, ssNextSmallMutableArray = succ next}
    pure [SmallMutableArray $ SmallMutArrIdx next]

  -- shrinkSmallMutableArray# :: SmallMutableArray# s a -> Int# -> State# s -> State# s
  ( "shrinkSmallMutableArray#",[SmallMutableArray src,IntV n,_s]) -> do
    v <- lookupSmallArrIdx src
    updateSmallArrIdx src $ V.slice 0 (fromIntegral n) v
    pure []

  -- readSmallArray# :: SmallMutableArray# s a -> Int# -> State# s -> (# State# s, a #)
  ( "readSmallArray#", [SmallMutableArray a, IntV i, _s]) -> do
    v <- lookupSmallArrIdx a
    pure [v V.! (fromIntegral i)]

  -- writeSmallArray#" :: SmallMutableArray# s a -> Int# -> a -> State# s -> State# s
  ( "writeSmallArray#", [SmallMutableArray m, IntV i, a, _s]) -> do
    v <- lookupSmallArrIdx m
    updateSmallArrIdx m (v V.// [(fromIntegral i, a)])
    pure []

  -- sizeofSmallArray# :: SmallArray# a -> Int#
  ( "sizeofSmallArray#", [SmallArray a]) -> do
    v <- lookupSmallArrIdx a
    pure [IntV . fromIntegral $ V.length v]

  -- sizeofSmallMutableArray# :: SmallMutableArray# s a -> Int#
  ( "sizeofSmallMutableArray#", [SmallMutableArray a]) -> do
    -- DEPRECATED: Use 'getSizeofSmallMutableArray#' instead
    v <- lookupSmallArrIdx a
    pure [IntV . fromIntegral $ V.length v]

  -- getSizeofSmallMutableArray# :: SmallMutableArray# s a -> State# s -> (# State# s, Int# #)
  ( "getSizeofSmallMutableArray#", [SmallMutableArray a, _s]) -> do
    v <- lookupSmallArrIdx a
    pure [IntV . fromIntegral $ V.length v]

  -- indexSmallArray# :: SmallArray# a -> Int# -> (# a #)
  ( "indexSmallArray#", [SmallArray a, IntV i]) -> do
    v <- lookupSmallArrIdx a
    pure [v V.! (fromIntegral i)]

  -- unsafeFreezeSmallArray# :: SmallMutableArray# s a -> State# s -> (# State# s, SmallArray# a #)
  ( "unsafeFreezeSmallArray#", [SmallMutableArray v, _s]) -> do
    pure [SmallArray v]

  -- unsafeThawSmallArray# :: SmallArray# a -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "unsafeThawSmallArray#", [SmallArray v, _s]) -> do
    pure [SmallMutableArray v]

  -- copySmallArray# :: SmallArray# a -> Int# -> SmallMutableArray# s a -> Int# -> Int# -> State# s -> State# s
  ( "copySmallArray#", [SmallArray src, IntV os, SmallMutableArray dst, IntV od, IntV n, _s]) -> do
    vsrc <- lookupSmallArrIdx src
    vdst <- lookupSmallArrIdx dst
    let vdst' = vdst V.// [ (fromIntegral di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! (fromIntegral si)
                          ]
    updateSmallArrIdx dst vdst'
    pure []

  -- copySmallMutableArray# :: SmallMutableArray# s a -> Int# -> SmallMutableArray# s a -> Int# -> Int# -> State# s -> State# s
  ( "copySmallMutableArray#", [SmallMutableArray src, IntV os, SmallMutableArray dst, IntV od, IntV n, _s]) -> do
    vsrc <- lookupSmallArrIdx src
    vdst <- lookupSmallArrIdx dst
    let vdst' = vdst V.// [ (fromIntegral di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! (fromIntegral si)
                          ]
    updateSmallArrIdx dst vdst'
    pure []

  -- cloneSmallArray# :: SmallArray# a -> Int# -> Int# -> SmallArray# a
  ( "cloneSmallArray#", [SmallArray src, IntV o, IntV n]) -> do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallArray
      in ([SmallArray $ SmallArrIdx next], s' {ssSmallArrays = IntMap.insert next vdst ssSmallArrays, ssNextSmallArray = succ next})

  -- cloneSmallMutableArray# :: SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "cloneSmallMutableArray#", [SmallMutableArray src, IntV o, IntV n, _s]) -> do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallMutableArray
      in ([SmallMutableArray $ SmallMutArrIdx next], s' {ssSmallMutableArrays = IntMap.insert next vdst ssSmallMutableArrays, ssNextSmallMutableArray = succ next})

  -- freezeSmallArray# :: SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallArray# a #)
  ( "freezeSmallArray#", [SmallMutableArray src, IntV o, IntV n, _s]) -> do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallArray
      in ([SmallArray $ SmallArrIdx next], s' {ssSmallArrays = IntMap.insert next vdst ssSmallArrays, ssNextSmallArray = next})

  -- thawSmallArray# :: SmallArray# a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "thawSmallArray#", [SmallArray src, IntV o, IntV n, _s]) -> do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallMutableArray
      in ([SmallMutableArray $ SmallMutArrIdx next], s' {ssSmallMutableArrays = IntMap.insert next vdst ssSmallMutableArrays, ssNextSmallMutableArray = succ next})

  -- casSmallArray# :: SmallMutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
  -- NOTE: CPU atomic
  ( "casSmallArray#", [SmallMutableArray src, IntV o, old, new, _s]) -> do
    vsrc <- lookupSmallArrIdx src
    let current = vsrc V.! (fromIntegral o)
    if current == old
      then do
        updateSmallArrIdx src (vsrc V.// [(fromIntegral o, new)])
        pure [IntV 0, new]
      else do
        pure [IntV 1, current]

  -- OBSOLETE from GHC 9.4
  -- sameSmallMutableArray# :: SmallMutableArray# s a -> SmallMutableArray# s a -> Int#
  ( "sameSmallMutableArray#", [SmallMutableArray a, SmallMutableArray b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  _ -> fallback op args t tc
