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

lookupSmallArrIdx :: SmallArrIdx -> M (V.Vector AtomAddr)
lookupSmallArrIdx = \case
  SmallMutArrIdx i -> lookupSmallMutableArray i
  SmallArrIdx    i -> lookupSmallArray i

updateSmallArrIdx :: SmallArrIdx -> V.Vector AtomAddr -> M ()
updateSmallArrIdx m v = do
  modify' $ \s@StgState{..} -> case m of
    SmallMutArrIdx n -> s { ssSmallMutableArrays = IntMap.insert n v ssSmallMutableArrays }
    SmallArrIdx    n -> s { ssSmallArrays        = IntMap.insert n v ssSmallArrays }

evalPrimOp :: PrimOpEval -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> M [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args, argsAddr) of

  -- newSmallArray# :: Int# -> a -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "newSmallArray#", [IntV i, _a, _s], [_, a, _]) -> do
    smallMutableArrays <- gets ssSmallMutableArrays
    next <- gets ssNextSmallMutableArray
    let v = V.replicate (fromIntegral i) a
    modify' $ \s@StgState{..} -> s {ssSmallMutableArrays = IntMap.insert next v ssSmallMutableArrays, ssNextSmallMutableArray = succ next}
    allocAtoms [SmallMutableArray $ SmallMutArrIdx next]

  -- sameSmallMutableArray# :: SmallMutableArray# s a -> SmallMutableArray# s a -> Int#
  ( "sameMutableArray#", [SmallMutableArray a, SmallMutableArray b], _) -> do
    allocAtoms [IntV $ if a == b then 1 else 0]

  -- shrinkSmallMutableArray# :: SmallMutableArray# s a -> Int# -> State# s -> State# s
  ( "shrinkSmallMutableArray#",[SmallMutableArray src, IntV n, _s], _) -> do
    v <- lookupSmallArrIdx src
    updateSmallArrIdx src $ V.slice 0 (fromIntegral n) v
    pure []

  -- readSmallArray# :: SmallMutableArray# s a -> Int# -> State# s -> (# State# s, a #)
  ( "readSmallArray#", [SmallMutableArray a, IntV i, _s], _) -> do
    v <- lookupSmallArrIdx a
    pure [v V.! (fromIntegral i)]

  -- writeSmallArray#" :: SmallMutableArray# s a -> Int# -> a -> State# s -> State# s
  ( "writeSmallArray#", [SmallMutableArray m, IntV i, _a, _s], [_, _, a, _]) -> do
    v <- lookupSmallArrIdx m
    updateSmallArrIdx m (v V.// [(fromIntegral i, a)])
    pure []

  -- sizeofSmallArray# :: SmallArray# a -> Int#
  ( "sizeofSmallArray#", [SmallArray a], _) -> do
    v <- lookupSmallArrIdx a
    allocAtoms [IntV . fromIntegral $ V.length v]

  -- sizeofSmallMutableArray# :: SmallMutableArray# s a -> Int#
  ( "sizeofSmallMutableArray#", [SmallMutableArray a], _) -> do
    -- DEPRECATED: Use 'getSizeofSmallMutableArray#' instead
    v <- lookupSmallArrIdx a
    allocAtoms [IntV . fromIntegral $ V.length v]

  -- getSizeofSmallMutableArray# :: SmallMutableArray# s a -> State# s -> (# State# s, Int# #)
  ( "getSizeofSmallMutableArray#", [SmallMutableArray a, _s], _) -> do
    v <- lookupSmallArrIdx a
    allocAtoms [IntV . fromIntegral $ V.length v]

  -- indexSmallArray# :: SmallArray# a -> Int# -> (# a #)
  ( "indexSmallArray#", [SmallArray a, IntV i], _) -> do
    v <- lookupSmallArrIdx a
    pure [v V.! (fromIntegral i)]

  -- unsafeFreezeSmallArray# :: SmallMutableArray# s a -> State# s -> (# State# s, SmallArray# a #)
  ( "unsafeFreezeSmallArray#", [SmallMutableArray v, _s], _) -> do
    allocAtoms [SmallArray v]

  -- unsafeThawSmallArray# :: SmallArray# a -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "unsafeThawSmallArray#", [SmallArray v, _s], _) -> do
    allocAtoms [SmallMutableArray v]

  -- copySmallArray# :: SmallArray# a -> Int# -> SmallMutableArray# s a -> Int# -> Int# -> State# s -> State# s
  ( "copySmallArray#", [SmallArray src, IntV os, SmallMutableArray dst, IntV od, IntV n, _s], _) -> do
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
  ( "copySmallMutableArray#", [SmallMutableArray src, IntV os, SmallMutableArray dst, IntV od, IntV n, _s], _) -> do
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
  ( "cloneSmallArray#", [SmallArray src, IntV o, IntV n], _) -> allocAtoms =<< do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallArray
      in ([SmallArray $ SmallArrIdx next], s' {ssSmallArrays = IntMap.insert next vdst ssSmallArrays, ssNextSmallArray = succ next})

  -- cloneSmallMutableArray# :: SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "cloneSmallMutableArray#", [SmallMutableArray src, IntV o, IntV n, _s], _) -> allocAtoms =<< do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallMutableArray
      in ([SmallMutableArray $ SmallMutArrIdx next], s' {ssSmallMutableArrays = IntMap.insert next vdst ssSmallMutableArrays, ssNextSmallMutableArray = succ next})

  -- freezeSmallArray# :: SmallMutableArray# s a -> Int# -> Int# -> State# s -> (# State# s, SmallArray# a #)
  ( "freezeSmallArray#", [SmallMutableArray src, IntV o, IntV n, _s], _) -> allocAtoms =<< do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallArray
      in ([SmallArray $ SmallArrIdx next], s' {ssSmallArrays = IntMap.insert next vdst ssSmallArrays, ssNextSmallArray = next})

  -- thawSmallArray# :: SmallArray# a -> Int# -> Int# -> State# s -> (# State# s, SmallMutableArray# s a #)
  ( "thawSmallArray#", [SmallArray src, IntV o, IntV n, _s], _) -> allocAtoms =<< do
    vsrc <- lookupSmallArrIdx src
    let vdst = V.slice (fromIntegral o) (fromIntegral n) vsrc
    state $ \s'@StgState{..} ->
      let next = ssNextSmallMutableArray
      in ([SmallMutableArray $ SmallMutArrIdx next], s' {ssSmallMutableArrays = IntMap.insert next vdst ssSmallMutableArrays, ssNextSmallMutableArray = succ next})

  -- casSmallArray# :: SmallMutableArray# s a -> Int# -> a -> a -> State# s -> (# State# s, Int#, a #)
  -- NOTE: CPU atomic
  ( "casSmallArray#", [SmallMutableArray src, IntV o, old, new, _s], [_, _, oldAddr, newAddr, _]) -> do
    vsrc <- lookupSmallArrIdx src
    current <- getAtom (vsrc V.! (fromIntegral o))
    if current == old
      then do
        updateSmallArrIdx src (vsrc V.// [(fromIntegral o, newAddr)])
        (:) <$> storeNewAtom (IntV 0) <*> pure [newAddr]
      else do
        (:) <$> storeNewAtom (IntV 1) <*> pure [oldAddr]

  _ -> fallback op argsAddr t tc
