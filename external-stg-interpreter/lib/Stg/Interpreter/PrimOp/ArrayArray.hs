{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.ArrayArray where

import qualified Data.Map as Map
import qualified Data.Vector as V

import Stg.Syntax
import Stg.Interpreter.Base

pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

lookupArrayArrIdx :: M sig m => ArrayArrIdx -> m (V.Vector AtomAddr)
lookupArrayArrIdx = \case
  ArrayMutArrIdx i -> lookupMutableArrayArray i
  ArrayArrIdx    i -> lookupArrayArray i

updateArrayArrIdx :: M sig m => ArrayArrIdx -> V.Vector AtomAddr -> m ()
updateArrayArrIdx m v = do
  modify $ \s@StgState{..} -> case m of
    ArrayMutArrIdx n -> s { ssMutableArrayArrays = Map.insert n v ssMutableArrayArrays }
    ArrayArrIdx    n -> s { ssArrayArrays        = Map.insert n v ssArrayArrays        }

evalPrimOp :: M sig m => PrimOpEval m -> Name -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimOp fallback op argsAddr t tc = do
 args <- getAtoms argsAddr
 case (op, args, argsAddr) of

  -- newArrayArray# :: Int# -> State# s -> (# State# s, MutableArrayArray# s #)
  ( "newArrayArray#", [IntV i, _s], _) -> do
    mutableArrayArrays <- gets ssMutableArrayArrays
    next <- freshMutableArrayArrayAddress
    result <- storeNewAtom $ MutableArrayArray $ ArrayMutArrIdx next
    let v = V.replicate (fromIntegral i) result -- HINT: initialize with self references
    modify $ \s -> s {ssMutableArrayArrays = Map.insert next v mutableArrayArrays}
    pure [result]

  -- sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Int#
  ( "sameMutableArrayArray#", [MutableArrayArray a, MutableArrayArray b], _) -> do
    allocAtoms [IntV $ if a == b then 1 else 0]

  -- unsafeFreezeArrayArray# :: MutableArrayArray# s -> State# s -> (# State# s, ArrayArray# #)
  ( "unsafeFreezeArrayArray#", [MutableArrayArray v, _s], _) -> do
    allocAtoms [ArrayArray v]

  -- sizeofArrayArray# :: ArrayArray# -> Int#
  ( "sizeofArrayArray#", [ArrayArray a], _) -> do
    v <- lookupArrayArrIdx a
    allocAtoms [IntV . fromIntegral $ V.length v]

  -- sizeofMutableArrayArray# :: MutableArrayArray# s -> Int#
  ( "sizeofMutableArrayArray#", [MutableArrayArray a], _) -> do
    v <- lookupArrayArrIdx a
    allocAtoms [IntV . fromIntegral $ V.length v]

  -- indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
  ( "indexByteArrayArray#", [ArrayArray a, IntV i], _) -> do
    v <- lookupArrayArrIdx a
    let x = v V.! (fromIntegral i)
    ByteArray{} <- getAtom x -- HINT: validation
    pure [x]

  -- indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#
  ( "indexArrayArrayArray#", [ArrayArray a, IntV i], _) -> do
    v <- lookupArrayArrIdx a
    let x = v V.! (fromIntegral i)
    ArrayArray{} <- getAtom x -- HINT: validation
    pure [x]

  -- readByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ByteArray# #)
  ( "readByteArrayArray#", [MutableArrayArray a, IntV i, _s], _) -> do
    v <- lookupArrayArrIdx a
    let x = v V.! (fromIntegral i)
    ByteArray{} <- getAtom x -- HINT: validation
    pure [x]

  -- readMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "readMutableByteArrayArray#", [MutableArrayArray a, IntV i, _s], _) -> do
    v <- lookupArrayArrIdx a
    let x = v V.! (fromIntegral i)
    MutableByteArray{} <- getAtom x -- HINT: validation
    pure [x]

  -- readArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ArrayArray# #)
  ( "readArrayArrayArray#", [MutableArrayArray a, IntV i, _s], _) -> do
    v <- lookupArrayArrIdx a
    let x = v V.! (fromIntegral i)
    ArrayArray{} <- getAtom x -- HINT: validation
    pure [x]

  -- readMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
  ( "readMutableArrayArrayArray#", [MutableArrayArray a, IntV i, _s], _) -> do
    v <- lookupArrayArrIdx a
    let x = v V.! (fromIntegral i)
    MutableArrayArray{} <- getAtom x -- HINT: validation
    pure [x]

  -- writeByteArrayArray# :: MutableArrayArray# s -> Int# -> ByteArray# -> State# s -> State# s
  ( "writeByteArrayArray#", [MutableArrayArray m, IntV i, ByteArray{}, _s], [_, _, a, _]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(fromIntegral i, a)])
    pure []

  -- writeMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> MutableByteArray# s -> State# s -> State# s
  ( "writeMutableByteArrayArray#", [MutableArrayArray m, IntV i, MutableByteArray{}, _s], [_, _, a, _]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(fromIntegral i, a)])
    pure []

  -- writeArrayArrayArray# :: MutableArrayArray# s -> Int# -> ArrayArray# -> State# s -> State# s
  ( "writeArrayArrayArray#", [MutableArrayArray m, IntV i, ArrayArray{}, _s], [_, _, a, _]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(fromIntegral i, a)])
    pure []

  -- writeMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> State# s -> State# s
  ( "writeMutableArrayArrayArray#", [MutableArrayArray m, IntV i, MutableArrayArray{}, _s], [_, _, a, _]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(fromIntegral i, a)])
    pure []

  -- copyArrayArray# :: ArrayArray# -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyArrayArray#", [ArrayArray src, IntV os, MutableArrayArray dst, IntV od, IntV n, _s], _) -> do
    vsrc <- lookupArrayArrIdx src
    vdst <- lookupArrayArrIdx dst
    let vdst' = vdst V.// [ (fromIntegral di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! (fromIntegral si)
                          ]
    updateArrayArrIdx dst vdst'
    pure []

  -- copyMutableArrayArray#" :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyMutableArrayArray#", [MutableArrayArray src, IntV os, MutableArrayArray dst, IntV od, IntV n, _s], _) -> do
    vsrc <- lookupArrayArrIdx src
    vdst <- lookupArrayArrIdx dst
    let vdst' = vdst V.// [ (fromIntegral di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! (fromIntegral si)
                          ]
    updateArrayArrIdx dst vdst'
    pure []

  _ -> fallback op argsAddr t tc
