{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Stg.Interpreter.PrimOp.ArrayArray where

import           Control.Applicative  (Applicative (..))
import           Control.Monad.State  (gets, modify')

import Prelude (Enum (..))
import           Data.Eq              (Eq (..))
import           Data.Function        (($))
import           Data.Int             (Int)
import qualified Data.IntMap          as IntMap
import           Data.Maybe           (Maybe)
import qualified Data.Vector          as V
import           Data.Word            (Word)

import           GHC.Num              (Num (..))

import           Stg.Interpreter.Base (ArrayArrIdx (..), Atom (..), M, PrimOpEval, StgState (..), lookupArrayArray,
                                       lookupMutableArrayArray)
import           Stg.Syntax           (Name, TyCon, Type)

pattern IntV :: Int -> Atom
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV :: Word -> Atom
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V :: Word -> Atom
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

lookupArrayArrIdx :: ArrayArrIdx -> M (V.Vector Atom)
lookupArrayArrIdx = \case
  ArrayMutArrIdx i -> lookupMutableArrayArray i
  ArrayArrIdx    i -> lookupArrayArray i

updateArrayArrIdx :: ArrayArrIdx -> V.Vector Atom -> M ()
updateArrayArrIdx m v = do
  modify' $ \s@StgState{..} -> case m of
    ArrayMutArrIdx n -> s { ssMutableArrayArrays = IntMap.insert n v ssMutableArrayArrays }
    ArrayArrIdx    n -> s { ssArrayArrays        = IntMap.insert n v ssArrayArrays        }

-- HINT: the whole 'ArrayArray' is OBSOLETE from GHC 9.4
evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- newArrayArray# :: Int# -> State# s -> (# State# s, MutableArrayArray# s #)
  ( "newArrayArray#", [IntV i, _s]) -> do
    mutableArrayArrays <- gets ssMutableArrayArrays
    next <- gets ssNextMutableArrayArray
    let result = MutableArrayArray $ ArrayMutArrIdx next
        v      = V.replicate i result -- wow
    modify' $ \s -> s {ssMutableArrayArrays = IntMap.insert next v mutableArrayArrays, ssNextMutableArrayArray = succ next}
    pure [result]

  -- sameMutableArrayArray# :: MutableArrayArray# s -> MutableArrayArray# s -> Int#
  ( "sameMutableArrayArray#", [MutableArrayArray a, MutableArrayArray b]) -> do
    pure [IntV $ if a == b then 1 else 0]

  -- unsafeFreezeArrayArray# :: MutableArrayArray# s -> State# s -> (# State# s, ArrayArray# #)
  ( "unsafeFreezeArrayArray#", [MutableArrayArray v, _s]) -> do
    pure [ArrayArray v]

  -- sizeofArrayArray# :: ArrayArray# -> Int#
  ( "sizeofArrayArray#", [ArrayArray a]) -> do
    v <- lookupArrayArrIdx a
    pure [IntV $ V.length v]

  -- sizeofMutableArrayArray# :: MutableArrayArray# s -> Int#
  ( "sizeofMutableArrayArray#", [MutableArrayArray a]) -> do
    v <- lookupArrayArrIdx a
    pure [IntV $ V.length v]

  -- indexByteArrayArray# :: ArrayArray# -> Int# -> ByteArray#
  ( "indexByteArrayArray#", [ArrayArray a, IntV i]) -> do
    v <- lookupArrayArrIdx a
    let x@ByteArray{} = v V.! i
    pure [x]

  -- indexArrayArrayArray# :: ArrayArray# -> Int# -> ArrayArray#
  ( "indexArrayArrayArray#", [ArrayArray a, IntV i]) -> do
    v <- lookupArrayArrIdx a
    let x@ArrayArray{} = v V.! i
    pure [x]

  -- readByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ByteArray# #)
  ( "readByteArrayArray#", [MutableArrayArray a, IntV i, _s]) -> do
    v <- lookupArrayArrIdx a
    let x@ByteArray{} = v V.! i
    pure [x]

  -- readMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "readMutableByteArrayArray#", [MutableArrayArray a, IntV i, _s]) -> do
    v <- lookupArrayArrIdx a
    let x@MutableByteArray{} = v V.! i
    pure [x]

  -- readArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, ArrayArray# #)
  ( "readArrayArrayArray#", [MutableArrayArray a, IntV i, _s]) -> do
    v <- lookupArrayArrIdx a
    let x@ArrayArray{} = v V.! i
    pure [x]

  -- readMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> State# s -> (# State# s, MutableArrayArray# s #)
  ( "readMutableArrayArrayArray#", [MutableArrayArray a, IntV i, _s]) -> do
    v <- lookupArrayArrIdx a
    let x@MutableArrayArray{} = v V.! i
    pure [x]

  -- writeByteArrayArray# :: MutableArrayArray# s -> Int# -> ByteArray# -> State# s -> State# s
  ( "writeByteArrayArray#", [MutableArrayArray m, IntV i, a@ByteArray{}, _s]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(i, a)])
    pure []

  -- writeMutableByteArrayArray# :: MutableArrayArray# s -> Int# -> MutableByteArray# s -> State# s -> State# s
  ( "writeMutableByteArrayArray#", [MutableArrayArray m, IntV i, a@MutableByteArray{}, _s]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(i, a)])
    pure []

  -- writeArrayArrayArray# :: MutableArrayArray# s -> Int# -> ArrayArray# -> State# s -> State# s
  ( "writeArrayArrayArray#", [MutableArrayArray m, IntV i, a@ArrayArray{}, _s]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(i, a)])
    pure []

  -- writeMutableArrayArrayArray# :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> State# s -> State# s
  ( "writeMutableArrayArrayArray#", [MutableArrayArray m, IntV i, a@MutableArrayArray{}, _s]) -> do
    v <- lookupArrayArrIdx m
    updateArrayArrIdx m (v V.// [(i, a)])
    pure []

  -- copyArrayArray# :: ArrayArray# -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyArrayArray#", [ArrayArray src, IntV os, MutableArrayArray dst, IntV od, IntV n, _s]) -> do
    vsrc <- lookupArrayArrIdx src
    vdst <- lookupArrayArrIdx dst
    let vdst' = vdst V.// [ (di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! si
                          ]
    updateArrayArrIdx dst vdst'
    pure []

  -- copyMutableArrayArray#" :: MutableArrayArray# s -> Int# -> MutableArrayArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyMutableArrayArray#", [MutableArrayArray src, IntV os, MutableArrayArray dst, IntV od, IntV n, _s]) -> do
    vsrc <- lookupArrayArrIdx src
    vdst <- lookupArrayArrIdx dst
    let vdst' = vdst V.// [ (di, v)
                          | i <- [ 0 .. n-1 ]
                          , let si = os + i
                          , let di = od + i
                          , let v = vsrc V.! si
                          ]
    updateArrayArrIdx dst vdst'
    pure []

  _ -> fallback op args t tc
