{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Prefetch where

import Stg.Syntax
import Stg.Interpreter.Base

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- level 3

  -- prefetchByteArray3# :: ByteArray# -> Int# ->  State# s -> State# s
  ( "prefetchByteArray3#", [_ba, _i, _s]) -> pure []

  -- prefetchMutableByteArray3# :: MutableByteArray# s -> Int# -> State# s -> State# s
  ( "prefetchMutableByteArray3#", [_ba, _i, _s]) -> pure []

  -- prefetchAddr3# :: Addr# -> Int# -> State# s -> State# s
  ( "prefetchAddr3#", [_a, _i, _s]) -> pure []

  -- prefetchValue3# :: a -> State# s -> State# s
  ( "prefetchValue3#", [_v, _s]) -> pure []

  -- level 2

  -- prefetchByteArray2# :: ByteArray# -> Int# ->  State# s -> State# s
  ( "prefetchByteArray2#", [_ba, _i, _s]) -> pure []

  -- prefetchMutableByteArray2# :: MutableByteArray# s -> Int# -> State# s -> State# s
  ( "prefetchMutableByteArray2#", [_ba, _i, _s]) -> pure []

  -- prefetchAddr2# :: Addr# -> Int# ->  State# s -> State# s
  ( "prefetchAddr2#", [_a, _i, _s]) -> pure []

  -- prefetchValue2# :: a ->  State# s -> State# s
  ( "prefetchValue2#", [_v, _s]) -> pure []

  -- level 1

  -- prefetchByteArray1# :: ByteArray# -> Int# -> State# s -> State# s
  ( "prefetchByteArray1#", [_ba, _i, _s]) -> pure []

  -- prefetchMutableByteArray1# :: MutableByteArray# s -> Int# -> State# s -> State# s
  ( "prefetchMutableByteArray1#", [_ba, _i, _s]) -> pure []

  -- prefetchAddr1# :: Addr# -> Int# -> State# s -> State# s
  ( "prefetchAddr1#", [_a, _i, _s]) -> pure []

  -- prefetchValue1# :: a -> State# s -> State# s
  ( "prefetchValue1#", [_v, _s]) -> pure []

  -- level 0

  -- prefetchByteArray0# :: ByteArray# -> Int# ->  State# s -> State# s
  ( "prefetchByteArray0#", [_ba, _i, _s]) -> pure []

  -- prefetchMutableByteArray0# :: MutableByteArray# s -> Int# -> State# s -> State# s
  ( "prefetchMutableByteArray0#", [_ba, _i, _s]) -> pure []

  -- prefetchAddr0# :: Addr# -> Int# -> State# s -> State# s
  ( "prefetchAddr0#", [_a, _i, _s]) -> pure []

  -- prefetchValue0# :: a -> State# s -> State# s
  ( "prefetchValue0#", [_v, _s]) -> pure []

  _ -> fallback op args t tc
