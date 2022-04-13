{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings #-}
module Stg.Interpreter.PrimOp.Prefetch where

import Stg.Syntax
import Stg.Interpreter.Base

primOps :: [(Name, PrimOpFunDef)]
primOps = getPrimOpList $ do

      -- level 3

      -- prefetchByteArray3# :: ByteArray# -> Int# ->  State# s -> State# s
  defOp "prefetchByteArray3#" $ \[_ba, _i, _s] -> pure []

      -- prefetchMutableByteArray3# :: MutableByteArray# s -> Int# -> State# s -> State# s
  defOp "prefetchMutableByteArray3#" $ \[_ba, _i, _s] -> pure []

      -- prefetchAddr3# :: Addr# -> Int# -> State# s -> State# s
  defOp "prefetchAddr3#" $ \[_a, _i, _s] -> pure []

      -- prefetchValue3# :: a -> State# s -> State# s
  defOp "prefetchValue3#" $ \[_v, _s] -> pure []

      -- level 2

      -- prefetchByteArray2# :: ByteArray# -> Int# ->  State# s -> State# s
  defOp "prefetchByteArray2#" $ \[_ba, _i, _s] -> pure []

      -- prefetchMutableByteArray2# :: MutableByteArray# s -> Int# -> State# s -> State# s
  defOp "prefetchMutableByteArray2#" $ \[_ba, _i, _s] -> pure []

      -- prefetchAddr2# :: Addr# -> Int# ->  State# s -> State# s
  defOp "prefetchAddr2#" $ \[_a, _i, _s] -> pure []

      -- prefetchValue2# :: a ->  State# s -> State# s
  defOp "prefetchValue2#" $ \[_v, _s] -> pure []

      -- level 1

      -- prefetchByteArray1# :: ByteArray# -> Int# -> State# s -> State# s
  defOp "prefetchByteArray1#" $ \[_ba, _i, _s] -> pure []

      -- prefetchMutableByteArray1# :: MutableByteArray# s -> Int# -> State# s -> State# s
  defOp "prefetchMutableByteArray1#" $ \[_ba, _i, _s] -> pure []

      -- prefetchAddr1# :: Addr# -> Int# -> State# s -> State# s
  defOp "prefetchAddr1#" $ \[_a, _i, _s] -> pure []

      -- prefetchValue1# :: a -> State# s -> State# s
  defOp "prefetchValue1#" $ \[_v, _s] -> pure []

      -- level 0

      -- prefetchByteArray0# :: ByteArray# -> Int# ->  State# s -> State# s
  defOp "prefetchByteArray0#" $ \[_ba, _i, _s] -> pure []

      -- prefetchMutableByteArray0# :: MutableByteArray# s -> Int# -> State# s -> State# s
  defOp "prefetchMutableByteArray0#" $ \[_ba, _i, _s] -> pure []

      -- prefetchAddr0# :: Addr# -> Int# -> State# s -> State# s
  defOp "prefetchAddr0#" $ \[_a, _i, _s] -> pure []

      -- prefetchValue0# :: a -> State# s -> State# s
  defOp "prefetchValue0#" $ \[_v, _s] -> pure []
