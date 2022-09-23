{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict #-}
module Stg.Interpreter.PrimOp.Addr where

import Control.Monad.State
import Data.Char
import Data.Word
import Data.Int
import Data.Bits
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign.Ptr
import Foreign.Storable

import Stg.Syntax
import Stg.Interpreter.Base

pattern CharV c = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int8V i   = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int32V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int64V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word8V i  = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word16V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word64V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- plusAddr# :: Addr# -> Int# -> Addr#
  ( "plusAddr#", [PtrAtom origin p, IntV offset])  -> pure [PtrAtom origin $ plusPtr p offset]

  -- minusAddr# :: Addr# -> Addr# -> Int#
  ( "minusAddr#", [PtrAtom _ p1, PtrAtom _ p2])    -> pure [IntV $ minusPtr p1 p2]

  -- remAddr# :: Addr# -> Int# -> Int#
  ( "remAddr#", [PtrAtom _ a, IntV b]) -> do
    -- NOTE: the GHC codegen semantics is: word % word
    -- see: https://gitlab.haskell.org/ghc/ghc/-/issues/19332
    pure [IntV $ fromIntegral (ptrToWordPtr a `rem` fromIntegral b)]

  -- addr2Int# :: Addr# -> Int#
  -- DEPRECATED: This operation is strongly deprecated.
  ( "addr2Int#", [PtrAtom _ a])                    -> pure [IntV . fromIntegral $ ptrToIntPtr a]

  -- int2Addr# :: Int# -> Addr#
  -- DEPRECATED: This operation is strongly deprecated.
  ( "int2Addr#", [IntV i])                         -> pure [PtrAtom RawPtr . intPtrToPtr $ IntPtr i]

  -- gtAddr# :: Addr# -> Addr# -> Int#
  ( "gtAddr#", [PtrAtom _ a, PtrAtom _ b])         -> pure [IntV $ if a > b then 1 else 0]

  -- geAddr# :: Addr# -> Addr# -> Int#
  ( "geAddr#", [PtrAtom _ a, PtrAtom _ b])         -> pure [IntV $ if a >= b then 1 else 0]

  -- eqAddr# :: Addr# -> Addr# -> Int#
  ( "eqAddr#", [PtrAtom _ a, PtrAtom _ b])         -> pure [IntV $ if a == b then 1 else 0]

  -- neAddr# :: Addr# -> Addr# -> Int#
  ( "neAddr#", [PtrAtom _ a, PtrAtom _ b])         -> pure [IntV $ if a /= b then 1 else 0]

  -- ltAddr# :: Addr# -> Addr# -> Int#
  ( "ltAddr#", [PtrAtom _ a, PtrAtom _ b])         -> pure [IntV $ if a < b then 1 else 0]

  -- leAddr# :: Addr# -> Addr# -> Int#
  ( "leAddr#", [PtrAtom _ a, PtrAtom _ b])         -> pure [IntV $ if a <= b then 1 else 0]

  -- indexCharOffAddr# :: Addr# -> Int# -> Char#
  ( "indexCharOffAddr#", [PtrAtom _ p, IntV index]) -> do
    -- 8 bit char
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word8) index
      pure [CharV . chr $ fromIntegral v]

  -- indexWideCharOffAddr# :: Addr# -> Int# -> Char#
  ( "indexWideCharOffAddr#", [PtrAtom _ p, IntV index]) -> do
    -- 32 bit unicode char
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Char) index
      pure [CharV v]

  -- indexIntOffAddr# :: Addr# -> Int# -> Int#
  ( "indexIntOffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int) index
      pure [IntV v]

  -- indexWordOffAddr# :: Addr# -> Int# -> Word#
  ( "indexWordOffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word) index
      pure [WordV v]

  -- indexAddrOffAddr# :: Addr# -> Int# -> Addr#
  ( "indexAddrOffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr (Ptr Word8)) index
      pure [PtrAtom RawPtr v]

  -- indexFloatOffAddr# :: Addr# -> Int# -> Float#
  ( "indexFloatOffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Float) index
      pure [FloatAtom v]

  -- indexDoubleOffAddr# :: Addr# -> Int# -> Double#
  ( "indexDoubleOffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Double) index
      pure [DoubleAtom v]

  -- indexStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a
  ( "indexStablePtrOffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr (Ptr Word8)) index
      pure [PtrAtom (StablePtr . fromIntegral $ ptrToIntPtr v) v]

  -- indexInt8OffAddr# :: Addr# -> Int# -> Int8#
  ( "indexInt8OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int8) index
      pure [Int8V $ fromIntegral v]

  -- indexInt16OffAddr# :: Addr# -> Int# -> Int16#
  ( "indexInt16OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int16) index
      pure [Int16V $ fromIntegral v]

  -- indexInt32OffAddr# :: Addr# -> Int# -> Int32#
  ( "indexInt32OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int32) index
      pure [Int32V $ fromIntegral v]

  -- indexInt64OffAddr# :: Addr# -> Int# -> Int64#
  ( "indexInt64OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int64) index
      pure [Int64V $ fromIntegral v]

  -- indexWord8OffAddr# :: Addr# -> Int# -> Word8#
  ( "indexWord8OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word8) index
      pure [Word8V $ fromIntegral v]

  -- indexWord16OffAddr# :: Addr# -> Int# -> Word16#
  ( "indexWord16OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word16) index
      pure [Word16V $ fromIntegral v]

  -- indexWord32OffAddr# :: Addr# -> Int# -> Word32#
  ( "indexWord32OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word32) index
      pure [Word32V $ fromIntegral v]

  -- indexWord64OffAddr# :: Addr# -> Int# -> Word64#
  ( "indexWord64OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word64) index
      pure [Word64V $ fromIntegral v]

  -- readCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Char# #)
  ( "readCharOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    -- 8 bit char
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word8) index
      pure [CharV . chr $ fromIntegral v]

  -- readWideCharOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Char# #)
  ( "readWideCharOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    -- 32 bit unicode char
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Char) index
      pure [CharV v]

  -- readIntOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
  ( "readIntOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int) index
      pure [IntV v]

  -- readWordOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWordOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word) index
      pure [WordV v]

  -- readAddrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Addr# #)
  ( "readAddrOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr (Ptr Word8)) index
      pure [PtrAtom RawPtr v]

  -- readFloatOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Float# #)
  ( "readFloatOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Float) index
      pure [FloatAtom v]

  -- readDoubleOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Double# #)
  ( "readDoubleOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Double) index
      pure [DoubleAtom v]

  -- readStablePtrOffAddr# :: Addr# -> Int# -> State# s -> (# State# s, StablePtr# a #)
  ( "readStablePtrOffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr (Ptr Word8)) index
      pure [PtrAtom (StablePtr . fromIntegral $ ptrToIntPtr v) v]

  -- readInt8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int8# #)
  ( "readInt8OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int8) index
      pure [Int8V $ fromIntegral v]

  -- readInt16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int16# #)
  ( "readInt16OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int16) index
      pure [Int16V $ fromIntegral v]

  -- readInt32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int32# #)
  ( "readInt32OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int32) index
      pure [Int32V $ fromIntegral v]

  -- readInt64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int64# #)
  ( "readInt64OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int64) index
      pure [Int64V $ fromIntegral v]

  -- readWord8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word8# #)
  ( "readWord8OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word8) index
      pure [Word8V $ fromIntegral v]

  -- readWord16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word16# #)
  ( "readWord16OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word16) index
      pure [Word16V $ fromIntegral v]

  -- readWord32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word32# #)
  ( "readWord32OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word32) index
      pure [Word32V $ fromIntegral v]

  -- readWord64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word64# #)
  ( "readWord64OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word64) index
      pure [Word64V $ fromIntegral v]

  -- writeCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
  ( "writeCharOffAddr#", [PtrAtom _ p, IntV index, CharV value, _s]) -> do
    -- 8 bit char
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word8) index (fromIntegral $ ord value)
    pure []

  -- writeWideCharOffAddr# :: Addr# -> Int# -> Char# -> State# s -> State# s
  ( "writeWideCharOffAddr#", [PtrAtom _ p, IntV index, CharV value, _s]) -> do
    -- 32 bit unicode char
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Char) index value
    pure []

  -- writeIntOffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
  ( "writeIntOffAddr#", [PtrAtom _ p, IntV index, IntV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int) index value
    pure []

  -- writeWordOffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
  ( "writeWordOffAddr#", [PtrAtom _ p, IntV index, WordV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word) index value
    pure []

  -- writeAddrOffAddr# :: Addr# -> Int# -> Addr# -> State# s -> State# s
  ( "writeAddrOffAddr#", [PtrAtom _ p, IntV index, PtrAtom _ value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr (Ptr Word8)) index value
    pure []

  -- writeFloatOffAddr# :: Addr# -> Int# -> Float# -> State# s -> State# s
  ( "writeFloatOffAddr#", [PtrAtom _ p, IntV index, FloatAtom value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Float) index value
    pure []

  -- writeDoubleOffAddr# :: Addr# -> Int# -> Double# -> State# s -> State# s
  ( "writeDoubleOffAddr#", [PtrAtom _ p, IntV index, DoubleAtom value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Double) index value
    pure []

  -- writeStablePtrOffAddr# :: Addr# -> Int# -> StablePtr# a -> State# s -> State# s
  ( "writeStablePtrOffAddr#", [PtrAtom _ p, IntV index, PtrAtom _ value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr (Ptr Word8)) index value
    pure []

  -- writeInt8OffAddr# :: Addr# -> Int# -> Int8# -> State# s -> State# s
  ( "writeInt8OffAddr#", [PtrAtom _ p, IntV index, Int8V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int8) index (fromIntegral value)
    pure []

  -- writeInt16OffAddr# :: Addr# -> Int# -> Int16# -> State# s -> State# s
  ( "writeInt16OffAddr#", [PtrAtom _ p, IntV index, Int16V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int16) index (fromIntegral value)
    pure []

  -- writeInt32OffAddr# :: Addr# -> Int# -> Int32# -> State# s -> State# s
  ( "writeInt32OffAddr#", [PtrAtom _ p, IntV index, Int32V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int32) index (fromIntegral value)
    pure []

  -- writeInt64OffAddr# :: Addr# -> Int# -> Int64# -> State# s -> State# s
  ( "writeInt64OffAddr#", [PtrAtom _ p, IntV index, Int64V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int64) index (fromIntegral value)
    pure []

  -- writeWord8OffAddr# :: Addr# -> Int# -> Word8# -> State# s -> State# s
  ( "writeWord8OffAddr#", [PtrAtom _ p, IntV index, Word8V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word8) index (fromIntegral value)
    pure []

  -- writeWord16OffAddr# :: Addr# -> Int# -> Word16# -> State# s -> State# s
  ( "writeWord16OffAddr#", [PtrAtom _ p, IntV index, Word16V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word16) index (fromIntegral value)
    pure []

  -- writeWord32OffAddr# :: Addr# -> Int# -> Word32# -> State# s -> State# s
  ( "writeWord32OffAddr#", [PtrAtom _ p, IntV index, Word32V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word32) index (fromIntegral value)
    pure []

  -- writeWord64OffAddr# :: Addr# -> Int# -> Word64# -> State# s -> State# s
  ( "writeWord64OffAddr#", [PtrAtom _ p, IntV index, Word64V value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word64) index (fromIntegral value)
    pure []

  -- atomicExchangeAddrAddr# :: Addr# -> Addr# -> State# s -> (# State# s, Addr# #)
  ( "atomicExchangeAddrAddr#", [PtrAtom _ p, PtrAtom _ value, _s]) -> do
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr (Ptr Word8))
      poke (castPtr p :: Ptr (Ptr Word8)) value
      pure [PtrAtom RawPtr oldValue]

  -- atomicExchangeWordAddr# :: Addr# -> Word# -> State# s -> (# State# s, Word# #)
  ( "atomicExchangeWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      poke (castPtr p :: Ptr Word) (fromIntegral value)
      pure [WordV oldValue]

  -- atomicCasAddrAddr# :: Addr# -> Addr# -> Addr# -> State# s -> (# State# s, Addr# #)
  ( "atomicCasAddrAddr#", [PtrAtom _ p, PtrAtom _ expected, PtrAtom _ value, _s]) -> do
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr (Ptr Word8))
      when (oldValue == expected) $ do
        poke (castPtr p :: Ptr (Ptr Word8)) value
      pure [PtrAtom RawPtr oldValue]

  -- atomicCasWordAddr# :: Addr# -> Word# -> Word# -> State# s -> (# State# s, Word# #)
  ( "atomicCasWordAddr#", [PtrAtom _ p, WordV expected, WordV value, _s]) -> do
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      when (oldValue == expected) $ do
        poke (castPtr p :: Ptr Word) (fromIntegral value)
      pure [WordV oldValue]

  -- atomicCasWord8Addr# :: Addr# -> Word8# -> Word8# -> State# t0 -> (# State# t0, Word8# #)
  ( "atomicCasWord8Addr#", [PtrAtom _ p, Word8V expected, Word8V value, _s]) -> do
    liftIO $ do
      oldValue <- fromIntegral <$> peek (castPtr p :: Ptr Word8)
      when (oldValue == expected) $ do
        poke (castPtr p :: Ptr Word8) (fromIntegral value)
      pure [Word8V oldValue]

  -- atomicCasWord16Addr# :: Addr# -> Word16# -> Word16# -> State# t0 -> (# State# t0, Word16# #)
  ( "atomicCasWord16Addr#", [PtrAtom _ p, Word16V expected, Word16V value, _s]) -> do
    liftIO $ do
      oldValue <- fromIntegral <$> peek (castPtr p :: Ptr Word16)
      when (oldValue == expected) $ do
        poke (castPtr p :: Ptr Word16) (fromIntegral value)
      pure [Word16V oldValue]

  -- atomicCasWord32Addr# :: Addr# -> Word32# -> Word32# -> State# t0 -> (# State# t0, Word32# #)
  ( "atomicCasWord32Addr#", [PtrAtom _ p, Word32V expected, Word32V value, _s]) -> do
    liftIO $ do
      oldValue <- fromIntegral <$> peek (castPtr p :: Ptr Word32)
      when (oldValue == expected) $ do
        poke (castPtr p :: Ptr Word32) (fromIntegral value)
      pure [Word32V oldValue]

  -- atomicCasWord64Addr# :: Addr# -> Word64# -> Word64# -> State# t0 -> (# State# t0, Word64# #)
  ( "atomicCasWord64Addr#", [PtrAtom _ p, Word64V expected, Word64V value, _s]) -> do
    liftIO $ do
      oldValue <- fromIntegral <$> peek (castPtr p :: Ptr Word64)
      when (oldValue == expected) $ do
        poke (castPtr p :: Ptr Word64) (fromIntegral value)
      pure [Word64V oldValue]

  -- fetchAddWordAddr# :: Addr# -> Word# -> State# t0 -> (# State# t0, Word# #)
  ( "fetchAddWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    -- NOTE: CPU atomic
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      poke (castPtr p :: Ptr Word) (oldValue + value)
      pure [WordV oldValue]

  -- fetchSubWordAddr# :: Addr# -> Word# -> State# t0 -> (# State# t0, Word# #)
  ( "fetchSubWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    -- NOTE: CPU atomic
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      poke (castPtr p :: Ptr Word) (oldValue - value)
      pure [WordV oldValue]

  -- fetchAndWordAddr# :: Addr# -> Word# -> State# t0 -> (# State# t0, Word# #)
  ( "fetchAndWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    -- NOTE: CPU atomic
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      poke (castPtr p :: Ptr Word) (oldValue .&. value)
      pure [WordV oldValue]

  -- fetchNandWordAddr# :: Addr# -> Word# -> State# t0 -> (# State# t0, Word# #)
  ( "fetchNandWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    -- NOTE: CPU atomic
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      poke (castPtr p :: Ptr Word) (complement $ oldValue .&. value)
      pure [WordV oldValue]

  -- fetchOrWordAddr# :: Addr# -> Word# -> State# t0 -> (# State# t0, Word# #)
  ( "fetchOrWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    -- NOTE: CPU atomic
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      poke (castPtr p :: Ptr Word) (oldValue .|. value)
      pure [WordV oldValue]

  -- fetchXorWordAddr# :: Addr# -> Word# -> State# t0 -> (# State# t0, Word# #)
  ( "fetchXorWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    -- NOTE: CPU atomic
    liftIO $ do
      oldValue <- peek (castPtr p :: Ptr Word)
      poke (castPtr p :: Ptr Word) (oldValue `xor` value)
      pure [WordV oldValue]

  -- atomicReadWordAddr# :: Addr# -> State# t0 -> (# State# t0, Word# #)
  ( "atomicReadWordAddr#", [PtrAtom _ p, _s]) -> do
    -- NOTE: CPU atomic. Implies a full memory barrier.
    liftIO $ do
      value <- peek (castPtr p :: Ptr Word)
      pure [WordV value]

  -- atomicWriteWordAddr# :: Addr# -> Word# -> State# t0 -> State# t0
  ( "atomicWriteWordAddr#", [PtrAtom _ p, WordV value, _s]) -> do
    -- NOTE: CPU atomic. Implies a full memory barrier.
    liftIO $ do
      poke (castPtr p :: Ptr Word) value
      pure []

  _ -> fallback op args t tc
