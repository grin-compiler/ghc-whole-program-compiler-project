{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.Addr where

import Control.Monad.State
import Data.Char
import Data.Word
import Data.Int
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Foreign.Ptr
import Foreign.Storable

import Stg.Syntax
import Stg.Interpreter.Base

pattern CharV c = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  -- plusAddr# :: Addr# -> Int# -> Addr#
  ( "plusAddr#", [PtrAtom origin p, IntV offset])  -> pure [PtrAtom origin $ plusPtr p offset]

  -- minusAddr# :: Addr# -> Addr# -> Int#
  ( "minusAddr#", [PtrAtom _ p1, PtrAtom _ p2])    -> pure [IntV $ minusPtr p1 p2]

  -- remAddr# :: Addr# -> Int# -> Int#
  ( "remAddr#", [PtrAtom _ a, IntV b])             -> pure [IntV $ fromIntegral (ptrToIntPtr a) `rem` b]   -- NOTE: int % int in C

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
  -- TODO

  -- indexInt8OffAddr# :: Addr# -> Int# -> Int#
  ( "indexInt8OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int8) index
      pure [IntV $ fromIntegral v]

  -- indexInt16OffAddr# :: Addr# -> Int# -> Int#
  ( "indexInt16OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int16) index
      pure [IntV $ fromIntegral v]

  -- indexInt32OffAddr# :: Addr# -> Int# -> INT32
  ( "indexInt32OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int32) index
      pure [IntV $ fromIntegral v]

  -- indexInt64OffAddr# :: Addr# -> Int# -> INT64
  ( "indexInt64OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int64) index
      pure [IntV $ fromIntegral v]

  -- indexWord8OffAddr# :: Addr# -> Int# -> Word#
  ( "indexWord8OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word8) index
      pure [WordV $ fromIntegral v]

  -- indexWord16OffAddr# :: Addr# -> Int# -> Word#
  ( "indexWord16OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word16) index
      pure [WordV $ fromIntegral v]

  -- indexWord32OffAddr# :: Addr# -> Int# -> WORD32
  ( "indexWord32OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word32) index
      pure [WordV $ fromIntegral v]

  -- indexWord64OffAddr# :: Addr# -> Int# -> WORD64
  ( "indexWord64OffAddr#", [PtrAtom _ p, IntV index]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word64) index
      pure [WordV $ fromIntegral v]

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
  -- TODO

  -- readInt8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
  ( "readInt8OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int8) index
      pure [IntV $ fromIntegral v]

  -- readInt16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
  ( "readInt16OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int16) index
      pure [IntV $ fromIntegral v]

  -- readInt32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, INT32 #)
  ( "readInt32OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int32) index
      pure [IntV $ fromIntegral v]

  -- readInt64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, INT64 #)
  ( "readInt64OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Int64) index
      pure [IntV $ fromIntegral v]

  -- readWord8OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord8OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word8) index
      pure [WordV $ fromIntegral v]

  -- readWord16OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord16OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word16) index
      pure [WordV $ fromIntegral v]

  -- readWord32OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, WORD32 #)
  ( "readWord32OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word32) index
      pure [WordV $ fromIntegral v]

  -- readWord64OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, WORD64 #)
  ( "readWord64OffAddr#", [PtrAtom _ p, IntV index, _s]) -> do
    liftIO $ do
      v <- peekElemOff (castPtr p :: Ptr Word64) index
      pure [WordV $ fromIntegral v]

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
  -- TODO

  -- writeInt8OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
  ( "writeInt8OffAddr#", [PtrAtom _ p, IntV index, IntV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int8) index (fromIntegral value)
    pure []

  -- writeInt16OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
  ( "writeInt16OffAddr#", [PtrAtom _ p, IntV index, IntV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int16) index (fromIntegral value)
    pure []

  -- writeInt32OffAddr# :: Addr# -> Int# -> INT32 -> State# s -> State# s
  ( "writeInt32OffAddr#", [PtrAtom _ p, IntV index, IntV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int32) index (fromIntegral value)
    pure []

  -- writeInt64OffAddr# :: Addr# -> Int# -> INT64 -> State# s -> State# s
  ( "writeInt64OffAddr#", [PtrAtom _ p, IntV index, IntV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Int64) index (fromIntegral value)
    pure []

  -- writeWord8OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
  ( "writeWord8OffAddr#", [PtrAtom _ p, IntV index, WordV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word8) index (fromIntegral value)
    pure []

  -- writeWord16OffAddr# :: Addr# -> Int# -> Word# -> State# s -> State# s
  ( "writeWord16OffAddr#", [PtrAtom _ p, IntV index, WordV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word16) index (fromIntegral value)
    pure []

  -- writeWord32OffAddr# :: Addr# -> Int# -> WORD32 -> State# s -> State# s
  ( "writeWord32OffAddr#", [PtrAtom _ p, IntV index, WordV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word32) index (fromIntegral value)
    pure []

  -- writeWord64OffAddr# :: Addr# -> Int# -> WORD64 -> State# s -> State# s
  ( "writeWord64OffAddr#", [PtrAtom _ p, IntV index, WordV value, _s]) -> do
    liftIO $ do
      pokeElemOff (castPtr p :: Ptr Word64) index (fromIntegral value)
    pure []

  _ -> fallback op args t tc
