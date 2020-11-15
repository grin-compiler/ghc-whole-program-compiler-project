{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.ByteArray where

import Data.Int
import Data.Word
import Data.Char
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.State
import qualified Data.IntMap as IntMap
import qualified Data.Primitive.ByteArray as BA

import Stg.Syntax
import Stg.Interpreter.Base

pattern CharV c   = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

lookupByteArray :: Int -> M BA.ByteArray
lookupByteArray baId = do
  ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
  case baaByteArray of
    Just ba -> pure ba
    Nothing -> stgErrorM $ "unknown ByteArray: " ++ show baId

getByteArrayContentPtr :: Int -> M (Ptr Word8)
getByteArrayContentPtr baId = do
  ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
  pure $ BA.mutableByteArrayContents baaMutableByteArray

newByteArray :: Int -> Int -> Bool -> M ByteArrayIdx
newByteArray size alignment pinned = do
  -- HINT: the implementation always uses pinned byte array because the primop implementation is not atomic
  --        GC may occur and the content data pointer must stay in place
  --        but this is only an interpreter implementation constraint
  ba <- liftIO $ BA.newPinnedByteArray size

  -- debug
  liftIO $ BA.fillByteArray ba 0 size 0

  byteArrays <- gets ssMutableByteArrays
  let next  = IntMap.size byteArrays
      desc  = ByteArrayDescriptor
        { baaMutableByteArray = ba
        , baaByteArray        = Nothing
        , baaPinned           = pinned
        , baaAlignment        = alignment
        }

  modify' $ \s -> s {ssMutableByteArrays = IntMap.insert next desc byteArrays}

  pure $ ByteArrayIdx
    { baId        = next
    , baPinned    = pinned
    , baAlignment = alignment
    }

{-
  NOTES:
  - only mutable bytearray can be created
  - bytearray is always an alias to mutable byte array
  - mutable array parameters: alignment :: Int, pinned :: Bool, size :: Int ; keep these in the index
-}

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of

  ---------------------------------------------
  --    construction
  ---------------------------------------------

  -- newByteArray# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "newByteArray#", [IntV size, _s]) -> do
    baIdx <- newByteArray size 1 False
    pure [MutableByteArray baIdx]


  -- newPinnedByteArray# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "newPinnedByteArray#", [IntV size, _s]) -> do
    baIdx <- newByteArray size 1 True
    pure [MutableByteArray baIdx]


  -- newAlignedPinnedByteArray# :: Int# -> Int# -> State# s -> (# State# s, MutableByteArray# s #)
  ( "newAlignedPinnedByteArray#", [IntV size, IntV alignment, _s]) -> do
    baIdx <- newByteArray size alignment True
    pure [MutableByteArray baIdx]

  ---------------------------------------------
  --    query implementation details
  ---------------------------------------------

  -- isMutableByteArrayPinned# :: MutableByteArray# s -> Int#
  ( "isMutableByteArrayPinned#", [MutableByteArray ByteArrayIdx{..}]) -> do
    pure [IntV $ if baPinned then 1 else 0]

  -- isByteArrayPinned# :: ByteArray# -> Int#
  ( "isByteArrayPinned#", [ByteArray ByteArrayIdx{..}]) -> do
    pure [IntV $ if baPinned then 1 else 0]

  ---------------------------------------------
  --    FFI and unsafe / raw pointer API
  ---------------------------------------------

  -- byteArrayContents# :: ByteArray# -> Addr#
  ( "byteArrayContents#", [ByteArray baIdx@ByteArrayIdx{..}]) -> do
    ptr <- getByteArrayContentPtr baId
    pure [PtrAtom (ByteArrayPtr baIdx) ptr]

  -- FIXME: GHC Core Coercions allow this:
  ( "byteArrayContents#", [MutableByteArray baIdx@ByteArrayIdx{..}]) -> do
    ptr <- getByteArrayContentPtr baId
    pure [PtrAtom (ByteArrayPtr baIdx) ptr]

  -- sameMutableByteArray# :: MutableByteArray# s -> MutableByteArray# s -> Int#
  ( "sameMutableByteArray#", [MutableByteArray baIdx1, MutableByteArray baIdx2]) -> do
    pure [IntV $ if baIdx1 == baIdx2 then 1 else 0]

  -- shrinkMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> State# s
  ( "shrinkMutableByteArray#", [MutableByteArray ByteArrayIdx{..}, IntV size, _s]) -> do
    ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
    liftIO $ BA.shrinkMutableByteArray baaMutableByteArray size
    pure []

  -- resizeMutableByteArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
  -- TODO

  -- unsafeFreezeByteArray# :: MutableByteArray# s -> State# s -> (# State# s, ByteArray# #)
  ( "unsafeFreezeByteArray#", [MutableByteArray baIdx@ByteArrayIdx{..}, _s]) -> do
    desc@ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
    case baaByteArray of
      Just{}  -> pure ()
      Nothing -> do
        ba <- liftIO $ BA.unsafeFreezeByteArray baaMutableByteArray
        let newDesc = desc {baaByteArray = Just ba}
        modify' $ \s@StgState{..} -> s {ssMutableByteArrays = IntMap.insert baId newDesc ssMutableByteArrays}
    pure [ByteArray baIdx]

  -- sizeofByteArray# :: ByteArray# -> Int#
  ( "sizeofByteArray#", [ByteArray ByteArrayIdx{..}]) -> do
    ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
    pure [IntV $ BA.sizeofMutableByteArray baaMutableByteArray]

  -- sizeofMutableByteArray# :: MutableByteArray# s -> Int#
  ( "sizeofMutableByteArray#", [MutableByteArray ByteArrayIdx{..}]) -> do
    ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
    pure [IntV $ BA.sizeofMutableByteArray baaMutableByteArray]

  -- getSizeofMutableByteArray# :: MutableByteArray# s -> State# s -> (# State# s, Int# #)
  ( "getSizeofMutableByteArray#", [MutableByteArray ByteArrayIdx{..}, _s]) -> do
    ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
    pure [IntV $ BA.sizeofMutableByteArray baaMutableByteArray]

  ---------------------------------------------
  --    read ByteArray (pure)
  ---------------------------------------------

  -- indexCharArray# :: ByteArray# -> Int# -> Char#
  ( "indexCharArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word8)
    pure [CharV . chr $ fromIntegral value]

  -- indexWideCharArray# :: ByteArray# -> Int# -> Char#
  ( "indexWideCharArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    -- 32 bit unicode char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Char)
    pure [CharV value]

  -- indexIntArray# :: ByteArray# -> Int# -> Int#
  ( "indexIntArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    pure [IntV value]

  -- indexWordArray# :: ByteArray# -> Int# -> Word#
  ( "indexWordArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word)
    pure [WordV value]

  -- indexAddrArray# :: ByteArray# -> Int# -> Addr#
  ( "indexAddrArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO (Ptr Word8))
    pure [PtrAtom RawPtr value]

  -- indexFloatArray# :: ByteArray# -> Int# -> Float#
  ( "indexFloatArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Float)
    pure [FloatAtom value]

  -- indexDoubleArray# :: ByteArray# -> Int# -> Double#
  ( "indexDoubleArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Double)
    pure [DoubleAtom value]

  -- indexStablePtrArray# :: ByteArray# -> Int# -> StablePtr# a
  -- TODO

  -- indexInt8Array# :: ByteArray# -> Int# -> Int#
  ( "indexInt8Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int8)
    pure [IntV $ fromIntegral value]

  -- indexInt16Array# :: ByteArray# -> Int# -> Int#
  ( "indexInt16Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int16)
    pure [IntV $ fromIntegral value]

  -- indexInt32Array# :: ByteArray# -> Int# -> INT32
  ( "indexInt32Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int32)
    pure [IntV $ fromIntegral value]

  -- indexInt64Array# :: ByteArray# -> Int# -> INT64
  ( "indexInt64Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int64)
    pure [IntV $ fromIntegral value]

  -- indexWord8Array# :: ByteArray# -> Int# -> Word#
  ( "indexWord8Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word8)
    pure [WordV $ fromIntegral value]

  -- indexWord16Array# :: ByteArray# -> Int# -> Word#
  ( "indexWord16Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word16)
    pure [WordV $ fromIntegral value]

  -- indexWord32Array# :: ByteArray# -> Int# -> WORD32
  ( "indexWord32Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word32)
    pure [WordV $ fromIntegral value]

  -- indexWord64Array# :: ByteArray# -> Int# -> WORD64
  ( "indexWord64Array#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word64)
    pure [WordV $ fromIntegral value]

  -- indexWord8ArrayAsChar# :: ByteArray# -> Int# -> Char#
  ( "indexWord8ArrayAsChar#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word8)
    pure [CharV . chr $ fromIntegral value]

  -- indexWord8ArrayAsWideChar# :: ByteArray# -> Int# -> Char#
  ( "indexWord8ArrayAsWideChar#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    -- 32 bit unicode char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Char)
    pure [CharV value]

  -- indexWord8ArrayAsAddr# :: ByteArray# -> Int# -> Addr#
  ( "indexWord8ArrayAsAddr#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO (Ptr Word8))
    pure [PtrAtom RawPtr value]

  -- indexWord8ArrayAsFloat# :: ByteArray# -> Int# -> Float#
  ( "indexWord8ArrayAsFloat#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Float)
    pure [FloatAtom value]

  -- indexWord8ArrayAsDouble# :: ByteArray# -> Int# -> Double#
  ( "indexWord8ArrayAsDouble#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Double)
    pure [DoubleAtom value]

  -- indexWord8ArrayAsStablePtr# :: ByteArray# -> Int# -> StablePtr# a
  -- TODO

  -- indexWord8ArrayAsInt16# :: ByteArray# -> Int# -> Int#
  ( "indexWord8ArrayAsInt16#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int16)
    pure [IntV $ fromIntegral value]

  -- indexWord8ArrayAsInt32# :: ByteArray# -> Int# -> INT32
  ( "indexWord8ArrayAsInt32#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int32)
    pure [IntV $ fromIntegral value]

  -- indexWord8ArrayAsInt64# :: ByteArray# -> Int# -> INT64
  ( "indexWord8ArrayAsInt64#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int64)
    pure [IntV $ fromIntegral value]

  -- indexWord8ArrayAsInt# :: ByteArray# -> Int# -> Int#
  ( "indexWord8ArrayAsInt#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int)
    pure [IntV value]

  -- indexWord8ArrayAsWord16# :: ByteArray# -> Int# -> Word#
  ( "indexWord8ArrayAsWord16#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word16)
    pure [WordV $ fromIntegral value]

  -- indexWord8ArrayAsWord32# :: ByteArray# -> Int# -> WORD32
  ( "indexWord8ArrayAsWord32#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word32)
    pure [WordV $ fromIntegral value]

  -- indexWord8ArrayAsWord64# :: ByteArray# -> Int# -> WORD64
  ( "indexWord8ArrayAsWord64#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word64)
    pure [WordV $ fromIntegral value]

  -- indexWord8ArrayAsWord# :: ByteArray# -> Int# -> Word#
  ( "indexWord8ArrayAsWord#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word)
    pure [WordV value]

  ---------------------------------------------
  --    read MutableByteArray (effectful)
  ---------------------------------------------

  -- readCharArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
  ( "readCharArray#", [ByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word8)
    pure [CharV . chr $ fromIntegral value]

  -- readWideCharArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
  ( "readWideCharArray#", [ByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    -- 32 bit unicode char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Char)
    pure [CharV value]

  -- readIntArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "readIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    pure [IntV value]

  -- readWordArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWordArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word)
    pure [WordV value]

  -- readAddrArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
  ( "readAddrArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO (Ptr Word8))
    pure [PtrAtom RawPtr value]

  -- readFloatArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
  ( "readFloatArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Float)
    pure [FloatAtom value]

  -- readDoubleArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
  ( "readDoubleArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Double)
    pure [DoubleAtom value]

  -- readStablePtrArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
  -- TODO

  -- readInt8Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "readInt8Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int8)
    pure [IntV $ fromIntegral value]

  -- readInt16Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "readInt16Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int16)
    pure [IntV $ fromIntegral value]

  -- readInt32Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
  ( "readInt32Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int32)
    pure [IntV $ fromIntegral value]

  -- readInt64Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
  ( "readInt64Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int64)
    pure [IntV $ fromIntegral value]

  -- readWord8Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord8Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word8)
    pure [WordV $ fromIntegral value]

  -- readWord16Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord16Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word16)
    pure [WordV $ fromIntegral value]

  -- readWord32Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
  ( "readWord32Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word32)
    pure [WordV $ fromIntegral value]

  -- readWord64Array# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
  ( "readWord64Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word64)
    pure [WordV $ fromIntegral value]

  -- readWord8ArrayAsChar# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
  ( "readWord8ArrayAsChar#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word8)
    pure [CharV . chr $ fromIntegral value]

  -- readWord8ArrayAsWideChar# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
  ( "readWord8ArrayAsWideChar#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    -- 32 bit unicode char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Char)
    pure [CharV value]

  -- readWord8ArrayAsAddr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
  ( "readWord8ArrayAsAddr#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO (Ptr Word8))
    pure [PtrAtom RawPtr value]

  -- readWord8ArrayAsFloat# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
  ( "readWord8ArrayAsFloat#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Float)
    pure [FloatAtom value]

  -- readWord8ArrayAsDouble# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
  ( "readWord8ArrayAsDouble#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Double)
    pure [DoubleAtom value]

  -- readWord8ArrayAsStablePtr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
  -- TODO

  -- readWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "readWord8ArrayAsInt16#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int16)
    pure [IntV $ fromIntegral value]

  -- readWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
  ( "readWord8ArrayAsInt32#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int32)
    pure [IntV $ fromIntegral value]

  -- readWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
  ( "readWord8ArrayAsInt64#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int64)
    pure [IntV $ fromIntegral value]

  -- readWord8ArrayAsInt# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "readWord8ArrayAsInt#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int)
    pure [IntV value]

  -- readWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord8ArrayAsWord16#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word16)
    pure [WordV $ fromIntegral value]

  -- readWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
  ( "readWord8ArrayAsWord32#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word32)
    pure [WordV $ fromIntegral value]

  -- readWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
  ( "readWord8ArrayAsWord64#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word64)
    pure [WordV $ fromIntegral value]

  -- readWord8ArrayAsWord# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord8ArrayAsWord#", [ByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word)
    pure [WordV value]

  ---------------------------------------------
  --    write MutableByteArray (effectful)
  ---------------------------------------------

  -- writeCharArray# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
  ( "writeCharArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, CharV value, _s]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral $ ord value :: Word8)
    pure []

  -- writeWideCharArray# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
  ( "writeWideCharArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, CharV value, _s]) -> do
    -- 32 bit unicode char
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

  -- writeIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "writeIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

  -- writeWordArray# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
  ( "writeWordArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

  -- writeAddrArray# :: MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
  ( "writeAddrArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, PtrAtom _ value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

  -- writeFloatArray# :: MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
  ( "writeFloatArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, FloatAtom value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

  -- writeDoubleArray# :: MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
  ( "writeDoubleArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, DoubleAtom value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

  -- writeStablePtrArray# :: MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
  -- TODO

  -- writeInt8Array# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "writeInt8Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Int8)
    pure []

  -- writeInt16Array# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "writeInt16Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Int16)
    pure []

  -- writeInt32Array# :: MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
  ( "writeInt32Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Int32)
    pure []

  -- writeInt64Array# :: MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
  ( "writeInt64Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Int64)
    pure []

  -- writeWord8Array# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
  ( "writeWord8Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Word8)
    pure []

  -- writeWord16Array# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
  ( "writeWord16Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Word16)
    pure []

  -- writeWord32Array# :: MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
  ( "writeWord32Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Word32)
    pure []

  -- writeWord64Array# :: MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
  ( "writeWord64Array#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index (fromIntegral value :: Word64)
    pure []

  -- writeWord8ArrayAsChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
  ( "writeWord8ArrayAsChar#", [MutableByteArray ByteArrayIdx{..}, IntV offset, CharV value, _s]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p offset (fromIntegral $ ord value :: Word8)
    pure []

  -- writeWord8ArrayAsWideChar# :: MutableByteArray# s -> Int# -> Char# -> State# s -> State# s
  ( "writeWord8ArrayAsWideChar#", [MutableByteArray ByteArrayIdx{..}, IntV offset, CharV value, _s]) -> do
    -- 32 bit unicode char
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p offset value
    pure []

  -- writeWord8ArrayAsAddr# :: MutableByteArray# s -> Int# -> Addr# -> State# s -> State# s
  ( "writeWord8ArrayAsAddr#", [MutableByteArray ByteArrayIdx{..}, IntV offset, PtrAtom _ value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p offset value
    pure []

  -- writeWord8ArrayAsFloat# :: MutableByteArray# s -> Int# -> Float# -> State# s -> State# s
  ( "writeWord8ArrayAsFloat#", [MutableByteArray ByteArrayIdx{..}, IntV offset, FloatAtom value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p offset value
    pure []

  -- writeWord8ArrayAsDouble# :: MutableByteArray# s -> Int# -> Double# -> State# s -> State# s
  ( "writeWord8ArrayAsDouble#", [MutableByteArray ByteArrayIdx{..}, IntV offset, DoubleAtom value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p offset value
    pure []

  -- writeWord8ArrayAsStablePtr# :: MutableByteArray# s -> Int# -> StablePtr# a -> State# s -> State# s
  -- TODO

  -- writeWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "writeWord8ArrayAsInt16#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index (fromIntegral value :: Int16)
    pure []

  -- writeWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> INT32 -> State# s -> State# s
  ( "writeWord8ArrayAsInt32#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index (fromIntegral value :: Int32)
    pure []

  -- writeWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> INT64 -> State# s -> State# s
  ( "writeWord8ArrayAsInt64#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index (fromIntegral value :: Int64)
    pure []

  -- writeWord8ArrayAsInt# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "writeWord8ArrayAsInt#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index value
    pure []

  -- writeWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
  ( "writeWord8ArrayAsWord16#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index (fromIntegral value :: Word16)
    pure []

  -- writeWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> WORD32 -> State# s -> State# s
  ( "writeWord8ArrayAsWord32#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index (fromIntegral value :: Word32)
    pure []

  -- writeWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> WORD64 -> State# s -> State# s
  ( "writeWord8ArrayAsWord64#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index (fromIntegral value :: Word64)
    pure []

  -- writeWord8ArrayAsWord# :: MutableByteArray# s -> Int# -> Word# -> State# s -> State# s
  ( "writeWord8ArrayAsWord#", [MutableByteArray ByteArrayIdx{..}, IntV index, WordV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p index value
    pure []

  ---------------------------------------------
  --    comparison
  ---------------------------------------------

  -- compareByteArrays# :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> Int#
  ( "compareByteArrays#",
      [ ByteArray ByteArrayIdx{baId = baIdA}, IntV offsetA
      , ByteArray ByteArrayIdx{baId = baIdB}, IntV offsetB
      , IntV length
      ]
  ) -> do
    baA <- lookupByteArray baIdA
    baB <- lookupByteArray baIdB
    case BA.compareByteArrays baA offsetA baB offsetB length of
      LT  -> pure [IntV (-1)]
      EQ  -> pure [IntV 0]
      GT  -> pure [IntV 1]

  ---------------------------------------------
  --    copy and fill
  ---------------------------------------------

  -- TODO
  -- copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  -- copyMutableByteArray# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  -- copyByteArrayToAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
  -- copyMutableByteArrayToAddr# :: MutableByteArray# s -> Int# -> Addr# -> Int# -> State# s -> State# s
  -- copyAddrToByteArray# :: Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  -- setByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s

  ---------------------------------------------
  --    atomic operations
  ---------------------------------------------

  -- TODO
  -- atomicReadIntArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  -- atomicWriteIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  -- casIntArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  -- fetchAddIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  -- fetchSubIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  -- fetchAndIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  -- fetchNandIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  -- fetchOrIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  -- fetchXorIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)

  _ -> fallback op args t tc

{-
------------------------------------------------------------------------
section "Byte Arrays"
        {Operations on {\tt ByteArray\#}. A {\tt ByteArray\#} is a just a region of
         raw memory in the garbage-collected heap, which is not
         scanned for pointers. It carries its own size (in bytes).
         There are
         three sets of operations for accessing byte array contents:
         index for reading from immutable byte arrays, and read/write
         for mutable byte arrays.  Each set contains operations for a
         range of useful primitive data types.  Each operation takes
         an offset measured in terms of the size of the primitive type
         being read or written.}

------------------------------------------------------------------------

primtype ByteArray#

primtype MutableByteArray# s


primop  ResizeMutableByteArrayOp_Char "resizeMutableByteArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s,MutableByteArray# s #)
   {Resize (unpinned) mutable byte array to new specified size (in bytes).
    The returned {\tt MutableByteArray\#} is either the original
    {\tt MutableByteArray\#} resized in-place or, if not possible, a newly
    allocated (unpinned) {\tt MutableByteArray\#} (with the original content
    copied over).

    To avoid undefined behaviour, the original {\tt MutableByteArray\#} shall
    not be accessed anymore after a {\tt resizeMutableByteArray\#} has been
    performed.  Moreover, no reference to the old one should be kept in order
    to allow garbage collection of the original {\tt MutableByteArray\#} in
    case a new {\tt MutableByteArray\#} had to be allocated.}
   with out_of_line = True
        has_side_effects = True


primop  CopyByteArrayOp "copyByteArray#" GenPrimOp
  ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {{\tt copyByteArray# src src_ofs dst dst_ofs n} copies the range
   starting at offset {\tt src_ofs} of length {\tt n} from the
   {\tt ByteArray#} {\tt src} to the {\tt MutableByteArray#} {\tt dst}
   starting at offset {\tt dst_ofs}.  Both arrays must fully contain
   the specified ranges, but this is not checked.  The two arrays must
   not be the same array in different states, but this is not checked
   either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayOp "copyMutableByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a range of the first MutableByteArray\# to the specified region in the second MutableByteArray\#.
   Both arrays must fully contain the specified ranges, but this is not checked. The regions are
   allowed to overlap, although this is only possible when the same array is provided
   as both the source and the destination.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

primop  CopyByteArrayToAddrOp "copyByteArrayToAddr#" GenPrimOp
  ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the ByteArray\# to the memory range starting at the Addr\#.
   The ByteArray\# and the memory region at Addr\# must fully contain the
   specified ranges, but this is not checked. The Addr\# must not point into the
   ByteArray\# (e.g. if the ByteArray\# were pinned), but this is not checked
   either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyMutableByteArrayToAddrOp "copyMutableByteArrayToAddr#" GenPrimOp
  MutableByteArray# s -> Int# -> Addr# -> Int# -> State# s -> State# s
  {Copy a range of the MutableByteArray\# to the memory range starting at the
   Addr\#. The MutableByteArray\# and the memory region at Addr\# must fully
   contain the specified ranges, but this is not checked. The Addr\# must not
   point into the MutableByteArray\# (e.g. if the MutableByteArray\# were
   pinned), but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  CopyAddrToByteArrayOp "copyAddrToByteArray#" GenPrimOp
  Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  {Copy a memory range starting at the Addr\# to the specified range in the
   MutableByteArray\#. The memory region at Addr\# and the ByteArray\# must fully
   contain the specified ranges, but this is not checked. The Addr\# must not
   point into the MutableByteArray\# (e.g. if the MutableByteArray\# were pinned),
   but this is not checked either.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4}
  can_fail = True

primop  SetByteArrayOp "setByteArray#" GenPrimOp
  MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
  {{\tt setByteArray# ba off len c} sets the byte range {\tt [off, off+len]} of
   the {\tt MutableByteArray#} to the byte {\tt c}.}
  with
  has_side_effects = True
  code_size = { primOpCodeSizeForeignCall + 4 }
  can_fail = True

-- Atomic operations

primop  AtomicReadByteArrayOp_Int "atomicReadIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array and an offset in machine words, read an element. The
    index is assumed to be in bounds. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop  AtomicWriteByteArrayOp_Int "atomicWriteIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
   {Given an array and an offset in machine words, write an element. The
    index is assumed to be in bounds. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop CasByteArrayOp_Int "casIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, an offset in machine words, the expected old value, and
    the new value, perform an atomic compare and swap i.e. write the new
    value if the current value matches the provided old value. Returns
    the value of the element before the operation. Implies a full memory
    barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAddByteArrayOp_Int "fetchAddIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to add,
    atomically add the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchSubByteArrayOp_Int "fetchSubIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to subtract,
    atomically subtract the value to the element. Returns the value of
    the element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchAndByteArrayOp_Int "fetchAndIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to AND,
    atomically AND the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchNandByteArrayOp_Int "fetchNandIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to NAND,
    atomically NAND the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchOrByteArrayOp_Int "fetchOrIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to OR,
    atomically OR the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True

primop FetchXorByteArrayOp_Int "fetchXorIntArray#" GenPrimOp
   MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
   {Given an array, and offset in machine words, and a value to XOR,
    atomically XOR the value to the element. Returns the value of the
    element before the operation. Implies a full memory barrier.}
   with has_side_effects = True
        can_fail = True
-}