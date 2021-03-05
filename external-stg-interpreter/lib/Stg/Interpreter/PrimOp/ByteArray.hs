{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimOp.ByteArray where

import Data.Bits
import Data.Int
import Data.Word
import Data.Char
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
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
  next <- gets ssNextMutableByteArray
  let desc = ByteArrayDescriptor
        { baaMutableByteArray = ba
        , baaByteArray        = Nothing
        , baaPinned           = pinned
        , baaAlignment        = alignment
        }

  modify' $ \s -> s {ssMutableByteArrays = IntMap.insert next desc byteArrays, ssNextMutableByteArray = succ next}

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
  ( "resizeMutableByteArray#", [MutableByteArray baIdx@ByteArrayIdx{..}, IntV newSizeInBytes, _s]) -> do
    desc@ByteArrayDescriptor{..} <- lookupByteArrayDescriptor baId
    -- sanity check
    when baaPinned $ do
      stgErrorM $ "(undefined behaviour) resizeMutableByteArray# on pinned MutableByteArray, primop args: " ++ show args
    -- HINT: the current implementation is always inplace
    resizedBA <- liftIO $ BA.resizeMutableByteArray baaMutableByteArray newSizeInBytes
    let desc' = ByteArrayDescriptor
                { baaMutableByteArray = resizedBA
                , baaByteArray        = Nothing
                , baaPinned           = False
                , baaAlignment        = baAlignment
                }
    modify' $ \s@StgState{..} -> s {ssMutableByteArrays = IntMap.insert baId desc' ssMutableByteArrays}
    -- TODO: do inplace when the new size <= old size, otherwise allocate new array and copy the required amount of content
    -- NOTE: always inplace resize should work also for the interpreter, so it is ok for now
    pure [MutableByteArray baIdx]

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
  ( "indexStablePtrArray#", [ByteArray ByteArrayIdx{..}, IntV index]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO (Ptr Word8))
    pure [PtrAtom (StablePtr . fromIntegral $ ptrToIntPtr value) value]

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
  ( "indexWord8ArrayAsStablePtr#", [ByteArray ByteArrayIdx{..}, IntV offset]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO (Ptr Word8))
    pure [PtrAtom (StablePtr . fromIntegral $ ptrToIntPtr value) value]

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
  ( "readCharArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO Word8)
    pure [CharV . chr $ fromIntegral value]

  -- readWideCharArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
  ( "readWideCharArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
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
  ( "readStablePtrArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekElemOff (castPtr p) index :: IO (Ptr Word8))
    pure [PtrAtom (StablePtr . fromIntegral $ ptrToIntPtr value) value]

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
  ( "readWord8ArrayAsChar#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    -- 8 bit char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word8)
    pure [CharV . chr $ fromIntegral value]

  -- readWord8ArrayAsWideChar# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Char# #)
  ( "readWord8ArrayAsWideChar#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    -- 32 bit unicode char
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Char)
    pure [CharV value]

  -- readWord8ArrayAsAddr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Addr# #)
  ( "readWord8ArrayAsAddr#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO (Ptr Word8))
    pure [PtrAtom RawPtr value]

  -- readWord8ArrayAsFloat# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Float# #)
  ( "readWord8ArrayAsFloat#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Float)
    pure [FloatAtom value]

  -- readWord8ArrayAsDouble# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Double# #)
  ( "readWord8ArrayAsDouble#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Double)
    pure [DoubleAtom value]

  -- readWord8ArrayAsStablePtr# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, StablePtr# a #)
  ( "readWord8ArrayAsStablePtr#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO (Ptr Word8))
    pure [PtrAtom (StablePtr . fromIntegral $ ptrToIntPtr value) value]

  -- readWord8ArrayAsInt16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "readWord8ArrayAsInt16#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int16)
    pure [IntV $ fromIntegral value]

  -- readWord8ArrayAsInt32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, INT32 #)
  ( "readWord8ArrayAsInt32#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int32)
    pure [IntV $ fromIntegral value]

  -- readWord8ArrayAsInt64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, INT64 #)
  ( "readWord8ArrayAsInt64#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int64)
    pure [IntV $ fromIntegral value]

  -- readWord8ArrayAsInt# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "readWord8ArrayAsInt#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Int)
    pure [IntV value]

  -- readWord8ArrayAsWord16# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord8ArrayAsWord16#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word16)
    pure [WordV $ fromIntegral value]

  -- readWord8ArrayAsWord32# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD32 #)
  ( "readWord8ArrayAsWord32#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word32)
    pure [WordV $ fromIntegral value]

  -- readWord8ArrayAsWord64# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, WORD64 #)
  ( "readWord8ArrayAsWord64#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
    p <- getByteArrayContentPtr baId
    value <- liftIO (peekByteOff p offset :: IO Word64)
    pure [WordV $ fromIntegral value]

  -- readWord8ArrayAsWord# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Word# #)
  ( "readWord8ArrayAsWord#", [MutableByteArray ByteArrayIdx{..}, IntV offset, _s]) -> do
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
  ( "writeStablePtrArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, PtrAtom _ value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

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
  ( "writeWord8ArrayAsStablePtr#", [MutableByteArray ByteArrayIdx{..}, IntV offset, PtrAtom _ value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ pokeByteOff p offset value
    pure []

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

  -- copyByteArray# :: ByteArray# -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyByteArray#",
      [ ByteArray ByteArrayIdx{baId = baIdSrc},        IntV offsetSrc
      , MutableByteArray ByteArrayIdx{baId = baIdDst}, IntV offsetDst
      , IntV length, _s
      ]
   ) -> do
    src <- getByteArrayContentPtr baIdSrc
    dst <- getByteArrayContentPtr baIdDst
    liftIO $ copyBytes (plusPtr dst offsetDst) (plusPtr src offsetSrc) length
    pure []

  -- copyMutableByteArray# :: MutableByteArray# s -> Int# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyMutableByteArray#",
      [ MutableByteArray ByteArrayIdx{baId = baIdSrc}, IntV offsetSrc
      , MutableByteArray ByteArrayIdx{baId = baIdDst}, IntV offsetDst
      , IntV length, _s
      ]
   ) -> do
    src <- getByteArrayContentPtr baIdSrc
    dst <- getByteArrayContentPtr baIdDst
    liftIO $ copyBytes (plusPtr dst offsetDst) (plusPtr src offsetSrc) length
    pure []

  -- copyByteArrayToAddr# :: ByteArray# -> Int# -> Addr# -> Int# -> State# s -> State# s
  ( "copyByteArrayToAddr#", [ByteArray ByteArrayIdx{..}, IntV offset, PtrAtom _ dst, IntV length, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ copyBytes dst (plusPtr p offset) length
    pure []

  -- copyMutableByteArrayToAddr# :: MutableByteArray# s -> Int# -> Addr# -> Int# -> State# s -> State# s
  ( "copyMutableByteArrayToAddr#", [MutableByteArray ByteArrayIdx{..}, IntV offset, PtrAtom _ dst, IntV length, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ copyBytes dst (plusPtr p offset) length
    pure []

  -- copyAddrToByteArray# :: Addr# -> MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "copyAddrToByteArray#", [PtrAtom _ src, MutableByteArray ByteArrayIdx{..}, IntV offset, IntV length, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ copyBytes (plusPtr p offset) src length
    pure []

  -- setByteArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> State# s
  ( "setByteArray#", [MutableByteArray ByteArrayIdx{..}, IntV offset, IntV length, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    liftIO $ fillBytes (plusPtr p offset) (fromIntegral value :: Word8) length
    pure []

  ---------------------------------------------
  --    atomic operations
  ---------------------------------------------

  -- atomicReadIntArray# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Int# #)
  ( "atomicReadIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- TODO: CPU atomic
    value <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    pure [IntV value]

  -- atomicWriteIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> State# s
  ( "atomicWriteIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- TODO: CPU atomic
    liftIO $ pokeElemOff (castPtr p) index value
    pure []

  -- casIntArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  ( "casIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV old, IntV new, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- NOTE: CPU atomic
    current <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    when (current == old) $ do
      liftIO $ pokeElemOff (castPtr p) index new
    pure [IntV current]

  -- fetchAddIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  ( "fetchAddIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- NOTE: CPU atomic
    original <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    liftIO $ pokeElemOff (castPtr p) index (original + value)
    pure [IntV original]

  -- fetchSubIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  ( "fetchSubIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- NOTE: CPU atomic
    original <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    liftIO $ pokeElemOff (castPtr p) index (original - value)
    pure [IntV original]

  -- fetchAndIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  ( "fetchAndIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- NOTE: CPU atomic
    original <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    liftIO $ pokeElemOff (castPtr p) index (original .&. value)
    pure [IntV original]

  -- fetchNandIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  ( "fetchNandIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- NOTE: CPU atomic
    original <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    liftIO $ pokeElemOff (castPtr p) index (complement $ original .&. value)
    pure [IntV original]

  -- fetchOrIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  ( "fetchOrIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- NOTE: CPU atomic
    original <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    liftIO $ pokeElemOff (castPtr p) index (original .|. value)
    pure [IntV original]

  -- fetchXorIntArray# :: MutableByteArray# s -> Int# -> Int# -> State# s -> (# State# s, Int# #)
  ( "fetchXorIntArray#", [MutableByteArray ByteArrayIdx{..}, IntV index, IntV value, _s]) -> do
    p <- getByteArrayContentPtr baId
    -- NOTE: CPU atomic
    original <- liftIO (peekElemOff (castPtr p) index :: IO Int)
    liftIO $ pokeElemOff (castPtr p) index (original `xor` value)
    pure [IntV original]

  _ -> fallback op args t tc
