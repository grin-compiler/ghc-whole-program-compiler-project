{-# LANGUAGE LambdaCase #-}
module Stg.Interpreter.StateOp.PrimTypes where

import GHC.Stack
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Internal as BS
import Data.ByteString.Char8 (ByteString)
import Data.Vector (Vector)
import Data.Word
import Foreign.Ptr
import Foreign.ForeignPtr.Unsafe

import Stg.Interpreter.BaseState
import Stg.Interpreter.StateOp.Atom

lookupWeakPointerDescriptor :: (HasCallStack, M sig m) => WeakPointerAddr -> m WeakPtrDescriptor
lookupWeakPointerDescriptor wpId = do
  Map.lookup wpId <$> gets ssWeakPointers >>= \case
    Nothing -> stgErrorM $ "unknown WeakPointer: " ++ show wpId
    Just a  -> pure a

lookupStablePointerPtr :: (HasCallStack, M sig m) => Ptr Word8 -> m AtomAddr
lookupStablePointerPtr sp = do
  let IntPtr spId = ptrToIntPtr sp
  lookupStablePointer . StablePointerAddr $ AddrInt spId

lookupStablePointer :: (HasCallStack, M sig m) => StablePointerAddr -> m AtomAddr
lookupStablePointer spId = do
  Map.lookup spId <$> gets ssStablePointers >>= \case
    Nothing -> stgErrorM $ "unknown StablePointer: " ++ show spId
    Just a  -> pure a

lookupMVar :: (HasCallStack, M sig m) => MVarAddr -> m MVarDescriptor
lookupMVar m = do
  Map.lookup m <$> gets ssMVars >>= \case
    Nothing -> stgErrorM $ "unknown MVar: " ++ show m
    Just a  -> pure a

lookupArray :: (HasCallStack, M sig m) => ArrayAddr -> m (Vector AtomAddr)
lookupArray m = do
  Map.lookup m <$> gets ssArrays >>= \case
    Nothing -> stgErrorM $ "unknown Array: " ++ show m
    Just a  -> pure a

lookupMutableArray :: (HasCallStack, M sig m) => MutableArrayAddr -> m (Vector AtomAddr)
lookupMutableArray m = do
  Map.lookup m <$> gets ssMutableArrays >>= \case
    Nothing -> stgErrorM $ "unknown MutableArray: " ++ show m
    Just a  -> pure a

lookupSmallArray :: (HasCallStack, M sig m) => SmallArrayAddr -> m (Vector AtomAddr)
lookupSmallArray m = do
  Map.lookup m <$> gets ssSmallArrays >>= \case
    Nothing -> stgErrorM $ "unknown SmallArray: " ++ show m
    Just a  -> pure a

lookupSmallMutableArray :: (HasCallStack, M sig m) => SmallMutableArrayAddr -> m (Vector AtomAddr)
lookupSmallMutableArray m = do
  Map.lookup m <$> gets ssSmallMutableArrays >>= \case
    Nothing -> stgErrorM $ "unknown SmallMutableArray: " ++ show m
    Just a  -> pure a

lookupArrayArray :: (HasCallStack, M sig m) => ArrayArrayAddr -> m (Vector AtomAddr)
lookupArrayArray m = do
  Map.lookup m <$> gets ssArrayArrays >>= \case
    Nothing -> stgErrorM $ "unknown ArrayArray: " ++ show m
    Just a  -> pure a

lookupMutableArrayArray :: (HasCallStack, M sig m) => MutableArrayArrayAddr -> m (Vector AtomAddr)
lookupMutableArrayArray m = do
  Map.lookup m <$> gets ssMutableArrayArrays >>= \case
    Nothing -> stgErrorM $ "unknown MutableArrayArray: " ++ show m
    Just a  -> pure a

lookupByteArrayDescriptor :: (HasCallStack, M sig m) => MutableByteArrayAddr -> m ByteArrayDescriptor
lookupByteArrayDescriptor m = do
  Map.lookup m <$> gets ssMutableByteArrays >>= \case
    Nothing -> stgErrorM $ "unknown ByteArrayDescriptor: " ++ show m
    Just a  -> pure a

lookupByteArrayDescriptorI :: (HasCallStack, M sig m) => ByteArrayIdx -> m ByteArrayDescriptor
lookupByteArrayDescriptorI = lookupByteArrayDescriptor . baId

-- string constants
-- NOTE: the string gets extended with a null terminator
getCStringConstantPtrAtom :: M sig m => ByteString -> m AtomAddr
getCStringConstantPtrAtom key = do
  strMap <- gets ssCStringConstants
  case Map.lookup key strMap of
    Just a  -> pure a
    Nothing -> do
      let bsCString = BS8.snoc key '\0'
          (bsFPtr, bsOffset, _bsLen) = BS.toForeignPtr bsCString
          a = PtrAtom (CStringPtr bsCString) $ plusPtr (unsafeForeignPtrToPtr bsFPtr) bsOffset
      addr <- storeNewAtom a
      modify $ \s -> s {ssCStringConstants = Map.insert key addr strMap}
      pure addr
