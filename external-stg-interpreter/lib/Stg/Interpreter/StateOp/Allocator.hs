{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.StateOp.Allocator where

import GHC.Stack

import Stg.Interpreter.BaseState

freshAtomAddress :: (HasCallStack, M sig m) => m AtomAddr
freshAtomAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextAtomAddr = succ ssNextAtomAddr}}
    , AddrInt ssNextAtomAddr
    )

freshHeapAddress :: (HasCallStack, M sig m) => m Addr
freshHeapAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextHeapAddr = succ ssNextHeapAddr}}
    , AddrInt ssNextHeapAddr
    )

freshStackAddress :: (HasCallStack, M sig m) => m Addr
freshStackAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStackAddr = succ ssNextStackAddr}}
    , AddrInt ssNextStackAddr
    )

freshThreadId :: (HasCallStack, M sig m) => m Addr
freshThreadId = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextThreadId = succ ssNextThreadId}}
    , AddrInt ssNextThreadId
    )

freshStablePointerAddress :: (HasCallStack, M sig m) => m Addr
freshStablePointerAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStablePointer = succ ssNextStablePointer}}
    , AddrInt ssNextStablePointer
    )

freshStableNameAddress :: (HasCallStack, M sig m) => m Addr
freshStableNameAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStableName = succ ssNextStableName}}
    , AddrInt ssNextStableName
    )

freshWeakPointerAddress :: (HasCallStack, M sig m) => m Addr
freshWeakPointerAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextWeakPointer = succ ssNextWeakPointer}}
    , AddrInt ssNextWeakPointer
    )

freshSmallArrayAddress :: (HasCallStack, M sig m) => m Addr
freshSmallArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextSmallArray = succ ssNextSmallArray}}
    , AddrInt ssNextSmallArray
    )

freshSmallMutableArrayAddress :: (HasCallStack, M sig m) => m Addr
freshSmallMutableArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextSmallMutableArray = succ ssNextSmallMutableArray}}
    , AddrInt ssNextSmallMutableArray
    )

freshArrayAddress :: (HasCallStack, M sig m) => m Addr
freshArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextArray = succ ssNextArray}}
    , AddrInt ssNextArray
    )

freshMutableArrayAddress :: (HasCallStack, M sig m) => m Addr
freshMutableArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableArray = succ ssNextMutableArray}}
    , AddrInt ssNextMutableArray
    )

freshArrayArrayAddress :: (HasCallStack, M sig m) => m Addr
freshArrayArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextArrayArray = succ ssNextArrayArray}}
    , AddrInt ssNextArrayArray
    )

freshMutableArrayArrayAddress :: (HasCallStack, M sig m) => m Addr
freshMutableArrayArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableArrayArray = succ ssNextMutableArrayArray}}
    , AddrInt ssNextMutableArrayArray
    )

freshMutableByteArrayAddress :: (HasCallStack, M sig m) => m Addr
freshMutableByteArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableByteArray = succ ssNextMutableByteArray}}
    , AddrInt ssNextMutableByteArray
    )

freshMVarAddress :: (HasCallStack, M sig m) => m Addr
freshMVarAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMVar = succ ssNextMVar}}
    , AddrInt ssNextMVar
    )
