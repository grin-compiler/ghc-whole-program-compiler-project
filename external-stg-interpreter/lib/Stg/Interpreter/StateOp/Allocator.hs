{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Stg.Interpreter.StateOp.Allocator where

import GHC.Stack

import Stg.Interpreter.BaseState

freshAtomAddress :: (HasCallStack, M sig m) => m AtomAddr
freshAtomAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextAtomAddr = succ ssNextAtomAddr}}
    , ssNextAtomAddr
    )

freshHeapAddress :: (HasCallStack, M sig m) => m Addr
freshHeapAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextHeapAddr = succ ssNextHeapAddr}}
    , ssNextHeapAddr
    )

freshStackAddress :: (HasCallStack, M sig m) => m Addr
freshStackAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStackAddr = succ ssNextStackAddr}}
    , ssNextStackAddr
    )

freshThreadId :: (HasCallStack, M sig m) => m Int
freshThreadId = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextThreadId = succ ssNextThreadId}}
    , ssNextThreadId
    )

freshStablePointerAddress :: (HasCallStack, M sig m) => m Int
freshStablePointerAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStablePointer = succ ssNextStablePointer}}
    , ssNextStablePointer
    )

freshStableNameAddress :: (HasCallStack, M sig m) => m Int
freshStableNameAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStableName = succ ssNextStableName}}
    , ssNextStableName
    )

freshWeakPointerAddress :: (HasCallStack, M sig m) => m Int
freshWeakPointerAddress = do
  state $ \s@StgState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextWeakPointer = succ ssNextWeakPointer}}
    , ssNextWeakPointer
    )

freshSmallArrayAddress :: (HasCallStack, M sig m) => m Int
freshSmallArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextSmallArray = succ ssNextSmallArray}}
    , ssNextSmallArray
    )

freshSmallMutableArrayAddress :: (HasCallStack, M sig m) => m Int
freshSmallMutableArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextSmallMutableArray = succ ssNextSmallMutableArray}}
    , ssNextSmallMutableArray
    )

freshArrayAddress :: (HasCallStack, M sig m) => m Int
freshArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextArray = succ ssNextArray}}
    , ssNextArray
    )

freshMutableArrayAddress :: (HasCallStack, M sig m) => m Int
freshMutableArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableArray = succ ssNextMutableArray}}
    , ssNextMutableArray
    )

freshArrayArrayAddress :: (HasCallStack, M sig m) => m Int
freshArrayArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextArrayArray = succ ssNextArrayArray}}
    , ssNextArrayArray
    )

freshMutableArrayArrayAddress :: (HasCallStack, M sig m) => m Int
freshMutableArrayArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableArrayArray = succ ssNextMutableArrayArray}}
    , ssNextMutableArrayArray
    )

freshMutableByteArrayAddress :: (HasCallStack, M sig m) => m Int
freshMutableByteArrayAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableByteArray = succ ssNextMutableByteArray}}
    , ssNextMutableByteArray
    )

freshMVarAddress :: (HasCallStack, M sig m) => m Int
freshMVarAddress = do
  state $ \s@StgState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMVar = succ ssNextMVar}}
    , ssNextMVar
    )
