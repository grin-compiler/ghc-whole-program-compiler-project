{-# LANGUAGE LambdaCase, RecordWildCards, TypeApplications, DataKinds #-}
module Stg.Interpreter.StateOp.Allocator where

import qualified Control.Effect.State.Labelled as L

import GHC.Stack

import Stg.Interpreter.BaseState

freshAtomAddress :: (HasCallStack, M sig m) => m AtomAddr
freshAtomAddress = do
  L.state @"Global" $ \s@GlobalState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextAtomAddr = succ ssNextAtomAddr}}
    , AtomAddr $ AddrInt ssNextAtomAddr
    )

freshHeapAddress :: (HasCallStack, M sig m) => m HeapAddr
freshHeapAddress = do
  L.state @"Global" $ \s@GlobalState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextHeapAddr = succ ssNextHeapAddr}}
    , HeapAddr $ AddrInt ssNextHeapAddr
    )

freshStackAddress :: (HasCallStack, M sig m) => m StackAddr
freshStackAddress = do
  L.state @"Global" $ \s@GlobalState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStackAddr = succ ssNextStackAddr}}
    , StackAddr $ AddrInt ssNextStackAddr
    )

freshThreadId :: (HasCallStack, M sig m) => m ThreadAddr
freshThreadId = do
  L.state @"Global" $ \s@GlobalState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextThreadId = succ ssNextThreadId}}
    , ThreadAddr $ AddrInt ssNextThreadId
    )

freshStablePointerAddress :: (HasCallStack, M sig m) => m StablePointerAddr
freshStablePointerAddress = do
  L.state @"Global" $ \s@GlobalState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStablePointer = succ ssNextStablePointer}}
    , StablePointerAddr $ AddrInt ssNextStablePointer
    )

freshStableNameAddress :: (HasCallStack, M sig m) => m StableNameAddr
freshStableNameAddress = do
  L.state @"Global" $ \s@GlobalState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextStableName = succ ssNextStableName}}
    , StableNameAddr $ AddrInt ssNextStableName
    )

freshWeakPointerAddress :: (HasCallStack, M sig m) => m WeakPointerAddr
freshWeakPointerAddress = do
  L.state @"Global" $ \s@GlobalState{ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextWeakPointer = succ ssNextWeakPointer}}
    , WeakPointerAddr $ AddrInt ssNextWeakPointer
    )

freshSmallArrayAddress :: (HasCallStack, M sig m) => m SmallArrayAddr
freshSmallArrayAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextSmallArray = succ ssNextSmallArray}}
    , SmallArrayAddr $ AddrInt ssNextSmallArray
    )

freshSmallMutableArrayAddress :: (HasCallStack, M sig m) => m SmallMutableArrayAddr
freshSmallMutableArrayAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextSmallMutableArray = succ ssNextSmallMutableArray}}
    , SmallMutableArrayAddr $ AddrInt ssNextSmallMutableArray
    )

freshArrayAddress :: (HasCallStack, M sig m) => m ArrayAddr
freshArrayAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextArray = succ ssNextArray}}
    , ArrayAddr $ AddrInt ssNextArray
    )

freshMutableArrayAddress :: (HasCallStack, M sig m) => m MutableArrayAddr
freshMutableArrayAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableArray = succ ssNextMutableArray}}
    , MutableArrayAddr $ AddrInt ssNextMutableArray
    )

freshArrayArrayAddress :: (HasCallStack, M sig m) => m ArrayArrayAddr
freshArrayArrayAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextArrayArray = succ ssNextArrayArray}}
    , ArrayArrayAddr $ AddrInt ssNextArrayArray
    )

freshMutableArrayArrayAddress :: (HasCallStack, M sig m) => m MutableArrayArrayAddr
freshMutableArrayArrayAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableArrayArray = succ ssNextMutableArrayArray}}
    , MutableArrayArrayAddr $ AddrInt ssNextMutableArrayArray
    )

freshMutableByteArrayAddress :: (HasCallStack, M sig m) => m MutableByteArrayAddr
freshMutableByteArrayAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutableByteArray = succ ssNextMutableByteArray}}
    , MutableByteArrayAddr $ AddrInt ssNextMutableByteArray
    )

freshMVarAddress :: (HasCallStack, M sig m) => m MVarAddr
freshMVarAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMVar = succ ssNextMVar}}
    , MVarAddr $ AddrInt ssNextMVar
    )

freshMutVarAddress :: (HasCallStack, M sig m) => m MutVarAddr
freshMutVarAddress = do
  L.state @"Global" $ \s@GlobalState{ ssAllocator = a@AllocatorState{..}} ->
    ( s {ssAllocator = a {ssNextMutVar = succ ssNextMutVar}}
    , MutVarAddr $ AddrInt ssNextMutVar
    )
