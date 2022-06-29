{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.PrimCall where

import Foreign

import Stg.Syntax
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Exceptions

pattern WordV i   = WordAtom i
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

-- HINT: prim call emulation of .cmm code, because the interpreter FFI does not support Cmm
--        the Cmm code operates on the native memory layout
--        the interpreter uses Haskell data structures for value representation

-- NOTE: the WordV should contain a 64 bit wide value

evalPrimCallOp :: M sig m => PrimCall -> [AtomAddr] -> Type -> Maybe TyCon -> m [AtomAddr]
evalPrimCallOp pCall@(PrimCall primCallTarget primCallUnitId) argsAddr t _tc = do
  args <- getAtoms argsAddr
  case primCallTarget of
  -- stg_raiseDivZZerozh :: State# RealWorld -> (# State# RealWorld, Void# #)
    "stg_raiseDivZZerozh"
      | [Void] <- args
      -> do
        RtsBaseInterop{..} <- gets ssRtsBaseInterop
        raiseEx rtsDivZeroException

  -- stg_raiseUnderflowzh :: State# RealWorld -> (# State# RealWorld, Void# #)
    "stg_raiseUnderflowzh"
      | [Void] <- args
      -> do
        RtsBaseInterop{..} <- gets ssRtsBaseInterop
        raiseEx rtsUnderflowException

  -- stg_raiseOverflowzh :: State# RealWorld -> (# State# RealWorld, Void# #)
    "stg_raiseOverflowzh"
      | [Void] <- args
      -> do
        RtsBaseInterop{..} <- gets ssRtsBaseInterop
        raiseEx rtsOverflowException

  -- stg_getThreadAllocationCounterzh :: State# RealWorld -> (# State# RealWorld, INT64 #)
    "stg_getThreadAllocationCounterzh"
      | [Void] <- args
      -> do
        i <- gets $ ssNextHeapAddr . ssAllocator
        allocAtoms [IntAtom (-i)]

  -- stg_doubleToWord64zh :: Double# -> Word#
    "stg_doubleToWord64zh"
      | [DoubleV a] <- args
      -> do
        -- HINT: bit-conversion
        w <- sendIO $ with a $ \p -> peek (castPtr p :: Ptr Word64)
        allocAtoms [WordV $ fromIntegral w]

  -- stg_floatToWord32zh :: Float# -> Word#
    "stg_floatToWord32zh"
      | [FloatV a] <- args
      -> do
        -- HINT: bit-conversion
        w <- sendIO $ with a $ \p -> peek (castPtr p :: Ptr Word32)
        allocAtoms [WordV $ fromIntegral w]

  -- stg_word32ToFloatzh :: Word# -> Float#
    "stg_word32ToFloatzh"
      | [WordV a] <- args
      -> do
        -- HINT: bit-conversion
        f <- sendIO $ with (fromIntegral a :: Word32) $ \p -> peek (castPtr p :: Ptr Float)
        allocAtoms [FloatV f]

  -- stg_word64ToDoublezh :: Word# -> Double#
    "stg_word64ToDoublezh"
      | [WordV a] <- args
      -> do
        -- HINT: bit-conversion
        d <- sendIO $ with (fromIntegral a :: Word64) $ \p -> peek (castPtr p :: Ptr Double)
        allocAtoms [DoubleV d]

    _ -> stgErrorM $ "unsupported StgPrimCallOp: " ++ show pCall ++ " :: " ++ show t ++ "\n args: " ++ show args
