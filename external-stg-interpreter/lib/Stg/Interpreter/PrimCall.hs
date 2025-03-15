
module Stg.Interpreter.PrimCall where

import           Control.Applicative               (Applicative (..))
import           Control.Monad.State.Strict        (MonadIO (..), gets)

import           Data.Function                     (($))
import           Data.List                         ((++))
import           Data.Maybe                        (Maybe)

import           Foreign                           (Ptr, Storable (..), Word, Word32, Word64, castPtr, with)

import           GHC.Float                         (Double, Float)
import           GHC.Real                          (fromIntegral)

import           Stg.Interpreter.Base              (Atom (..), M, Rts (..), StgState (..), stgErrorM)
import           Stg.Interpreter.PrimOp.Exceptions (raiseEx)
import           Stg.Syntax                        (PrimCall (..), TyCon, Type)

import           Text.Show                         (Show (..))

pattern WordV :: Word -> Atom
pattern WordV i   = WordAtom i
pattern FloatV :: Float -> Atom
pattern FloatV f  = FloatAtom f
pattern DoubleV :: Double -> Atom
pattern DoubleV d = DoubleAtom d

-- HINT: prim call emulation of .cmm code, because the interpreter FFI does not support Cmm
--        the Cmm code operates on the native memory layout
--        the interpreter uses Haskell data structures for value representation

-- NOTE: the WordV should contain a 64 bit wide value

evalPrimCallOp :: PrimCall -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimCallOp pCall@(PrimCall primCallTarget _primCallUnitId) args t _tc = do
  case primCallTarget of
  -- stg_raiseDivZZerozh :: State# RealWorld -> (# State# RealWorld, Void# #)
    "stg_raiseDivZZerozh"
      | [Void] <- args
      -> do
        Rts{..} <- gets ssRtsSupport
        raiseEx rtsDivZeroException

  -- stg_raiseUnderflowzh :: State# RealWorld -> (# State# RealWorld, Void# #)
    "stg_raiseUnderflowzh"
      | [Void] <- args
      -> do
        Rts{..} <- gets ssRtsSupport
        raiseEx rtsUnderflowException

  -- stg_raiseOverflowzh :: State# RealWorld -> (# State# RealWorld, Void# #)
    "stg_raiseOverflowzh"
      | [Void] <- args
      -> do
        Rts{..} <- gets ssRtsSupport
        raiseEx rtsOverflowException

  -- stg_getThreadAllocationCounterzh :: State# RealWorld -> (# State# RealWorld, INT64 #)
    "stg_getThreadAllocationCounterzh"
      | [Void] <- args
      -> do
        i <- gets ssNextHeapAddr
        pure [IntAtom (-i)]

  -- stg_doubleToWord64zh :: Double# -> Word#
    "stg_doubleToWord64zh"
      | [DoubleV a] <- args
      -> do
        -- HINT: bit-conversion
        w <- liftIO $ with a $ \p -> peek (castPtr p :: Ptr Word64)
        pure [WordV $ fromIntegral w]

  -- stg_floatToWord32zh :: Float# -> Word#
    "stg_floatToWord32zh"
      | [FloatV a] <- args
      -> do
        -- HINT: bit-conversion
        w <- liftIO $ with a $ \p -> peek (castPtr p :: Ptr Word32)
        pure [WordV $ fromIntegral w]

  -- stg_word32ToFloatzh :: Word# -> Float#
    "stg_word32ToFloatzh"
      | [WordV a] <- args
      -> do
        -- HINT: bit-conversion
        f <- liftIO $ with (fromIntegral a :: Word32) $ \p -> peek (castPtr p :: Ptr Float)
        pure [FloatV f]

  -- stg_word64ToDoublezh :: Word# -> Double#
    "stg_word64ToDoublezh"
      | [WordV a] <- args
      -> do
        -- HINT: bit-conversion
        d <- liftIO $ with (fromIntegral a :: Word64) $ \p -> peek (castPtr p :: Ptr Double)
        pure [DoubleV d]


    _ -> stgErrorM $ "unsupported StgPrimCallOp: " ++ show pCall ++ " :: " ++ show t ++ "\n args: " ++ show args
