{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, Strict, MagicHash #-}
module Stg.Interpreter.PrimOp.Idris where

-- This module is a temporary module. It helps focusing on the development of
-- the Idris backend as we don't have to depend on the runtime semantics of the GHC
-- for the time being.

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
import GHC.CString
import GHC.Prim
import GHC.Ptr
import Data.Primitive.ByteArray (mutableByteArrayContents)

unPtr :: Ptr a -> Addr#
unPtr (Ptr addr) = addr

evalPrimOp :: PrimOpEval -> Name -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalPrimOp fallback op args t tc = case (op, args) of
  ("putStr#", [HeapPtr strPtr, HeapPtr worldPtr, Void]) -> do
    obj1 <- readHeap $ HeapPtr strPtr
    case obj1 of
      Con { hoCon = DataCon { dcName = "Idris.String.Lit" }, hoConArgs = [HeapPtr ptr2] } -> do
        obj2 <- readHeap $ HeapPtr ptr2
        case obj2 of
          Con { hoCon = DataCon { dcName = "Idris.String.Addr" }, hoConArgs = [ PtrAtom _ ptr ] } -> do
            lift $ putStr $ unpackCString# (unPtr ptr)
          o -> stgErrorM $ "putStr# #1:" ++ show o
      Con { hoCon = DataCon { dcName = "Idris.String.Val" }, hoConArgs = [HeapPtr ptr2] } -> do
        obj2 <- readHeap $ HeapPtr ptr2
        case obj2 of
          Con { hoCon = DataCon { dcName = "Idris.String.ByteArray" }, hoConArgs = [ ByteArray arrIdx ] } -> do
            desc <- lookupByteArrayDescriptorI arrIdx
            let mutableByteArray = baaMutableByteArray desc
            let (Ptr addr) = mutableByteArrayContents mutableByteArray
            lift $ putStr $ unpackCString# addr
          o -> stgErrorM $ "putStr# #2:" ++ show o
      o -> stgErrorM $ "putStr# #3:" ++ show o
    pure [] -- TODO

  _ -> fallback op args t tc
