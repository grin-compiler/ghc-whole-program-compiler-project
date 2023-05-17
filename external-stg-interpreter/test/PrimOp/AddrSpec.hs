{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, CPP, ScopedTypeVariables #-}

module PrimOp.AddrSpec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Addr

import Foreign.Ptr
import Data.Word
import GHC.Exts

runTests :: IO ()
runTests = hspec spec

evalOp :: Name -> [Atom] -> IO [Atom]
evalOp op args = do
  let dummyType   = PolymorphicRep
      dummyTyCon  = Nothing
      dummyFun    = \_ _ _ _ -> pure []
      value = evalPrimOp dummyFun op args dummyType dummyTyCon
  evalStateT value fakeStgStateForPrimopTests

unboxPtr :: Ptr Word8 -> Addr#
unboxPtr (Ptr x) = x

unboxInt :: Int -> Int#
unboxInt (I# x) = x

shouldReturnShow :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnShow m a = fmap show m `shouldReturn` show a

spec :: Spec
spec = do

  describe "Addr" $ do

    it "plusAddr#" $
      property $ \(a' :: Word, b :: Int) -> do
        let a = wordPtrToPtr $ WordPtr a'
        evalOp "plusAddr#" [PtrAtom RawPtr a, IntV b] `shouldReturn` [PtrAtom RawPtr (Ptr (plusAddr# (unboxPtr a) (unboxInt b)))]

    it "minusAddr#" $
      property $ \(a' :: Word, b' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
            b = wordPtrToPtr $ WordPtr b'
        evalOp "minusAddr#" [PtrAtom RawPtr a, PtrAtom RawPtr b] `shouldReturn` [IntV (I# (minusAddr# (unboxPtr a) (unboxPtr b)))]

    it "remAddr#" $
      property $ \(a' :: Word, NonZero b :: NonZero Int) -> do
        let a = wordPtrToPtr $ WordPtr a'
        evalOp "remAddr#" [PtrAtom RawPtr a, IntV b] `shouldReturn` [IntV (I# (remAddr# (unboxPtr a) (unboxInt b)))]

    it "addr2Int#" $
      property $ \(a' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
        evalOp "addr2Int#" [PtrAtom RawPtr a] `shouldReturn` [IntV (I# (addr2Int# (unboxPtr a)))]

    it "int2Addr#" $
      property $ \(a :: Int) -> do
        evalOp "int2Addr#" [IntV a] `shouldReturn` [PtrAtom RawPtr (Ptr (int2Addr# (unboxInt a)))]

    it "gtAddr#" $
      property $ \(a' :: Word, b' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
            b = wordPtrToPtr $ WordPtr b'
        evalOp "gtAddr#" [PtrAtom RawPtr a, PtrAtom RawPtr b] `shouldReturn` [IntV (I# (gtAddr# (unboxPtr a) (unboxPtr b)))]

    it "geAddr#" $
      property $ \(a' :: Word, b' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
            b = wordPtrToPtr $ WordPtr b'
        evalOp "geAddr#" [PtrAtom RawPtr a, PtrAtom RawPtr b] `shouldReturn` [IntV (I# (geAddr# (unboxPtr a) (unboxPtr b)))]

    it "eqAddr#" $
      property $ \(a' :: Word, b' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
            b = wordPtrToPtr $ WordPtr b'
        evalOp "eqAddr#" [PtrAtom RawPtr a, PtrAtom RawPtr b] `shouldReturn` [IntV (I# (eqAddr# (unboxPtr a) (unboxPtr b)))]

    it "neAddr#" $
      property $ \(a' :: Word, b' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
            b = wordPtrToPtr $ WordPtr b'
        evalOp "neAddr#" [PtrAtom RawPtr a, PtrAtom RawPtr b] `shouldReturn` [IntV (I# (neAddr# (unboxPtr a) (unboxPtr b)))]

    it "ltAddr#" $
      property $ \(a' :: Word, b' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
            b = wordPtrToPtr $ WordPtr b'
        evalOp "ltAddr#" [PtrAtom RawPtr a, PtrAtom RawPtr b] `shouldReturn` [IntV (I# (ltAddr# (unboxPtr a) (unboxPtr b)))]

    it "leAddr#" $
      property $ \(a' :: Word, b' :: Word) -> do
        let a = wordPtrToPtr $ WordPtr a'
            b = wordPtrToPtr $ WordPtr b'
        evalOp "leAddr#" [PtrAtom RawPtr a, PtrAtom RawPtr b] `shouldReturn` [IntV (I# (leAddr# (unboxPtr a) (unboxPtr b)))]
