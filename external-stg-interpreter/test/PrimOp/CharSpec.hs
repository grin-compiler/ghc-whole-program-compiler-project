{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, Strict #-}

module PrimOp.CharSpec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Char

import GHC.Exts

runTests :: IO ()
runTests = hspec spec

evalOp :: Name -> [Atom] -> PropertyM IO [Atom]
evalOp op args = run $ do
  let dummyType   = PolymorphicRep
      dummyTyCon  = Nothing
      dummyFun    = \_ _ _ _ -> pure []
      value = evalPrimOp dummyFun op args dummyType dummyTyCon
  evalStateT value fakeStgStateForPrimopTests

unboxChar :: Char -> Char#
unboxChar (C# x) = x

spec :: Spec
spec = do

  describe "Char" $ do

    it "gtChar#" $
      property $ forAll (arbitrary :: Gen (Char, Char)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "gtChar#" [CharV a, CharV b]
        assert $ stgVal == (I# (gtChar# (unboxChar a) (unboxChar b)))

    it "geChar#" $
      property $ forAll (arbitrary :: Gen (Char, Char)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "geChar#" [CharV a, CharV b]
        assert $ stgVal == (I# (geChar# (unboxChar a) (unboxChar b)))

    it "eqChar#" $
      property $ forAll (arbitrary :: Gen (Char, Char)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "eqChar#" [CharV a, CharV b]
        assert $ stgVal == (I# (eqChar# (unboxChar a) (unboxChar b)))

    it "neChar#" $
      property $ forAll (arbitrary :: Gen (Char, Char)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "neChar#" [CharV a, CharV b]
        assert $ stgVal == (I# (neChar# (unboxChar a) (unboxChar b)))

    it "ltChar#" $
      property $ forAll (arbitrary :: Gen (Char, Char)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "ltChar#" [CharV a, CharV b]
        assert $ stgVal == (I# (ltChar# (unboxChar a) (unboxChar b)))

    it "leChar#" $
      property $ forAll (arbitrary :: Gen (Char, Char)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "leChar#" [CharV a, CharV b]
        assert $ stgVal == (I# (leChar# (unboxChar a) (unboxChar b)))

    it "ord#" $
      property $ forAll (arbitrary :: Gen Char) $ \a -> monadicIO $ do
        [IntV stgVal] <- evalOp "ord#" [CharV a]
        assert $ stgVal == (I# (ord# (unboxChar a)))
