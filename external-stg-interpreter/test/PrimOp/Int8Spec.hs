{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns #-}

module PrimOp.Int8Spec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Int8

import GHC.Exts
import GHC.Int

runTests :: IO ()
runTests = hspec spec

evalOp :: Name -> [Atom] -> PropertyM IO [Atom]
evalOp op args = run $ do
  let dummyType   = PolymorphicRep
      dummyTyCon  = Nothing
      dummyFun    = \_ _ _ _ -> pure []
      value = evalPrimOp dummyFun op args dummyType dummyTyCon
  evalStateT value fakeStgStateForPrimopTests

{-
  NOTE:
    the Int8 uses Int# represenation,
    so manual conversion needed to Int8#
    (hopefully the Int8 upper bytes are zeros)
-}
unboxInt8 :: Int8 -> Int8#
unboxInt8 (I8# x) = x

boxInt8 :: Int8# -> Int
boxInt8 x = I# (int8ToInt# x)

unboxInt :: Int -> Int#
unboxInt (I# x) = x

spec :: Spec
spec = do

  describe "Int8" $ do

    it "extendInt8#" $
      property $ forAll (arbitrary :: Gen Int8) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "extendInt8#" [Int8V $ fromIntegral a]

        assert $ stgVal == (I# (int8ToInt# (unboxInt8 a)))

    it "narrowInt8#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [Int8V stgVal] <- evalOp "narrowInt8#" [IntV a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt8 (intToInt8# (unboxInt a)))

    it "negateInt8#" $
      property $ forAll (arbitrary :: Gen Int8) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int8V stgVal] <- evalOp "negateInt8#" [Int8V $ fromIntegral a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt8 (negateInt8# (unboxInt8 a)))

    it "plusInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int8V stgVal] <- evalOp "plusInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt8 (plusInt8# (unboxInt8 a) (unboxInt8 b)))

    it "subInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int8V stgVal] <- evalOp "subInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt8 (subInt8# (unboxInt8 a) (unboxInt8 b)))

    it "timesInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int8V stgVal] <- evalOp "timesInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt8 (timesInt8# (unboxInt8 a) (unboxInt8 b)))

    it "quotInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, NonZero Int8)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int8V stgVal] <- evalOp "quotInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt8 (quotInt8# (unboxInt8 a) (unboxInt8 b)))

    it "remInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, NonZero Int8)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int8V stgVal] <- evalOp "remInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt8 (remInt8# (unboxInt8 a) (unboxInt8 b)))

    it "quotRemInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, NonZero Int8)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int8V stgVal1, Int8V stgVal2] <- evalOp "quotRemInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        -- HINT: compare the wider representations
        let !(# x, y #) = quotRemInt8# (unboxInt8 a) (unboxInt8 b)
        assert $ (stgVal1, stgVal2) == (boxInt8 x, boxInt8 y)

    it "eqInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "eqInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        assert $ stgVal == (I# (eqInt8# (unboxInt8 a) (unboxInt8 b)))

    it "geInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "geInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        assert $ stgVal == (I# (geInt8# (unboxInt8 a) (unboxInt8 b)))

    it "gtInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "gtInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        assert $ stgVal == (I# (gtInt8# (unboxInt8 a) (unboxInt8 b)))

    it "leInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "leInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        assert $ stgVal == (I# (leInt8# (unboxInt8 a) (unboxInt8 b)))

    it "ltInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "ltInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        assert $ stgVal == (I# (ltInt8# (unboxInt8 a) (unboxInt8 b)))

    it "neInt8#" $
      property $ forAll (arbitrary :: Gen (Int8, Int8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "neInt8#" [Int8V $ fromIntegral a, Int8V $ fromIntegral b]

        assert $ stgVal == (I# (neInt8# (unboxInt8 a) (unboxInt8 b)))
