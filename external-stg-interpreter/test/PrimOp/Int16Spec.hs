{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns #-}

module PrimOp.Int16Spec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Int16

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
    the Int16 uses Int# represenation,
    so manual conversion needed to Int16#
    (hopefully the Int16 upper bytes are zeros)
-}
unboxInt16 :: Int16 -> Int16#
unboxInt16 (I16# x) = x

boxInt16 :: Int16# -> Int
boxInt16 x = I# (int16ToInt# x)

unboxInt :: Int -> Int#
unboxInt (I# x) = x

spec :: Spec
spec = do

  describe "Int16" $ do

    it "extendInt16#" $
      property $ forAll (arbitrary :: Gen Int16) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "extendInt16#" [Int16V $ fromIntegral a]

        assert $ stgVal == (I# (int16ToInt# (unboxInt16 a)))

    it "narrowInt16#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [Int16V stgVal] <- evalOp "narrowInt16#" [IntV a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt16 (intToInt16# (unboxInt a)))

    it "negateInt16#" $
      property $ forAll (arbitrary :: Gen Int16) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int16V stgVal] <- evalOp "negateInt16#" [Int16V $ fromIntegral a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt16 (negateInt16# (unboxInt16 a)))

    it "plusInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int16V stgVal] <- evalOp "plusInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt16 (plusInt16# (unboxInt16 a) (unboxInt16 b)))

    it "subInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int16V stgVal] <- evalOp "subInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt16 (subInt16# (unboxInt16 a) (unboxInt16 b)))

    it "timesInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int16V stgVal] <- evalOp "timesInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt16 (timesInt16# (unboxInt16 a) (unboxInt16 b)))

    it "quotInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, NonZero Int16)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int16V stgVal] <- evalOp "quotInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt16 (quotInt16# (unboxInt16 a) (unboxInt16 b)))

    it "remInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, NonZero Int16)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int16V stgVal] <- evalOp "remInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxInt16 (remInt16# (unboxInt16 a) (unboxInt16 b)))

    it "quotRemInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, NonZero Int16)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Int16V stgVal1, Int16V stgVal2] <- evalOp "quotRemInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        -- HINT: compare the wider representations
        let !(# x, y #) = quotRemInt16# (unboxInt16 a) (unboxInt16 b)
        assert $ (stgVal1, stgVal2) == (boxInt16 x, boxInt16 y)

    it "eqInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "eqInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        assert $ stgVal == (I# (eqInt16# (unboxInt16 a) (unboxInt16 b)))

    it "geInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "geInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        assert $ stgVal == (I# (geInt16# (unboxInt16 a) (unboxInt16 b)))

    it "gtInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "gtInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        assert $ stgVal == (I# (gtInt16# (unboxInt16 a) (unboxInt16 b)))

    it "leInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "leInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        assert $ stgVal == (I# (leInt16# (unboxInt16 a) (unboxInt16 b)))

    it "ltInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "ltInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        assert $ stgVal == (I# (ltInt16# (unboxInt16 a) (unboxInt16 b)))

    it "neInt16#" $
      property $ forAll (arbitrary :: Gen (Int16, Int16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "neInt16#" [Int16V $ fromIntegral a, Int16V $ fromIntegral b]

        assert $ stgVal == (I# (neInt16# (unboxInt16 a) (unboxInt16 b)))
