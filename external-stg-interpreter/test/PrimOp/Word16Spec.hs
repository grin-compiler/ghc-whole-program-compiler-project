{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns #-}

module PrimOp.Word16Spec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Word16

import GHC.Exts
import GHC.Word

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
    the Word16 uses Word# represenation,
    so manual conversion needed to Word16#
    (hopefully the Word16 upper bytes are zeros)
-}
unboxWord16 :: Word16 -> Word16#
unboxWord16 (W16# x) = x

boxWord16 :: Word16# -> Word
boxWord16 x = W# (word16ToWord# x)

unboxWord :: Word -> Word#
unboxWord (W# x) = x

spec :: Spec
spec = do

  describe "Word16" $ do

    it "extendWord16#" $
      property $ forAll (arbitrary :: Gen Word16) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [WordV stgVal] <- evalOp "extendWord16#" [Word16V $ fromIntegral a]

        assert $ stgVal == (W# (word16ToWord# (unboxWord16 a)))

    it "narrowWord16#" $
      property $ forAll (arbitrary :: Gen Word) $ \a -> monadicIO $ do
        [Word16V stgVal] <- evalOp "narrowWord16#" [WordV a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord16 (wordToWord16# (unboxWord a)))

    it "notWord16#" $
      property $ forAll (arbitrary :: Gen Word16) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word16V stgVal] <- evalOp "notWord16#" [Word16V $ fromIntegral a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord16 (notWord16# (unboxWord16 a)))

    it "plusWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word16V stgVal] <- evalOp "plusWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord16 (plusWord16# (unboxWord16 a) (unboxWord16 b)))

    it "subWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word16V stgVal] <- evalOp "subWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord16 (subWord16# (unboxWord16 a) (unboxWord16 b)))

    it "timesWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word16V stgVal] <- evalOp "timesWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord16 (timesWord16# (unboxWord16 a) (unboxWord16 b)))

    it "quotWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, NonZero Word16)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word16V stgVal] <- evalOp "quotWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord16 (quotWord16# (unboxWord16 a) (unboxWord16 b)))

    it "remWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, NonZero Word16)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word16V stgVal] <- evalOp "remWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord16 (remWord16# (unboxWord16 a) (unboxWord16 b)))

    it "quotRemWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, NonZero Word16)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word16V stgVal1, Word16V stgVal2] <- evalOp "quotRemWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        -- HINT: compare the wider representations
        let !(# x, y #) = quotRemWord16# (unboxWord16 a) (unboxWord16 b)
        assert $ (stgVal1, stgVal2) == (boxWord16 x, boxWord16 y)

    it "eqWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "eqWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        assert $ stgVal == (I# (eqWord16# (unboxWord16 a) (unboxWord16 b)))

    it "geWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "geWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        assert $ stgVal == (I# (geWord16# (unboxWord16 a) (unboxWord16 b)))

    it "gtWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "gtWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        assert $ stgVal == (I# (gtWord16# (unboxWord16 a) (unboxWord16 b)))

    it "leWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "leWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        assert $ stgVal == (I# (leWord16# (unboxWord16 a) (unboxWord16 b)))

    it "ltWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "ltWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        assert $ stgVal == (I# (ltWord16# (unboxWord16 a) (unboxWord16 b)))

    it "neWord16#" $
      property $ forAll (arbitrary :: Gen (Word16, Word16)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "neWord16#" [Word16V $ fromIntegral a, Word16V $ fromIntegral b]

        assert $ stgVal == (I# (neWord16# (unboxWord16 a) (unboxWord16 b)))
