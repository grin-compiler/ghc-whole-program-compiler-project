{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns #-}

module PrimOp.Word8Spec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Word8

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
    the Word8 uses Word# represenation,
    so manual conversion needed to Word8#
    (hopefully the Word8 upper bytes are zeros)
-}
unboxWord8 :: Word8 -> Word8#
unboxWord8 (W8# x) = x

boxWord8 :: Word8# -> Word
boxWord8 x = W# (word8ToWord# x)

unboxWord :: Word -> Word#
unboxWord (W# x) = x

spec :: Spec
spec = do

  describe "Word8" $ do

    it "extendWord8#" $
      property $ forAll (arbitrary :: Gen Word8) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [WordV stgVal] <- evalOp "extendWord8#" [Word8V $ fromIntegral a]

        assert $ stgVal == (W# (word8ToWord# (unboxWord8 a)))

    it "narrowWord8#" $
      property $ forAll (arbitrary :: Gen Word) $ \a -> monadicIO $ do
        [Word8V stgVal] <- evalOp "narrowWord8#" [WordV a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord8 (wordToWord8# (unboxWord a)))

    it "notWord8#" $
      property $ forAll (arbitrary :: Gen Word8) $ \a -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word8V stgVal] <- evalOp "notWord8#" [Word8V $ fromIntegral a]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord8 (notWord8# (unboxWord8 a)))

    it "plusWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word8V stgVal] <- evalOp "plusWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord8 (plusWord8# (unboxWord8 a) (unboxWord8 b)))

    it "subWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word8V stgVal] <- evalOp "subWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord8 (subWord8# (unboxWord8 a) (unboxWord8 b)))

    it "timesWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word8V stgVal] <- evalOp "timesWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord8 (timesWord8# (unboxWord8 a) (unboxWord8 b)))

    it "quotWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, NonZero Word8)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word8V stgVal] <- evalOp "quotWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord8 (quotWord8# (unboxWord8 a) (unboxWord8 b)))

    it "remWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, NonZero Word8)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word8V stgVal] <- evalOp "remWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        -- HINT: compare the wider representations
        assert $ stgVal == (boxWord8 (remWord8# (unboxWord8 a) (unboxWord8 b)))

    it "quotRemWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, NonZero Word8)) $ \(a, NonZero b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [Word8V stgVal1, Word8V stgVal2] <- evalOp "quotRemWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        -- HINT: compare the wider representations
        let !(# x, y #) = quotRemWord8# (unboxWord8 a) (unboxWord8 b)
        assert $ (stgVal1, stgVal2) == (boxWord8 x, boxWord8 y)

    it "eqWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "eqWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        assert $ stgVal == (I# (eqWord8# (unboxWord8 a) (unboxWord8 b)))

    it "geWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "geWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        assert $ stgVal == (I# (geWord8# (unboxWord8 a) (unboxWord8 b)))

    it "gtWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "gtWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        assert $ stgVal == (I# (gtWord8# (unboxWord8 a) (unboxWord8 b)))

    it "leWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "leWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        assert $ stgVal == (I# (leWord8# (unboxWord8 a) (unboxWord8 b)))

    it "ltWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "ltWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        assert $ stgVal == (I# (ltWord8# (unboxWord8 a) (unboxWord8 b)))

    it "neWord8#" $
      property $ forAll (arbitrary :: Gen (Word8, Word8)) $ \(a, b) -> monadicIO $ do
        -- HINT: it is safe to extend representation size
        [IntV stgVal] <- evalOp "neWord8#" [Word8V $ fromIntegral a, Word8V $ fromIntegral b]

        assert $ stgVal == (I# (neWord8# (unboxWord8 a) (unboxWord8 b)))
