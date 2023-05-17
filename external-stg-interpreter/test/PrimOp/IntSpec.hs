{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, CPP #-}

module PrimOp.IntSpec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Int

import GHC.Exts
import Data.Char

runTests :: IO ()
runTests = hspec spec

evalOp :: Name -> [Atom] -> PropertyM IO [Atom]
evalOp op args = run $ do
  let dummyType   = PolymorphicRep
      dummyTyCon  = Nothing
      dummyFun    = \_ _ _ _ -> pure []
      value = evalPrimOp dummyFun op args dummyType dummyTyCon
  evalStateT value fakeStgStateForPrimopTests

unboxInt :: Int -> Int#
unboxInt (I# x) = x

unboxWord :: Word -> Word#
unboxWord (W# x) = x

spec :: Spec
spec = do

  describe "Int" $ do

    it "+#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "+#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) +# (unboxInt b)))

    it "-#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "-#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) -# (unboxInt b)))

    it "*#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "*#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) *# (unboxInt b)))

#if __GLASGOW_HASKELL__ >= 900
    it "timesInt2#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal1, IntV stgVal2, IntV stgVal3] <- evalOp "timesInt2#" [IntV a, IntV b]

        let !(# x, y, z #) = timesInt2# (unboxInt a) (unboxInt b)
        assert $ (stgVal1, stgVal2, stgVal3) == (I# x, I# y, I# z)
#endif

    it "mulIntMayOflo#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "mulIntMayOflo#" [IntV a, IntV b]
        assert $ stgVal == (I# (mulIntMayOflo# (unboxInt a) (unboxInt b)))

    it "quotInt#" $
      property $ forAll (arbitrary :: Gen (Int, NonZero Int)) $ \(a, NonZero b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "quotInt#" [IntV a, IntV b]
        assert $ stgVal == (I# (quotInt# (unboxInt a) (unboxInt b)))

    it "remInt#" $
      property $ forAll (arbitrary :: Gen (Int, NonZero Int)) $ \(a, NonZero b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "remInt#" [IntV a, IntV b]
        assert $ stgVal == (I# (remInt# (unboxInt a) (unboxInt b)))

    it "quotRemInt#" $
      property $ forAll (arbitrary :: Gen (Int, NonZero Int)) $ \(a, NonZero b) -> monadicIO $ do
        [IntV stgVal1, IntV stgVal2] <- evalOp "quotRemInt#" [IntV a, IntV b]

        let !(# x, y #) = quotRemInt# (unboxInt a) (unboxInt b)
        assert $ (stgVal1, stgVal2) == (I# x, I# y)

    it "andI#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "andI#" [IntV a, IntV b]
        assert $ stgVal == (I# (andI# (unboxInt a) (unboxInt b)))

    it "orI#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "orI#" [IntV a, IntV b]
        assert $ stgVal == (I# (orI# (unboxInt a) (unboxInt b)))

    it "xorI#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "xorI#" [IntV a, IntV b]
        assert $ stgVal == (I# (xorI# (unboxInt a) (unboxInt b)))

    it "notI#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [IntV stgVal] <- evalOp "notI#" [IntV a]
        assert $ stgVal == (I# (notI# (unboxInt a)))

    it "negateInt#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [IntV stgVal] <- evalOp "negateInt#" [IntV a]
        assert $ stgVal == (I# (negateInt# (unboxInt a)))

    it "addIntC#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal1, IntV stgVal2] <- evalOp "addIntC#" [IntV a, IntV b]

        let !(# x, y #) = addIntC# (unboxInt a) (unboxInt b)
        assert $ (stgVal1, stgVal2) == (I# x, I# y)

    it "subIntC#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal1, IntV stgVal2] <- evalOp "subIntC#" [IntV a, IntV b]

        let !(# x, y #) = subIntC# (unboxInt a) (unboxInt b)
        assert $ (stgVal1, stgVal2) == (I# x, I# y)

    it ">#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp ">#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) ># (unboxInt b)))

    it ">=#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp ">=#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) >=# (unboxInt b)))

    it "==#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "==#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) ==# (unboxInt b)))

    it "/=#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "/=#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) /=# (unboxInt b)))

    it "<#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "<#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) <# (unboxInt b)))

    it "<=#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "<=#" [IntV a, IntV b]
        assert $ stgVal == (I# ((unboxInt a) <=# (unboxInt b)))

    it "chr#" $
      property $ forAll (arbitrary :: Gen Char) $ \a -> monadicIO $ do
        -- NOTE: chr# is valid in the unicode range: 0x0 .. 0x10FFFF
        [CharV stgVal] <- evalOp "chr#" [IntV $ ord a]
        assert $ stgVal == (C# (chr# (unboxInt $ ord a)))

    it "int2Word#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [WordV stgVal] <- evalOp "int2Word#" [IntV a]
        assert $ stgVal == (W# (int2Word# (unboxInt a)))

    it "int2Float#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "int2Float#" [IntV a]
        assert $ stgVal == (F# (int2Float# (unboxInt a)))

    it "int2Double#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [DoubleV stgVal] <- evalOp "int2Double#" [IntV a]
        assert $ stgVal == (D# (int2Double# (unboxInt a)))

    it "word2Float#" $
      property $ forAll (arbitrary :: Gen Word) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "word2Float#" [WordV a]
        assert $ stgVal == (F# (word2Float# (unboxWord a)))

    it "word2Double#" $
      property $ forAll (arbitrary :: Gen Word) $ \a -> monadicIO $ do
        [DoubleV stgVal] <- evalOp "word2Double#" [WordV a]
        assert $ stgVal == (D# (word2Double# (unboxWord a)))

    it "uncheckedIShiftL#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "uncheckedIShiftL#" [IntV a, IntV b]
        assert $ stgVal == (I# (uncheckedIShiftL# (unboxInt a) (unboxInt b)))

    it "uncheckedIShiftRA#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "uncheckedIShiftRA#" [IntV a, IntV b]
        assert $ stgVal == (I# (uncheckedIShiftRA# (unboxInt a) (unboxInt b)))

    it "uncheckedIShiftRL#" $
      property $ forAll (arbitrary :: Gen (Int, Int)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "uncheckedIShiftRL#" [IntV a, IntV b]
        assert $ stgVal == (I# (uncheckedIShiftRL# (unboxInt a) (unboxInt b)))
