{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, CPP, ScopedTypeVariables #-}

module PrimOp.FloatSpec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Float

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

evalOp2 :: Name -> [Atom] -> IO [Atom]
evalOp2 op args = do
  let dummyType   = PolymorphicRep
      dummyTyCon  = Nothing
      dummyFun    = \_ _ _ _ -> pure []
      value = evalPrimOp dummyFun op args dummyType dummyTyCon
  evalStateT value fakeStgStateForPrimopTests

unboxFloat :: Float -> Float#
unboxFloat (F# x) = x

shouldReturnShow :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnShow m a = fmap show m `shouldReturn` show a

spec :: Spec
spec = do

  describe "Float" $ do

    it "gtFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "gtFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (I# (gtFloat# (unboxFloat a) (unboxFloat b)))

    it "geFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "geFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (I# (geFloat# (unboxFloat a) (unboxFloat b)))

    it "eqFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "eqFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (I# (eqFloat# (unboxFloat a) (unboxFloat b)))

    it "neFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "neFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (I# (neFloat# (unboxFloat a) (unboxFloat b)))

    it "ltFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "ltFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (I# (ltFloat# (unboxFloat a) (unboxFloat b)))

    it "leFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [IntV stgVal] <- evalOp "leFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (I# (leFloat# (unboxFloat a) (unboxFloat b)))

    it "plusFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "plusFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (F# (plusFloat# (unboxFloat a) (unboxFloat b)))

    it "minusFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "minusFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (F# (minusFloat# (unboxFloat a) (unboxFloat b)))

    it "timesFloat#" $
      property $ forAll (arbitrary :: Gen (Float, Float)) $ \(a, b) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "timesFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (F# (timesFloat# (unboxFloat a) (unboxFloat b)))

    it "divideFloat#" $
      property $ forAll (arbitrary :: Gen (Float, NonZero Float)) $ \(a, NonZero b) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "divideFloat#" [FloatV a, FloatV b]
        assert $ stgVal == (F# (divideFloat# (unboxFloat a) (unboxFloat b)))

    it "negateFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "negateFloat#" [FloatV a]
        assert $ stgVal == (F# (negateFloat# (unboxFloat a)))

    it "fabsFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "fabsFloat#" [FloatV a]
        assert $ stgVal == (F# (fabsFloat# (unboxFloat a)))

    it "float2Int#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [IntV stgVal] <- evalOp "float2Int#" [FloatV a]
        assert $ stgVal == (I# (float2Int# (unboxFloat a)))

    it "expFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "expFloat#" [FloatV a]
        assert $ stgVal == (F# (expFloat# (unboxFloat a)))

    it "logFloat#" $
      property $ forAll (arbitrary :: Gen (Positive Float)) $ \(Positive a) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "logFloat#" [FloatV a]
        assert $ stgVal == (F# (logFloat# (unboxFloat a)))

#if __GLASGOW_HASKELL__ >= 810
    it "expm1Float#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "expm1Float#" [FloatV a]
        assert $ stgVal == (F# (expm1Float# (unboxFloat a)))

    it "log1pFloat#" $
      property $ forAll (arbitrary :: Gen (Positive Float)) $ \(Positive a) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "log1pFloat#" [FloatV a]
        assert $ stgVal == (F# (log1pFloat# (unboxFloat a)))
#endif

    it "logFloat#" $
      property $ forAll (arbitrary :: Gen (Positive Float)) $ \(Positive a) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "logFloat#" [FloatV a]
        assert $ stgVal == (F# (logFloat# (unboxFloat a)))

    it "sqrtFloat#" $
      property $ forAll (arbitrary :: Gen (NonNegative Float)) $ \(NonNegative a) -> monadicIO $ do
        [FloatV stgVal] <- evalOp "sqrtFloat#" [FloatV a]
        assert $ stgVal == (F# (sqrtFloat# (unboxFloat a)))

    it "sinFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "sinFloat#" [FloatV a]
        assert $ stgVal == (F# (sinFloat# (unboxFloat a)))

    it "cosFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "cosFloat#" [FloatV a]
        assert $ stgVal == (F# (cosFloat# (unboxFloat a)))

    it "tanFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "tanFloat#" [FloatV a]
        assert $ stgVal == (F# (tanFloat# (unboxFloat a)))

    it "asinFloat#" $
      property $ \(a :: Float)-> do
        evalOp2 "asinFloat#" [FloatV a] `shouldReturnShow` [FloatV (F# (asinFloat# (unboxFloat a)))]

    it "acosFloat#" $
      property $ \(a :: Float) -> do
        evalOp2 "acosFloat#" [FloatV a] `shouldReturnShow` [FloatV (F# (acosFloat# (unboxFloat a)))]

    it "atanFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "atanFloat#" [FloatV a]
        assert $ stgVal == (F# (atanFloat# (unboxFloat a)))

    it "sinhFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "sinhFloat#" [FloatV a]
        assert $ stgVal == (F# (sinhFloat# (unboxFloat a)))

    it "coshFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "coshFloat#" [FloatV a]
        assert $ stgVal == (F# (coshFloat# (unboxFloat a)))

    it "tanhFloat#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [FloatV stgVal] <- evalOp "tanhFloat#" [FloatV a]
        assert $ stgVal == (F# (tanhFloat# (unboxFloat a)))

    it "asinhFloat#" $
      property $ \(a :: Float) -> do
        evalOp2 "asinhFloat#" [FloatV a] `shouldReturnShow` [FloatV (F# (asinhFloat# (unboxFloat a)))]

    it "acoshFloat#" $
      property $ \(a :: Float) -> do
        evalOp2 "acoshFloat#" [FloatV a] `shouldReturnShow` [FloatV (F# (acoshFloat# (unboxFloat a)))]

    it "atanhFloat#" $
      property $ \(a :: Float) -> do
        evalOp2 "atanhFloat#" [FloatV a] `shouldReturnShow` [FloatV (F# (atanhFloat# (unboxFloat a)))]

    it "powerFloat#" $
      property $ \(a :: Float, b :: Float) -> do
        evalOp2 "powerFloat#" [FloatV a, FloatV b] `shouldReturnShow` [FloatV (F# (powerFloat# (unboxFloat a) (unboxFloat b)))]

    it "float2Double#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [DoubleV stgVal] <- evalOp "float2Double#" [FloatV a]
        assert $ stgVal == (D# (float2Double# (unboxFloat a)))

    it "decodeFloat_Int#" $
      property $ forAll (arbitrary :: Gen Float) $ \a -> monadicIO $ do
        [IntV stgVal1, IntV stgVal2] <- evalOp "decodeFloat_Int#" [FloatV a]

        let !(# x, y #) = decodeFloat_Int# (unboxFloat a)
        assert $ (stgVal1, stgVal2) == (I# x, I# y)
