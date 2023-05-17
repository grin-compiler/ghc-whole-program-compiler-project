{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, CPP, ScopedTypeVariables #-}

module PrimOp.DoubleSpec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Double

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

unboxDouble :: Double -> Double#
unboxDouble (D# x) = x

shouldReturnShow :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnShow m a = fmap show m `shouldReturn` show a

spec :: Spec
spec = do

  describe "Double" $ do

    it ">##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp ">##" [DoubleV a, DoubleV b] `shouldReturn` [IntV (I# ((unboxDouble a) >## (unboxDouble b)))]

    it ">=##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp ">=##" [DoubleV a, DoubleV b] `shouldReturn` [IntV (I# ((unboxDouble a) >=## (unboxDouble b)))]

    it "==##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "==##" [DoubleV a, DoubleV b] `shouldReturn` [IntV (I# ((unboxDouble a) ==## (unboxDouble b)))]

    it "/=##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "/=##" [DoubleV a, DoubleV b] `shouldReturn` [IntV (I# ((unboxDouble a) /=## (unboxDouble b)))]

    it "<##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "<##" [DoubleV a, DoubleV b] `shouldReturn` [IntV (I# ((unboxDouble a) <## (unboxDouble b)))]

    it "<=##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "<=##" [DoubleV a, DoubleV b] `shouldReturn` [IntV (I# ((unboxDouble a) <=## (unboxDouble b)))]

    it "+##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "+##" [DoubleV a, DoubleV b] `shouldReturn` [DoubleV (D# ((unboxDouble a) +## (unboxDouble b)))]

    it "-##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "-##" [DoubleV a, DoubleV b] `shouldReturn` [DoubleV (D# ((unboxDouble a) -## (unboxDouble b)))]

    it "*##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "*##" [DoubleV a, DoubleV b] `shouldReturn` [DoubleV (D# ((unboxDouble a) *## (unboxDouble b)))]

    it "/##" $
      property $ \(a :: Double, NonZero b :: NonZero Double) -> do
        evalOp "/##" [DoubleV a, DoubleV b] `shouldReturn` [DoubleV (D# ((unboxDouble a) /## (unboxDouble b)))]

    it "negateDouble#" $
      property $ \(a :: Double) -> do
        evalOp "negateDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (negateDouble# (unboxDouble a)))]

    it "fabsDouble#" $
      property $ \(a :: Double) -> do
        evalOp "fabsDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (fabsDouble# (unboxDouble a)))]

    it "double2Int#" $
      property $ \(a :: Double) -> do
        evalOp "double2Int#" [DoubleV a] `shouldReturn` [IntV (I# (double2Int# (unboxDouble a)))]

    it "double2Float#" $
      property $ \(a :: Double) -> do
        evalOp "double2Float#" [DoubleV a] `shouldReturn` [FloatV (F# (double2Float# (unboxDouble a)))]

    it "expDouble#" $
      property $ \(a :: Double) -> do
        evalOp "expDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (expDouble# (unboxDouble a)))]

    it "logDouble#" $
      property $ \(a :: Double) -> do
        evalOp "logDouble#" [DoubleV a] `shouldReturnShow` [DoubleV (D# (logDouble# (unboxDouble a)))]

#if __GLASGOW_HASKELL__ >= 810
    it "expm1Double#" $
      property $ \(a :: Double) -> do
        evalOp "expm1Double#" [DoubleV a] `shouldReturn` [DoubleV (D# (expm1Double# (unboxDouble a)))]

    it "log1pDouble#" $
      property $ \(a :: Double) -> do
        evalOp "log1pDouble#" [DoubleV a] `shouldReturnShow` [DoubleV (D# (log1pDouble# (unboxDouble a)))]
#endif

    it "sqrtDouble#" $
      property $ \(a :: Double) -> do
        evalOp "sqrtDouble#" [DoubleV a] `shouldReturnShow` [DoubleV (D# (sqrtDouble# (unboxDouble a)))]

    it "sinDouble#" $
      property $ \(a :: Double) -> do
        evalOp "sinDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (sinDouble# (unboxDouble a)))]

    it "cosDouble#" $
      property $ \(a :: Double) -> do
        evalOp "cosDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (cosDouble# (unboxDouble a)))]

    it "tanDouble#" $
      property $ \(a :: Double) -> do
        evalOp "tanDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (tanDouble# (unboxDouble a)))]

    it "asinDouble#" $
      property $ \(a :: Double) -> do
        evalOp "asinDouble#" [DoubleV a] `shouldReturnShow` [DoubleV (D# (asinDouble# (unboxDouble a)))]

    it "acosDouble#" $
      property $ \(a :: Double) -> do
        evalOp "acosDouble#" [DoubleV a] `shouldReturnShow` [DoubleV (D# (acosDouble# (unboxDouble a)))]

    it "atanDouble#" $
      property $ \(a :: Double) -> do
        evalOp "atanDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (atanDouble# (unboxDouble a)))]

    it "sinhDouble#" $
      property $ \(a :: Double) -> do
        evalOp "sinhDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (sinhDouble# (unboxDouble a)))]

    it "coshDouble#" $
      property $ \(a :: Double) -> do
        evalOp "coshDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (coshDouble# (unboxDouble a)))]

    it "tanhDouble#" $
      property $ \(a :: Double) -> do
        evalOp "tanhDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (tanhDouble# (unboxDouble a)))]

    it "asinhDouble#" $
      property $ \(a :: Double) -> do
        evalOp "asinhDouble#" [DoubleV a] `shouldReturn` [DoubleV (D# (asinhDouble# (unboxDouble a)))]

    it "acoshDouble#" $
      property $ \(a :: Double) -> do
        evalOp "acoshDouble#" [DoubleV a] `shouldReturnShow` [DoubleV (D# (acoshDouble# (unboxDouble a)))]

    it "atanhDouble#" $
      property $ \(a :: Double) -> do
        evalOp "atanhDouble#" [DoubleV a] `shouldReturnShow` [DoubleV (D# (atanhDouble# (unboxDouble a)))]

    it "**##" $
      property $ \(a :: Double, b :: Double) -> do
        evalOp "**##" [DoubleV a, DoubleV b] `shouldReturnShow` [DoubleV (D# ((unboxDouble a) **## (unboxDouble b)))]


    it "decodeDouble_2Int#" $
      property $ \(NonZero a :: NonZero Double) -> do
        let !(# x, y, z, w #) = decodeDouble_2Int# (unboxDouble a)
        evalOp "decodeDouble_2Int#" [DoubleV a] `shouldReturn` [IntV (I# x), WordV (W# y), WordV (W# z), IntV (I# w)]

    it "decodeDouble_Int64#" $
      property $ \(a :: Double) -> do
        let !(# x, y #) = decodeDouble_Int64# (unboxDouble a)
        evalOp "decodeDouble_Int64#" [DoubleV a] `shouldReturn` [IntV (I# x), IntV (I# y)]
