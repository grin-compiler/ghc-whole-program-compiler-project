{-# LANGUAGE MagicHash #-}

module PrimOp.NarrowingsSpec where

import           Control.Applicative               (Applicative (..))
import           Control.Monad.State.Strict        (evalStateT)

import           Data.Eq                           (Eq (..))
import           Data.Function                     (($))
import           Data.Maybe                        (Maybe (..))

import           GHC.Exts

import           Stg.Interpreter.Base 
import           Stg.Interpreter.PrimOp.Narrowings
import           Stg.Syntax                        (Name, Type (..))

import           System.IO                         (IO)

import           Test.Hspec                        (Spec, describe, hspec, it)
import           Test.QuickCheck                   (Arbitrary (..), Gen, Testable (..), forAll)
import           Test.QuickCheck.Monadic           (PropertyM, assert, monadicIO, run)

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

  describe "Narrowings" $ do

    it "narrow8Int#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [IntV stgVal] <- evalOp "narrow8Int#" [IntV a]
        assert $ stgVal == (I# (narrow8Int# (unboxInt a)))

    it "narrow16Int#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [IntV stgVal] <- evalOp "narrow16Int#" [IntV a]
        assert $ stgVal == (I# (narrow16Int# (unboxInt a)))

    it "narrow32Int#" $
      property $ forAll (arbitrary :: Gen Int) $ \a -> monadicIO $ do
        [IntV stgVal] <- evalOp "narrow32Int#" [IntV a]
        assert $ stgVal == (I# (narrow32Int# (unboxInt a)))

    it "narrow8Word#" $
      property $ forAll (arbitrary :: Gen Word) $ \a -> monadicIO $ do
        [WordV stgVal] <- evalOp "narrow8Word#" [WordV a]
        assert $ stgVal == (W# (narrow8Word# (unboxWord a)))

    it "narrow16Word#" $
      property $ forAll (arbitrary :: Gen Word) $ \a -> monadicIO $ do
        [WordV stgVal] <- evalOp "narrow16Word#" [WordV a]
        assert $ stgVal == (W# (narrow16Word# (unboxWord a)))

    it "narrow32Word#" $
      property $ forAll (arbitrary :: Gen Word) $ \a -> monadicIO $ do
        [WordV stgVal] <- evalOp "narrow32Word#" [WordV a]
        assert $ stgVal == (W# (narrow32Word# (unboxWord a)))
