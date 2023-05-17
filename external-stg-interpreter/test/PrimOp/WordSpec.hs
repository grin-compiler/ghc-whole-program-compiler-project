{-# LANGUAGE OverloadedStrings, PatternSynonyms, MagicHash, UnboxedTuples, BangPatterns, CPP, ScopedTypeVariables #-}

module PrimOp.WordSpec where

import Control.Monad.State.Strict

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Monadic

import Stg.Syntax (Name, Type(..))
import Stg.Interpreter.Base
import Stg.Interpreter.PrimOp.Word

import GHC.Word
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

unboxWord :: Word -> Word#
unboxWord (W# x) = x

unboxInt :: Int -> Int#
unboxInt (I# x) = x

shouldReturnShow :: (HasCallStack, Show a, Eq a) => IO a -> a -> Expectation
shouldReturnShow m a = fmap show m `shouldReturn` show a

spec :: Spec
spec = do

  describe "Word" $ do

    it "plusWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "plusWord#" [WordV a, WordV b] `shouldReturn` [WordV (W# (plusWord# (unboxWord a) (unboxWord b)))]

    it "addWordC#" $
      property $ \(a :: Word, b :: Word) -> monadicIO $ do
        [WordV stgVal1, IntV stgVal2] <- run $ evalOp "addWordC#" [WordV a, WordV b]

        let !(# x, y #) = addWordC# (unboxWord a) (unboxWord b)
        assert $ (stgVal1, stgVal2) == (W# x, I# y)

    it "subWordC#" $
      property $ \(a :: Word, b :: Word) -> monadicIO $ do
        [WordV stgVal1, IntV stgVal2] <- run $ evalOp "subWordC#" [WordV a, WordV b]

        let !(# x, y #) = subWordC# (unboxWord a) (unboxWord b)
        assert $ (stgVal1, stgVal2) == (W# x, I# y)

    it "plusWord2#" $
      property $ \(a :: Word, b :: Word) -> monadicIO $ do
        [WordV stgVal1, WordV stgVal2] <- run $ evalOp "plusWord2#" [WordV a, WordV b]

        let !(# x, y #) = plusWord2# (unboxWord a) (unboxWord b)
        assert $ (stgVal1, stgVal2) == (W# x, W# y)

    it "minusWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "minusWord#" [WordV a, WordV b] `shouldReturn` [WordV (W# (minusWord# (unboxWord a) (unboxWord b)))]

    it "timesWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "timesWord#" [WordV a, WordV b] `shouldReturn` [WordV (W# (timesWord# (unboxWord a) (unboxWord b)))]

    it "timesWord2#" $
      property $ \(a :: Word, b :: Word) -> monadicIO $ do
        [WordV stgVal1, WordV stgVal2] <- run $ evalOp "timesWord2#" [WordV a, WordV b]

        let !(# x, y #) = timesWord2# (unboxWord a) (unboxWord b)
        assert $ (stgVal1, stgVal2) == (W# x, W# y)


    it "quotWord#" $
      property $ \(a :: Word, NonZero b :: NonZero Word) -> do
        evalOp "quotWord#" [WordV a, WordV b] `shouldReturn` [WordV (W# (quotWord# (unboxWord a) (unboxWord b)))]

    it "remWord#" $
      property $ \(a :: Word, NonZero b :: NonZero Word) -> do
        evalOp "remWord#" [WordV a, WordV b] `shouldReturn` [WordV (W# (remWord# (unboxWord a) (unboxWord b)))]

    it "quotRemWord#" $
      property $ \(a :: Word, NonZero b :: NonZero Word) -> monadicIO $ do
        [WordV stgVal1, WordV stgVal2] <- run $ evalOp "quotRemWord#" [WordV a, WordV b]

        let !(# x, y #) = quotRemWord# (unboxWord a) (unboxWord b)
        assert $ (stgVal1, stgVal2) == (W# x, W# y)

    it "quotRemWord2#" $
      property $ \(NonZero hi' :: NonZero Word, lo :: Word, NonZero divisor' :: NonZero Word) -> monadicIO $ do
        let (hi, divisor) = (pred $ min hi' divisor', max hi' divisor') -- NOTE: primop precondition: hi < divisor
        [WordV stgVal1, WordV stgVal2] <- run $ evalOp "quotRemWord2#" [WordV hi, WordV lo, WordV divisor]

        let !(# x, y #) = quotRemWord2# (unboxWord hi) (unboxWord lo) (unboxWord divisor)
        assert $ (stgVal1, stgVal2) == (W# x, W# y)

    it "and#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "and#" [WordV a, WordV b] `shouldReturn` [WordV (W# (and# (unboxWord a) (unboxWord b)))]

    it "or#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "or#" [WordV a, WordV b] `shouldReturn` [WordV (W# (or# (unboxWord a) (unboxWord b)))]

    it "xor#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "xor#" [WordV a, WordV b] `shouldReturn` [WordV (W# (xor# (unboxWord a) (unboxWord b)))]

    it "not#" $
      property $ \(a :: Word) -> do
        evalOp "not#" [WordV a] `shouldReturn` [WordV (W# (not# (unboxWord a)))]

    it "uncheckedShiftL#" $
      property $ \(a :: Word, b :: Int) -> do
        evalOp "uncheckedShiftL#" [WordV a, IntV b] `shouldReturn` [WordV (W# (uncheckedShiftL# (unboxWord a) (unboxInt b)))]

    it "uncheckedShiftRL#" $
      property $ \(a :: Word, b :: Int) -> do
        evalOp "uncheckedShiftRL#" [WordV a, IntV b] `shouldReturn` [WordV (W# (uncheckedShiftRL# (unboxWord a) (unboxInt b)))]

    it "word2Int#" $
      property $ \(a :: Word) -> do
        evalOp "word2Int#" [WordV a] `shouldReturn` [IntV (I# (word2Int# (unboxWord a)))]

    it "gtWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "gtWord#" [WordV a, WordV b] `shouldReturn` [IntV (I# (gtWord# (unboxWord a) (unboxWord b)))]

    it "geWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "geWord#" [WordV a, WordV b] `shouldReturn` [IntV (I# (geWord# (unboxWord a) (unboxWord b)))]

    it "eqWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "eqWord#" [WordV a, WordV b] `shouldReturn` [IntV (I# (eqWord# (unboxWord a) (unboxWord b)))]

    it "neWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "neWord#" [WordV a, WordV b] `shouldReturn` [IntV (I# (neWord# (unboxWord a) (unboxWord b)))]

    it "ltWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "ltWord#" [WordV a, WordV b] `shouldReturn` [IntV (I# (ltWord# (unboxWord a) (unboxWord b)))]

    it "leWord#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "leWord#" [WordV a, WordV b] `shouldReturn` [IntV (I# (leWord# (unboxWord a) (unboxWord b)))]

    it "popCnt8#" $
      property $ \(a :: Word) -> do
        evalOp "popCnt8#" [WordV a] `shouldReturn` [WordV (W# (popCnt8# (unboxWord a)))]

    it "popCnt16#" $
      property $ \(a :: Word) -> do
        evalOp "popCnt16#" [WordV a] `shouldReturn` [WordV (W# (popCnt16# (unboxWord a)))]

    it "popCnt32#" $
      property $ \(a :: Word) -> do
        evalOp "popCnt32#" [WordV a] `shouldReturn` [WordV (W# (popCnt32# (unboxWord a)))]

    it "popCnt64#" $
      property $ \(a :: Word) -> do
        evalOp "popCnt64#" [WordV a] `shouldReturn` [WordV (W# (popCnt64# (unboxWord a)))]

    it "popCnt#" $
      property $ \(a :: Word) -> do
        evalOp "popCnt#" [WordV a] `shouldReturn` [WordV (W# (popCnt# (unboxWord a)))]

    it "pdep8#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pdep8#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pdep8# (unboxWord a) (unboxWord b)))]

    it "pdep16#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pdep16#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pdep16# (unboxWord a) (unboxWord b)))]

    it "pdep32#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pdep32#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pdep32# (unboxWord a) (unboxWord b)))]

    it "pdep64#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pdep64#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pdep64# (unboxWord a) (unboxWord b)))]

    it "pdep#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pdep#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pdep# (unboxWord a) (unboxWord b)))]

    it "pext8#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pext8#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pext8# (unboxWord a) (unboxWord b)))]

    it "pext16#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pext16#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pext16# (unboxWord a) (unboxWord b)))]

    it "pext32#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pext32#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pext32# (unboxWord a) (unboxWord b)))]

    it "pext64#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pext64#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pext64# (unboxWord a) (unboxWord b)))]

    it "pext#" $
      property $ \(a :: Word, b :: Word) -> do
        evalOp "pext#" [WordV a, WordV b] `shouldReturn` [WordV (W# (pext# (unboxWord a) (unboxWord b)))]

    it "clz8#" $
      property $ \(a :: Word) -> do
        evalOp "clz8#" [WordV a] `shouldReturn` [WordV (W# (clz8# (unboxWord a)))]

    it "clz16#" $
      property $ \(a :: Word) -> do
        evalOp "clz16#" [WordV a] `shouldReturn` [WordV (W# (clz16# (unboxWord a)))]

    it "clz32#" $
      property $ \(a :: Word) -> do
        evalOp "clz32#" [WordV a] `shouldReturn` [WordV (W# (clz32# (unboxWord a)))]

    it "clz64#" $
      property $ \(a :: Word) -> do
        evalOp "clz64#" [WordV a] `shouldReturn` [WordV (W# (clz64# (unboxWord a)))]

    it "clz#" $
      property $ \(a :: Word) -> do
        evalOp "clz#" [WordV a] `shouldReturn` [WordV (W# (clz# (unboxWord a)))]

    it "ctz8#" $
      property $ \(a :: Word) -> do
        evalOp "ctz8#" [WordV a] `shouldReturn` [WordV (W# (ctz8# (unboxWord a)))]

    it "ctz16#" $
      property $ \(a :: Word) -> do
        evalOp "ctz16#" [WordV a] `shouldReturn` [WordV (W# (ctz16# (unboxWord a)))]

    it "ctz32#" $
      property $ \(a :: Word) -> do
        evalOp "ctz32#" [WordV a] `shouldReturn` [WordV (W# (ctz32# (unboxWord a)))]

    it "ctz64#" $
      property $ \(a :: Word) -> do
        evalOp "ctz64#" [WordV a] `shouldReturn` [WordV (W# (ctz64# (unboxWord a)))]

    it "ctz#" $
      property $ \(a :: Word) -> do
        evalOp "ctz#" [WordV a] `shouldReturn` [WordV (W# (ctz# (unboxWord a)))]

    it "byteSwap16#" $
      property $ \(a :: Word) -> do
        evalOp "byteSwap16#" [WordV a] `shouldReturn` [WordV (W# (byteSwap16# (unboxWord a)))]

    it "byteSwap32#" $
      property $ \(a :: Word) -> do
        evalOp "byteSwap32#" [WordV a] `shouldReturn` [WordV (W# (byteSwap32# (unboxWord a)))]

    it "byteSwap64#" $
      property $ \(a :: Word) -> do
        evalOp "byteSwap64#" [WordV a] `shouldReturn` [WordV (W# (byteSwap64# (unboxWord a)))]

    it "byteSwap#" $
      property $ \(a :: Word) -> do
        evalOp "byteSwap#" [WordV a] `shouldReturn` [WordV (W# (byteSwap# (unboxWord a)))]

#if __GLASGOW_HASKELL__ >= 810
    it "bitReverse8#" $
      property $ \(a :: Word) -> do
        evalOp "bitReverse8#" [WordV a] `shouldReturn` [WordV (W# (bitReverse8# (unboxWord a)))]

    it "bitReverse16#" $
      property $ \(a :: Word) -> do
        evalOp "bitReverse16#" [WordV a] `shouldReturn` [WordV (W# (bitReverse16# (unboxWord a)))]

    it "bitReverse32#" $
      property $ \(a :: Word) -> do
        evalOp "bitReverse32#" [WordV a] `shouldReturn` [WordV (W# (bitReverse32# (unboxWord a)))]

    it "bitReverse64#" $
      property $ \(a :: Word) -> do
        evalOp "bitReverse64#" [WordV a] `shouldReturn` [WordV (W# (bitReverse64# (unboxWord a)))]

    it "bitReverse#" $
      property $ \(a :: Word) -> do
        evalOp "bitReverse#" [WordV a] `shouldReturn` [WordV (W# (bitReverse# (unboxWord a)))]
#endif
