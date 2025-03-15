module Stg.Interpreter.EmulatedLibFFI where

----- FFI experimental
import           Control.Applicative        (Applicative (..), (<$>))
import           Control.Monad.State.Strict (MonadIO (..), gets)

import qualified Data.ByteString            as BS
import           Data.Char                  (Char)
import           Data.Eq                    (Eq (..))
import           Data.Function              (($), (.))
import           Data.Int                   (Int)
import           Data.List                  (filter, (++))
import           Data.Maybe                 (Maybe)
import qualified Data.Primitive.ByteArray   as BA
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Word                  (Word)

import qualified GHC.Exts                   as Exts
import           GHC.Float                  (Double, Float)

import           Stg.Interpreter.Base       (Atom (..), ByteArrayDescriptor (..), EvalOnNewThread, M, PtrOrigin (..),
                                             Rts (..), StgState (..), lookupByteArrayDescriptorI, stgErrorM)
import           Stg.Syntax                 (CCallTarget (..), ForeignCall (..), Lit (..), TyCon, Type)

import           System.FilePath            (takeBaseName)
import           System.IO                  (hFlush, hPutStr, hPutStrLn, stderr)

import           Text.Printf                (printf)
import           Text.Show                  (Show (..))

pattern CharV :: Char -> Atom
pattern CharV c   = Literal (LitChar c)
pattern IntV :: Int -> Atom
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int8V :: Int -> Atom
pattern Int8V i   = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V :: Int -> Atom
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int32V :: Int -> Atom
pattern Int32V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int64V :: Int -> Atom
pattern Int64V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV :: Word -> Atom
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word8V :: Word -> Atom
pattern Word8V i  = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word16V :: Word -> Atom
pattern Word16V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V :: Word -> Atom
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word64V :: Word -> Atom
pattern Word64V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV :: Float -> Atom
pattern FloatV f  = FloatAtom f
pattern DoubleV :: Double -> Atom
pattern DoubleV d = DoubleAtom d

{-# NOINLINE evalFCallOp #-}
evalFCallOp :: EvalOnNewThread -> ForeignCall -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalFCallOp _evalOnNewThread fCall@ForeignCall{..} args t _tc = do
    --liftIO $ putStrLn $ "[evalFCallOp]  " ++ show foreignCTarget ++ " " ++ show args
    case foreignCTarget of

      StaticTarget _ "debugBelch2" _ _
        | [PtrAtom (ByteArrayPtr bai1) _, PtrAtom (ByteArrayPtr bai2) _, Void] <- args
        -> do
          let
            showByteArray b = do
              ByteArrayDescriptor{..} <- lookupByteArrayDescriptorI b
              Text.unpack . Text.decodeUtf8 . BS.pack . filter (/=0) . Exts.toList <$> BA.unsafeFreezeByteArray baaMutableByteArray
          formatStr <- showByteArray bai1
          value <- showByteArray bai2
          liftIO $ do
            hPutStr stderr $ printf formatStr value
            hFlush stderr
          pure []


      StaticTarget _ "errorBelch2" _ _
        | [PtrAtom (ByteArrayPtr bai1) _, PtrAtom (ByteArrayPtr bai2) _, Void] <- args
        -> do
          let
            showByteArray b = do
              ByteArrayDescriptor{..} <- lookupByteArrayDescriptorI b
              Text.unpack . Text.decodeUtf8 . BS.pack . filter (/=0) . Exts.toList <$> BA.unsafeFreezeByteArray baaMutableByteArray
          formatStr <- showByteArray bai1
          value <- showByteArray bai2
          Rts{..} <- gets ssRtsSupport
          liftIO $ hPutStrLn stderr $ takeBaseName rtsProgName ++ ": " ++ printf formatStr value
          pure []
      StaticTarget _ "errorBelch2" _ _
        -> stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args

      _ -> stgErrorM $ "unsupported emulation of user StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args
