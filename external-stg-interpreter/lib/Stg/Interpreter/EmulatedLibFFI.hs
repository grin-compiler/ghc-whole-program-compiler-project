{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP     #-}
module Stg.Interpreter.EmulatedLibFFI where

----- FFI experimental
import           Control.Applicative        (Applicative (..), (<$>))
import           Control.Monad.State.Strict (MonadIO (..), gets)

import qualified Data.ByteString            as BS
import           Data.Eq                    (Eq (..))
import           Data.Function              (($), (.))
import           Data.List                  (filter, (++))
import           Data.Maybe                 (Maybe)
import qualified Data.Primitive.ByteArray   as BA
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Word                  (Word8)

import           Foreign.C.Types            (CInt (..), CSize (..))
import           Foreign.Ptr                (Ptr)

import qualified GHC.Exts                   as Exts
import           GHC.Stack                  (HasCallStack)

import           Prelude                    (Enum (..))

import           Stg.Interpreter.Base
import           Stg.Syntax                 (CCallTarget (..), ForeignCall (..), TyCon, Type)

import           System.FilePath            (takeBaseName)
import           System.IO                  (IO, hFlush, hPutStr, hPutStrLn, stderr)
import           System.Posix               (CSsize (..))

import           Text.Printf                (printf)
import           Text.Show                  (Show (..))

{-# NOINLINE evalFCallOp #-}
evalFCallOp :: HasCallStack => EvalOnNewThread -> ForeignCall -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
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

      -- #include <unistd.h> c_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
      -- https://pubs.opengroup.org/onlinepubs/7908799/xsh/write.html
      StaticTarget _ "ghczuwrapperZC21ZCghczminternalZCGHCziInternalziSystemziPosixziInternalsZCwrite" _ _
        | [IntAtom i, PtrAtom (ByteArrayPtr _) ptr, WordAtom w, Void] <- args
        -> do
          let i' = toEnum i
              w' = toEnum $ fromEnum w
          res <- liftIO $ c_write i' ptr w'
          pure [IntV $ fromEnum res]

      StaticTarget _ "errorBelch2" _ _
        -> stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args

      _ -> stgErrorM $ "unsupported emulation of user StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args

foreign import capi unsafe "unistd.h write"
   c_write :: CInt -> Ptr Word8 -> CSize -> IO CSsize
