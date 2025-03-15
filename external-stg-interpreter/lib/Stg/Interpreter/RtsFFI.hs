
module Stg.Interpreter.RtsFFI where

----- FFI experimental
import           Control.Applicative        (Applicative (..), (<$>))
import           Control.Monad              (Monad (..), mapM)
import           Control.Monad.State.Strict (MonadIO (..), MonadState (..), gets, modify')

import           Data.Bool                  (Bool (..))
import qualified Data.ByteString            as BS
import           Data.Char                  (Char)
import           Data.Eq                    (Eq (..))
import           Data.Function              (($), (.))
import           Data.Int                   (Int)
import           Data.List                  (filter, length, (++))
import qualified Data.Map                   as Map
import           Data.Maybe                 (Maybe (..))
import           Data.Ord                   (Ord (..))
import qualified Data.Primitive.ByteArray   as BA
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import           Data.Time.Clock            (UTCTime (..), diffTimeToPicoseconds, getCurrentTime)
import           Data.Word                  (Word, Word64)

import           Foreign.C.String           (CString, newCString, peekCString)
import           Foreign.C.Types            (CInt (..))
import           Foreign.Marshal.Array      (newArray, peekArray)
import           Foreign.Ptr                (Ptr, castPtr, nullPtr)
import           Foreign.Storable           (Storable (..))

import qualified GHC.Exts                   as Exts
import           GHC.Float                  (Double, Float)
import           GHC.Num                    (Num (..))
import           GHC.Real                   (Integral (..), fromIntegral)

import           Stg.Interpreter.Base       (Atom (..), ByteArrayDescriptor (..), EvalOnNewThread, M, PtrOrigin (..),
                                             Rts (..), StgState (..), lookupByteArrayDescriptorI,
                                             lookupWeakPointerDescriptor, stgErrorM)
import           Stg.Interpreter.Debug      (exportCallGraph)
import           Stg.Interpreter.Rts        (globalStoreSymbols)
import           Stg.Syntax                 (CCallTarget (..), ForeignCall (..), Lit (..), PrimRep (..), TyCon,
                                             Type (..))

import           System.Exit                (ExitCode (..), exitWith)
import           System.FilePath            (takeBaseName)
import           System.IO                  (IO, hFlush, hPutStr, hPutStrLn, print, putStrLn, stderr)

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

      ----------------
      -- GHC RTS API
      ----------------

      -- static pointer API
      {-
        hs_spt_lookup
        hs_spt_insert
        hs_spt_insert_stableptr
        hs_spt_remove
        hs_spt_keys
        hs_spt_key_count
      -}

      -- misc
      StaticTarget _ "__int_encodeDouble" _ _
        | [IntV j, IntV e, Void] <- args
        , UnboxedTuple [DoubleRep] <- t
        -> pure [DoubleV $ rts_intEncodeDouble j e]

      StaticTarget _ "__word_encodeDouble" _ _
        | [WordV j, IntV e, Void] <- args
        , UnboxedTuple [DoubleRep] <- t
        -> pure [DoubleV $ rts_wordEncodeDouble j e]

      StaticTarget _ "__int_encodeFloat" _ _
        | [IntV j, IntV e, Void] <- args
        , UnboxedTuple [FloatRep] <- t
        -> pure [FloatV $ rts_intEncodeFloat j e]

      StaticTarget _ "__word_encodeFloat" _ _
        | [WordV j, IntV e, Void] <- args
        , UnboxedTuple [FloatRep] <- t
        -> pure [FloatV $ rts_wordEncodeFloat j e]

      StaticTarget _ "stg_interp_constr1_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr2_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr3_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr4_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr5_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr6_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr7_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget

      StaticTarget _ "freeHaskellFunctionPtr" _ _ -> pure [] -- TODO
      StaticTarget _ "performMajorGC" _ _ -> do
        modify' $ \s -> s {ssRequestMajorGC = True}
        pure []

      StaticTarget _ "setNumCapabilities" _ _
        | [WordV _num_caps, Void] <- args
        -> do
          pure [] -- TODO

      StaticTarget _ "eq_thread" _ _
        | [ThreadId a, ThreadId b, Void] <- args
        , UnboxedTuple [Word8Rep] <- t
        -> do
          pure [Word8V $ if a == b then 1 else 0]

      {-
      StaticTarget _ "rts_enableThreadAllocationLimit" _ _
        | [ThreadId tid, Void] <- args
        -> do
          pure [] -- TODO
      -}

      StaticTarget _ "rts_setMainThread" _ _
        | [WeakPointer weakId, Void] <- args
        -> do
          _wd <- lookupWeakPointerDescriptor weakId
          --error $ show wd
          pure [] -- TODO

      StaticTarget _ "rts_getThreadId" _ _
        | [ThreadId threadId,Void] <- args
        -> do
          pure [Int32V threadId]

      StaticTarget _ "cmp_thread" _ _
        | [ThreadId id1, ThreadId id2, Void] <- args
        , UnboxedTuple [Int32Rep] <- t
        -> do
          pure [Int32V $ if id1 == id2 then 0 else if id1 < id2 then -1 else 1]

      StaticTarget _ "eq_thread" _ _
        | [ThreadId id1, ThreadId id2, Void] <- args
        , UnboxedTuple [Int32Rep] <- t
        -> do
          pure [Int32V $ if id1 == id2 then 1 else 0]

      StaticTarget _ "getRTSStatsEnabled" _ _ -> pure [IntV 0]

      StaticTarget _ "stg_sig_install" _ _ -> pure [IntV (-1)]                          -- TODO: for testsuite

      StaticTarget _ "lockFile" _ _
        | [Word64V id', Word64V dev, Word64V ino, Int32V for_writing, Void] <- args
        , UnboxedTuple [Int32Rep] <- t
        -> do
          result <- liftIO $ lockFile (fromIntegral id') (fromIntegral dev) (fromIntegral ino) (fromIntegral for_writing)
          pure [Int32V $ fromIntegral result]

      StaticTarget _ "unlockFile" _ _
        | [Word64V id', Void] <- args
        , UnboxedTuple [Int32Rep] <- t
        -> do
          result <- liftIO $ unlockFile (fromIntegral id')
          pure [Int32V $ fromIntegral result]

      StaticTarget _ "rtsSupportsBoundThreads" _ _ -> pure [IntV 0]

      StaticTarget _ "getMonotonicNSec" _ _
        | [Void] <- args
        -> do
          nowPico <- diffTimeToPicoseconds . utctDayTime <$> liftIO getCurrentTime
          pure [WordV . fromInteger $ nowPico `div` 1000]

      StaticTarget _ "setProgArgv" _ _
        | [IntAtom argc, PtrAtom _ argvPtr, Void] <- args
        -> do
          liftIO $ do
            -- peekCString :: CString -> IO String
            argv <- peekArray argc (castPtr argvPtr) >>= mapM peekCString
            print (argc, argv)
          -- TODO: save to the env!!
          pure []

      StaticTarget _ "getProgArgv" _ _
        | [PtrAtom (ByteArrayPtr _ba1) ptrArgc, PtrAtom (ByteArrayPtr _ba2) ptrArgv, Void] <- args
        -> do
          Rts{..} <- gets ssRtsSupport
          liftIO $ do
            -- HINT: getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
            poke (castPtr ptrArgc :: Ptr CInt) (fromIntegral $ 1 + length rtsProgArgs)

            -- FIXME: this has a race condition with the GC!!!! because it is pure
            arr1 <- newCString rtsProgName :: IO CString
            args' <- mapM newCString rtsProgArgs :: IO [CString]
            arr2 <- newArray (arr1 : args' ++ [nullPtr]) :: IO (Ptr CString)

            poke (castPtr ptrArgv :: Ptr (Ptr CString)) arr2--(castPtr arr2 :: Ptr CString )
          pure []

      StaticTarget _ "shutdownHaskellAndExit" _ _
        | [IntV retCode, IntV _sfastExit, Void] <- args
        , UnboxedTuple [] <- t
        -> do
          --showDebug evalOnNewThread
          --error $ "shutdownHaskellAndExit exit code:  " ++ show retCode ++ ", fast exit: " ++ show fastExit
          exportCallGraph
          liftIO . exitWith $ case retCode of
            0 -> ExitSuccess
            n -> ExitFailure n

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

      StaticTarget _ "errorBelch" _ _ -> do
        liftIO $ putStrLn $ "errorBelch: " ++ show args
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

      StaticTarget _ "hs_free_stable_ptr" _ _ -> pure []

      -- GHC RTS global store getOrSet function implementation
      StaticTarget _ foreignSymbol _ _
        | Set.member foreignSymbol globalStoreSymbols
        , [value, Void] <- args
        -> do
            -- HINT: set once with the first value, then return it always, only for the globalStoreSymbols
            store' <- gets $ rtsGlobalStore . ssRtsSupport
            case Map.lookup foreignSymbol store' of
              Nothing -> state $ \s@StgState{..} -> ([value], s {ssRtsSupport = ssRtsSupport {rtsGlobalStore = Map.insert foreignSymbol value store'}})
              Just v  -> pure [v]

      _ -> stgErrorM $ "unsupported RTS StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args


foreign import ccall unsafe "__int_encodeDouble"  rts_intEncodeDouble  :: Int  -> Int -> Double
foreign import ccall unsafe "__word_encodeDouble" rts_wordEncodeDouble :: Word -> Int -> Double
foreign import ccall unsafe "__int_encodeFloat"   rts_intEncodeFloat   :: Int  -> Int -> Float
foreign import ccall unsafe "__word_encodeFloat"  rts_wordEncodeFloat  :: Word -> Int -> Float

foreign import ccall unsafe "lockFile"    lockFile    :: Word64 -> Word64 -> Word64 -> CInt -> IO CInt
foreign import ccall unsafe "unlockFile"  unlockFile  :: Word64 -> IO CInt
