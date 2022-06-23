{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, RankNTypes, FlexibleContexts, ConstraintKinds, DataKinds #-}
module Stg.Interpreter.ForeignCall.Interpreted where

import qualified GHC.Exts as Exts
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Primitive.ByteArray as BA
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array
import System.Exit
import System.IO
import System.FilePath
import Text.Printf

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Effect.State
import Control.Effect.Lift

import Stg.Syntax
import Stg.Interpreter.Base
import Stg.Interpreter.Rts (globalStoreSymbols)

pattern CharV c = Literal (LitChar c)
pattern IntV i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

{-# NOINLINE evalFCallOp #-}
evalFCallOp :: M sig m => FCallEval m -> ForeignCall -> [AtomAddr] -> Type -> m [AtomAddr]
evalFCallOp fallback fCall@ForeignCall{..} argsAddr t = do
    args <- getAtoms argsAddr
    --sendIO $ putStrLn $ "  " ++ show foreignCTarget ++ " " ++ show args
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
        -> allocAtoms [DoubleV $ rts_intEncodeDouble j e]

      StaticTarget _ "__word_encodeDouble" _ _
        | [WordV j, IntV e, Void] <- args
        , UnboxedTuple [DoubleRep] <- t
        -> allocAtoms [DoubleV $ rts_wordEncodeDouble j e]

      StaticTarget _ "__int_encodeFloat" _ _
        | [IntV j, IntV e, Void] <- args
        , UnboxedTuple [FloatRep] <- t
        -> allocAtoms [FloatV $ rts_intEncodeFloat j e]

      StaticTarget _ "__word_encodeFloat" _ _
        | [WordV j, IntV e, Void] <- args
        , UnboxedTuple [FloatRep] <- t
        -> allocAtoms [FloatV $ rts_wordEncodeFloat j e]

      StaticTarget _ "stg_interp_constr1_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr2_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr3_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr4_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr5_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr6_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget
      StaticTarget _ "stg_interp_constr7_entry" _ _ -> stgErrorM $ "not implemented: " ++ show foreignCTarget

      StaticTarget _ "freeHaskellFunctionPtr" _ _ -> pure [] -- TODO
      StaticTarget _ "performMajorGC" _ _ -> pure []
      StaticTarget _ "rts_setMainThread" _ _ -> pure [] -- TODO

      StaticTarget _ "stg_sig_install" _ _ -> allocAtoms [IntV (-1)]                          -- TODO: for testsuite

      StaticTarget _ "lockFile" _ _ -> allocAtoms [IntV 0]
      StaticTarget _ "unlockFile" _ _ -> allocAtoms [IntV 0]
      StaticTarget _ "rtsSupportsBoundThreads" _ _ -> allocAtoms [IntV 0]
{-
      StaticTarget _ "getMonotonicNSec" _ _
        | [Void] <- args
        -> do
          now <- sendIO getCurrentTime
          pure [WordV nSec]
-}
{-
StgWord64 getMonotonicNSec(void)
{
#if defined(HAVE_CLOCK_GETTIME)
    return getClockTime(CLOCK_ID);

#elif defined(darwin_HOST_OS)

    uint64_t time = mach_absolute_time();
    return (time * timer_scaling_factor_numer) / timer_scaling_factor_denom;

#else // use gettimeofday()

    struct timeval tv;

    if (gettimeofday(&tv, (struct timezone *) NULL) != 0) {
        debugBlech("getMonotonicNSec: gettimeofday failed: %s", strerror(errno));
    };
    return (StgWord64)tv.tv_sec * 1000000000 +
           (StgWord64)tv.tv_usec * 1000;

#endif
}

-}
      {-
void
getProgArgv(int *argc, char **argv[])
{
    if (argc) { *argc = prog_argc; }
    if (argv) { *argv = prog_argv; }
}
      -}
      StaticTarget _ "getProgArgv" _ _
        | [PtrAtom (ByteArrayPtr ba1) ptrArgc, PtrAtom (ByteArrayPtr ba2) ptrArgv, Void] <- args
        -> do
          Rts{..} <- gets ssRtsSupport
          sendIO $ do
            -- HINT: getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()
            poke (castPtr ptrArgc :: Ptr CInt) (fromIntegral $ 1 + length rtsProgArgs)

            -- FIXME: this has a race condition with the GC!!!! because it is pure
            arr1 <- newCString rtsProgName :: IO CString
            args <- mapM newCString rtsProgArgs :: IO [CString]
            arr2 <- newArray (arr1 : args ++ [nullPtr]) :: IO (Ptr CString)

            poke (castPtr ptrArgv :: Ptr (Ptr CString)) arr2--(castPtr arr2 :: Ptr CString )
          pure []

      StaticTarget _ "shutdownHaskellAndExit" _ _
        | [IntV retCode, IntV fastExit, Void] <- args
        , UnboxedTuple [] <- t
        -> do
          --showDebug evalOnNewThread
          --error $ "shutdownHaskellAndExit exit code:  " ++ show retCode ++ ", fast exit: " ++ show fastExit
          --exportCallGraph
          sendIO . exitWith $ case retCode of
            0 -> ExitSuccess
            n -> ExitFailure n

      StaticTarget _ "debugBelch2" _ _
        | [PtrAtom (ByteArrayPtr bai1) _, PtrAtom (ByteArrayPtr bai2) _, Void] <- args
        -> do
          let
            showByteArray :: M sig m => ByteArrayIdx -> m String
            showByteArray b = do
              ByteArrayDescriptor{..} <- lookupByteArrayDescriptorI b
              Text.unpack . Text.decodeUtf8 . BS.pack . filter (/=0) . Exts.toList <$> sendIO (BA.unsafeFreezeByteArray baaMutableByteArray)
          formatStr <- showByteArray bai1
          value <- showByteArray bai2
          sendIO $ do
            hPutStr stderr $ printf formatStr value
            hFlush stderr
          pure []

      StaticTarget _ "errorBelch" _ _ -> do
        sendIO $ putStrLn $ "errorBelch: " ++ show args
        pure []
      StaticTarget _ "errorBelch2" _ _
        | [PtrAtom (ByteArrayPtr bai1) _, PtrAtom (ByteArrayPtr bai2) _, Void] <- args
        -> do
          let
            showByteArray :: M sig m => ByteArrayIdx -> m String
            showByteArray b = do
              ByteArrayDescriptor{..} <- lookupByteArrayDescriptorI b
              Text.unpack . Text.decodeUtf8 . BS.pack . filter (/=0) . Exts.toList <$> sendIO (BA.unsafeFreezeByteArray baaMutableByteArray)
          formatStr <- showByteArray bai1
          value <- showByteArray bai2
          Rts{..} <- gets ssRtsSupport
          sendIO $ hPutStrLn stderr $ takeBaseName rtsProgName ++ ": " ++ printf formatStr value
          pure []

      StaticTarget _ "hs_free_stable_ptr" _ _ -> pure []

      -- GHC RTS global store getOrSet function implementation
      StaticTarget _ foreignSymbol _ _
        | Set.member foreignSymbol globalStoreSymbols
        , [value, Void] <- args
        , [valueAddr, _] <- argsAddr
        -> do
            -- HINT: set once with the first value, then return it always, only for the globalStoreSymbols
            store <- gets $ rtsGlobalStore . ssRtsSupport
            case Map.lookup foreignSymbol store of
              Nothing -> do
                modify $ \s@StgState{..} -> s {ssRtsSupport = ssRtsSupport {rtsGlobalStore = Map.insert foreignSymbol valueAddr store}}
                pure [valueAddr]
              Just v  -> pure [v]

      _ -> fallback fCall argsAddr t

foreign import ccall unsafe "__int_encodeDouble"  rts_intEncodeDouble  :: Int  -> Int -> Double
foreign import ccall unsafe "__word_encodeDouble" rts_wordEncodeDouble :: Word -> Int -> Double
foreign import ccall unsafe "__int_encodeFloat"   rts_intEncodeFloat   :: Int  -> Int -> Float
foreign import ccall unsafe "__word_encodeFloat"  rts_wordEncodeFloat  :: Word -> Int -> Float
