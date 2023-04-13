{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.FFI where

----- FFI experimental
import qualified GHC.Exts as Exts
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Control.Concurrent

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.Posix.DynamicLinker
import Data.Word
import Data.Int
import Data.Maybe
import qualified Foreign.LibFFI as FFI
import qualified Foreign.LibFFI.Internal as FFI
import qualified Foreign.LibFFI.FFITypes as FFI
import qualified Foreign.LibFFI.Closure as FFI
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import qualified Data.Primitive.ByteArray as BA
import qualified Data.ByteString.Char8 as BS8
-----
import System.Exit
import System.IO
import System.FilePath
import Text.Printf

import Data.Time.Clock

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import GHC.Stack
import Control.Monad.State.Strict
import Control.Concurrent.MVar

import Stg.Syntax
import Stg.GHC.Symbols
import Stg.Interpreter.Base
import Stg.Interpreter.Debug
import qualified Stg.Interpreter.GC as GC
import Stg.Interpreter.Rts (globalStoreSymbols)

pattern CharV c   = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int8V i   = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int16V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int32V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern Int64V i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word8V i  = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word16V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word64V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

{-
data ForeignCall
  = ForeignCall
  { foreignCTarget  :: !CCallTarget
  , foreignCConv    :: !CCallConv
  , foreignCSafety  :: !Safety
  }

data Safety = PlaySafe | PlayInterruptible | PlayRisky

data CCallTarget
  = StaticTarget !SourceText !BS8.ByteString !(Maybe UnitId) !Bool
  | DynamicTarget

data CCallConv = CCallConv | CApiConv | StdCallConv | PrimCallConv | JavaScriptCallConv
-}

rtsSymbolSet :: Set Name
rtsSymbolSet = Set.fromList $ map (BS8.pack . getSymbolName) rtsSymbols

getFFISymbol :: Name -> M (FunPtr a)
getFFISymbol name = do
  dl <- gets ssCBitsMap
  funPtr <- liftIO . BS8.useAsCString name $ c_dlsym (packDL dl)
  case funPtr == nullFunPtr of
    False -> pure funPtr
    True  -> if Set.member name rtsSymbolSet
      then stgErrorM $ "this RTS symbol is not implemented yet: " ++ BS8.unpack name
      else stgErrorM $ "unknown foreign symbol: " ++ BS8.unpack name

getFFILabelPtrAtom :: Name -> LabelSpec -> M Atom
getFFILabelPtrAtom labelName labelSpec = do
  funPtr <- getFFISymbol labelName
  pure $ PtrAtom (LabelPtr labelName labelSpec) $ castFunPtrToPtr funPtr

mkFFIArg :: Atom -> M (Maybe FFI.Arg)
mkFFIArg = \case
  Void              -> pure Nothing
  PtrAtom _ p       -> pure . Just $ FFI.argPtr p
  IntV i            -> pure . Just $ FFI.argInt64 $ fromIntegral i
  Int8V i           -> pure . Just $ FFI.argInt8 $ fromIntegral i
  Int16V i          -> pure . Just $ FFI.argInt16 $ fromIntegral i
  Int32V i          -> pure . Just $ FFI.argInt32 $ fromIntegral i
  Int64V i          -> pure . Just $ FFI.argInt64 $ fromIntegral i
  WordV w           -> pure . Just $ FFI.argWord64 $ fromIntegral w
  Word8V w          -> pure . Just $ FFI.argWord8 $ fromIntegral w
  Word16V w         -> pure . Just $ FFI.argWord16 $ fromIntegral w
  Word32V w         -> pure . Just $ FFI.argWord32 $ fromIntegral w
  Word64V w         -> pure . Just $ FFI.argWord64 $ fromIntegral w
  FloatAtom f       -> pure . Just . FFI.argCFloat $ CFloat f
  DoubleAtom d      -> pure . Just . FFI.argCDouble $ CDouble d
  ByteArray bai -> do
    ba <- baaMutableByteArray <$> lookupByteArrayDescriptorI bai
    pure . Just . FFI.argPtr $ BA.mutableByteArrayContents ba
  MutableByteArray bai -> do
    ba <- baaMutableByteArray <$> lookupByteArrayDescriptorI bai
    pure . Just . FFI.argPtr $ BA.mutableByteArrayContents ba
  Literal LitNullAddr -> pure . Just $ FFI.argPtr nullPtr
  a -> error $ "mkFFIArg - unsupported atom: " ++ show a


evalForeignCall :: FunPtr a -> [FFI.Arg] -> Type -> IO [Atom]
evalForeignCall funPtr cArgs retType = case retType of
  UnboxedTuple [] -> do
    _result <- FFI.callFFI funPtr FFI.retVoid cArgs
    pure []

  UnboxedTuple [IntRep] -> do
    result <- FFI.callFFI funPtr FFI.retInt64 cArgs
    pure [IntV $ fromIntegral result]

  UnboxedTuple [Int8Rep] -> do
    result <- FFI.callFFI funPtr FFI.retInt8 cArgs
    pure [Int8V $ fromIntegral result]

  UnboxedTuple [Int16Rep] -> do
    result <- FFI.callFFI funPtr FFI.retInt16 cArgs
    pure [Int16V $ fromIntegral result]

  UnboxedTuple [Int32Rep] -> do
    result <- FFI.callFFI funPtr FFI.retInt32 cArgs
    pure [Int32V $ fromIntegral result]

  UnboxedTuple [Int64Rep] -> do
    result <- FFI.callFFI funPtr FFI.retInt64 cArgs
    pure [Int64V $ fromIntegral result]

  UnboxedTuple [WordRep] -> do
    result <- FFI.callFFI funPtr FFI.retWord64 cArgs
    pure [WordV $ fromIntegral result]

  UnboxedTuple [Word8Rep] -> do
    result <- FFI.callFFI funPtr FFI.retWord8 cArgs
    pure [Word8V $ fromIntegral result]

  UnboxedTuple [Word16Rep] -> do
    result <- FFI.callFFI funPtr FFI.retWord16 cArgs
    pure [Word16V $ fromIntegral result]

  UnboxedTuple [Word32Rep] -> do
    result <- FFI.callFFI funPtr FFI.retWord32 cArgs
    pure [Word32V $ fromIntegral result]

  UnboxedTuple [Word64Rep] -> do
    result <- FFI.callFFI funPtr FFI.retWord64 cArgs
    pure [Word64V $ fromIntegral result]

  UnboxedTuple [AddrRep] -> do
    result <- FFI.callFFI funPtr (FFI.retPtr FFI.retWord8) cArgs
    pure [PtrAtom RawPtr result]

  UnboxedTuple [FloatRep] -> do
    CFloat result <- FFI.callFFI funPtr FFI.retCFloat cArgs
    pure [FloatAtom result]

  UnboxedTuple [DoubleRep] -> do
    CDouble result <- FFI.callFFI funPtr FFI.retCDouble cArgs
    pure [DoubleAtom result]

  _ -> error $ "unsupported retType: " ++ show retType

{-# NOINLINE evalFCallOp #-}
evalFCallOp :: EvalOnNewThread -> ForeignCall -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalFCallOp evalOnNewThread fCall@ForeignCall{..} args t _tc = do
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
      StaticTarget _ "performMajorGC" _ _ -> pure []
      StaticTarget _ "rts_setMainThread" _ _ -> pure [] -- TODO

      StaticTarget _ "stg_sig_install" _ _ -> pure [IntV (-1)]                          -- TODO: for testsuite

      StaticTarget _ "lockFile" _ _ -> pure [IntV 0]
      StaticTarget _ "unlockFile" _ _ -> pure [IntV 0]
      StaticTarget _ "rtsSupportsBoundThreads" _ _ -> pure [IntV 0]

      StaticTarget _ "getMonotonicNSec" _ _
        | [Void] <- args
        -> do
          now <- liftIO getCurrentTime
          pure [WordV 0]

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
setProgArgv(int argc, char *argv[])
{
    freeArgv(prog_argc,prog_argv);
    prog_argc = argc;
    prog_argv = copyArgv(argc,argv);
    setProgName(prog_argv);
}
-}
      StaticTarget _ "setProgArgv" _ _
        | [IntAtom argc, PtrAtom _ argvPtr, Void] <- args
        -> do
          liftIO $ do
            -- peekCString :: CString -> IO String 
            argv <- peekArray argc (castPtr argvPtr) >>= mapM peekCString
            print (argc, argv)
          -- TODO: save to the env!!
          pure []
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
          liftIO $ do
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

      -- support for exporting haskell function (GHC RTS specific)
      StaticTarget _ "createAdjustor" _ _
        | [ IntV 1
          , PtrAtom StablePtr{} sp
          , Literal (LitLabel wrapperName _)
          , PtrAtom CStringPtr{} _
          , Void
          ] <- args
        , UnboxedTuple [AddrRep] <- t
        -> do
          fun@HeapPtr{} <- lookupStablePointerPtr sp
          {-
          unsupported StgFCallOp: StgFCallOp
            (ForeignCall {foreignCTarget = StaticTarget NoSourceText "createAdjustor" Nothing True, foreignCConv = CCallConv, foreignCSafety = PlayRisky})
            :: UnboxedTuple [AddrRep]
          args:
            [ Literal (LitNumber LitNumInt 1)
            , StablePointer (HeapPtr 120502)
            , Literal (LitLabel "zdGLUTzm2zi7zi0zi15zm1pzzTWDEZZBcYHcS36qZZ2lppzdGraphicsziUIziGLUTziRawziCallbackszdGLUTzzm2zzi7zzi0zzi15zzm1pzzzzTWDEZZZZBcYHcS36qZZZZ2lppzuGraphicszziUIzziGLUTzziRawzziCallbackszumakeDisplayFunc" (FunctionLabel Nothing))
            , CStringPtr 0 "\NUL"
            , Void
            ]
          -}
          cwrapperDesc <- lookupCWrapperHsType wrapperName
          -- FIXME: _freeWrapper needs to be called otherwise it will leak the memory!!!!
          --let Just (typeString, _)    = BS8.unsnoc typeCString    -- HINT: drop null terminator
          --liftIO $ putStrLn $ "createAdjustor: (wrapper, hs-ty, c-ty) = " ++ show (wrapperName, hsTypeString, typeString)
          (funPtr, _freeWrapper) <- createAdjustor evalOnNewThread fun cwrapperDesc
          pure [PtrAtom RawPtr $ castFunPtrToPtr funPtr]

      StaticTarget _ "createAdjustor" _ _
        -> stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args

      -- GHC RTS global store getOrSet function implementation
      StaticTarget _ foreignSymbol _ _
        | Set.member foreignSymbol globalStoreSymbols
        , [value, Void] <- args
        -> do
            -- HINT: set once with the first value, then return it always, only for the globalStoreSymbols
            store <- gets $ rtsGlobalStore . ssRtsSupport
            case Map.lookup foreignSymbol store of
              Nothing -> state $ \s@StgState{..} -> ([value], s {ssRtsSupport = ssRtsSupport {rtsGlobalStore = Map.insert foreignSymbol value store}})
              Just v  -> pure [v]

------------------------------------------

      StaticTarget _ foreignSymbol _ _
        -> do
          --liftIO $ print foreignSymbol
          cArgs <- catMaybes <$> mapM mkFFIArg args
          funPtr <- getFFISymbol foreignSymbol
          liftIOAndBorrowStgState $ do
            {-
            when (False || "hs_OpenGLRaw_getProcAddress" == foreignSymbol) $ do
              print args
              getLine
              pure ()
            -}
            evalForeignCall funPtr cArgs t

      DynamicTarget
        | (PtrAtom RawPtr funPtr) : funArgs <- args
        -> do
          cArgs <- catMaybes <$> mapM mkFFIArg funArgs
          liftIOAndBorrowStgState $ do
            evalForeignCall (castPtrToFunPtr funPtr) cArgs t

------------------------------------------

      _ -> stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args


{-
  rtsTopHandlerRunIO
  rtsTopHandlerRunNonIO
-}

{-
void zdGLUTzm2zi7zi0zi15zm1pzzTWDEZZBcYHcS36qZZ2lppzdGraphicsziUIziGLUTziRawziCallbackszdGLUTzzm2zzi7zzi0zzi15zzm1pzzzzTWDEZZZZBcYHcS36qZZZZ2lppzuGraphicszziUIzziGLUTzziRawzziCallbackszumakePositionFunc
  ( StgStablePtr the_stableptr
  , HsInt32 a1
  , HsInt32 a2)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
rts_evalIO( &cap
          , rts_apply(cap
           , (HaskellObj)runIO_closure
           , rts_apply(cap
            , rts_apply(cap, (StgClosure*)deRefStablePtr(the_stableptr), rts_mkInt32(cap,a1))
            , rts_mkInt32(cap,a2))) ,&ret);
rts_checkSchedStatus("zdGLUTzm2zi7zi0zi15zm1pzzTWDEZZBcYHcS36qZZ2lppzdGraphicsziUIziGLUTziRawziCallbackszdGLUTzzm2zzi7zzi0zzi15zzm1pzzzzTWDEZZZZBcYHcS36qZZZZ2lppzuGraphicszziUIzziGLUTzziRawzziCallbackszumakePositionFunc",cap);
rts_unlock(cap);
}
-}
--------------------
{-
                 <> ptext (if is_IO_res_ty
                                then (sLit "runIO_closure")
                                else (sLit "runNonIO_closure"))
-}


{-
/*
 * rts_evalIO() evaluates a value of the form (IO a), forcing the action's
 * result to WHNF before returning.
 */
void rts_evalIO (/* inout */ Capability **cap,
                 /* in    */ HaskellObj p,
                 /* out */   HaskellObj *ret)
{
    StgTSO* tso;

    tso = createStrictIOThread(*cap, RtsFlags.GcFlags.initialStkSize, p);
    scheduleWaitThread(tso,ret,cap);
}

/*
 * Same as above, but also evaluate the result of the IO action
 * to whnf while we're at it.
 */

StgTSO *
createStrictIOThread(Capability *cap, W_ stack_size,  StgClosure *closure)
{
  StgTSO *t;
  t = createThread(cap, stack_size);
  pushClosure(t, (W_)&stg_forceIO_info);
  pushClosure(t, (W_)&stg_ap_v_info);
  pushClosure(t, (W_)closure);
  pushClosure(t, (W_)&stg_enter_info);
  return t;
}

/* -----------------------------------------------------------------------------
    Strict IO application - performing an IO action and entering its result.

    rts_evalIO() lets you perform Haskell IO actions from outside of
    Haskell-land, returning back to you their result. Want this result
    to be evaluated to WHNF by that time, so that we can easily get at
    the int/char/whatever using the various get{Ty} functions provided
    by the RTS API.

    stg_forceIO takes care of this, performing the IO action and entering
    the results that comes back.

    ------------------------------------------------------------------------- */

INFO_TABLE_RET(stg_forceIO, RET_SMALL, P_ info_ptr)
    return (P_ ret)
{
    ENTER(ret);
}
-}

{-# NOINLINE ffiCallbackBridge #-}
ffiCallbackBridge :: HasCallStack => EvalOnNewThread -> MVar StgState -> Atom -> CWrapperDesc -> Ptr FFI.CIF -> Ptr FFI.CValue -> Ptr (Ptr FFI.CValue) -> Ptr Word8 -> IO ()
ffiCallbackBridge evalOnNewThread stateStore fun wd@(isIOCall, retTypeName, argTypeNames) _cif retStorage argsStoragePtr _userData = do
  -- read args from ffi
  argsStorage <- peekArray (length argTypeNames) argsStoragePtr
  argAtoms <- zipWithM (ffiRepToGetter . ffiTypeToFFIRep) argTypeNames argsStorage
  
  putStrLn $ "got FFI callback, fun: " ++ show fun
  putStrLn $ " argAtoms: " ++ show argAtoms
  putStrLn $ " wrapper-desc: " ++ show wd
  putStrLn $ " wrapper-argTypeNames: " ++ show argTypeNames

  putStrLn $ "[callback BEGIN] " ++ show fun
  
  before <- takeMVar stateStore
  (unboxedResult, after) <- flip runStateT before $ do
    funStr <- GC.debugPrintHeapObject <$> readHeap fun
    liftIO $ putStrLn $ "  ** fun str ** = " ++ funStr
    
    {-
    oldThread <- gets ssCurrentThreadId
    -- TODO: properly setup ffi thread
    (tidFFI, tsFFI) <- createThread
    insertThread tidFFI tsFFI
    scheduleToTheEnd tidFFI
    switchToThread tidFFI
    -}
    fuel <- gets ssDebugFuel
    liftIO $ putStrLn $ "[step 1] fuel = " ++ show fuel
    boxedResult <- evalOnNewThread $ do
      -- TODO: box FFI arg atoms
      --  i.e. rts_mkWord8
      -- TODO: check how the stubs are generated and what types are need to be boxed
      liftIO $ putStrLn $ "[step 2]"
      boxedArgs <- zipWithM boxFFIAtom argTypeNames argAtoms
      liftIO $ putStrLn $ "[step 3] boxedArgs: " ++ show boxedArgs
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      -- Q: what stack shall we use here?
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      stackPush $ RunScheduler SR_ThreadFinishedFFICallback -- return from callback
      stackPush $ Apply [] -- force result to WHNF ; is this needed?
      liftIO $ putStrLn $ "[step 4]"
      stackPush $ Apply $ boxedArgs ++ if isIOCall then [Void] else []
      liftIO $ putStrLn $ "[step 5]"
      --modify' $ \s@StgState{..} -> s {ssDebugState = DbgStepByStep}
      pure [fun]

    liftIO $ putStrLn $ "[pre - callback END]   " ++ show fun ++ " boxed-result: " ++ show boxedResult
    zipWithM unboxFFIAtom [retTypeName] boxedResult
{-
--=============================================================================
    -- force result to WHNF
    resultLazy <- evalOnNewThread fun $ boxedArgs ++ [Void]
    finalResult <- case resultLazy of
      []            -> pure resultLazy
      [valueThunk]  -> evalOnNewThread valueThunk []
    switchToThread oldThread
--=============================================================================
    pure finalResult
-}
  putStrLn $ "[pre - callback END]   " ++ show fun ++ " result: " ++ show unboxedResult
  putMVar stateStore after
  putStrLn $ "[callback END]   " ++ show fun ++ " result: " ++ show unboxedResult

  -- HINT: need some kind of channel between the IO world and the interpreters StateT IO
  -- NOTE: stg apply fun argAtoms
  case unboxedResult of
    []        -> pure ()
    [retAtom] -> do
      -- write result to ffi
      -- NOTE: only single result is supported
      ffiRepToSetter (ffiTypeToFFIRep retTypeName) retStorage retAtom retTypeName

createAdjustor :: HasCallStack => EvalOnNewThread -> Atom -> (Bool, Name, [Name]) -> M (FunPtr a, IO ())
createAdjustor evalOnNewThread fun cwrapperDesc@(_, retTy, argTys) = do
  liftIO $ putStrLn $ "created adjustor: " ++ show fun ++ " " ++ show cwrapperDesc

  let (retCType : argsCType) = map (ffiRepToCType . ffiTypeToFFIRep) $ retTy : argTys
  stateStore <- gets $ unPrintableMVar . ssStateStore
  liftIO $ FFI.wrapper retCType argsCType (ffiCallbackBridge evalOnNewThread stateStore fun cwrapperDesc)

-- NOTE: LiftedRep and UnliftedRep is not used in FFIRep only AddrRep
newtype FFIRep = FFIRep {unFFIRep :: PrimRep}

ffiTypeToFFIRep :: Name -> FFIRep
ffiTypeToFFIRep = FFIRep . \case
  "()"        -> VoidRep
  "Char"      -> WordRep
  "Int"       -> IntRep
  "Int8"      -> Int8Rep
  "Int16"     -> Int16Rep
  "Int32"     -> Int32Rep
  "Int64"     -> Int64Rep
  "Word"      -> WordRep
  "Word8"     -> Word8Rep
  "Word16"    -> Word16Rep
  "Word32"    -> Word32Rep
  "Word64"    -> Word64Rep
  "Ptr"       -> AddrRep
  "FunPtr"    -> AddrRep
  "Float"     -> FloatRep
  "Double"    -> DoubleRep
  "StablePtr" -> AddrRep
  "Bool"      -> AddrRep
  "String"    -> AddrRep

  -- additional allowed ffi import types
  "Array#"              -> AddrRep
  "MutableArray#"       -> AddrRep

  "SmallArray#"         -> AddrRep
  "MutableSmallArray#"  -> AddrRep

  "ArrayArray#"         -> AddrRep
  "MutableArrayArray#"  -> AddrRep

  "ByteArray#"          -> AddrRep
  "MutableByteArray#"   -> AddrRep

  x -> error $ "ffiTypeToFFIRep - unsupported: " ++ show x

ffiRepToCType :: FFIRep -> Ptr FFI.CType
ffiRepToCType (FFIRep r) = case r of
  VoidRep     -> FFI.ffi_type_void
  LiftedRep   -> FFI.ffi_type_pointer
  UnliftedRep -> FFI.ffi_type_pointer
  Int8Rep     -> FFI.ffi_type_sint8
  Int16Rep    -> FFI.ffi_type_sint16
  Int32Rep    -> FFI.ffi_type_sint32
  Int64Rep    -> FFI.ffi_type_sint64
  IntRep      -> FFI.ffi_type_sint64
  Word8Rep    -> FFI.ffi_type_uint8
  Word16Rep   -> FFI.ffi_type_uint16
  Word32Rep   -> FFI.ffi_type_uint32
  Word64Rep   -> FFI.ffi_type_uint64
  WordRep     -> FFI.ffi_type_uint64
  AddrRep     -> FFI.ffi_type_pointer
  FloatRep    -> FFI.ffi_type_float
  DoubleRep   -> FFI.ffi_type_double
  rep         -> error $ "ffiRepToCType - unsupported: " ++ show rep

ffiRepToGetter :: FFIRep -> Ptr FFI.CValue -> IO Atom
ffiRepToGetter (FFIRep r) p = case r of
  VoidRep     -> pure Void
  Int64Rep    -> Int64V  . fromIntegral <$> peek (castPtr p :: Ptr Int64)
  Int32Rep    -> Int32V  . fromIntegral <$> peek (castPtr p :: Ptr Int32)
  Int16Rep    -> Int16V  . fromIntegral <$> peek (castPtr p :: Ptr Int16)
  Int8Rep     -> Int8V   . fromIntegral <$> peek (castPtr p :: Ptr Int8)
  IntRep      -> IntV    . fromIntegral <$> peek (castPtr p :: Ptr Int)
  Word64Rep   -> Word64V . fromIntegral <$> peek (castPtr p :: Ptr Word64)
  Word32Rep   -> Word32V . fromIntegral <$> peek (castPtr p :: Ptr Word32)
  Word16Rep   -> Word16V . fromIntegral <$> peek (castPtr p :: Ptr Word16)
  Word8Rep    -> Word8V  . fromIntegral <$> peek (castPtr p :: Ptr Word8)
  WordRep     -> WordV   . fromIntegral <$> peek (castPtr p :: Ptr Word)
  AddrRep     -> PtrAtom RawPtr <$> peek (castPtr p)
  FloatRep    -> FloatAtom <$> peek (castPtr p)
  DoubleRep   -> DoubleAtom <$> peek (castPtr p)
  rep         -> error $ "ffiRepToGetter - unsupported: " ++ show rep

ffiRepToSetter :: FFIRep -> Ptr FFI.CValue -> Atom -> Name -> IO ()
ffiRepToSetter (FFIRep r) p a retTypeName = case (r, a) of
  (VoidRep,   Void)         -> pure ()
  (FloatRep,  FloatAtom v)  -> poke (castPtr p) v
  (DoubleRep, DoubleAtom v) -> poke (castPtr p) v
  (Int64Rep,  Int64V v)     -> poke (castPtr p :: Ptr Int64)  $ fromIntegral v
  (Int32Rep,  Int32V v)     -> poke (castPtr p :: Ptr Int32)  $ fromIntegral v
  (Int16Rep,  Int16V v)     -> poke (castPtr p :: Ptr Int16)  $ fromIntegral v
  (Int8Rep,   Int8V  v)     -> poke (castPtr p :: Ptr Int8)   $ fromIntegral v
  (IntRep,    IntV   v)     -> poke (castPtr p :: Ptr Int)    $ fromIntegral v
  (Word64Rep, Word64V v)    -> poke (castPtr p :: Ptr Word64) $ fromIntegral v
  (Word32Rep, Word32V v)    -> poke (castPtr p :: Ptr Word32) $ fromIntegral v
  (Word16Rep, Word16V v)    -> poke (castPtr p :: Ptr Word16) $ fromIntegral v
  (Word8Rep,  Word8V v)     -> poke (castPtr p :: Ptr Word8)  $ fromIntegral v
  (WordRep,   WordV v)      -> poke (castPtr p :: Ptr Word)   $ fromIntegral v
  (AddrRep,   PtrAtom RawPtr v)  -> poke (castPtr p) v
  x -> error $ "ffiRepToSetter - unsupported: " ++ show (x, retTypeName)

unboxFFIAtom :: Name -> Atom -> M Atom
unboxFFIAtom hsFFIType a = case (hsFFIType, a) of
  ("()",      HeapPtr{})  -> pure Void
  ("Int",     HeapPtr{})  -> con1Unbox
  ("Int32",   HeapPtr{})  -> con1Unbox
  ("Double",  HeapPtr{})  -> con1Unbox
  -- TODO: make this complete
  x -> error $ "unboxFFIAtom - unknown pattern: " ++ show x
 where
  con1Unbox = do
    readHeap a >>= \case
      Con{hoConArgs = [x]} -> pure x
      o -> error $ "unboxFFIAtom " ++ show (hsFFIType, a, o)

boxFFIAtom :: Name -> Atom -> M Atom
boxFFIAtom hsFFIType a = case (hsFFIType, a) of
  -- boxed Char
  ("Char", WordV _)     -> mkWiredInCon rtsCharCon    [a]

  -- boxed Ints
  ("Int", IntV _)       -> mkWiredInCon rtsIntCon     [a]
  ("Int8", Int8V _)     -> mkWiredInCon rtsInt8Con    [a]
  ("Int16", Int16V _)   -> mkWiredInCon rtsInt16Con   [a]
  ("Int32", Int32V _)   -> mkWiredInCon rtsInt32Con   [a]
  ("Int64", Int64V _)   -> mkWiredInCon rtsInt64Con   [a]

  -- boxed Words
  ("Word", WordV _)     -> mkWiredInCon rtsWordCon    [a]
  ("Word8", Word8V _)   -> mkWiredInCon rtsWord8Con   [a]
  ("Word16", Word16V _) -> mkWiredInCon rtsWord16Con  [a]
  ("Word32", Word32V _) -> mkWiredInCon rtsWord32Con  [a]
  ("Word64", Word64V _) -> mkWiredInCon rtsWord64Con  [a]

  ("Ptr", PtrAtom RawPtr _)       -> mkWiredInCon rtsPtrCon     [a]
  ("FunPtr", PtrAtom RawPtr _)    -> mkWiredInCon rtsFunPtrCon  [a]

  ("Float", FloatAtom _)          -> mkWiredInCon rtsFloatCon   [a]
  ("Double", DoubleAtom _)        -> mkWiredInCon rtsDoubleCon  [a]

  ("StablePtr", PtrAtom RawPtr _) -> mkWiredInCon rtsStablePtrCon   [a]
  ("Bool", IntV i)                -> mkWiredInCon (if i == 0 then rtsFalseCon else rtsTrueCon) []
  ("String", PtrAtom RawPtr _)    -> error "TODO: support C string FFI arg boxing"

  x -> error $ "boxFFIAtom - unknown pattern: " ++ show x

mkWiredInCon :: (Rts -> DataCon) -> [Atom] -> M Atom
mkWiredInCon conFun args = do
  dc <- gets $ conFun . ssRtsSupport
  HeapPtr <$> allocAndStore (Con False dc args)

foreign import ccall unsafe "__int_encodeDouble"  rts_intEncodeDouble  :: Int  -> Int -> Double
foreign import ccall unsafe "__word_encodeDouble" rts_wordEncodeDouble :: Word -> Int -> Double
foreign import ccall unsafe "__int_encodeFloat"   rts_intEncodeFloat   :: Int  -> Int -> Float
foreign import ccall unsafe "__word_encodeFloat"  rts_wordEncodeFloat  :: Word -> Int -> Float

type CWrapperDesc = (Bool, Name, [Name])

lookupCWrapperHsType :: Name -> M CWrapperDesc
lookupCWrapperHsType name = do
  Map.lookup name <$> gets ssCWrapperHsTypeMap >>= \case
    Nothing -> stgErrorM $ "unknown CWrapper label: " ++ show name
    Just a  -> pure a

buildCWrapperHsTypeMap :: [Module] -> M ()
buildCWrapperHsTypeMap mods = do
  let m = Map.fromListWithKey (\k a b -> error $ "CWrapper name duplication: " ++ show k ++ " with hsTypes: " ++ show (a, b))
          [ (name, (isIOCall, retType, argTypes))
          | ForeignStubs{..} <- map moduleForeignStubs mods
          , StubDeclImport _ (Just (StubImplImportCWrapper name _ isIOCall retType argTypes)) <- fsDecls
          ]
  modify' $ \s@StgState{..} -> s {ssCWrapperHsTypeMap = m}
  {-
  liftIO $ do
    putStrLn $ "CWrappers:"
    forM_ (Map.toList m) $ \(k, v) -> print k >> print v
  -}