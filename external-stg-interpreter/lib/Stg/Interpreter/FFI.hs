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
import Stg.Interpreter.Base
import Stg.Interpreter.Debug
import Stg.Interpreter.Rts (globalStoreSymbols)

pattern CharV c = Literal (LitChar c)
pattern IntV i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

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

getFFILabelPtrAtom :: Name -> LabelSpec -> M Atom
getFFILabelPtrAtom labelName labelSpec = do
  dl <- gets ssCBitsMap
  funPtr <- liftIO . dlsym dl $ BS8.unpack labelName
  pure $ PtrAtom (LabelPtr labelName labelSpec) $ castFunPtrToPtr funPtr

mkFFIArg :: Atom -> M (Maybe FFI.Arg)
mkFFIArg = \case
  Void              -> pure Nothing
  PtrAtom _ p       -> pure . Just $ FFI.argPtr p
  IntV i            -> pure . Just $ FFI.argInt $ fromIntegral i
  WordV w           -> pure . Just $ FFI.argWord $ fromIntegral w
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
    result <- FFI.callFFI funPtr FFI.retInt cArgs
    pure [IntV $ fromIntegral result]

  UnboxedTuple [WordRep] -> do
    result <- FFI.callFFI funPtr FFI.retWord cArgs
    pure [WordV $ fromIntegral result]

  UnboxedTuple [AddrRep] -> do
    result <- FFI.callFFI funPtr (FFI.retPtr FFI.retWord8) cArgs
    pure [PtrAtom RawPtr result]

  UnboxedTuple [FloatRep] -> do
    CFloat result <- FFI.callFFI funPtr FFI.retCFloat cArgs
    pure [FloatAtom result]

  UnboxedTuple [DoubleRep] -> do
    CDouble result <- FFI.callFFI funPtr FFI.retCDouble cArgs
    pure [DoubleAtom result]

{-# NOINLINE evalFCallOp #-}
evalFCallOp :: EvalOnNewThread -> ForeignCall -> [Atom] -> Type -> Maybe TyCon -> M [Atom]
evalFCallOp evalOnNewThread fCall@ForeignCall{..} args t _tc = do
    --liftIO $ putStrLn $ "  " ++ show foreignCTarget ++ " " ++ show args
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

      StaticTarget _ "freeHaskellFunctionPtr" _ _ -> pure [] -- TODO
      StaticTarget _ "performMajorGC" _ _ -> pure []
      StaticTarget _ "rts_setMainThread" _ _ -> pure [] -- TODO

      StaticTarget _ "stg_sig_install" _ _ -> pure [IntV (-1)]                          -- TODO: for testsuite

      StaticTarget _ "lockFile" _ _ -> pure [IntV 0]
      StaticTarget _ "unlockFile" _ _ -> pure [IntV 0]
      StaticTarget _ "rtsSupportsBoundThreads" _ _ -> pure [IntV 0]
{-
      StaticTarget _ "getMonotonicNSec" _ _
        | [Void] <- args
        -> do
          now <- liftIO getCurrentTime
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

{-
      StaticTarget _ "hs_free_stable_ptr" _ _ -> pure []
-}
      -- support for exporting haskell function (GHC RTS specific)
      StaticTarget _ "createAdjustor" _ _
        | [ IntV 1
          , PtrAtom StablePtr{} sp
          , _
          , PtrAtom (CStringPtr typeCString) _
          , PtrAtom (CStringPtr hsTypeCString) _
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
          -- FIXME: _freeWrapper needs to be called otherwise it will leak the memory!!!!
          let Just (typeString, _)    = BS8.unsnoc typeCString
              Just (hsTypeString, _)  = BS8.unsnoc hsTypeCString
          (funPtr, _freeWrapper) <- createAdjustor evalOnNewThread fun (BS8.unpack typeString) (BS8.unpack hsTypeString)
          pure [PtrAtom RawPtr $ castFunPtrToPtr funPtr]

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
          dl <- gets ssCBitsMap
          liftIOAndBorrowStgState $ do
            {-
            when (False || "hs_OpenGLRaw_getProcAddress" == foreignSymbol) $ do
              print args
              getLine
              pure ()
            -}
            funPtr <- dlsym dl $ BS8.unpack foreignSymbol
            evalForeignCall funPtr cArgs t

      DynamicTarget
        | (PtrAtom RawPtr funPtr) : funArgs <- args
        -> do
          cArgs <- catMaybes <$> mapM mkFFIArg funArgs
          liftIOAndBorrowStgState $ do
            evalForeignCall (castPtrToFunPtr funPtr) cArgs t

------------------------------------------

      _ -> stgErrorM $ "unsupported StgFCallOp: " ++ show fCall ++ " :: " ++ show t ++ "\n args: " ++ show args

charToFFIType :: Char -> Ptr FFI.CType
charToFFIType = \case
  'v' -> FFI.ffi_type_void
  'f' -> FFI.ffi_type_float
  'd' -> FFI.ffi_type_double
  'L' -> FFI.ffi_type_sint64
  'l' -> FFI.ffi_type_uint64
  'W' -> FFI.ffi_type_sint32
  'w' -> FFI.ffi_type_uint32
  'S' -> FFI.ffi_type_sint16
  's' -> FFI.ffi_type_uint16
  'B' -> FFI.ffi_type_sint8
  'b' -> FFI.ffi_type_uint8
  'p' -> FFI.ffi_type_pointer
  c   -> error $ "charToFFIType: unknown type " ++ show c

charToGetter :: Char -> Ptr FFI.CValue -> IO Atom
charToGetter c p = case c of
  'v' -> pure Void
  'f' -> FloatAtom <$> peek (castPtr p)
  'd' -> DoubleAtom <$> peek (castPtr p)
  'L' -> IntV  . fromIntegral <$> peek (castPtr p :: Ptr Int64)
  'l' -> WordV . fromIntegral <$> peek (castPtr p :: Ptr Word64)
  'W' -> IntV  . fromIntegral <$> peek (castPtr p :: Ptr Int32)
  'w' -> WordV . fromIntegral <$> peek (castPtr p :: Ptr Word32)
  'S' -> IntV  . fromIntegral <$> peek (castPtr p :: Ptr Int16)
  's' -> WordV . fromIntegral <$> peek (castPtr p :: Ptr Word16)
  'B' -> IntV  . fromIntegral <$> peek (castPtr p :: Ptr Int8)
  'b' -> WordV . fromIntegral <$> peek (castPtr p :: Ptr Word8)
  'p' -> PtrAtom RawPtr <$> peek (castPtr p)
  x   -> error $ "charToGetter: unknown type " ++ show x

charToSetter :: Char -> Ptr FFI.CValue -> Atom -> IO ()
charToSetter c p a = case (c, a) of
  ('v', Void)         -> pure ()
  ('v', HeapPtr{})    -> pure () -- WTF???
  ('f', FloatAtom  v) -> poke (castPtr p) v
  ('d', DoubleAtom v) -> poke (castPtr p) v
  ('L', IntV  v)      -> poke (castPtr p :: Ptr Int64)  $ fromIntegral v
  ('l', WordV v)      -> poke (castPtr p :: Ptr Word64) $ fromIntegral v
  ('W', IntV  v)      -> poke (castPtr p :: Ptr Int32)  $ fromIntegral v
  ('w', WordV v)      -> poke (castPtr p :: Ptr Word32) $ fromIntegral v
  ('S', IntV  v)      -> poke (castPtr p :: Ptr Int16)  $ fromIntegral v
  ('s', WordV v)      -> poke (castPtr p :: Ptr Word16) $ fromIntegral v
  ('B', IntV  v)      -> poke (castPtr p :: Ptr Int8)   $ fromIntegral v
  ('b', WordV v)      -> poke (castPtr p :: Ptr Word8)  $ fromIntegral v
  ('p', PtrAtom RawPtr v)  -> poke (castPtr p) v
  x   -> error $ "charToSetter: unknown type " ++ show x

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
ffiCallbackBridge :: HasCallStack => EvalOnNewThread -> MVar StgState -> Atom -> String -> String -> Ptr FFI.CIF -> Ptr FFI.CValue -> Ptr (Ptr FFI.CValue) -> Ptr Word8 -> IO ()
ffiCallbackBridge evalOnNewThread stateStore fun typeString hsTypeString _cif retStorage argsStoragePtr _userData = do
  let (retType : argsType) = typeString
      ('e' : hsRetType : hsArgsType) = hsTypeString
  -- read args from ffi
  argsStorage <- peekArray (length argsType) argsStoragePtr
  argAtoms <- zipWithM charToGetter argsType argsStorage
  {-
  putStrLn $ "got FFI callback: " ++ show fun ++ " " ++ show argAtoms
  putStrLn $ " typeString:   " ++ show typeString
  putStrLn $ " hsTypeString: " ++ show hsTypeString

  putStrLn $ "[callback BEGIN] " ++ show fun
  -}
  before <- takeMVar stateStore
  (result, after) <- flip runStateT before $ do
    {-
    oldThread <- gets ssCurrentThreadId
    -- TODO: properly setup ffi thread
    (tidFFI, tsFFI) <- createThread
    insertThread tidFFI tsFFI
    scheduleToTheEnd tidFFI
    switchToThread tidFFI
    -}
    evalOnNewThread $ do
      -- TODO: box FFI arg atoms
      --  i.e. rts_mkWord8
      -- TODO: check how the stubs are generated and what types are need to be boxed
      boxedArgs <- zipWithM boxFFIAtom hsArgsType argAtoms
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      -- Q: what stack shall we use here?
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      stackPush $ Apply [] -- force result to WHNF
      stackPush $ Apply $ boxedArgs ++ [Void]
      pure [fun]
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
  putMVar stateStore after
  --putStrLn $ "[callback END]   " ++ show fun

  -- HINT: need some kind of channel between the IO world and the interpreters StateT IO
  -- NOTE: stg apply fun argAtoms
  case result of
    []        -> pure ()
    [retAtom] -> do
      -- write result to ffi
      -- NOTE: only single result is supported
      charToSetter retType retStorage retAtom

createAdjustor :: HasCallStack => EvalOnNewThread -> Atom -> String -> String -> M (FunPtr a, IO ())
createAdjustor evalOnNewThread fun typeString hsTypeString = do
  --liftIO $ putStrLn $ "created adjustor: " ++ show fun ++ " " ++ show typeString ++ " " ++ show hsTypeString

  let (retCType : argsCType) = map charToFFIType typeString
  stateStore <- gets $ unPrintableMVar . ssStateStore
  liftIO $ FFI.wrapper retCType argsCType (ffiCallbackBridge evalOnNewThread stateStore fun typeString hsTypeString)

boxFFIAtom :: Char -> Atom -> M Atom
boxFFIAtom c a = case (c, a) of
  -- boxed Char
  ('c', WordV _)      -> mkWiredInCon rtsCharCon    [a]

  -- boxed Ints
  ('I', IntV _)       -> mkWiredInCon rtsIntCon     [a]
  ('X', IntV _)       -> mkWiredInCon rtsInt8Con    [a]
  ('Y', IntV _)       -> mkWiredInCon rtsInt16Con   [a]
  ('Z', IntV _)       -> mkWiredInCon rtsInt32Con   [a]
  ('W', IntV _)       -> mkWiredInCon rtsInt64Con   [a]

  -- boxed Words
  ('i', WordV _)       -> mkWiredInCon rtsWordCon    [a]
  ('x', WordV _)       -> mkWiredInCon rtsWord8Con   [a]
  ('y', WordV _)       -> mkWiredInCon rtsWord16Con  [a]
  ('z', WordV _)       -> mkWiredInCon rtsWord32Con  [a]
  ('w', WordV _)       -> mkWiredInCon rtsWord64Con  [a]

  ('p', PtrAtom RawPtr _)     -> mkWiredInCon rtsPtrCon     [a]
  ('*', PtrAtom RawPtr _)     -> mkWiredInCon rtsFunPtrCon  [a]

  ('f', FloatAtom _)  -> mkWiredInCon rtsFloatCon   [a]
  ('d', DoubleAtom _) -> mkWiredInCon rtsDoubleCon  [a]

  ('P', PtrAtom RawPtr _)     -> mkWiredInCon rtsStablePtrCon   [a]
  ('b', IntV i)       -> mkWiredInCon (if i == 0 then rtsFalseCon else rtsTrueCon) []
  ('s', PtrAtom RawPtr _)     -> error "TODO: support C string FFI arg boxing"
  x                   -> error $ "boxFFIAtom - unknown pattern: " ++ show x

mkWiredInCon :: (Rts -> DataCon) -> [Atom] -> M Atom
mkWiredInCon conFun args = do
  dc <- gets $ conFun . ssRtsSupport
  HeapPtr <$> allocAndStore (Con False dc args)

unboxFFIAtom :: Char -> Atom -> M Atom
unboxFFIAtom c heapObj = do
  (Con _ dc args) <- readHeapCon heapObj
  case (c, args) of
    -- boxed Char
    ('c', [v])  -> pure v

    -- boxed Ints
    ('I', [v])  -> pure v
    ('X', [v])  -> pure v
    ('Y', [v])  -> pure v
    ('Z', [v])  -> pure v
    ('W', [v])  -> pure v

    -- boxed Words
    ('i', [v])  -> pure v
    ('x', [v])  -> pure v
    ('y', [v])  -> pure v
    ('z', [v])  -> pure v
    ('w', [v])  -> pure v

    ('p', [v])  -> pure v
    ('*', [v])  -> pure v

    ('f', [v])  -> pure v
    ('d', [v])  -> pure v

    ('P', [v])  -> pure v

    ('b', []) -> do
      trueCon <- gets $ rtsTrueCon . ssRtsSupport
      pure . IntV $ if dc == trueCon then 1 else 0

{-
hsTyDescChar :: Type -> Char
hsTyDescChar ty
  | ty `eqType` unitTy = 'v'
  | otherwise = case showFFIType ty of
      "Char"      -> 'c'
      "Int"       -> 'I'
      "Int8"      -> 'X'
      "Int16"     -> 'Y'
      "Int32"     -> 'Z'
      "Int64"     -> 'W'
      "Word"      -> 'i'
      "Word8"     -> 'x'
      "Word16"    -> 'y'
      "Word32"    -> 'z'
      "Word64"    -> 'w'

      "Ptr"       -> 'p'
      "FunPtr"    -> '*'
      "Float"     -> 'f'
      "Double"    -> 'd'
      "StablePtr" -> 'P'
      "Bool"      -> 'b'
      "String"    -> 's'
      _other -> pprPanic "GHC.HsToCore.Foreign.Decl.hsTyDescChar" (ppr ty)

  hs_type_string  = effect_char : (map hsTyDescChar $ res_hty : arg_htys)
  effect_char     = if is_IO_res_ty then 'e' else 'p' -- HINT: effectful or pure
-}
