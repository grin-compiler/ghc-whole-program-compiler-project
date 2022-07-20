{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms, RankNTypes, FlexibleContexts, ConstraintKinds, DataKinds, TypeApplications, GADTs #-}
module Stg.Interpreter.ForeignCall.Native where

----- FFI experimental
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


import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import GHC.Stack
import Control.Monad.IO.Class
import Control.Effect.Labelled
import Control.Concurrent.MVar

import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Control.Carrier.Lift

import qualified Control.Effect.State.Labelled as L


import Stg.Syntax
import Stg.GHC.Symbols
import Stg.Interpreter.Base

pattern CharV c = Literal (LitChar c)
pattern IntV i  = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern FloatV f  = FloatAtom f
pattern DoubleV d = DoubleAtom d

newtype PrintableMVar a = PrintableMVar {unPrintableMVar :: MVar a} deriving Eq
instance Show (PrintableMVar a) where
  show _ = "MVar"

type I2 sig m =
  ( M sig m
  , HasLabelled "FFI" (State FFIState) sig m
  )

data FFIState
  = FFIState
  { ssCBitsMap    :: DL
  , ssStateStore  :: PrintableMVar FullState
  }
  deriving (Show)

--run m   = runState  emptyMutVarState (runLabelled @"MutVar" m)
evalFFI s m = evalState s (runLabelled @"FFI" m)

type C =
  StateC StgState
    (NonDetC
      (Labelled "FFI" (StateC FFIState)
        (Labelled "Global" (StateC GlobalState)
          (LiftC IO)
        )
      )
    )

data FullState
  = FullState
  { fsStg     :: StgState
  , fsFFI     :: FFIState
  , fsGlobal  :: GlobalState
  }
  deriving Show

getFullState :: I2 sig m => m FullState
getFullState = FullState
  <$> get
  <*> L.get @"FFI"
  <*> L.get @"Global"

putFullState :: I2 sig m => FullState -> m ()
putFullState FullState{..} = do
  put fsStg
  L.put @"FFI" fsFFI
  L.put @"Global" fsGlobal

withFullState :: I2 sig m => m a -> m (FullState, a)
withFullState action = do
  result <- action
  fullState <- getFullState
  pure (fullState, result)

  -- FFI related

{-# NOINLINE evalFCallOp #-}
--evalFCallOp :: (I2 sig m) => EvalOnNewThread m -> FCallEval m -> ForeignCall -> [AtomAddr] -> Type -> m [AtomAddr]
evalFCallOp :: EvalOnNewThread C -> FCallEval C -> ForeignCall -> [AtomAddr] -> Type -> C [AtomAddr]
evalFCallOp evalOnNewThread fallback fCall@ForeignCall{..} argsAddr t = do
    args <- getAtoms argsAddr
    --sendIO $ putStrLn $ "  " ++ show foreignCTarget ++ " " ++ show args
    case foreignCTarget of

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
          funAddr <- lookupStablePointerPtr sp
          fun@HeapPtr{} <- getAtom funAddr
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
          (funPtr, _freeWrapper) <- createAdjustor evalOnNewThread funAddr (BS8.unpack typeString) (BS8.unpack hsTypeString)
          allocAtoms [PtrAtom RawPtr $ castFunPtrToPtr funPtr]

------------------------------------------

      StaticTarget _ foreignSymbol _ _
        -> do
          --sendIO $ print foreignSymbol
          cArgs <- catMaybes <$> mapM mkFFIArg args
          funPtr <- getFFISymbol foreignSymbol
          result <- liftIOAndBorrowState $ do
            {-
            when (False || "hs_OpenGLRaw_getProcAddress" == foreignSymbol) $ do
              print args
              getLine
              pure ()
            -}
            evalNativeCall funPtr cArgs t
          allocAtoms result

      DynamicTarget
        | (PtrAtom RawPtr funPtr) : funArgs <- args
        -> do
          cArgs <- catMaybes <$> mapM mkFFIArg funArgs
          result <- liftIOAndBorrowState $ do
            evalNativeCall (castPtrToFunPtr funPtr) cArgs t
          allocAtoms result

------------------------------------------

      _ -> fallback fCall argsAddr t


{-# NOINLINE liftIOAndBorrowState #-}
liftIOAndBorrowState :: (HasCallStack, I2 sig m) => IO a -> m a
liftIOAndBorrowState action = do
  -- TODO: save and restore full state
  stateStore <- L.gets @"FFI" $ unPrintableMVar . ssStateStore
  -- HINT: remember the local thread id
  myThread <- gets ssCurrentThreadId
  before <- getFullState
  (result, after) <- sendIO $ do
    -- save current state
    putMVar stateStore before
    -- execute acition
    r <- action
    -- load the state back
    s <- takeMVar stateStore
    pure (r, s)

  putFullState after
  -- HINT: continue the local thread
  switchToThread myThread
  pure result


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
rtsSymbolSet = Set.fromList $ map BS8.pack rtsSymbols

getFFISymbol :: I2 sig m => Name -> m (FunPtr a)
getFFISymbol name = do
  dl <- L.gets @"FFI" ssCBitsMap
  funPtr <- sendIO . BS8.useAsCString name $ c_dlsym (packDL dl)
  case funPtr == nullFunPtr of
    False -> pure funPtr
    True  -> if Set.member name rtsSymbolSet
      then stgErrorM $ "this RTS symbol is not implemented yet: " ++ BS8.unpack name
      else stgErrorM $ "unknown foreign symbol: " ++ BS8.unpack name

getFFILabelPtrAtom :: I2 sig m => Name -> LabelSpec -> m AtomAddr
getFFILabelPtrAtom labelName labelSpec = do
  funPtr <- getFFISymbol labelName
  storeNewAtom $ PtrAtom (LabelPtr labelName labelSpec) $ castFunPtrToPtr funPtr

mkFFIArg :: I2 sig m => Atom -> m (Maybe FFI.Arg)
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


evalNativeCall :: FunPtr a -> [FFI.Arg] -> Type -> IO [Atom]
evalNativeCall funPtr cArgs retType = case retType of
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
ffiCallbackBridge :: HasCallStack => EvalOnNewThread C -> MVar FullState -> AtomAddr -> String -> String -> Ptr FFI.CIF -> Ptr FFI.CValue -> Ptr (Ptr FFI.CValue) -> Ptr Word8 -> IO ()
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
  FullState{..} <- takeMVar stateStore
  (after, result) <- runM . evalGlobal fsGlobal . evalFFI fsFFI . fmap head . runNonDetM (:[]) . evalState fsStg . withFullState $ do
    {-
    oldThread <- gets ssCurrentThreadId
    -- TODO: properly setup ffi thread
    (tidFFI, tsFFI) <- createThread
    insertThread tidFFI tsFFI
    scheduleToTheEnd tidFFI
    switchToThread tidFFI
    -}

    resultAddress <- evalOnNewThread $ do
      -- TODO: box FFI arg atoms
      --  i.e. rts_mkWord8
      -- TODO: check how the stubs are generated and what types are need to be boxed
      boxedArgs <- zipWithM boxFFIAtom hsArgsType argAtoms
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      -- Q: what stack shall we use here?
      -- !!!!!!!!!!!!!!!!!!!!!!!!!!
      stackPush $ Apply [] -- force result to WHNF
      boxedArgsAddr <- allocAtoms $ boxedArgs ++ [Void]
      stackPush $ Apply boxedArgsAddr
      pure [fun]
    getAtoms resultAddress

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

createAdjustor :: HasCallStack => EvalOnNewThread C -> AtomAddr -> String -> String -> C (FunPtr a, IO ())
createAdjustor evalOnNewThread fun typeString hsTypeString = do
  --sendIO $ putStrLn $ "created adjustor: " ++ show fun ++ " " ++ show typeString ++ " " ++ show hsTypeString

  let (retCType : argsCType) = map charToFFIType typeString
  stateStore <- L.gets @"FFI" $ unPrintableMVar . ssStateStore
  sendIO $ FFI.wrapper retCType argsCType (ffiCallbackBridge evalOnNewThread stateStore fun typeString hsTypeString)

boxFFIAtom :: I2 sig m => Char -> Atom -> m Atom
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

mkWiredInCon :: I2 sig m => (RtsBaseInterop -> DataCon) -> [Atom] -> m Atom
mkWiredInCon conFun args = do
  argsAddr <- allocAtoms args
  dc <- gets $ conFun . ssRtsBaseInterop
  HeapPtr <$> allocAndStore (Con False dc argsAddr)

{- dead code
unboxFFIAtom :: Char -> Atom -> I2 Atom
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
-}
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
