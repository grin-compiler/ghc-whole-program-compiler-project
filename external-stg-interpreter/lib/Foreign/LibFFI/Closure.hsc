{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
{- | The internals of the C library libffi -}
module Foreign.LibFFI.Closure where

#include <ffi.h>

-- low level API import
import Foreign.LibFFI.Internal
import Foreign.Storable
import Foreign.Ptr
import Data.Word

-- high level API import
import Control.Monad
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.List (genericLength)

-- low level API

sizeOf_closure :: Int
sizeOf_closure = #size ffi_closure

type FFI_Impl = Ptr CIF -> Ptr CValue -> Ptr (Ptr CValue) -> Ptr Word8 -> IO () -- cif -> ret storage -> arg value array -> user data -> IO ()

foreign import ccall "wrapper" wrap_FFI_Impl :: FFI_Impl -> IO (FunPtr FFI_Impl)

foreign import ccall ffi_prep_closure_loc :: Closure -> Ptr CIF -> FunPtr FFI_Impl -> Ptr Word8 -> Entry -> IO C_ffi_status

newtype Closure = Closure (Ptr Closure)
newtype Entry = Entry (FunPtr Entry) deriving (Eq, Ord, Show, Storable)

foreign import ccall ffi_closure_alloc :: Int -> Ptr Entry -> IO Closure
foreign import ccall ffi_closure_free :: Closure -> IO ()

-- high level API

wrapper :: Ptr CType -> [Ptr CType] -> FFI_Impl -> IO (FunPtr a, IO ())
wrapper cRetType args ffiImpl = do

  {-
    TODO:
      done - allocate cif
      done - setup cif with ffi_prep_cif
      done - malloc cif
      done - newArray arg types
  -}
  cTypesPtr <- newArray args

  cif <- mallocBytes sizeOf_cif
  statusCif <- ffi_prep_cif cif ffi_default_abi (genericLength args) cRetType cTypesPtr
  unless (statusCif == ffi_ok) $
    error "ffi wrapper: ffi_prep_cif failed"

  (Entry p, clo) <- alloca $ \entryPtr -> do
    closure <- ffi_closure_alloc sizeOf_closure entryPtr
    entry <- peek entryPtr
    impl <- wrap_FFI_Impl ffiImpl
    statusClo <- ffi_prep_closure_loc closure cif impl nullPtr entry
    unless (statusClo == ffi_ok) $
      error "ffi wrapper: ffi_prep_closure_loc failed"
    pure (entry, closure)

  let freeFFICall = do
        ffi_closure_free clo
        free cif
        free cTypesPtr
        pure ()

  pure (castFunPtr p, freeFFICall)
