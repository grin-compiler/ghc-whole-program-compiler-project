module WPC.GlobalEnv where

import           Data.IORef             (IORef, newIORef)

import           GHC.Plugins            (HscEnv, ModGuts, ModSummary)
import           GHC.Stg.Syntax         (CgStgTopBinding)
import           GHC.Types.ForeignStubs (ForeignStubs)

import           Prelude                (Maybe (..), ($))

import           System.IO.Unsafe       (unsafePerformIO)

import           WPC.ForeignStubDecls   (StubDecl)

data GlobalEnv
  = GlobalEnv
  { geModSummary :: Maybe ModSummary
  , geModGuts    :: Maybe ModGuts
  , geStgBinds   :: Maybe [CgStgTopBinding]
  , geHscEnv     :: Maybe HscEnv
  , geStubDecls  :: Maybe [(ForeignStubs, StubDecl)]
  }

emptyGlobalEnv :: GlobalEnv
emptyGlobalEnv
  = GlobalEnv
  { geModSummary  = Nothing
  , geModGuts     = Nothing
  , geStgBinds    = Nothing
  , geHscEnv      = Nothing
  , geStubDecls   = Nothing
  }

{-# NOINLINE globalEnvIORef #-}
globalEnvIORef :: IORef GlobalEnv
globalEnvIORef = unsafePerformIO $ newIORef emptyGlobalEnv
