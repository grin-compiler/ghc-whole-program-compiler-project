module WPC.GlobalEnv where

import Data.IORef
import System.IO.Unsafe

import GHC.Plugins
import GHC.Stg.Syntax
import GHC.Types.ForeignStubs

import WPC.ForeignStubDecls

data GlobalEnv
  = GlobalEnv
  { geModSummary  :: Maybe ModSummary
  , geModGuts     :: Maybe ModGuts
  , geStgBinds    :: Maybe [CgStgTopBinding]
  , geHscEnv      :: Maybe HscEnv
  , geStubDecls   :: Maybe [(ForeignStubs, StubDecl)]
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
