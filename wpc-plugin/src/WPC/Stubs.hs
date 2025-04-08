module WPC.Stubs where

import           Control.Monad                 (mapM_)

import           Data.Bool                     (Bool (..))
import           Data.Function                 (($), (.))
import Data.List ( (++), foldr1 )                    
import Data.Maybe ( fromMaybe )                    
import           Data.String                   (String)

import           GHC.Driver.CodeOutput         (outputForeignStubs)
import           GHC.Driver.Pipeline.Execute   (compileStub)
import           GHC.Plugins                   (DynFlags (..), GenLocated (..), GenModule (..), HscEnv (..),
                                                ModLocation, Module, ModuleName, Outputable (..), hsc_units, showSDoc,
                                                split)
import           GHC.Types.ForeignCall         (CCallConv (..))
import           GHC.Types.ForeignStubs        (ForeignStubs)

import           Language.Haskell.Syntax.Decls (ForeignImport (..))

import           System.Directory              (copyFile, createDirectoryIfMissing)
import           System.FilePath               (FilePath, takeDirectory, takeExtension, (</>))
import           System.IO                     (IO, putStrLn)

import           WPC.ForeignStubDecls          (StubDecl (..), mergeForeignStubs)

outputCapiStubs
  :: HscEnv
  -> Module
  -> ModLocation
  -> [(ForeignStubs, StubDecl)]
  -> IO ()
outputCapiStubs hscEnv cg_module modLocation stubDecls = do
  let dflags        = hsc_dflags hscEnv
      tmpfs         = hsc_tmpfs hscEnv
      logger        = hsc_logger hscEnv
      modName       = moduleName cg_module

      capiStubs = mergeForeignStubs
        [s | (s, StubDeclImport (CImport  _ (L _ CApiConv) _ _ _) _) <- stubDecls]

  (_has_h, maybe_capi_stub_c) <-
    outputForeignStubs
      logger
      tmpfs
      dflags
      (hsc_units hscEnv)
      cg_module
      modLocation
      capiStubs
  mapM_ (compileCapiStubs hscEnv modName) maybe_capi_stub_c

compileCapiStubs :: HscEnv -> ModuleName -> FilePath -> IO ()
compileCapiStubs hscEnv modName capi_stub_c = do
  capi_stub_o <- compileStub hscEnv capi_stub_c
  let dflags      = hsc_dflags hscEnv
      odir        = fromMaybe "." (objectDir dflags)

      pp :: Outputable a => a -> String
      pp = showSDoc dflags . ppr

      stubPath    = foldr1 (</>) . split '.' $ pp modName
      wpcCapiStub =
        odir
          </> "extra-compilation-artifacts"
          </> "wpc-plugin"
          </> "capi-stubs"
          </> stubPath
          </> "capi_stub" ++ takeExtension capi_stub_o

  putStrLn $ "compileCapiStubs odir        - " ++ odir
  putStrLn $ "compileCapiStubs capi_stub_o - " ++ capi_stub_o
  putStrLn $ "compileCapiStubs wpcCapiStub - " ++ wpcCapiStub
  createDirectoryIfMissing True (takeDirectory wpcCapiStub)
  copyFile capi_stub_o wpcCapiStub
