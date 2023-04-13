module WPC.Stubs where

import GHC.Plugins
import GHC.Types.ForeignStubs
import GHC.Types.ForeignCall
import GHC.Driver.CodeOutput
import GHC.Driver.Pipeline.Execute
import Language.Haskell.Syntax.Decls
import WPC.ForeignStubDecls

import System.Directory
import System.FilePath

import Data.Maybe

outputCapiStubs :: HscEnv -> Module -> ModLocation -> [(ForeignStubs, StubDecl)] -> IO ()
outputCapiStubs hscEnv cg_module modLocation stubDecls = do
  let dflags        = hsc_dflags hscEnv
      tmpfs         = hsc_tmpfs hscEnv
      logger        = hsc_logger hscEnv
      modName       = moduleName cg_module

      capiStubs = mergeForeignStubs [s | (s, StubDeclImport (CImport  _srcText(L _ CApiConv) _safety _mHeader _spec) _) <- stubDecls]

  (_has_h, maybe_capi_stub_c) <- outputForeignStubs logger tmpfs dflags (hsc_units hscEnv) cg_module modLocation capiStubs
  mapM_ (compileCapiStubs hscEnv modName) maybe_capi_stub_c

compileCapiStubs :: HscEnv -> ModuleName -> FilePath -> IO ()
compileCapiStubs hscEnv modName capi_stub_c = do
  capi_stub_o <- compileStub hscEnv capi_stub_c
  let dflags      = hsc_dflags hscEnv
      odir        = fromMaybe "." (objectDir dflags)

      pp :: Outputable a => a -> String
      pp = showSDoc dflags . ppr

      stubPath    = foldr1 (</>) . split '.' $ pp modName
      wpcCapiStub = odir </> "extra-compilation-artifacts" </> "wpc-plugin" </> "capi-stubs" </> stubPath </> "capi_stub" ++ takeExtension capi_stub_o

  putStrLn $ "compileCapiStubs odir        - " ++ odir
  putStrLn $ "compileCapiStubs capi_stub_o - " ++ capi_stub_o
  putStrLn $ "compileCapiStubs wpcCapiStub - " ++ wpcCapiStub
  createDirectoryIfMissing True (takeDirectory wpcCapiStub)
  copyFile capi_stub_o wpcCapiStub
