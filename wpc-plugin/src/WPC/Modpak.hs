{-# LANGUAGE BangPatterns #-}
module WPC.Modpak where

import System.Directory
import System.FilePath
import Data.Maybe
import Data.Containers.ListUtils ( nubOrd )

-- for external stg
import qualified WPC.StgToExtStg as ExtStg
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Binary

-- for .modpak
import GHC.SysTools.Process
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Types.ForeignStubs
import GHC.Stg.Syntax

import GHC.Plugins
import GHC.Utils.TmpFs
import GHC.Core.Ppr    ( pprCoreBindings )
import GHC.Unit.Finder
import GHC.Driver.Config.Finder (initFinderOpts)

import GHC.Iface.Load
import GHC.Iface.Make
import GHC.Iface.Tidy
import GHC.Driver.Config.Tidy
import Control.DeepSeq (force)

import GHC.Prelude

import WPC.ForeignStubDecls

outputModPak
  :: HscEnv
  -> Module
  -> CoreProgram
  -> [CgStgTopBinding]
  -> ForeignStubs
  -> ForeignStubDecls
  -> ModLocation
  -> FilePath
  -> FilePath
  -> Bool
  -> Maybe FilePath
  -> Maybe (ModGuts, ModSummary)
  -> IO ()
outputModPak hsc_env this_mod core_binds stg_binds foreign_stubs0 foreign_decls location output_filename cmm_filename has_stub_h m_stub_c mb_mod_guts = do
  let dflags = hsc_dflags hsc_env
      logger = hsc_logger hsc_env
      tmpfs  = hsc_tmpfs hsc_env

  --- save stg ---
  let stgBin      = encode (ExtStg.cvtModule dflags "stg" modUnitId modName mSrcPath stg_binds foreign_stubs0 foreign_decls)
      modName     = moduleName this_mod
      modUnitId   = moduleUnit this_mod
      mSrcPath    = ml_hs_file location

      odir            = fromMaybe "." (objectDir dflags)
      modpak_output0  = replaceExtension (ml_hi_file location) (objectSuf dflags ++ "_modpak")
      modpak_output   = odir </> "extra-compilation-artifacts" </> "wpc-plugin" </> "modpaks" </> makeRelative odir modpak_output0
  createDirectoryIfMissing True (takeDirectory modpak_output)

  -- stgbin
  stgbinFile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule (objectSuf dflags ++ "_stgbin")
  BSL.writeFile stgbinFile stgBin

  -- name pretty printer setup
  let qualifyImportedNames mod _
        | mod == this_mod = NameUnqual
        | otherwise       = NameNotInScope1
      print_unqual = QueryQualify qualifyImportedNames
                                  neverQualifyModules
                                  neverQualifyPackages
                                  alwaysPrintPromTick
      dumpStyle = mkDumpStyle print_unqual

  -- ghc stg pretty printed code
  ghcstgFile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule (objectSuf dflags ++ "_ghcstg")
  BSL.writeFile ghcstgFile . BSL8.pack $ showSDoc dflags $ withPprStyle dumpStyle $ pprStgTopBindings panicStgPprOpts stg_binds

  -- ghc core pretty printed code
  ghccoreFile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule (objectSuf dflags ++ "_ghccore")
  BSL.writeFile ghccoreFile . BSL8.pack $ showSDoc dflags $ withPprStyle dumpStyle $ pprCoreBindings core_binds

  -- ghc full core .hi file
  let (mod_guts, mod_summary) = fromMaybe (error "missing ModGuts for fullcore .hi") mb_mod_guts

  fullcoreHiFile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule (objectSuf dflags ++ "_fullcore-hi")
  writeFullCoreInterface hsc_env mod_guts mod_summary fullcoreHiFile

  -- module compilation info
  let ppYamlList key l = unlines $ key : ["- " ++ x | x <- nubOrd $ map show l]
      ppYamlSingle key v = unwords [key, show v]
  infoFile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule (objectSuf dflags ++ "_info")
  writeFile infoFile $ unlines
    [ ppYamlSingle "ghc_name:"              (ghcNameVersion_programName $ ghcNameVersion dflags)
    , ppYamlSingle "ghc_version:"           (ghcNameVersion_projectVersion $ ghcNameVersion dflags)
    , ppYamlSingle "llvm_opt_level:"        (llvmOptLevel dflags)
    , ppYamlSingle "debug_level:"           (debugLevel dflags)
    , ppYamlSingle "simpl_phases:"          (simplPhases dflags)
    , ppYamlSingle "max_simpl_iterations:"  (maxSimplIterations dflags)
    , ppYamlList   "general_flags:"         (EnumSet.toList $ generalFlags dflags)
    , ppYamlList   "extension_flags:"       (EnumSet.toList $ extensionFlags dflags)
    , ppYamlSingle "platform:"              (targetPlatform dflags)
    ]

  let addToZip dst src =
        [ Option "LoadEntry"
        , Option "--compressionMethod=Zstd"
        , FileOption "--entryPath=" dst
        , FileOption "--sourcePath=" src
        ]
  runSomething logger "finish .modpak" "zip-cmd" $
    [ Option "CreateArchive", FileOption "--zipPath=" modpak_output] ++
    addToZip "module.stgbin" stgbinFile ++
    addToZip "module.fullcore-hi" fullcoreHiFile ++
    addToZip "module.ghcstg" ghcstgFile ++
    addToZip "module.ghccore" ghccoreFile ++
    addToZip "module.cmm" cmm_filename ++
    addToZip "module.s" output_filename ++
    addToZip "module.info" infoFile ++
    (case mSrcPath of
      Nothing -> []
      Just fn -> addToZip "module.hs" fn
    ) ++
    (if has_stub_h
      then addToZip "module_stub.h" (mkStubPaths (initFinderOpts dflags) modName location)
      else []
    ) ++
    (case m_stub_c of
      Nothing   -> []
      Just fn -> addToZip "module_stub.c" fn
    )

writeFullCoreInterface :: HscEnv -> ModGuts -> ModSummary -> FilePath -> IO ()
writeFullCoreInterface hscEnv0 mod_guts mod_summary output_name = do
  let logger  = hsc_logger hscEnv0
      dflags0 = hsc_dflags hscEnv0
      -- HINT: export the whole module core IR
      dflags  = dflags0
                  `gopt_set` Opt_WriteIfSimplifiedCore
      hscEnv = hscEnv0 {hsc_dflags = dflags}
  opts <- initTidyOpts hscEnv
  (cg_guts, details) <- tidyProgram opts mod_guts

  let !partial_iface =
        {-# SCC "GHC.Driver.Main.mkPartialIface" #-}
        -- This `force` saves 2M residency in test T10370
        -- See Note [Avoiding space leaks in toIface*] for details.
        force (mkPartialIface hscEnv (cg_binds cg_guts) details mod_summary mod_guts)

  -- In interpreted mode the regular codeGen backend is not run so we
  -- generate a interface without codeGen info.
  final_iface <- mkFullIface hscEnv partial_iface Nothing Nothing
  writeIface logger (targetProfile dflags) output_name final_iface
