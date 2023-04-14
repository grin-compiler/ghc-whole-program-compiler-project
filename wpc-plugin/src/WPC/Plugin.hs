{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module WPC.Plugin (plugin) where

import System.Directory
import System.FilePath
import System.IO
import Data.IORef
import Data.Maybe

import GHC.Plugins
import GHC.Driver.Hooks
import GHC.Driver.Pipeline as Pipeline
import GHC.Driver.Pipeline.Phases
import GHC.Driver.Pipeline.Execute
import GHC.Unit.Module.Status
import qualified GHC.StgToCmm as StgToCmm ( codeGen )
import GHC.Driver.Config.StgToCmm (initStgToCmmConfig)
import GHC.StgToCmm.Config
import GHC.Types.IPE
import GHC.Stg.Syntax
import GHC.StgToCmm.Types (ModuleLFInfos)
import GHC.Types.CostCentre
import GHC.Types.HpcInfo
import qualified GHC.Data.Stream as Stream
import GHC.Data.Stream (Stream)
import GHC.Cmm
import GHC.Cmm.Info
import GHC.Utils.TmpFs
import GHC.Utils.Misc
import GHC.Unit.Home.ModInfo
import GHC.Driver.CodeOutput
import Language.Haskell.Syntax.Decls
import GHC.Types.ForeignCall

import Control.Monad
import qualified Data.Map as Map

import WPC.Modpak
import WPC.GhcStgApp
import WPC.Foreign
import WPC.Stubs
import WPC.GlobalEnv
import WPC.ForeignStubDecls

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos  = coreToDosFun
  , driverPlugin      = driverFun
  }

coreToDosFun :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreToDosFun cmdOpts todo0 = do
  let captureCore :: ModGuts -> CoreM ModGuts
      captureCore mg = do
        putMsgS $ "wpc-plugin captureCore pass"
        liftIO $ modifyIORef globalEnvIORef $ \d -> d {geModGuts = Just mg}
        pure mg

      todo = todo0 ++ [CoreDoPluginPass "capture IR" captureCore]

  putMsgS $ "wpc-plugin coreToDosFun cmdOpts: " ++ show cmdOpts
  putMsg $ text "wpc-plugin coreToDosFun todo: " <+> vcat (map ppr todo)
  return todo

driverFun :: [CommandLineOption] -> HscEnv -> IO HscEnv
driverFun cmdOpts hscEnv = do
  putStrLn $ " ###### wpc-plugin driverFun cmdOpts: " ++ show cmdOpts
  let hooks = (hsc_hooks hscEnv)
                { runPhaseHook    = Just (PhaseHook runPhaseFun)
                , linkHook        = Just linkFun
                , dsForeignsHook  = Just dsForeignsFun
                }
  pure $ hscEnv {hsc_hooks = hooks}

{-
type instance DsForeignsHook = [LForeignDecl GhcTc] -> DsM (ForeignStubs, ForeignStubDecls, OrdList (Id, CoreExpr))

data Hooks
  = Hooks
  { dsForeignsHook         :: !(Maybe DsForeignsHook)
  , tcForeignImportsHook   :: !(Maybe ([LForeignDecl GhcRn] -> TcM ([Id], [LForeignDecl GhcTc], Bag GlobalRdrElt)))
  , tcForeignExportsHook   :: !(Maybe ([LForeignDecl GhcRn] -> TcM (LHsBinds GhcTc, [LForeignDecl GhcTc], Bag GlobalRdrElt)))
  , hscFrontendHook        :: !(Maybe (ModSummary -> Hsc FrontendResult))
  , hscCompileCoreExprHook :: !(Maybe (HscEnv -> SrcSpan -> CoreExpr -> IO (ForeignHValue, [Linkable], PkgsLoaded)))
  , ghcPrimIfaceHook       :: !(Maybe ModIface)
  , runPhaseHook           :: !(Maybe PhaseHook)
  , runMetaHook            :: !(Maybe (MetaHook TcM))
  , linkHook               :: !(Maybe (GhcLink -> DynFlags -> Bool -> HomePackageTable -> IO SuccessFlag))
  , runRnSpliceHook        :: !(Maybe (HsUntypedSplice GhcRn -> RnM (HsUntypedSplice GhcRn)))
  , getValueSafelyHook     :: !(Maybe (HscEnv -> Name -> Type -> IO (Either Type (HValue, [Linkable], PkgsLoaded))))
  , createIservProcessHook :: !(Maybe (CreateProcess -> IO ProcessHandle))
  , stgToCmmHook           :: !(Maybe (StgToCmmConfig -> InfoTableProvMap -> [TyCon] -> CollectedCCs -> [CgStgTopBinding] -> HpcInfo -> Stream IO CmmGroup ModuleLFInfos))
  , cmmToRawCmmHook        :: !(forall a . Maybe (DynFlags -> Maybe Module -> Stream IO CmmGroupSRTs a -> IO (Stream IO RawCmmGroup a)))
  }
-}
{-
data TPhase res where
  T_Unlit :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_FileArgs :: HscEnv -> FilePath -> TPhase (DynFlags, Messages PsMessage, [Warn])
  T_Cpp   :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_HsPp  :: PipeEnv -> HscEnv -> FilePath -> FilePath -> TPhase FilePath
  T_HscRecomp :: PipeEnv -> HscEnv -> FilePath -> HscSource -> TPhase (HscEnv, ModSummary, HscRecompStatus)
  T_Hsc :: HscEnv -> ModSummary -> TPhase (FrontendResult, Messages GhcMessage)
  T_HscPostTc :: HscEnv -> ModSummary
              -> FrontendResult
              -> Messages GhcMessage
              -> Maybe Fingerprint
              -> TPhase HscBackendAction
  T_HscBackend :: PipeEnv -> HscEnv -> ModuleName -> HscSource -> ModLocation -> HscBackendAction -> TPhase ([(String, FilePath)], [FilePath], ModIface, Maybe Linkable, FilePath)
  T_CmmCpp :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_Cmm :: PipeEnv -> HscEnv -> FilePath -> TPhase ([FilePath], FilePath)
  T_Cc :: Phase -> PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_As :: Bool -> PipeEnv -> HscEnv -> Maybe ModLocation -> FilePath -> TPhase FilePath
  T_LlvmOpt :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_LlvmLlc :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_LlvmMangle :: PipeEnv -> HscEnv -> FilePath -> TPhase FilePath
  T_MergeForeign :: PipeEnv -> HscEnv -> FilePath -> [(String, FilePath)] -> [FilePath] -> TPhase FilePath
-}

runPhaseFun :: forall a . TPhase a -> IO a
runPhaseFun phase = do
  let phaseStr = case phase of
        T_Unlit{}         -> "T_Unlit"
        T_FileArgs{}      -> "T_FileArgs"
        T_Cpp{}           -> "T_Cpp"
        T_HsPp{}          -> "T_HsPp"
        T_HscRecomp{}     -> "T_HscRecomp"
        T_Hsc{}           -> "T_Hsc"
        T_HscPostTc{}     -> "T_HscPostTc"
        T_HscBackend{}    -> "T_HscBackend"
        T_CmmCpp{}        -> "T_CmmCpp"
        T_Cmm{}           -> "T_Cmm"
        T_Cc{}            -> "T_Cc"
        T_As{}            -> "T_As"
        T_LlvmOpt{}       -> "T_LlvmOpt"
        T_LlvmLlc{}       -> "T_LlvmLlc"
        T_LlvmMangle{}    -> "T_LlvmMangle"
        T_MergeForeign{}  -> "T_MergeForeign"

  putStrLn $ " ###### wpc-plugin runPhaseFun phase: " ++ phaseStr
  --undefined

  case phase of
    T_Cc phase pipe_env hsc_env location input_fn -> do
      output_fn <- runCcPhase phase pipe_env hsc_env location input_fn
      putStrLn $ " ###### wpc-plugin runPhaseFun T_Cc input_fn: " ++ input_fn ++ " output_fn: " ++ output_fn
      let dflags        = hsc_dflags hsc_env
          odir          = fromMaybe "." (objectDir dflags)
          wpcForeignObj = odir </> "extra-compilation-artifacts" </> "wpc-plugin" </> "cbits" </> makeRelative odir output_fn
      createDirectoryIfMissing True (takeDirectory wpcForeignObj)
      copyFile output_fn wpcForeignObj
      {-
        TODO:
          make output_fn relative to odir
          copy output object file to extra-compilation-artifacts/wpc-plugin/cbits/
      -}
      -- save cbits source as well
      do
        let wpcForeignSrc = odir </> "extra-compilation-artifacts" </> "wpc-plugin" </> "cbits-source" </> input_fn
        createDirectoryIfMissing True (takeDirectory wpcForeignSrc)
        copyFile input_fn wpcForeignSrc
        putStrLn $ " ###### wpc-plugin runPhaseFun copy file (from, to) " ++ show (input_fn, wpcForeignSrc)
      pure output_fn
    T_Hsc _hscEnv modSummary -> do
      modifyIORef globalEnvIORef $ \d -> d {geModSummary = Just modSummary}
      runPhase phase

    T_HscBackend pipeEnv hscEnv modName hscSource modLocation action@HscRecomp{..} -> do
      let dflags        = hsc_dflags hscEnv
          modpak_output = replaceExtension (ml_hi_file modLocation) (objectSuf dflags ++ "_modpak")
          tmpfs         = hsc_tmpfs hscEnv
          logger        = hsc_logger hscEnv
      putStrLn $ " ###### wpc-plugin runPhaseFun modpak_output: " ++ modpak_output
      cmmFile <- newTempName logger tmpfs (tmpDir dflags) TFL_CurrentModule (objectSuf dflags ++ "_cmm")
      cmmHandle <- openFile cmmFile WriteMode
      -- HINT: setup stgToCmmHook to capture stg binds
      let hooks           = (hsc_hooks hscEnv)
                              { stgToCmmHook    = Just (stgToCmmFun hscEnvWithHooks)
                              , cmmToRawCmmHook = Just (cmmToRawCmmFun cmmHandle hscEnv)
                              }
          hscEnvWithHooks = hscEnv {hsc_hooks = hooks}
      result@(_, _, _, outputFileName) <- runPhase (T_HscBackend pipeEnv hscEnvWithHooks modName hscSource modLocation action)
      hClose cmmHandle
      GlobalEnv{..} <- readIORef globalEnvIORef
      --writeIORef globalEnvIORef (emptyModpakData {geHscEnv = Just hscEnv})
      modifyIORef globalEnvIORef $ \d -> d {geHscEnv = Just hscEnv}
      let CgGuts{..}            = hscs_guts
          Just (mg@ModGuts{..}) = geModGuts
          Just ms               = geModSummary
          Just stgBinds         = geStgBinds
          Just stubDecls        = geStubDecls

      ----------------
      -- handle stubs
      ----------------
      -- TODO: save stubs to extra-compilation-artifacts/wpc-plugin/capi-stubs
      -- TODO: save stubs to extra-compilation-artifacts/wpc-plugin/all-stubs
      -- gen capi stubs
      outputCapiStubs hscEnv cg_module modLocation stubDecls
      --compileWrapperStubs hscEnv modName wrapperList
      -- gen all stubs for modpak
      (has_h_allStub, c_allStub) <- outputForeignStubs logger tmpfs dflags (hsc_units hscEnv) cg_module modLocation cg_foreign

      --------------------------

      outputModPak hscEnv cg_module mg_binds stgBinds cg_foreign (ForeignStubDecls stubDecls) modLocation outputFileName cmmFile has_h_allStub c_allStub (Just (mg, ms))
      pure result

    _ -> runPhase phase

stgToCmmFun :: HscEnv -> StgToCmmConfig -> InfoTableProvMap -> [TyCon] -> CollectedCCs -> [CgStgTopBinding] -> HpcInfo -> Stream IO CmmGroup ModuleLFInfos
stgToCmmFun hscEnv cfg itpm tcList ccc stgBinds hpcInfo = do
  liftIO $ do
    putStrLn $ " ###### run stgToCmmFun"
    modifyIORef globalEnvIORef $ \d -> d {geStgBinds = Just stgBinds}
  StgToCmm.codeGen (hsc_logger hscEnv) (hsc_tmpfs hscEnv) cfg itpm tcList ccc stgBinds hpcInfo

cmmToRawCmmFun :: Handle -> HscEnv -> DynFlags -> Maybe Module -> Stream IO CmmGroupSRTs a -> IO (Stream IO RawCmmGroup a)
cmmToRawCmmFun cmmHandle hscEnv dflags mMod cmms = do
  let logger    = hsc_logger hscEnv
      profile   = targetProfile dflags
      platform  = targetPlatform dflags
  rawcmms0 <- cmmToRawCmm logger profile cmms

  -- name pretty printer setup
  let qualifyImportedNames mod _
        | Just mod == mMod  = NameUnqual
        | otherwise         = NameNotInScope1
      print_unqual = QueryQualify qualifyImportedNames
                                  neverQualifyModules
                                  neverQualifyPackages
                                  alwaysPrintPromTick
      dumpStyle = mkDumpStyle print_unqual

  let dump a = do
        let cmmDoc = vcat $ map (\i -> pdoc platform i $$ blankLine) a
        hPutStr cmmHandle . showSDoc dflags $ withPprStyle dumpStyle cmmDoc
        pure a
  pure $ Stream.mapM dump rawcmms0

linkFun :: GhcLink -> DynFlags -> Bool -> HomePackageTable -> IO SuccessFlag
linkFun ghcLink dflags isBatchMode hpt = do
  putStrLn " ###### linkFun"
  GlobalEnv{..} <- readIORef globalEnvIORef
  let Just HscEnv{..} = geHscEnv
      hooks = hsc_hooks {linkHook = Nothing}
  result <- Pipeline.link ghcLink hsc_logger hsc_tmpfs hooks dflags hsc_unit_env isBatchMode Nothing hpt
  {-
    IDEA: generate ghcstgapp file along with modpak file for the main module
            do not use the link hook
            this will make the plugin work for 'ghc -c' + 'ghc Main.o -o ExeName' use cases
          OR
            call the link hook in 'ghc Main.o -o ExeName' mode
            This seems to be a better idea!!!
  -}
  {-
    hsc_HPT :: HscEnv -> HomePackageTable
    hsc_HPT = ue_hpt . hsc_unit_env
  -}
  writeGhcStgApp dflags hsc_unit_env hpt
  pure result
