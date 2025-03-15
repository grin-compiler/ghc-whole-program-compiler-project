module WPC.Plugin (plugin) where

import           Control.Applicative         (Applicative (..))
import           Control.Monad               (Functor (..), Monad (..))

import           Data.Bool                   (Bool (..), otherwise)
import           Data.Eq                     (Eq (..))
import           Data.Function               (($), (.))
import           Data.IORef                  (modifyIORef, readIORef)
import           Data.List                   ((++))
import           Data.Maybe                  (Maybe (..), fromMaybe)

import           GHC.Cmm                     (CmmGroup, CmmGroupSRTs, RawCmmGroup)
import           GHC.Cmm.Info                (cmmToRawCmm)
import           GHC.Data.Stream             (Stream)
import qualified GHC.Data.Stream             as Stream
import           GHC.Driver.CodeOutput       (outputForeignStubs)
import           GHC.Driver.Hooks            (Hooks (..))
import           GHC.Driver.Pipeline         as Pipeline (TPhase (..), link, runPhase)
import           GHC.Driver.Pipeline.Execute (runCcPhase)
import           GHC.Driver.Pipeline.Phases  (PhaseHook (..))
import           GHC.Plugins                 (CgGuts (..), CommandLineOption, CoreM, CoreToDo (..), DynFlags (..),
                                              GhcLink, HscEnv (..), IsDoc (..), ModGuts (..), ModLocation (..), Module,
                                              NamePprCtx (..), OutputableP (..), Plugin (..), QualifyName (..),
                                              SuccessFlag, TyCon, alwaysPrintPromTick, blankLine, defaultPlugin,
                                              hsc_units, liftIO, mkDumpStyle, neverQualifyModules, neverQualifyPackages,
                                              objectSuf, showSDoc, targetProfile, withPprStyle)
import           GHC.Stg.Syntax              (CgStgTopBinding)
import qualified GHC.StgToCmm                as StgToCmm (codeGen)
import           GHC.StgToCmm.Config         (StgToCmmConfig)
import           GHC.StgToCmm.Types          (ModuleLFInfos)
import           GHC.Types.CostCentre        (CollectedCCs)
import           GHC.Types.HpcInfo           (HpcInfo)
import           GHC.Types.IPE               (InfoTableProvMap)
import           GHC.Unit.Home.ModInfo       (HomePackageTable)
import           GHC.Unit.Module.Status      (HscBackendAction (..))
import           GHC.Utils.TmpFs             (TempFileLifetime (..), newTempName)

import           Prelude                     (Show (..))

import           System.Directory            (copyFile, createDirectoryIfMissing)
import           System.FilePath             (makeRelative, replaceExtension, takeDirectory, (</>))
import           System.IO                   (Handle, IO, IOMode (..), hClose, hPutStr, openFile, putStrLn)

import           WPC.Foreign                 (dsForeignsFun)
import           WPC.ForeignStubDecls        (ForeignStubDecls (..))
import           WPC.GhcStgApp               (writeGhcStgApp)
import           WPC.GlobalEnv               (GlobalEnv (..), globalEnvIORef)
import           WPC.Modpak                  (outputModPak)
import           WPC.Stubs                   (outputCapiStubs)

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos  = coreToDosFun
  , driverPlugin      = driverFun
  }

coreToDosFun :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
coreToDosFun _cmdOpts todo0 = do
  let captureCore :: ModGuts -> CoreM ModGuts
      captureCore mg = do
        --putMsgS $ "wpc-plugin captureCore pass"
        liftIO $ modifyIORef globalEnvIORef $ \d -> d {geModGuts = Just mg}
        pure mg

      todo = todo0 ++ [CoreDoPluginPass "capture IR" captureCore]

  --putMsgS $ "wpc-plugin coreToDosFun cmdOpts: " ++ show cmdOpts
  --putMsg $ text "wpc-plugin coreToDosFun todo: " <+> vcat (map ppr todo)
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
        T_Unlit{}        -> "T_Unlit"
        T_FileArgs{}     -> "T_FileArgs"
        T_Cpp{}          -> "T_Cpp"
        T_HsPp{}         -> "T_HsPp"
        T_HscRecomp{}    -> "T_HscRecomp"
        T_Hsc{}          -> "T_Hsc"
        T_HscPostTc{}    -> "T_HscPostTc"
        T_HscBackend{}   -> "T_HscBackend"
        T_CmmCpp{}       -> "T_CmmCpp"
        T_Cmm{}          -> "T_Cmm"
        T_Cc{}           -> "T_Cc"
        T_As{}           -> "T_As"
        T_Js{}           -> "T_Js"
        T_ForeignJs{}    -> "T_ForeignJs"
        T_LlvmOpt{}      -> "T_LlvmOpt"
        T_LlvmLlc{}      -> "T_LlvmLlc"
        T_LlvmAs{}       -> "T_LlvmAs"
        T_LlvmMangle{}   -> "T_LlvmMangle"
        T_MergeForeign{} -> "T_MergeForeign"

  putStrLn $ " ###### wpc-plugin runPhaseFun phase: " ++ phaseStr
  --undefined

  case phase of
    T_Cc phase' pipe_env hsc_env location input_fn -> do
      output_fn <- runCcPhase phase' pipe_env hsc_env location input_fn
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
        let cmmDoc = vcat $ fmap (\i -> pdoc platform i $$ blankLine) a
        hPutStr cmmHandle . showSDoc dflags $ withPprStyle dumpStyle cmmDoc
        pure a
  pure $ Stream.mapM dump rawcmms0

linkFun :: GhcLink -> DynFlags -> Bool -> HomePackageTable -> IO SuccessFlag
linkFun ghcLink dflags isBatchMode hpt = do
  putStrLn " ###### linkFun"
  GlobalEnv{..} <- readIORef globalEnvIORef
  let Just HscEnv{..} = geHscEnv
      hooks = hsc_hooks {linkHook = Nothing}
  result <- Pipeline.link ghcLink hsc_logger hsc_tmpfs hsc_FC hooks dflags hsc_unit_env isBatchMode Nothing hpt
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
