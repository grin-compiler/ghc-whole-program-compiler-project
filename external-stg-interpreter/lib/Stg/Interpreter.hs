{-# LANGUAGE RecordWildCards, LambdaCase #-}
module Stg.Interpreter where

import GHC.Stack
import Control.Carrier.NonDet.Church
import Control.Carrier.State.Strict
import Control.Carrier.Lift
import Control.Exception

import Data.List (partition, isSuffixOf)
import Control.Concurrent.MVar
import qualified Data.Map as Map

import System.Posix.DynamicLinker
import Codec.Archive.Zip

import System.FilePath
import System.Directory

import Stg.Syntax
import Stg.Program
import Stg.Analysis.LiveVariable
import Stg.Foreign.Linker

import Stg.Interpreter.Base
import Stg.Interpreter.EvalStg
import qualified Stg.Interpreter.ForeignCall.Native as FCallNative

------------------
-- init / finish
------------------

--declareTopBindings :: (HasCallStack, I sig m) => [Module] -> m ()
declareTopBindings :: (HasCallStack) => [Module] -> C ()
declareTopBindings mods = do
  let (strings, closures) = partition isStringLit $ (concatMap moduleTopBindings) mods
      isStringLit = \case
        StgTopStringLit{} -> True
        _                 -> False
  -- bind string lits
  stringEnv <- forM strings $ \(StgTopStringLit b str) -> do
    strPtr <- getCStringConstantPtrAtom str
    pure (Id b, (SO_TopLevel, strPtr))

  -- bind closures
  let bindings = concatMap getBindings closures
      getBindings = \case
        StgTopLifted (StgNonRec i rhs) -> [(i, rhs)]
        StgTopLifted (StgRec l) -> l

  (closureEnv, rhsList) <- fmap unzip . forM bindings $ \(b, rhs) -> do
    addr <- freshHeapAddress
    atomAddr <- storeNewAtom $ HeapPtr addr
    pure ((Id b, (SO_TopLevel, atomAddr)), (b, addr, rhs))

  -- set the top level binder env
  modify $ \s@StgState{..} -> s {ssStaticGlobalEnv = Map.fromList $ stringEnv ++ closureEnv}

  -- HINT: top level closures does not capture local variables
  forM_ rhsList $ \(b, addr, rhs) -> storeRhs False mempty b addr rhs

-------------------------

-------------------------

--flushStdHandles :: I sig m => m ()
flushStdHandles :: C ()
flushStdHandles = do
  RtsBaseInterop{..} <- gets ssRtsBaseInterop
  evalOnNewThread $ do
    stackPush $ Apply [] -- HINT: force IO monad result to WHNF
    voidAddr <- storeNewAtom Void
    stackPush $ Apply [voidAddr]
    pure [rtsTopHandlerFlushStdHandles]
{-
  (tid, ts) <- createThread
  insertThread tid ts
  scheduleToTheEnd tid
  switchToThread tid

  -- force result to WHNF
  --stackPush $ Apply []
  resultLazy <- builtinStackMachineApply rtsTopHandlerFlushStdHandles [Void]
  case resultLazy of
    []            -> pure resultLazy
    [valueThunk]  -> builtinStgEval valueThunk -- pure resultLazy -- builtinStackMachineApply valueThunk []
-}
  pure ()

loadCbitsSO :: Bool -> FilePath -> IO DL
loadCbitsSO isQuiet progFilePath = do
  workDir <- getExtStgWorkDirectory progFilePath
  createDirectoryIfMissing True workDir
  let soName = workDir </> "cbits.so"
  doesFileExist soName >>= \case
    True  -> pure ()
    False -> case takeExtension progFilePath of
      ".fullpak" -> do
        unless isQuiet $ putStrLn "unpacking cbits.so"
        withArchive progFilePath $ do
          s <- mkEntrySelector "cbits/cbits.so"
          saveEntry s soName
      _ -> do
        unless isQuiet $ putStrLn "linking cbits.so"
        linkForeignCbitsSharedLib progFilePath
  dlopen soName [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]
  --dlmopen LM_ID_BASE soName [{-RTLD_NOW-}RTLD_LAZY, RTLD_LOCAL]
  --dlmopen LM_ID_NEWLM "./libHSbase-4.14.0.0.cbits.so" [RTLD_NOW, RTLD_LOCAL]

------------
-- driver
------------

loadAndRunProgram :: HasCallStack => Bool -> String -> [String] -> IO ()
loadAndRunProgram switchCWD fullpak_name progArgs = do

  mods0 <- case takeExtension fullpak_name of
    ".fullpak"                          -> getFullpakModules fullpak_name
    ".json"                             -> getJSONModules fullpak_name
    ext | isSuffixOf "_ghc_stgapp" ext  -> getGhcStgAppModules fullpak_name
    _                                   -> error "unknown input file format"
  runProgram switchCWD fullpak_name mods0 progArgs

runProgram :: HasCallStack => Bool -> String -> [Module] -> [String] -> IO ()
runProgram switchCWD progFilePath mods0 progArgs = do
  let mods      = map annotateWithLiveVariables $ extStgRtsSupportModule : mods0 -- NOTE: add RTS support module
      progName  = dropExtension progFilePath

  currentDir <- getCurrentDirectory
  stgappDir <- makeAbsolute $ takeDirectory progFilePath
  --putStrLn $ "progName: " ++ show progName ++ " progArgs: " ++ show progArgs
  let runStgMain = do
        declareTopBindings mods
        initRtsSupport progName progArgs mods
        env <- gets ssStaticGlobalEnv
        let rootMain = unId $ case [i | i <- Map.keys env, show i == "main_:Main.main"] of
              [mainId]  -> mainId
              []        -> error "main_:Main.main not found"
              _         -> error "multiple main_:Main.main have found"
        --limit <- gets $ ssNextHeapAddr . ssAllocator
        --modify $ \s@StgState{..} -> s {ssHeapStartAddress = limit}
        --modify $ \s@StgState{..} -> s {ssStgErrorAction = Printable $ Debugger.processCommandsUntilExit}

        -- TODO: check how it is done in the native RTS: call hs_main
        mainAtom <- lookupEnv mempty rootMain

        evalOnMainThread $ do
          voidAddr <- storeNewAtom Void
          stackPush $ Apply [voidAddr]
          pure [mainAtom]

        {-
        Capability *cap = rts_lock();
        rts_evalLazyIO(&cap, main_closure, NULL);
        rts_unlock(cap);
        -}
        flushStdHandles
        --showDebug evalOnNewThread
        -- TODO: do everything that 'hs_exit_' does

        --exportCallGraph
        {-
        -- HINT: start debugger REPL in debug mode
        when (dbgState == DbgStepByStep) $ do
          Debugger.processCommandsUntilExit
        -}

  stateStore <- FCallNative.PrintableMVar <$> newEmptyMVar
  dl <- loadCbitsSO False progFilePath
  let freeResources = do
        dlclose dl
        --killThread gcThreadId
  flip catch (\e -> do {freeResources; throw (e :: SomeException)}) $ do
    when switchCWD $ setCurrentDirectory stgappDir
    s@StgState{..} <- runM . evalGlobal (emptyGlobalState ConcreteEval) . FCallNative.evalFFI (FCallNative.FFIState dl stateStore) . fmap head . runNonDetM (:[]) . execState emptyStgState $ runStgMain
    when switchCWD $ setCurrentDirectory currentDir
    freeResources

    -- TODO: handle :Main.main properly ; currenlty it is in conflict with Main.main
