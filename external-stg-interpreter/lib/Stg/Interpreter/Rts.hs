
module Stg.Interpreter.Rts (initRtsSupport, extStgRtsSupportModule, globalStoreSymbols) where

import           Control.Monad         (Functor (..), forM_)
import           Control.Monad.State   (MonadIO (..), gets, modify')

import           Data.Bool             (Bool (..))
import           Data.Eq               (Eq (..))
import           Data.Function         (($))
import           Data.Int              (Int)
import           Data.List             (concatMap, (++))
import qualified Data.Map              as Map
import           Data.Maybe            (Maybe (..))
import           Data.Monoid           (Monoid (..))
import qualified Data.Set              as Set
import           Data.String           (String)
import           Data.Tuple            (fst)

import           Foreign.Marshal.Utils (new)

import           GHC.Err               (error, undefined)

import           Stg.Interpreter.Base  (Atom, M, Rts (..), StgState (..), lookupEnv, promptM_)
import           Stg.Reconstruct       (reconModule)
import           Stg.Syntax            (Alt' (..), AltCon' (..), AltType' (..), Arg' (..), Binder (..), BinderId (..),
                                        Binding' (..), DataCon (..), DataConId (..), DataConRep (..), Expr' (..),
                                        ForeignStubs' (..), IdDetails (..), Module, Module' (..), ModuleName (..), Name,
                                        PrimRep (..), Rhs' (..), SBinder (..), SDataCon (..), STyCon (..), Scope (..),
                                        SrcSpan (..), TopBinding' (..), TyCon (..), TyConId (..), Type (..),
                                        UnhelpfulSpanReason (..), Unique (..), UnitId (..), UpdateFlag (..))

import           System.IO             (print, putStrLn)

import           Text.Show             (Show (..))

emptyRts :: String -> [String] -> Rts
emptyRts progName progArgs = Rts
  { rtsGlobalStore = Map.empty
  , rtsProgName    = progName
  , rtsProgArgs    = progArgs
  , rtsCharCon     = undefined
  , rtsIntCon   = undefined
  , rtsInt8Con  = undefined
  , rtsInt16Con = undefined
  , rtsInt32Con = undefined
  , rtsInt64Con = undefined
  , rtsWordCon   = undefined
  , rtsWord8Con  = undefined
  , rtsWord16Con = undefined
  , rtsWord32Con = undefined
  , rtsWord64Con = undefined
  , rtsPtrCon       = undefined
  , rtsFunPtrCon    = undefined
  , rtsFloatCon     = undefined
  , rtsDoubleCon    = undefined
  , rtsStablePtrCon = undefined
  , rtsTrueCon  = undefined
  , rtsFalseCon = undefined
  , rtsUnpackCString             = undefined
  , rtsTopHandlerRunIO           = undefined
  , rtsTopHandlerRunNonIO        = undefined
  , rtsTopHandlerFlushStdHandles = undefined
  , rtsDivZeroException   = undefined
  , rtsUnderflowException = undefined
  , rtsOverflowException  = undefined
  , rtsNestedAtomically          = undefined
  , rtsBlockedIndefinitelyOnMVar = undefined
  , rtsBlockedIndefinitelyOnSTM  = undefined
  , rtsNonTermination = undefined
  , rtsApplyFun1Arg   = undefined
  , rtsTuple2Proj0    = undefined
  , rtsDataSymbol_enabled_capabilities = undefined
  }

initRtsCDataSymbols :: M ()
initRtsCDataSymbols = do
  sym_enabled_capabilities <- liftIO $ new 2
  rts0 <- gets ssRtsSupport
  let rts1 = rts0
        { rtsDataSymbol_enabled_capabilities = sym_enabled_capabilities
        }
  modify' $ \s -> s {ssRtsSupport = rts1}

initRtsSupport :: String -> [String] -> [Module] -> M ()
initRtsSupport progName progArgs mods = do

  -- create empty Rts data con, it is filled gradually
  modify' $ \s -> s {ssRtsSupport = emptyRts progName progArgs}
  initRtsCDataSymbols

  -- collect rts related modules
  let rtsModSet = Set.fromList $
                    [(UnitId u, ModuleName m) | (u, m, _, _, _) <- wiredInCons] ++
                    [(UnitId u, ModuleName m) | (u, m, _, _) <- wiredInClosures]
      rtsMods = [m | m@Module{..} <- mods, Set.member (moduleUnitId, moduleName) rtsModSet]

  -- lookup wired-in constructors
  let dcMap = Map.fromList
                [ ((moduleUnitId, moduleName, tcName, dcName), dc)
                | Module{..} <- rtsMods
                , (tcU, tcMs) <- moduleTyCons
                , tcU == moduleUnitId
                , (tcM, tcs) <- tcMs
                , tcM == moduleName
                , TyCon{..} <- tcs
                , dc@DataCon{..} <- tcDataCons
                ]

  forM_ wiredInCons $ \(u, m, t, d, setter) -> do
    case Map.lookup (UnitId u, ModuleName m, t, d) dcMap of
        Nothing -> error $ "missing wired in data con: " ++ show (u, m, t, d)
        Just dc -> modify' $ \s@StgState{..} -> s {ssRtsSupport = setter ssRtsSupport dc}

  -- lookup wired-in closures
  let getBindings = \case
        StgTopLifted (StgNonRec i _) -> [i]
        StgTopLifted (StgRec l) -> fmap fst l
        _ -> []
      closureMap = Map.fromList
                [ ((uId, mName, bName), topBinding)
                | Module{..} <- rtsMods
                , topBinding@Binder{..} <- concatMap getBindings moduleTopBindings
                , (uId, mName, bName, _) <- wiredInClosures
                , UnitId uId == moduleUnitId
                , ModuleName mName == moduleName
                , bName == binderName
                ]

  promptM_ $ do
    forM_ mods $ \Module{..} -> do
      liftIO $ print (moduleUnitId, moduleName)

  forM_ wiredInClosures $ \(u, m, n, setter) -> do
    case Map.lookup (u, m, n) closureMap of
        Nothing -> liftIO $ putStrLn $ "missing wired in closure: " ++ show (u, m, n)-- ++ "\n" ++ unlines (map show $ Map.keys closureMap)
        Just b  -> do
          cl <- lookupEnv mempty b
          modify' $ \s@StgState{..} -> s {ssRtsSupport = setter ssRtsSupport cl}

globalStoreSymbols :: Set.Set Name
globalStoreSymbols = Set.fromList
  [ "getOrSetGHCConcSignalSignalHandlerStore"
  , "getOrSetGHCConcWindowsPendingDelaysStore"
  , "getOrSetGHCConcWindowsIOManagerThreadStore"
  , "getOrSetGHCConcWindowsProddingStore"
  , "getOrSetSystemEventThreadEventManagerStore"
  , "getOrSetSystemEventThreadIOManagerThreadStore"
  , "getOrSetSystemTimerThreadEventManagerStore"
  , "getOrSetSystemTimerThreadIOManagerThreadStore"
  , "getOrSetLibHSghcFastStringTable"
  , "getOrSetLibHSghcPersistentLinkerState"
  , "getOrSetLibHSghcInitLinkerDone"
  , "getOrSetLibHSghcGlobalDynFlags"
  , "getOrSetLibHSghcStaticOptions"
  , "getOrSetLibHSghcStaticOptionsReady"
  ]

-- HINT: needed for FFI value boxing
wiredInCons :: [(Name, Name, Name, Name, Rts -> DataCon -> Rts)]
wiredInCons =
  -- unit-id,     module,       type con,     data con
  [ ("ghc-prim",  "GHC.Types",  "Char",       "C#",         \s dc -> s {rtsCharCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Int",        "I#",         \s dc -> s {rtsIntCon       = dc})
  , ("ghc-prim",  "GHC.Types",  "Word",       "W#",         \s dc -> s {rtsWordCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Float",      "F#",         \s dc -> s {rtsFloatCon     = dc})
  , ("ghc-prim",  "GHC.Types",  "Double",     "D#",         \s dc -> s {rtsDoubleCon    = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "True",       \s dc -> s {rtsTrueCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "False",      \s dc -> s {rtsFalseCon     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int8",       "I8#",        \s dc -> s {rtsInt8Con      = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int16",      "I16#",       \s dc -> s {rtsInt16Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int32",      "I32#",       \s dc -> s {rtsInt32Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Int",    "Int64",      "I64#",       \s dc -> s {rtsInt64Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word8",      "W8#",        \s dc -> s {rtsWord8Con     = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word16",     "W16#",       \s dc -> s {rtsWord16Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word32",     "W32#",       \s dc -> s {rtsWord32Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Word",   "Word64",     "W64#",       \s dc -> s {rtsWord64Con    = dc})
  , ("ghc-internal",      "GHC.Internal.Ptr",    "Ptr",        "Ptr",        \s dc -> s {rtsPtrCon       = dc})
  , ("ghc-internal",      "GHC.Internal.Ptr",    "FunPtr",     "FunPtr",     \s dc -> s {rtsFunPtrCon    = dc})
  , ("ghc-internal",      "GHC.Internal.Stable", "StablePtr",  "StablePtr",  \s dc -> s {rtsStablePtrCon = dc})

  -- validation for extStgRtsSupportModule
  , ("ghc-prim",  "GHC.Tuple",  "Tuple2",        "(,)",        \s _dc -> s)
  ]
{-
         "-Wl,-u,ghczmprim_GHCziTuple_Z0T_closure"
-}

{-
  TODO:
    bind wired in closures when allocating static top level closures
-}
wiredInClosures :: [(Name, Name, Name, Rts -> Atom -> Rts)]
wiredInClosures =
  -- unit-id,     module,                   binder,                         closure setter
  [ ("ghc-internal",      "GHC.Internal.TopHandler",         "runIO",                        \s cl -> s {rtsTopHandlerRunIO            = cl})
  , ("ghc-internal",      "GHC.Internal.TopHandler",         "runNonIO",                     \s cl -> s {rtsTopHandlerRunNonIO         = cl})
  , ("ghc-internal",      "GHC.Internal.TopHandler",         "flushStdHandles",              \s cl -> s {rtsTopHandlerFlushStdHandles  = cl})
  , ("ghc-internal",      "GHC.Internal.Pack",               "unpackCString",                \s cl -> s {rtsUnpackCString              = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "divZeroException",             \s cl -> s {rtsDivZeroException           = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "underflowException",           \s cl -> s {rtsUnderflowException         = cl})
  , ("ghc-internal",      "GHC.Internal.Exception.Type",     "overflowException",            \s cl -> s {rtsOverflowException          = cl})
  , (":ext-stg",  ":ExtStg.RTS.Support",    "applyFun1Arg",                 \s cl -> s {rtsApplyFun1Arg               = cl})
  , (":ext-stg",  ":ExtStg.RTS.Support",    "tuple2Proj0",                  \s cl -> s {rtsTuple2Proj0                = cl})
  , ("ghc-internal",      "GHC.Internal.Control.Exception.Base", "nestedAtomically",             \s cl -> s {rtsNestedAtomically           = cl})
  , ("ghc-internal",      "GHC.Internal.Control.Exception.Base", "nonTermination",               \s cl -> s {rtsNonTermination             = cl})
  , ("ghc-internal",      "GHC.Internal.IO.Exception",       "blockedIndefinitelyOnMVar",    \s cl -> s {rtsBlockedIndefinitelyOnMVar  = cl})
  , ("ghc-internal",      "GHC.Internal.IO.Exception",       "blockedIndefinitelyOnSTM",     \s cl -> s {rtsBlockedIndefinitelyOnSTM   = cl})
  ]

{-
  , ("base",      "GHC.Weak",               "runFinalizerBatch",
  , ("base",      "GHC.IO.Exception",       "stackOverflow",
  , ("base",      "GHC.IO.Exception",       "heapOverflow",
  , ("base",      "GHC.IO.Exception",       "allocationLimitExceeded",
  , ("base",      "GHC.IO.Exception",       "cannotCompactFunction",
  , ("base",      "GHC.IO.Exception",       "cannotCompactPinned",
  , ("base",      "GHC.IO.Exception",       "cannotCompactMutable",
  , ("base",      "Control.Exception.Base", "absentSumFieldError",
  , ("base",      "GHC.Event.Thread",       "blockedOnBadFD",
  , ("base",      "GHC.Conc.Sync",          "runSparks",
  , ("base",      "GHC.Conc.IO",            "ensureIOManagerIsRunning",
  , ("base",      "GHC.Conc.IO",            "ioManagerCapabilitiesChanged",
  , ("base",      "GHC.Conc.Signal",        "runHandlersPtr",
  , ("base",      "GHC.TopHandler",         "runMainIO",
-}

{-
#if !defined(mingw32_HOST_OS)
    getStablePtr((StgPtr)blockedOnBadFD_closure);
    getStablePtr((StgPtr)runHandlersPtr_closure);
#endif
-}

extStgRtsSupportModule :: Module
extStgRtsSupportModule = reconModule $ Module
  { modulePhase               = "ext-stg interpreter"
  , moduleUnitId              = UnitId ":ext-stg"
  , moduleName                = ModuleName ":ExtStg.RTS.Support"
  , moduleSourceFilePath      = Nothing
  , moduleForeignStubs        = NoStubs
  , moduleHasForeignExported  = False
  , moduleDependency          = [(UnitId "ghc-prim", [ModuleName "GHC.Tuple"])]
  , moduleExternalTopIds      = []
  , moduleTyCons              = [(UnitId "ghc-prim", [(ModuleName "GHC.Tuple", [tup2STyCon])])]
  , moduleTopBindings         = [tuple2Proj0, applyFun1Arg]
  } where
      u :: Int -> Unique
      u = Unique '+'

      sbinder :: Int -> Name -> Scope -> SBinder
      sbinder i n s = SBinder
                      { sbinderName     = n
                      , sbinderId       = BinderId $ u i
                      , sbinderType     = SingleValue LiftedRep
                      , sbinderTypeSig  = mempty
                      , sbinderScope    = s
                      , sbinderDetails  = VanillaId
                      , sbinderInfo     = mempty
                      , sbinderDefLoc   = UnhelpfulSpan $ UnhelpfulOther "ext-stg-interpreter-rts"
                      }

      localLiftedVanillaId :: Int -> Name -> (BinderId, SBinder)
      localLiftedVanillaId i n = (BinderId $ u i, sbinder i n ClosurePrivate)

      exportedLiftedVanillaId :: Int -> Name -> SBinder
      exportedLiftedVanillaId i n = sbinder i n ModulePublic

      tup2DCOcc       = DataConId $ u 0
      tup2SDataCon    = SDataCon
                        { sdcName   = "(,)"
                        , sdcId     = tup2DCOcc
                        , sdcRep    = AlgDataCon [LiftedRep, LiftedRep]
                        , sdcWorker = exportedLiftedVanillaId 666 "fake ext-stg Tup2 worker"
                        , sdcDefLoc = UnhelpfulSpan $ UnhelpfulOther "ext-stg-interpreter-rts"
                        }
      tup2TCOcc       = TyConId $ u 1
      tup2STyCon      = STyCon
                        { stcName     = "(,)"
                        , stcId       = tup2TCOcc
                        , stcDataCons = [tup2SDataCon]
                        , stcDefLoc   = UnhelpfulSpan $ UnhelpfulOther "ext-stg-interpreter-rts"
                        }

      -- code for tuple2Proj0 = \t -> case t of GHC.Tuple.(,) a b -> a
      (aOcc, aBnd)    = localLiftedVanillaId 100 "a"
      (_,    bBnd)    = localLiftedVanillaId 101 "b"
      (tOcc, tBnd)    = localLiftedVanillaId 102 "t"
      (_,    rBnd)    = localLiftedVanillaId 103 "r"

      tuple2Proj0Bnd  = exportedLiftedVanillaId 104 "tuple2Proj0"
      tuple2Proj0     = StgTopLifted $ StgNonRec tuple2Proj0Bnd $ StgRhsClosure [] Updatable [tBnd] $
                          StgCase (StgApp tOcc []) rBnd (AlgAlt tup2TCOcc)
                            [ Alt
                              { altCon      = AltDataCon tup2DCOcc
                              , altBinders  = [aBnd, bBnd]
                              , altRHS      = StgApp aOcc []
                              }
                            ]

      -- code for applyFun1Arg = \f p -> f p
      (fOcc, fBnd)    = localLiftedVanillaId 200 "f"
      (pOcc, pBnd)    = localLiftedVanillaId 201 "p"

      applyFun1ArgBnd = exportedLiftedVanillaId 202 "applyFun1Arg"
      applyFun1Arg    = StgTopLifted $ StgNonRec applyFun1ArgBnd $ StgRhsClosure [] Updatable [fBnd, pBnd] $
                          StgApp fOcc [StgVarArg pOcc]
