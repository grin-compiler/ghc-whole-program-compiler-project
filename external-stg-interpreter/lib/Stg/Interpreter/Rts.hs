{-# LANGUAGE RecordWildCards, LambdaCase, OverloadedStrings, PatternSynonyms #-}
module Stg.Interpreter.Rts (initRtsSupport) where

import GHC.Stack
import Control.Monad.State
import Control.Concurrent.MVar

import Data.List (foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map

import Stg.Syntax
import Stg.Interpreter.Base

pattern CharV c = Literal (LitChar c)
pattern IntV i    = IntAtom i -- Literal (LitNumber LitNumInt i)
pattern WordV i   = WordAtom i -- Literal (LitNumber LitNumWord i)
pattern Word32V i = WordAtom i -- Literal (LitNumber LitNumWord i)

initRtsSupport :: [Module] -> M ()
initRtsSupport mods = do
  -- collect rts related modules
  let rtsModSet = Set.fromList $
                    [(UnitId u, ModuleName m) | (u, m, _, _, _) <- wiredInCons] ++
                    [(UnitId u, ModuleName m) | (u, m, _, _) <- wiredInClosures]
      rtsMods = [m | m@Module{..} <- mods, Set.member (moduleUnitId, moduleName) rtsModSet]

  -- lookup wired-in constructors
  let dcMap = Map.fromList
                [ ((moduleUnitId, moduleName, tcName, dcName), dc)
                | m@Module{..} <- rtsMods
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
        StgTopLifted (StgRec l) -> map fst l
        _ -> []
      closureMap = Map.fromList
                [ ((uId, mName, bName), topBinding)
                | m@Module{..} <- rtsMods
                , topBinding@Binder{..} <- concatMap getBindings moduleTopBindings
                , (uId, mName, bName, _) <- wiredInClosures
                , UnitId uId == moduleUnitId
                , ModuleName mName == moduleName
                , bName == binderName
                ]

  forM_ wiredInClosures $ \(u, m, n, setter) -> do
    case Map.lookup (u, m, n) closureMap of
        Nothing -> error $ "missing wired in closure: " ++ show (u, m, n)
        Just b  -> do
          cl <- lookupEnv mempty b
          modify' $ \s@StgState{..} -> s {ssRtsSupport = setter ssRtsSupport cl}

  -- TODO: add these
  --  rtsApply2Args   :: Atom
  --  rtsTuple2Proj0  :: Atom
  pure ()

-- HINT: needed for FFI value boxing
wiredInCons :: [(Name, Name, Name, Name, Rts -> DataCon -> Rts)]
wiredInCons =
  -- unit-id,     module,       type con,     data con
  [ ("ghc-prim",  "GHC.Types",  "Char",       "C#",         \s dc -> s {rtsCharCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Int",        "I#",         \s dc -> s {rtsIntCon       = dc})
  , ("base",      "GHC.Int",    "Int8",       "I8#",        \s dc -> s {rtsInt8Con      = dc})
  , ("base",      "GHC.Int",    "Int16",      "I16#",       \s dc -> s {rtsInt16Con     = dc})
  , ("base",      "GHC.Int",    "Int32",      "I32#",       \s dc -> s {rtsInt32Con     = dc})
  , ("base",      "GHC.Int",    "Int64",      "I64#",       \s dc -> s {rtsInt64Con     = dc})
  , ("ghc-prim",  "GHC.Types",  "Word",       "W#",         \s dc -> s {rtsWordCon      = dc})
  , ("base",      "GHC.Word",   "Word8",      "W8#",        \s dc -> s {rtsWord8Con     = dc})
  , ("base",      "GHC.Word",   "Word16",     "W16#",       \s dc -> s {rtsWord16Con    = dc})
  , ("base",      "GHC.Word",   "Word32",     "W32#",       \s dc -> s {rtsWord32Con    = dc})
  , ("base",      "GHC.Word",   "Word64",     "W64#",       \s dc -> s {rtsWord64Con    = dc})
  , ("base",      "GHC.Ptr",    "Ptr",        "Ptr",        \s dc -> s {rtsPtrCon       = dc})
  , ("base",      "GHC.Ptr",    "FunPtr",     "FunPtr",     \s dc -> s {rtsFunPtrCon    = dc})
  , ("ghc-prim",  "GHC.Types",  "Float",      "F#",         \s dc -> s {rtsFloatCon     = dc})
  , ("ghc-prim",  "GHC.Types",  "Double",     "D#",         \s dc -> s {rtsDoubleCon    = dc})
  , ("base",      "GHC.Stable", "StablePtr",  "StablePtr",  \s dc -> s {rtsStablePtrCon = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "True",       \s dc -> s {rtsTrueCon      = dc})
  , ("ghc-prim",  "GHC.Types",  "Bool",       "False",      \s dc -> s {rtsFalseCon     = dc})
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
  [ ("base",      "GHC.TopHandler",         "runIO",                        \s cl -> s {rtsTopHandlerRunIO      = cl})
  , ("base",      "GHC.TopHandler",         "runNonIO",                     \s cl -> s {rtsTopHandlerRunNonIO   = cl})
  , ("base",      "GHC.Pack",               "unpackCString",                \s cl -> s {rtsUnpackCString        = cl})
  , ("base",      "GHC.Exception.Type",     "divZeroException",             \s cl -> s {rtsDivZeroException     = cl})
  , ("base",      "GHC.Exception.Type",     "underflowException",           \s cl -> s {rtsUnderflowException   = cl})
  , ("base",      "GHC.Exception.Type",     "overflowException",            \s cl -> s {rtsOverflowException    = cl})
  ]
{-
  , ("base",      "GHC.Weak",               "runFinalizerBatch",
  , ("base",      "GHC.IO.Exception",       "stackOverflow",
  , ("base",      "GHC.IO.Exception",       "heapOverflow",
  , ("base",      "GHC.IO.Exception",       "allocationLimitExceeded",
  , ("base",      "GHC.IO.Exception",       "blockedIndefinitelyOnMVar",
  , ("base",      "GHC.IO.Exception",       "blockedIndefinitelyOnSTM",
  , ("base",      "GHC.IO.Exception",       "cannotCompactFunction",
  , ("base",      "GHC.IO.Exception",       "cannotCompactPinned",
  , ("base",      "GHC.IO.Exception",       "cannotCompactMutable",
  , ("base",      "Control.Exception.Base", "absentSumFieldError",
  , ("base",      "Control.Exception.Base", "nonTermination",
  , ("base",      "Control.Exception.Base", "nestedAtomically",
  , ("base",      "GHC.Event.Thread",       "blockedOnBadFD",
  , ("base",      "GHC.Conc.Sync",          "runSparks",
  , ("base",      "GHC.Conc.IO",            "ensureIOManagerIsRunning",
  , ("base",      "GHC.Conc.IO",            "ioManagerCapabilitiesChanged",
  , ("base",      "GHC.Conc.Signal",        "runHandlersPtr",
  , ("base",      "GHC.TopHandler",         "flushStdHandles",
  , ("base",      "GHC.TopHandler",         "runMainIO",
-}

{-
#if !defined(mingw32_HOST_OS)
    getStablePtr((StgPtr)blockedOnBadFD_closure);
    getStablePtr((StgPtr)runHandlersPtr_closure);
#endif
-}
