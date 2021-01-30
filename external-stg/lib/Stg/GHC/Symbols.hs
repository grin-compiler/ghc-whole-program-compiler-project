module Stg.GHC.Symbols where

import Stg.Syntax

liveSymbols :: [(String, String, String)]
liveSymbols =
  -- base
  [ ("base", "Control.Exception.Base", "absentSumFieldError")
  , ("base", "Control.Exception.Base", "nestedAtomically")
  , ("base", "Control.Exception.Base", "nonTermination")
  , ("base", "GHC.Conc.IO", "ensureIOManagerIsRunning")
  , ("base", "GHC.Conc.IO", "ioManagerCapabilitiesChanged")
  , ("base", "GHC.Conc.Signal", "runHandlersPtr")
  , ("base", "GHC.Conc.Sync", "runSparks")
  , ("base", "GHC.Event.Thread", "blockedOnBadFD")
  , ("base", "GHC.Exception.Type", "divZeroException")
  , ("base", "GHC.Exception.Type", "overflowException")
  , ("base", "GHC.Exception.Type", "underflowException")
  , ("base", "GHC.Int", "I16#")
  , ("base", "GHC.Int", "I32#")
  , ("base", "GHC.Int", "I64#")
  , ("base", "GHC.Int", "I8#")
  , ("base", "GHC.IO.Exception", "allocationLimitExceeded")
  , ("base", "GHC.IO.Exception", "blockedIndefinitelyOnMVar")
  , ("base", "GHC.IO.Exception", "blockedIndefinitelyOnSTM")
  , ("base", "GHC.IO.Exception", "cannotCompactFunction")
  , ("base", "GHC.IO.Exception", "cannotCompactMutable")
  , ("base", "GHC.IO.Exception", "cannotCompactPinned")
  , ("base", "GHC.IO.Exception", "heapOverflow")
  , ("base", "GHC.IO.Exception", "stackOverflow")
  , ("base", "GHC.Pack", "unpackCString")
  , ("base", "GHC.Ptr", "FunPtr")
  , ("base", "GHC.Ptr", "Ptr")
  , ("base", "GHC.Stable", "StablePtr")
  , ("base", "GHC.TopHandler", "flushStdHandles")
  , ("base", "GHC.TopHandler", "runIO")
  , ("base", "GHC.TopHandler", "runMainIO")
  , ("base", "GHC.TopHandler", "runNonIO")
  , ("base", "GHC.Weak", "runFinalizerBatch")
  , ("base", "GHC.Word", "W16#")
  , ("base", "GHC.Word", "W32#")
  , ("base", "GHC.Word", "W64#")
  , ("base", "GHC.Word", "W8#")

  -- ghc-prim
  , ("ghc-prim", "GHC.Tuple", "()")
  , ("ghc-prim", "GHC.Types", "C#")
  , ("ghc-prim", "GHC.Types", "D#")
  , ("ghc-prim", "GHC.Types", "F#")
  , ("ghc-prim", "GHC.Types", "False")
  , ("ghc-prim", "GHC.Types", "I#")
  , ("ghc-prim", "GHC.Types", "True")
  , ("ghc-prim", "GHC.Types", "W#")

  -- main
  , ("main", ":Main", "main")
  ]

liveQualifiedSymbols :: [String]
liveQualifiedSymbols = [p ++ "_" ++ m ++ "." ++ n | (p,m,n) <- liveSymbols]
