module Stg.GHC.Symbols where

import Stg.Syntax

liveSymbols :: [(String, String, String)]
liveSymbols =
  -- exceptions (top level CAFs that create an Exception data constructor that can passed to raise# or raiseIO#)
  [ ("base", "Control.Exception.Base", "absentSumFieldError")
  , ("base", "Control.Exception.Base", "nestedAtomically")
  , ("base", "Control.Exception.Base", "nonTermination")
  , ("base", "GHC.Exception.Type", "divZeroException")
  , ("base", "GHC.Exception.Type", "overflowException")
  , ("base", "GHC.Exception.Type", "underflowException")
  , ("base", "GHC.IO.Exception", "allocationLimitExceeded")
  , ("base", "GHC.IO.Exception", "blockedIndefinitelyOnMVar")
  , ("base", "GHC.IO.Exception", "blockedIndefinitelyOnSTM")
  , ("base", "GHC.IO.Exception", "cannotCompactFunction")
  , ("base", "GHC.IO.Exception", "cannotCompactMutable")
  , ("base", "GHC.IO.Exception", "cannotCompactPinned")
  , ("base", "GHC.IO.Exception", "heapOverflow")
  , ("base", "GHC.IO.Exception", "stackOverflow")
  , ("base", "GHC.Event.Thread", "blockedOnBadFD")

  -- data constructors (corresponding top level closure)
  , ("base", "GHC.Int", "I16#")
  , ("base", "GHC.Int", "I32#")
  , ("base", "GHC.Int", "I64#")
  , ("base", "GHC.Int", "I8#")
  , ("base", "GHC.Ptr", "FunPtr")
  , ("base", "GHC.Ptr", "Ptr")
  , ("base", "GHC.Stable", "StablePtr")
  , ("base", "GHC.Word", "W16#")
  , ("base", "GHC.Word", "W32#")
  , ("base", "GHC.Word", "W64#")
  , ("base", "GHC.Word", "W8#")

  , ("ghc-prim", "GHC.Tuple", "()")
  , ("ghc-prim", "GHC.Types", "C#")
  , ("ghc-prim", "GHC.Types", "D#")
  , ("ghc-prim", "GHC.Types", "F#")
  , ("ghc-prim", "GHC.Types", "False")
  , ("ghc-prim", "GHC.Types", "I#")
  , ("ghc-prim", "GHC.Types", "True")
  , ("ghc-prim", "GHC.Types", "W#")

  -- top level functions
  , ("base", "GHC.Conc.IO", "ensureIOManagerIsRunning")
  , ("base", "GHC.Conc.IO", "ioManagerCapabilitiesChanged")
  , ("base", "GHC.Conc.Signal", "runHandlersPtr")
  , ("base", "GHC.Conc.Sync", "runSparks")
  , ("base", "GHC.Pack", "unpackCString")
  , ("base", "GHC.TopHandler", "flushStdHandles")
  , ("base", "GHC.TopHandler", "runIO")
  , ("base", "GHC.TopHandler", "runMainIO")
  , ("base", "GHC.TopHandler", "runNonIO")
  , ("base", "GHC.Weak", "runFinalizerBatch")

  -- HINT: main_:Main.main is the program entry point called from the program's main C function using the RTS API
  , ("main", ":Main", "main")
  ]

liveQualifiedSymbols :: [String]
liveQualifiedSymbols = [p ++ "_" ++ m ++ "." ++ n | (p,m,n) <- liveSymbols]
