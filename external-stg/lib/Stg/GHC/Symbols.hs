module Stg.GHC.Symbols where

import Stg.Syntax

liveSymbols :: [(String, String, String)]
liveSymbols =
  -- exceptions (top level CAFs that create an Exception data constructor that can passed to raise# or raiseIO#)
  [ ("ghc-prim", "GHC.Prim.Panic", "absentSumFieldError")
  , ("ghc-prim", "GHC.Prim.Exception", "raiseUnderflow")
  , ("ghc-prim", "GHC.Prim.Exception", "raiseOverflow")
  , ("ghc-prim", "GHC.Prim.Exception", "raiseDivZero")
  , ("base", "Control.Exception.Base", "nonTermination")
  , ("base", "Control.Exception.Base", "nestedAtomically")
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

  , ("base", "GHC.IOPort", "doubleReadException")
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
  , ("base", "GHC.Event.Windows", "processRemoteCompletion")
  , ("base", "GHC.Conc.IO", "interruptIOManager")
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

data Symbol
  = CFun    { getSymbolName :: String }
  | CData   { getSymbolName :: String }
  | CmmFun  { getSymbolName :: String }
  | CmmData { getSymbolName :: String }
  deriving (Eq, Ord, Show)

rtsSymbols :: [Symbol]
rtsSymbols =
  -- used by haskell unix library ; RTS hook API for signal handling
  [ CData "nocldstop"                         -- c data
  , CFun "genericRaise"                      -- c fun

  -- used by haskell process library ; RTS hook API for signal handling
  , CFun "blockUserSignals"                  -- c fun
  , CFun "unblockUserSignals"                -- c fun

  -- RTS API for generated FFI stubs
  , CFun "registerForeignExports"            -- c fun

  -- used by haskell base library ; GHC RTS stats
  , CFun "getRTSStatsEnabled"                -- c fun
  , CFun "getRTSStats"                       -- c fun

  -- used by haskell base library ; stack trace
  , CFun "backtraceFree"                     -- c fun
  , CFun "libdwGetBacktrace"                 -- c fun
  , CFun "libdwLookupLocation"               -- c fun
  , CFun "libdwPoolTake"                     -- c fun
  , CFun "libdwPoolRelease"                  -- c fun
  , CFun "libdwPoolClear"                    -- c fun

  -- used by haskell base library ; TTY (terminal)
  , CFun "__hscore_get_saved_termios"        -- c fun
  , CFun "__hscore_set_saved_termios"        -- c fun

  -- store ; seem dead / not used ; global store functions
  , CFun "getOrSetLibHSghcInitLinkerDone"        -- c fun
  , CFun "getOrSetLibHSghcPersistentLinkerState" -- c fun

  -- GHC only ; global store functions
  , CFun "getOrSetLibHSghcGlobalDynFlags"    -- c fun
  , CFun "getOrSetLibHSghcFastStringTable"   -- c fun

  -- RTS IO manager
  , CFun "ioManagerWakeup"                   -- c fun

  -- stable pointer c api ; used by RTS and generated FFI stubs
  , CFun "deRefStablePtr"                    -- c fun
  , CFun "getStablePtr"                      -- c fun

  -- primop as c call
  , CFun "newSpark"                          -- c fun

  -- used by ghc-bignum library
  , CFun "__int_encodeDouble"                -- c fun
  , CFun "__word_encodeDouble"               -- c fun
  , CFun "__int_encodeFloat"                 -- c fun
  , CFun "__word_encodeFloat"                -- c fun

  -- used by haskell base library ; IO manager
  , CFun "setIOManagerControlFd"             -- c fun
  , CFun "setTimerManagerControlFd"          -- c fun
  , CFun "setIOManagerWakeupFd"              -- c fun

  -- used by haskell base library ; top handler
  , CFun "rts_setMainThread"                 -- c fun
  , CFun "shutdownHaskellAndSignal"          -- c fun
  , CFun "stg_sig_install"                   -- c fun

  -- haskell program coverage (HPC)
  , CFun "hs_hpc_rootModule"                 -- c fun
  , CFun "hs_hpc_module"                     -- c fun

  -- HsFFI.h garbage collector
  , CFun "hs_perform_gc"                     -- c fun

  -- HsFFI.h threading
  , CFun "hs_thread_done"                    -- c fun

  -- HsFFI.h funptr
  , CFun "hs_free_fun_ptr"                   -- c fun
  , CFun "freeHaskellFunctionPtr"            -- c fun

  -- HsFFI.h stable pointer
  , CFun "hs_lock_stable_ptr_table"          -- c fun
  , CFun "hs_unlock_stable_ptr_table"        -- c fun
  , CFun "hs_lock_stable_tables"             -- c fun
  , CFun "hs_unlock_stable_tables"           -- c fun
  , CFun "hs_free_stable_ptr"                -- c fun
  , CFun "hs_free_stable_ptr_unsafe"         -- c fun

  -- HsFFI.h mvar
  , CFun "hs_try_putmvar"                    -- c fun

  -- GHC Cmm STG machine globals
  , CData "stg_arg_bitmaps"                   -- c data

  -- RTS globals
  , CData "RtsFlags"                          -- c data
  , CData "defaultRtsConfig"                  -- c data
  , CData "closure_flags"                     -- c data
  , CData "MainCapability"                    -- c data
  , CData "signal_handlers"                   -- c data
  , CData "stable_name_table"                 -- c data
  , CData "stable_ptr_table"                  -- c data

  -- RTS start and exit
  , CFun "startupHaskell"                    -- c fun
  , CFun "stg_exit"                          -- c fun
  , CFun "shutdownHaskell"                   -- c fun
  , CFun "shutdownHaskellAndExit"            -- c fun
  , CFun "hs_init"                           -- c fun
  , CFun "hs_init_with_rtsopts"              -- c fun
  , CFun "hs_init_ghc"                       -- c fun
  , CFun "hs_exit"                           -- c fun
  , CFun "hs_exit_nowait"                    -- c fun
  , CFun "hs_main"                           -- c fun

  -- referenced by GHC Cmm RTS
  , CmmFun "ZCMain_main_closure"               -- cmm fun

  -- RTS error reporting
  , CFun "reportStackOverflow"               -- c fun
  , CFun "reportHeapOverflow"                -- c fun

  -- RTS GHCi bytecode interpreter
  , CData "rts_breakpoint_io_action"          -- c data
  , CData "rts_stop_next_breakpoint"          -- c data
  , CData "rts_stop_on_exception"             -- c data

  -- RTS time
  , CFun "getProcessElapsedTime"             -- c fun
  , CFun "getMonotonicNSec"                  -- c fun

  -- RTS program and arguments
  , CData "prog_argc"                         -- c data
  , CData "prog_argv"                         -- c data
  , CFun "setProgArgv"                       -- c fun
  , CFun "hs_set_argv"                       -- c fun
  , CFun "getProgArgv"                       -- c fun
  , CFun "getFullProgArgv"                   -- c fun
  , CFun "setFullProgArgv"                   -- c fun
  , CFun "freeFullProgArgv"                  -- c fun

  -- RTS timer
  , CFun "startTimer"                        -- c fun
  , CFun "stopTimer"                         -- c fun
  , CFun "rtsTimerSignal"                    -- c fun

  -- RTS file lock
  , CFun "lockFile"                          -- c fun
  , CFun "unlockFile"                        -- c fun

  -- RTS internal
  , CFun "_assertFail"                       -- c fun
  , CFun "debugBelch"                        -- c fun
  , CFun "errorBelch"                        -- c fun
  , CFun "sysErrorBelch"                     -- c fun
  , CFun "barf"                              -- c fun

  -- cost centre
  , CFun "startProfTimer"                    -- c fun
  , CFun "stopProfTimer"                     -- c fun

  -- RTS FFI adjustor
  , CFun "createAdjustor"                    -- c fun
  , CFun "allocateExecPage"                  -- c fun
  , CFun "freezeExecPage"                    -- c fun
  , CFun "freeExecPage"                      -- c fun

  -- RTS linker
  , CFun "loadObj"                           -- c fun
  , CFun "loadArchive"                       -- c fun
  , CFun "purgeObj"                          -- c fun
  , CFun "initLinker"                        -- c fun
  , CFun "initLinker_"                       -- c fun
  , CFun "insertSymbol"                      -- c fun
  , CFun "lookupSymbol"                      -- c fun
  , CFun "resolveObjs"                       -- c fun
  , CFun "unloadObj"                         -- c fun
  , CFun "addDLL"                            -- c fun
  , CFun "addLibrarySearchPath"              -- c fun
  , CFun "removeLibrarySearchPath"           -- c fun
  , CFun "findSystemLibrary"                 -- c fun

  -- RTS threads
  , CFun "performGC"                         -- c fun
  , CFun "performMajorGC"                    -- c fun
  , CFun "rts_enableThreadAllocationLimit"   -- c fun
  , CFun "rts_disableThreadAllocationLimit"  -- c fun
  , CFun "rtsSupportsBoundThreads"           -- c fun
  , CFun "resumeThread"                      -- c fun
  , CFun "setNumCapabilities"                -- c fun
  , CFun "getNumberOfProcessors"             -- c fun
  , CFun "eq_thread"                         -- c fun
  , CFun "cmp_thread"                        -- c fun
  , CFun "forkProcess"                       -- c fun
  , CFun "forkOS_createThread"               -- c fun
  , CFun "suspendThread"                     -- c fun
  , CData "enabled_capabilities"              -- c data
  , CData "n_capabilities"                    -- c data

  -- RTS SMP internal
  , CFun "atomic_inc"                        -- c fun
  , CFun "atomic_dec"                        -- c fun
  , CFun "write_barrier"                     -- c fun
  , CFun "store_load_barrier"                -- c fun
  , CFun "load_load_barrier"                 -- c fun
  , CFun "cas"                               -- c fun

  -- garbage collector
  , CData "large_alloc_lim"                   -- c data
  , CFun "allocate"                          -- c fun
  , CFun "revertCAFs"                        -- c fun
  , CData "keepCAFs"                          -- c data
  , CData "g0"                                -- c data
  , CFun "updateRemembSetPushThunk"          -- c fun
  , CFun "updateRemembSetPushThunk_"         -- c fun
  , CFun "updateRemembSetPushClosure_"       -- c fun

  -- RTS global store functions
  , CFun "getOrSetGHCConcSignalSignalHandlerStore"         -- c fun
  , CFun "getOrSetGHCConcWindowsPendingDelaysStore"        -- c fun
  , CFun "getOrSetGHCConcWindowsIOManagerThreadStore"      -- c fun
  , CFun "getOrSetGHCConcWindowsProddingStore"             -- c fun
  , CFun "getOrSetSystemEventThreadEventManagerStore"      -- c fun
  , CFun "getOrSetSystemEventThreadIOManagerThreadStore"   -- c fun
  , CFun "getOrSetSystemTimerThreadEventManagerStore"      -- c fun
  , CFun "getOrSetSystemTimerThreadIOManagerThreadStore"   -- c fun

  -- static pointer table
  , CFun "hs_spt_lookup"                   -- c fun
  , CFun "hs_spt_insert"                   -- c fun
  , CFun "hs_spt_insert_stableptr"         -- c fun
  , CFun "hs_spt_remove"                   -- c fun
  , CFun "hs_spt_keys"                     -- c fun
  , CFun "hs_spt_key_count"                -- c fun

  -- RTS internal settings
  , CFun "rts_isProfiled"                  -- c fun
  , CFun "rts_isDynamic"                   -- c fun

  -- RTS API
  , CFun "rts_setInCallCapability"         -- c fun

  , CFun "getAllocations"                  -- c fun

  , CFun "rts_apply"                       -- c fun
  , CFun "rts_checkSchedStatus"            -- c fun
  , CFun "rts_eval"                        -- c fun
  , CFun "rts_evalIO"                      -- c fun
  , CFun "rts_evalLazyIO"                  -- c fun
  , CFun "rts_evalStableIOMain"            -- c fun
  , CFun "rts_evalStableIO"                -- c fun
  , CFun "rts_eval_"                       -- c fun
  , CFun "rts_lock"                        -- c fun
  , CFun "rts_unlock"                      -- c fun
  , CFun "rts_unsafeGetMyCapability"       -- c fun

  , CFun "rts_mkBool"                      -- c fun
  , CFun "rts_mkChar"                      -- c fun
  , CFun "rts_mkDouble"                    -- c fun
  , CFun "rts_mkFloat"                     -- c fun
  , CFun "rts_mkInt"                       -- c fun
  , CFun "rts_mkInt8"                      -- c fun
  , CFun "rts_mkInt16"                     -- c fun
  , CFun "rts_mkInt32"                     -- c fun
  , CFun "rts_mkInt64"                     -- c fun
  , CFun "rts_mkPtr"                       -- c fun
  , CFun "rts_mkFunPtr"                    -- c fun
  , CFun "rts_mkStablePtr"                 -- c fun
  , CFun "rts_mkString"                    -- c fun
  , CFun "rts_mkWord"                      -- c fun
  , CFun "rts_mkWord8"                     -- c fun
  , CFun "rts_mkWord16"                    -- c fun
  , CFun "rts_mkWord32"                    -- c fun
  , CFun "rts_mkWord64"                    -- c fun

  , CFun "rts_getBool"                     -- c fun
  , CFun "rts_getChar"                     -- c fun
  , CFun "rts_getDouble"                   -- c fun
  , CFun "rts_getFloat"                    -- c fun
  , CFun "rts_getInt"                      -- c fun
  , CFun "rts_getInt8"                     -- c fun
  , CFun "rts_getInt16"                    -- c fun
  , CFun "rts_getInt32"                    -- c fun
  , CFun "rts_getInt64"                    -- c fun
  , CFun "rts_getPtr"                      -- c fun
  , CFun "rts_getFunPtr"                   -- c fun
  , CFun "rts_getStablePtr"                -- c fun
  , CFun "rts_getThreadId"                 -- c fun
  , CFun "rts_getWord"                     -- c fun
  , CFun "rts_getWord8"                    -- c fun
  , CFun "rts_getWord16"                   -- c fun
  , CFun "rts_getWord32"                   -- c fun
  , CFun "rts_getWord64"                   -- c fun

  -- GHC Cmm STG machine
  , CmmFun "StgReturn"                       -- cmm fun

  , CmmFun "stg_badAlignment_entry"          -- cmm fun

  , CmmData "stg_CHARLIKE_closure"            -- cmm data
  , CmmData "stg_INTLIKE_closure"             -- cmm data

  , CFun "dirty_MUT_VAR"                   -- c fun
  , CFun "dirty_TVAR"                      -- c fun

  , CmmData "stg_CAF_BLACKHOLE_info"                    -- cmm data
  , CmmData "stg_BLACKHOLE_info"                        -- cmm data
  , CmmData "__stg_EAGER_BLACKHOLE_info"                -- cmm data
  , CmmData "stg_BLOCKING_QUEUE_CLEAN_info"             -- cmm data
  , CmmData "stg_BLOCKING_QUEUE_DIRTY_info"             -- cmm data
  , CmmData "stg_MVAR_CLEAN_info"                       -- cmm data
  , CmmData "stg_MVAR_DIRTY_info"                       -- cmm data
  , CmmData "stg_TVAR_CLEAN_info"                       -- cmm data
  , CmmData "stg_TVAR_DIRTY_info"                       -- cmm data
  , CmmData "stg_IND_STATIC_info"                       -- cmm data
  , CmmData "stg_ARR_WORDS_info"                        -- cmm data
  , CmmData "stg_MUT_ARR_PTRS_DIRTY_info"               -- cmm data
  , CmmData "stg_MUT_ARR_PTRS_FROZEN_CLEAN_info"        -- cmm data
  , CmmData "stg_MUT_ARR_PTRS_FROZEN_DIRTY_info"        -- cmm data
  , CmmData "stg_SMALL_MUT_ARR_PTRS_DIRTY_info"         -- cmm data
  , CmmData "stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info"  -- cmm data
  , CmmData "stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info"  -- cmm data
  , CmmData "stg_MUT_VAR_CLEAN_info"                    -- cmm data
  , CmmData "stg_MUT_VAR_DIRTY_info"                    -- cmm data
  , CmmData "stg_WEAK_info"                             -- cmm data
  , CmmFun "stg_upd_frame_info"                        -- cmm fun/data
  , CmmFun "stg_bh_upd_frame_info"                     -- cmm fun/data

  , CmmFun "stg_gc_noregs"                     -- cmm fun
  , CmmFun "stg_ret_v_info"                    -- cmm fun/data
  , CmmFun "stg_ret_p_info"                    -- cmm fun/data
  , CmmFun "stg_ret_n_info"                    -- cmm fun/data
  , CmmFun "stg_ret_f_info"                    -- cmm fun/data
  , CmmFun "stg_ret_d_info"                    -- cmm fun/data
  , CmmFun "stg_ret_l_info"                    -- cmm fun/data
  , CmmFun "stg_gc_prim_p"                     -- cmm fun
  , CmmFun "stg_gc_prim_pp"                    -- cmm fun
  , CmmFun "stg_gc_prim_n"                     -- cmm fun
  , CmmFun "stg_enter_info"                    -- cmm fun/data
  , CmmFun "__stg_gc_enter_1"                  -- cmm fun
  , CmmFun "stg_gc_unpt_r1"                    -- cmm fun
  , CmmFun "stg_gc_unbx_r1"                    -- cmm fun
  , CmmFun "stg_gc_f1"                         -- cmm fun
  , CmmFun "stg_gc_d1"                         -- cmm fun
  , CmmFun "stg_gc_l1"                         -- cmm fun
  , CmmFun "stg_gc_pp"                         -- cmm fun
  , CmmFun "stg_gc_ppp"                        -- cmm fun
  , CmmFun "stg_gc_pppp"                       -- cmm fun
  , CmmFun "__stg_gc_fun"                      -- cmm fun
  , CmmFun "stg_gc_fun_info"                   -- cmm fun/data
  , CmmFun "stg_yield_noregs"                  -- cmm fun
  , CmmFun "stg_yield_to_interpreter"          -- cmm fun
  , CmmFun "stg_block_noregs"                  -- cmm fun
  , CmmFun "stg_block_takemvar"                -- cmm fun
  , CmmFun "stg_block_readmvar"                -- cmm fun
  , CmmFun "stg_block_putmvar"                 -- cmm fun

  , CmmFun "stg_interp_constr1_entry"          -- cmm fun
  , CmmFun "stg_interp_constr2_entry"          -- cmm fun
  , CmmFun "stg_interp_constr3_entry"          -- cmm fun
  , CmmFun "stg_interp_constr4_entry"          -- cmm fun
  , CmmFun "stg_interp_constr5_entry"          -- cmm fun
  , CmmFun "stg_interp_constr6_entry"          -- cmm fun
  , CmmFun "stg_interp_constr7_entry"          -- cmm fun

  , CmmData "stg_SRT_1_info"                    -- cmm data
  , CmmData "stg_SRT_2_info"                    -- cmm data
  , CmmData "stg_SRT_3_info"                    -- cmm data
  , CmmData "stg_SRT_4_info"                    -- cmm data
  , CmmData "stg_SRT_5_info"                    -- cmm data
  , CmmData "stg_SRT_6_info"                    -- cmm data
  , CmmData "stg_SRT_7_info"                    -- cmm data
  , CmmData "stg_SRT_8_info"                    -- cmm data
  , CmmData "stg_SRT_9_info"                    -- cmm data
  , CmmData "stg_SRT_10_info"                   -- cmm data
  , CmmData "stg_SRT_11_info"                   -- cmm data
  , CmmData "stg_SRT_12_info"                   -- cmm data
  , CmmData "stg_SRT_13_info"                   -- cmm data
  , CmmData "stg_SRT_14_info"                   -- cmm data
  , CmmData "stg_SRT_15_info"                   -- cmm data
  , CmmData "stg_SRT_16_info"                   -- cmm data

  -- generated
  , CmmFun "stg_sel_0_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_1_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_2_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_3_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_4_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_5_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_6_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_7_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_8_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_9_upd_info"                -- cmm fun/data
  , CmmFun "stg_sel_10_upd_info"               -- cmm fun/data
  , CmmFun "stg_sel_11_upd_info"               -- cmm fun/data
  , CmmFun "stg_sel_12_upd_info"               -- cmm fun/data
  , CmmFun "stg_sel_13_upd_info"               -- cmm fun/data
  , CmmFun "stg_sel_14_upd_info"               -- cmm fun/data
  , CmmFun "stg_sel_15_upd_info"               -- cmm fun/data
  , CmmFun "stg_sel_0_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_1_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_2_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_3_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_4_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_5_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_6_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_7_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_8_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_9_noupd_info"              -- cmm fun/data
  , CmmFun "stg_sel_10_noupd_info"             -- cmm fun/data
  , CmmFun "stg_sel_11_noupd_info"             -- cmm fun/data
  , CmmFun "stg_sel_12_noupd_info"             -- cmm fun/data
  , CmmFun "stg_sel_13_noupd_info"             -- cmm fun/data
  , CmmFun "stg_sel_14_noupd_info"             -- cmm fun/data
  , CmmFun "stg_sel_15_noupd_info"             -- cmm fun/data

  , CmmFun "stg_ap_v_info"                     -- cmm fun/data
  , CmmFun "stg_ap_f_info"                     -- cmm fun/data
  , CmmFun "stg_ap_d_info"                     -- cmm fun/data
  , CmmFun "stg_ap_l_info"                     -- cmm fun/data
  , CmmFun "stg_ap_v16_info"                   -- cmm fun/data
  , CmmFun "stg_ap_v32_info"                   -- cmm fun/data
  , CmmFun "stg_ap_v64_info"                   -- cmm fun/data
  , CmmFun "stg_ap_n_info"                     -- cmm fun/data
  , CmmFun "stg_ap_p_info"                     -- cmm fun/data
  , CmmFun "stg_ap_pv_info"                    -- cmm fun/data
  , CmmFun "stg_ap_pp_info"                    -- cmm fun/data
  , CmmFun "stg_ap_ppv_info"                   -- cmm fun/data
  , CmmFun "stg_ap_ppp_info"                   -- cmm fun/data
  , CmmFun "stg_ap_pppv_info"                  -- cmm fun/data
  , CmmFun "stg_ap_pppp_info"                  -- cmm fun/data
  , CmmFun "stg_ap_ppppp_info"                 -- cmm fun/data
  , CmmFun "stg_ap_pppppp_info"                -- cmm fun/data
  , CmmFun "stg_ap_0_fast"                     -- cmm fun
  , CmmFun "stg_ap_v_fast"                     -- cmm fun
  , CmmFun "stg_ap_f_fast"                     -- cmm fun
  , CmmFun "stg_ap_d_fast"                     -- cmm fun
  , CmmFun "stg_ap_l_fast"                     -- cmm fun
  , CmmFun "stg_ap_v16_fast"                   -- cmm fun
  , CmmFun "stg_ap_v32_fast"                   -- cmm fun
  , CmmFun "stg_ap_v64_fast"                   -- cmm fun
  , CmmFun "stg_ap_n_fast"                     -- cmm fun
  , CmmFun "stg_ap_p_fast"                     -- cmm fun
  , CmmFun "stg_ap_pv_fast"                    -- cmm fun
  , CmmFun "stg_ap_pp_fast"                    -- cmm fun
  , CmmFun "stg_ap_ppv_fast"                   -- cmm fun
  , CmmFun "stg_ap_ppp_fast"                   -- cmm fun
  , CmmFun "stg_ap_pppv_fast"                  -- cmm fun
  , CmmFun "stg_ap_pppp_fast"                  -- cmm fun
  , CmmFun "stg_ap_ppppp_fast"                 -- cmm fun
  , CmmFun "stg_ap_pppppp_fast"                -- cmm fun
  , CmmFun "stg_ap_1_upd_info"                 -- cmm fun/data
  , CmmFun "stg_ap_2_upd_info"                 -- cmm fun/data
  , CmmFun "stg_ap_3_upd_info"                 -- cmm fun/data
  , CmmFun "stg_ap_4_upd_info"                 -- cmm fun/data
  , CmmFun "stg_ap_5_upd_info"                 -- cmm fun/data
  , CmmFun "stg_ap_6_upd_info"                 -- cmm fun/data
  , CmmFun "stg_ap_7_upd_info"                 -- cmm fun/data

  -- primops: compact regions
  , CmmFun "stg_compactAddWithSharingzh"       -- cmm fun
  , CmmFun "stg_compactAddzh"                  -- cmm fun
  , CmmFun "stg_compactNewzh"                  -- cmm fun
  , CmmFun "stg_compactResizzezh"              -- cmm fun
  , CmmFun "stg_compactContainszh"             -- cmm fun
  , CmmFun "stg_compactContainsAnyzh"          -- cmm fun
  , CmmFun "stg_compactGetFirstBlockzh"        -- cmm fun
  , CmmFun "stg_compactGetNextBlockzh"         -- cmm fun
  , CmmFun "stg_compactAllocateBlockzh"        -- cmm fun
  , CmmFun "stg_compactFixupPointerszh"        -- cmm fun
  , CmmFun "stg_compactSizzezh"                -- cmm fun

  -- primops: weak pointers
  , CmmFun "stg_mkWeakzh"                      -- cmm fun
  , CmmFun "stg_mkWeakNoFinalizzerzh"          -- cmm fun
  , CmmFun "stg_addCFinalizzerToWeakzh"        -- cmm fun
  , CmmFun "stg_finalizzeWeakzh"               -- cmm fun
  , CmmFun "stg_deRefWeakzh"                   -- cmm fun

  -- primops: stable pointer
  , CmmFun "stg_deRefStablePtrzh"              -- cmm fun
  , CmmFun "stg_makeStablePtrzh"               -- cmm fun

  -- primops: stable name
  , CmmFun "stg_makeStableNamezh"              -- cmm fun

  -- primops: STM
  , CmmFun "stg_atomicallyzh"                  -- cmm fun
  , CmmFun "stg_catchRetryzh"                  -- cmm fun
  , CmmFun "stg_catchSTMzh"                    -- cmm fun
  , CmmFun "stg_newTVarzh"                     -- cmm fun
  , CmmFun "stg_readTVarzh"                    -- cmm fun
  , CmmFun "stg_readTVarIOzh"                  -- cmm fun
  , CmmFun "stg_writeTVarzh"                   -- cmm fun
  , CmmFun "stg_retryzh"                       -- cmm fun

  -- primops: exceptions
  , CmmFun "stg_getMaskingStatezh"             -- cmm fun
  , CmmFun "stg_maskAsyncExceptionszh"         -- cmm fun
  , CmmFun "stg_maskUninterruptiblezh"         -- cmm fun
  , CmmFun "stg_catchzh"                       -- cmm fun
  , CmmFun "stg_raisezh"                       -- cmm fun
  , CmmFun "stg_raiseDivZZerozh"               -- cmm fun
  , CmmFun "stg_raiseUnderflowzh"              -- cmm fun
  , CmmFun "stg_raiseOverflowzh"               -- cmm fun
  , CmmFun "stg_raiseIOzh"                     -- cmm fun
  , CmmFun "stg_unmaskAsyncExceptionszh"       -- cmm fun

  -- primops: misc, etc
  , CmmFun "stg_clearCCSzh"                    -- cmm fun
  , CmmFun "stg_traceCcszh"                    -- cmm fun
  , CmmFun "stg_traceEventzh"                  -- cmm fun
  , CmmFun "stg_traceMarkerzh"                 -- cmm fun
  , CmmFun "stg_traceBinaryEventzh"            -- cmm fun

  -- primops: double
  , CmmFun "stg_decodeDoublezu2Intzh"          -- cmm fun
  , CmmFun "stg_decodeDoublezuInt64zh"         -- cmm fun

  -- primops: float
  , CmmFun "stg_decodeFloatzuIntzh"            -- cmm fun

  -- primops: delay wait
  , CmmFun "stg_delayzh"                       -- cmm fun
  , CmmFun "stg_waitReadzh"                    -- cmm fun
  , CmmFun "stg_waitWritezh"                   -- cmm fun

  -- primops: concurrency
  , CmmFun "stg_forkzh"                        -- cmm fun
  , CmmFun "stg_forkOnzh"                      -- cmm fun
  , CmmFun "stg_isCurrentThreadBoundzh"        -- cmm fun
  , CmmFun "stg_killThreadzh"                  -- cmm fun
  , CmmFun "stg_labelThreadzh"                 -- cmm fun
  , CmmFun "stg_noDuplicatezh"                 -- cmm fun
  , CmmFun "stg_threadStatuszh"                -- cmm fun
  , CmmFun "stg_yieldzh"                       -- cmm fun

  -- primops: GHCi bytecode
  , CmmFun "stg_unpackClosurezh"               -- cmm fun
  , CmmFun "stg_closureSizzezh"                -- cmm fun
  , CmmFun "stg_getApStackValzh"               -- cmm fun
  , CmmFun "stg_mkApUpd0zh"                    -- cmm fun
  , CmmFun "stg_newBCOzh"                      -- cmm fun

  -- primops: parallelism
  , CmmFun "stg_getSparkzh"                    -- cmm fun
  , CmmFun "stg_numSparkszh"                   -- cmm fun

  -- primops: mvar
  , CmmFun "stg_isEmptyMVarzh"                 -- cmm fun
  , CmmFun "stg_newMVarzh"                     -- cmm fun
  , CmmFun "stg_putMVarzh"                     -- cmm fun
  , CmmFun "stg_tryPutMVarzh"                  -- cmm fun
  , CmmFun "stg_tryTakeMVarzh"                 -- cmm fun
  , CmmFun "stg_tryReadMVarzh"                 -- cmm fun
  , CmmFun "stg_takeMVarzh"                    -- cmm fun
  , CmmFun "stg_readMVarzh"                    -- cmm fun

  -- primops: array
  , CmmFun "stg_newArrayzh"                    -- cmm fun
  , CmmFun "stg_copyArrayzh"                   -- cmm fun
  , CmmFun "stg_copyMutableArrayzh"            -- cmm fun
  , CmmFun "stg_cloneArrayzh"                  -- cmm fun
  , CmmFun "stg_cloneMutableArrayzh"           -- cmm fun
  , CmmFun "stg_freezzeArrayzh"                -- cmm fun
  , CmmFun "stg_thawArrayzh"                   -- cmm fun
  , CmmFun "stg_casArrayzh"                    -- cmm fun
  , CmmFun "stg_copyArray_barrier"             -- cmm fun
  , CmmFun "stg_unsafeThawArrayzh"             -- cmm fun

  -- primops: array array
  , CmmFun "stg_copyArrayArrayzh"              -- cmm fun
  , CmmFun "stg_copyMutableArrayArrayzh"       -- cmm fun
  , CmmFun "stg_newArrayArrayzh"               -- cmm fun

  -- primops: small array
  , CmmFun "stg_newSmallArrayzh"               -- cmm fun
  , CmmFun "stg_unsafeThawSmallArrayzh"        -- cmm fun
  , CmmFun "stg_cloneSmallArrayzh"             -- cmm fun
  , CmmFun "stg_cloneSmallMutableArrayzh"      -- cmm fun
  , CmmFun "stg_freezzeSmallArrayzh"           -- cmm fun
  , CmmFun "stg_thawSmallArrayzh"              -- cmm fun
  , CmmFun "stg_copySmallArrayzh"              -- cmm fun
  , CmmFun "stg_copySmallMutableArrayzh"       -- cmm fun
  , CmmFun "stg_casSmallArrayzh"               -- cmm fun
  , CmmFun "stg_shrinkSmallMutableArrayzh"     -- cmm fun

  -- primops: bytearray
  , CmmFun "stg_newByteArrayzh"                -- cmm fun
  , CmmFun "stg_casIntArrayzh"                 -- cmm fun
  , CmmFun "stg_newPinnedByteArrayzh"          -- cmm fun
  , CmmFun "stg_newAlignedPinnedByteArrayzh"   -- cmm fun
  , CmmFun "stg_isByteArrayPinnedzh"           -- cmm fun
  , CmmFun "stg_isMutableByteArrayPinnedzh"    -- cmm fun
  , CmmFun "stg_shrinkMutableByteArrayzh"      -- cmm fun
  , CmmFun "stg_resizzeMutableByteArrayzh"     -- cmm fun

  -- primops: mutvar
  , CmmFun "stg_newMutVarzh"                   -- cmm fun
  , CmmFun "stg_atomicModifyMutVar2zh"         -- cmm fun
  , CmmFun "stg_atomicModifyMutVarzuzh"        -- cmm fun
  , CmmFun "stg_casMutVarzh"                   -- cmm fun

  -- primops: Synchronized I/O Ports
  , CmmFun "stg_readIOPortzh"                  -- cmm fun
  , CmmFun "stg_writeIOPortzh"                 -- cmm fun
  , CmmFun "stg_newIOPortzh"                   -- cmm fun

  , CmmFun "stg_paniczh"                       -- cmm fun
  , CmmFun "stg_getThreadAllocationCounterzh"  -- cmm fun
  , CmmFun "stg_setThreadAllocationCounterzh"  -- cmm fun

  -- new / unchecked
  , CFun "setKeepCAFs"
  ]

handledRTSSymbols :: [String]
handledRTSSymbols =
  [ "createAdjustor"
  , "hs_free_stable_ptr"
  , "errorBelch2"
  , "errorBelch"
  , "debugBelch2"
  , "shutdownHaskellAndExit"
  , "getProgArgv"
  , "freeHaskellFunctionPtr"
  , "performMajorGC"
  , "rts_setMainThread"
  , "stg_sig_install"
  , "lockFile"
  , "unlockFile"
  , "rtsSupportsBoundThreads"
  , "stg_getThreadAllocationCounterzh"
  , "stg_doubleToWord64zh"
  , "stg_floatToWord32zh"
  , "stg_word32ToFloatzh"
  , "stg_word64ToDoublezh"
  , "getOrSetGHCConcSignalSignalHandlerStore"
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
