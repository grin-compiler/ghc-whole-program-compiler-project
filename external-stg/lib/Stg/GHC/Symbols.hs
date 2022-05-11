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

rtsSymbols :: [String]
rtsSymbols =
  -- used by haskell unix library ; RTS hook API for signal handling
  [ "nocldstop"                         -- c data
  , "genericRaise"                      -- c fun

  -- used by haskell process library ; RTS hook API for signal handling
  , "blockUserSignals"                  -- c fun
  , "unblockUserSignals"                -- c fun

  -- RTS API for generated FFI stubs
  , "registerForeignExports"            -- c fun

  -- used by haskell base library ; GHC RTS stats
  , "getRTSStatsEnabled"                -- c fun
  , "getRTSStats"                       -- c fun

  -- used by haskell base library ; stack trace
  , "backtraceFree"                     -- c fun
  , "libdwGetBacktrace"                 -- c fun
  , "libdwLookupLocation"               -- c fun
  , "libdwPoolTake"                     -- c fun
  , "libdwPoolRelease"                  -- c fun
  , "libdwPoolClear"                    -- c fun

  -- used by haskell base library ; TTY (terminal)
  , "__hscore_get_saved_termios"        -- c fun
  , "__hscore_set_saved_termios"        -- c fun

  -- store ; seem dead / not used ; global store functions
  , "getOrSetLibHSghcInitLinkerDone"        -- c fun
  , "getOrSetLibHSghcPersistentLinkerState" -- c fun

  -- GHC only ; global store functions
  , "getOrSetLibHSghcGlobalDynFlags"    -- c fun
  , "getOrSetLibHSghcFastStringTable"   -- c fun

  -- RTS IO manager
  , "ioManagerWakeup"                   -- c fun

  -- stable pointer c api ; used by RTS and generated FFI stubs
  , "deRefStablePtr"                    -- c fun
  , "getStablePtr"                      -- c fun

  -- primop as c call
  , "newSpark"                          -- c fun

  -- used by ghc-bignum library
  , "__int_encodeDouble"                -- c fun
  , "__word_encodeDouble"               -- c fun
  , "__int_encodeFloat"                 -- c fun
  , "__word_encodeFloat"                -- c fun

  -- used by haskell base library ; IO manager
  , "setIOManagerControlFd"             -- c fun
  , "setTimerManagerControlFd"          -- c fun
  , "setIOManagerWakeupFd"              -- c fun

  -- used by haskell base library ; top handler
  , "rts_setMainThread"                 -- c fun
  , "shutdownHaskellAndSignal"          -- c fun
  , "stg_sig_install"                   -- c fun

  -- haskell program coverage (HPC)
  , "hs_hpc_rootModule"                 -- c fun
  , "hs_hpc_module"                     -- c fun

  -- HsFFI.h garbage collector
  , "hs_perform_gc"                     -- c fun

  -- HsFFI.h threading
  , "hs_thread_done"                    -- c fun

  -- HsFFI.h funptr
  , "hs_free_fun_ptr"                   -- c fun
  , "freeHaskellFunctionPtr"            -- c fun

  -- HsFFI.h stable pointer
  , "hs_lock_stable_ptr_table"          -- c fun
  , "hs_unlock_stable_ptr_table"        -- c fun
  , "hs_lock_stable_tables"             -- c fun
  , "hs_unlock_stable_tables"           -- c fun
  , "hs_free_stable_ptr"                -- c fun
  , "hs_free_stable_ptr_unsafe"         -- c fun

  -- HsFFI.h mvar
  , "hs_try_putmvar"                    -- c fun

  -- GHC Cmm STG machine globals
  , "stg_arg_bitmaps"                   -- c data

  -- RTS globals
  , "RtsFlags"                          -- c data
  , "defaultRtsConfig"                  -- c data
  , "closure_flags"                     -- c data
  , "MainCapability"                    -- c data
  , "signal_handlers"                   -- c data
  , "stable_name_table"                 -- c data
  , "stable_ptr_table"                  -- c data

  -- RTS start and exit
  , "startupHaskell"                    -- c fun
  , "stg_exit"                          -- c fun
  , "shutdownHaskell"                   -- c fun
  , "shutdownHaskellAndExit"            -- c fun
  , "hs_init"                           -- c fun
  , "hs_init_with_rtsopts"              -- c fun
  , "hs_init_ghc"                       -- c fun
  , "hs_exit"                           -- c fun
  , "hs_exit_nowait"                    -- c fun
  , "hs_main"                           -- c fun

  -- referenced by GHC Cmm RTS
  , "ZCMain_main_closure"               -- cmm fun

  -- RTS error reporting
  , "reportStackOverflow"               -- c fun
  , "reportHeapOverflow"                -- c fun

  -- RTS GHCi bytecode interpreter
  , "rts_breakpoint_io_action"          -- c data
  , "rts_stop_next_breakpoint"          -- c data
  , "rts_stop_on_exception"             -- c data

  -- RTS time
  , "getProcessElapsedTime"             -- c fun
  , "getMonotonicNSec"                  -- c fun

  -- RTS program and arguments
  , "prog_argc"                         -- c data
  , "prog_argv"                         -- c data
  , "setProgArgv"                       -- c fun
  , "hs_set_argv"                       -- c fun
  , "getProgArgv"                       -- c fun
  , "getFullProgArgv"                   -- c fun
  , "setFullProgArgv"                   -- c fun
  , "freeFullProgArgv"                  -- c fun

  -- RTS timer
  , "startTimer"                        -- c fun
  , "stopTimer"                         -- c fun
  , "rtsTimerSignal"                    -- c fun

  -- RTS file lock
  , "lockFile"                          -- c fun
  , "unlockFile"                        -- c fun

  -- RTS internal
  , "_assertFail"                       -- c fun
  , "debugBelch"                        -- c fun
  , "errorBelch"                        -- c fun
  , "sysErrorBelch"                     -- c fun
  , "barf"                              -- c fun

  -- cost centre
  , "startProfTimer"                    -- c fun
  , "stopProfTimer"                     -- c fun

  -- RTS FFI adjustor
  , "createAdjustor"                    -- c fun
  , "allocateExecPage"                  -- c fun
  , "freezeExecPage"                    -- c fun
  , "freeExecPage"                      -- c fun

  -- RTS linker
  , "loadObj"                           -- c fun
  , "loadArchive"                       -- c fun
  , "purgeObj"                          -- c fun
  , "initLinker"                        -- c fun
  , "initLinker_"                       -- c fun
  , "insertSymbol"                      -- c fun
  , "lookupSymbol"                      -- c fun
  , "resolveObjs"                       -- c fun
  , "unloadObj"                         -- c fun
  , "addDLL"                            -- c fun
  , "addLibrarySearchPath"              -- c fun
  , "removeLibrarySearchPath"           -- c fun
  , "findSystemLibrary"                 -- c fun

  -- RTS threads
  , "performGC"                         -- c fun
  , "performMajorGC"                    -- c fun
  , "rts_enableThreadAllocationLimit"   -- c fun
  , "rts_disableThreadAllocationLimit"  -- c fun
  , "rtsSupportsBoundThreads"           -- c fun
  , "resumeThread"                      -- c fun
  , "setNumCapabilities"                -- c fun
  , "getNumberOfProcessors"             -- c fun
  , "eq_thread"                         -- c fun
  , "cmp_thread"                        -- c fun
  , "forkProcess"                       -- c fun
  , "forkOS_createThread"               -- c fun
  , "suspendThread"                     -- c fun
  , "enabled_capabilities"              -- c data
  , "n_capabilities"                    -- c data

  -- RTS SMP internal
  , "atomic_inc"                        -- c fun
  , "atomic_dec"                        -- c fun
  , "write_barrier"                     -- c fun
  , "store_load_barrier"                -- c fun
  , "load_load_barrier"                 -- c fun
  , "cas"                               -- c fun

  -- garbage collector
  , "large_alloc_lim"                   -- c data
  , "allocate"                          -- c fun
  , "revertCAFs"                        -- c fun
  , "keepCAFs"                          -- c data
  , "g0"                                -- c data
  , "updateRemembSetPushThunk"          -- c fun
  , "updateRemembSetPushThunk_"         -- c fun
  , "updateRemembSetPushClosure_"       -- c fun

  -- RTS global store functions
  , "getOrSetGHCConcSignalSignalHandlerStore"         -- c fun
  , "getOrSetGHCConcWindowsPendingDelaysStore"        -- c fun
  , "getOrSetGHCConcWindowsIOManagerThreadStore"      -- c fun
  , "getOrSetGHCConcWindowsProddingStore"             -- c fun
  , "getOrSetSystemEventThreadEventManagerStore"      -- c fun
  , "getOrSetSystemEventThreadIOManagerThreadStore"   -- c fun
  , "getOrSetSystemTimerThreadEventManagerStore"      -- c fun
  , "getOrSetSystemTimerThreadIOManagerThreadStore"   -- c fun

  -- static pointer table
  , "hs_spt_lookup"                   -- c fun
  , "hs_spt_insert"                   -- c fun
  , "hs_spt_insert_stableptr"         -- c fun
  , "hs_spt_remove"                   -- c fun
  , "hs_spt_keys"                     -- c fun
  , "hs_spt_key_count"                -- c fun

  -- RTS internal settings
  , "rts_isProfiled"                  -- c fun
  , "rts_isDynamic"                   -- c fun

  -- RTS API
  , "rts_setInCallCapability"         -- c fun

  , "getAllocations"                  -- c fun

  , "rts_apply"                       -- c fun
  , "rts_checkSchedStatus"            -- c fun
  , "rts_eval"                        -- c fun
  , "rts_evalIO"                      -- c fun
  , "rts_evalLazyIO"                  -- c fun
  , "rts_evalStableIOMain"            -- c fun
  , "rts_evalStableIO"                -- c fun
  , "rts_eval_"                       -- c fun
  , "rts_lock"                        -- c fun
  , "rts_unlock"                      -- c fun
  , "rts_unsafeGetMyCapability"       -- c fun

  , "rts_mkBool"                      -- c fun
  , "rts_mkChar"                      -- c fun
  , "rts_mkDouble"                    -- c fun
  , "rts_mkFloat"                     -- c fun
  , "rts_mkInt"                       -- c fun
  , "rts_mkInt8"                      -- c fun
  , "rts_mkInt16"                     -- c fun
  , "rts_mkInt32"                     -- c fun
  , "rts_mkInt64"                     -- c fun
  , "rts_mkPtr"                       -- c fun
  , "rts_mkFunPtr"                    -- c fun
  , "rts_mkStablePtr"                 -- c fun
  , "rts_mkString"                    -- c fun
  , "rts_mkWord"                      -- c fun
  , "rts_mkWord8"                     -- c fun
  , "rts_mkWord16"                    -- c fun
  , "rts_mkWord32"                    -- c fun
  , "rts_mkWord64"                    -- c fun

  , "rts_getBool"                     -- c fun
  , "rts_getChar"                     -- c fun
  , "rts_getDouble"                   -- c fun
  , "rts_getFloat"                    -- c fun
  , "rts_getInt"                      -- c fun
  , "rts_getInt8"                     -- c fun
  , "rts_getInt16"                    -- c fun
  , "rts_getInt32"                    -- c fun
  , "rts_getInt64"                    -- c fun
  , "rts_getPtr"                      -- c fun
  , "rts_getFunPtr"                   -- c fun
  , "rts_getStablePtr"                -- c fun
  , "rts_getThreadId"                 -- c fun
  , "rts_getWord"                     -- c fun
  , "rts_getWord8"                    -- c fun
  , "rts_getWord16"                   -- c fun
  , "rts_getWord32"                   -- c fun
  , "rts_getWord64"                   -- c fun

  -- GHC Cmm STG machine
  , "StgReturn"                       -- cmm fun

  , "stg_badAlignment_entry"          -- cmm fun

  , "stg_CHARLIKE_closure"            -- cmm data
  , "stg_INTLIKE_closure"             -- cmm data

  , "dirty_MUT_VAR"                   -- c fun
  , "dirty_TVAR"                      -- c fun

  , "stg_CAF_BLACKHOLE_info"                    -- cmm data
  , "stg_BLACKHOLE_info"                        -- cmm data
  , "__stg_EAGER_BLACKHOLE_info"                -- cmm data
  , "stg_BLOCKING_QUEUE_CLEAN_info"             -- cmm data
  , "stg_BLOCKING_QUEUE_DIRTY_info"             -- cmm data
  , "stg_MVAR_CLEAN_info"                       -- cmm data
  , "stg_MVAR_DIRTY_info"                       -- cmm data
  , "stg_TVAR_CLEAN_info"                       -- cmm data
  , "stg_TVAR_DIRTY_info"                       -- cmm data
  , "stg_IND_STATIC_info"                       -- cmm data
  , "stg_ARR_WORDS_info"                        -- cmm data
  , "stg_MUT_ARR_PTRS_DIRTY_info"               -- cmm data
  , "stg_MUT_ARR_PTRS_FROZEN_CLEAN_info"        -- cmm data
  , "stg_MUT_ARR_PTRS_FROZEN_DIRTY_info"        -- cmm data
  , "stg_SMALL_MUT_ARR_PTRS_DIRTY_info"         -- cmm data
  , "stg_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN_info"  -- cmm data
  , "stg_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY_info"  -- cmm data
  , "stg_MUT_VAR_CLEAN_info"                    -- cmm data
  , "stg_MUT_VAR_DIRTY_info"                    -- cmm data
  , "stg_WEAK_info"                             -- cmm data
  , "stg_upd_frame_info"                        -- cmm fun/data
  , "stg_bh_upd_frame_info"                     -- cmm fun/data

  , "stg_gc_noregs"                     -- cmm fun
  , "stg_ret_v_info"                    -- cmm fun/data
  , "stg_ret_p_info"                    -- cmm fun/data
  , "stg_ret_n_info"                    -- cmm fun/data
  , "stg_ret_f_info"                    -- cmm fun/data
  , "stg_ret_d_info"                    -- cmm fun/data
  , "stg_ret_l_info"                    -- cmm fun/data
  , "stg_gc_prim_p"                     -- cmm fun
  , "stg_gc_prim_pp"                    -- cmm fun
  , "stg_gc_prim_n"                     -- cmm fun
  , "stg_enter_info"                    -- cmm fun/data
  , "__stg_gc_enter_1"                  -- cmm fun
  , "stg_gc_unpt_r1"                    -- cmm fun
  , "stg_gc_unbx_r1"                    -- cmm fun
  , "stg_gc_f1"                         -- cmm fun
  , "stg_gc_d1"                         -- cmm fun
  , "stg_gc_l1"                         -- cmm fun
  , "stg_gc_pp"                         -- cmm fun
  , "stg_gc_ppp"                        -- cmm fun
  , "stg_gc_pppp"                       -- cmm fun
  , "__stg_gc_fun"                      -- cmm fun
  , "stg_gc_fun_info"                   -- cmm fun/data
  , "stg_yield_noregs"                  -- cmm fun
  , "stg_yield_to_interpreter"          -- cmm fun
  , "stg_block_noregs"                  -- cmm fun
  , "stg_block_takemvar"                -- cmm fun
  , "stg_block_readmvar"                -- cmm fun
  , "stg_block_putmvar"                 -- cmm fun

  , "stg_interp_constr1_entry"          -- cmm fun
  , "stg_interp_constr2_entry"          -- cmm fun
  , "stg_interp_constr3_entry"          -- cmm fun
  , "stg_interp_constr4_entry"          -- cmm fun
  , "stg_interp_constr5_entry"          -- cmm fun
  , "stg_interp_constr6_entry"          -- cmm fun
  , "stg_interp_constr7_entry"          -- cmm fun

  , "stg_SRT_1_info"                    -- cmm data
  , "stg_SRT_2_info"                    -- cmm data
  , "stg_SRT_3_info"                    -- cmm data
  , "stg_SRT_4_info"                    -- cmm data
  , "stg_SRT_5_info"                    -- cmm data
  , "stg_SRT_6_info"                    -- cmm data
  , "stg_SRT_7_info"                    -- cmm data
  , "stg_SRT_8_info"                    -- cmm data
  , "stg_SRT_9_info"                    -- cmm data
  , "stg_SRT_10_info"                   -- cmm data
  , "stg_SRT_11_info"                   -- cmm data
  , "stg_SRT_12_info"                   -- cmm data
  , "stg_SRT_13_info"                   -- cmm data
  , "stg_SRT_14_info"                   -- cmm data
  , "stg_SRT_15_info"                   -- cmm data
  , "stg_SRT_16_info"                   -- cmm data

  -- generated
  , "stg_sel_0_upd_info"                -- cmm fun/data
  , "stg_sel_1_upd_info"                -- cmm fun/data
  , "stg_sel_2_upd_info"                -- cmm fun/data
  , "stg_sel_3_upd_info"                -- cmm fun/data
  , "stg_sel_4_upd_info"                -- cmm fun/data
  , "stg_sel_5_upd_info"                -- cmm fun/data
  , "stg_sel_6_upd_info"                -- cmm fun/data
  , "stg_sel_7_upd_info"                -- cmm fun/data
  , "stg_sel_8_upd_info"                -- cmm fun/data
  , "stg_sel_9_upd_info"                -- cmm fun/data
  , "stg_sel_10_upd_info"               -- cmm fun/data
  , "stg_sel_11_upd_info"               -- cmm fun/data
  , "stg_sel_12_upd_info"               -- cmm fun/data
  , "stg_sel_13_upd_info"               -- cmm fun/data
  , "stg_sel_14_upd_info"               -- cmm fun/data
  , "stg_sel_15_upd_info"               -- cmm fun/data
  , "stg_sel_0_noupd_info"              -- cmm fun/data
  , "stg_sel_1_noupd_info"              -- cmm fun/data
  , "stg_sel_2_noupd_info"              -- cmm fun/data
  , "stg_sel_3_noupd_info"              -- cmm fun/data
  , "stg_sel_4_noupd_info"              -- cmm fun/data
  , "stg_sel_5_noupd_info"              -- cmm fun/data
  , "stg_sel_6_noupd_info"              -- cmm fun/data
  , "stg_sel_7_noupd_info"              -- cmm fun/data
  , "stg_sel_8_noupd_info"              -- cmm fun/data
  , "stg_sel_9_noupd_info"              -- cmm fun/data
  , "stg_sel_10_noupd_info"             -- cmm fun/data
  , "stg_sel_11_noupd_info"             -- cmm fun/data
  , "stg_sel_12_noupd_info"             -- cmm fun/data
  , "stg_sel_13_noupd_info"             -- cmm fun/data
  , "stg_sel_14_noupd_info"             -- cmm fun/data
  , "stg_sel_15_noupd_info"             -- cmm fun/data

  , "stg_ap_v_info"                     -- cmm fun/data
  , "stg_ap_f_info"                     -- cmm fun/data
  , "stg_ap_d_info"                     -- cmm fun/data
  , "stg_ap_l_info"                     -- cmm fun/data
  , "stg_ap_v16_info"                   -- cmm fun/data
  , "stg_ap_v32_info"                   -- cmm fun/data
  , "stg_ap_v64_info"                   -- cmm fun/data
  , "stg_ap_n_info"                     -- cmm fun/data
  , "stg_ap_p_info"                     -- cmm fun/data
  , "stg_ap_pv_info"                    -- cmm fun/data
  , "stg_ap_pp_info"                    -- cmm fun/data
  , "stg_ap_ppv_info"                   -- cmm fun/data
  , "stg_ap_ppp_info"                   -- cmm fun/data
  , "stg_ap_pppv_info"                  -- cmm fun/data
  , "stg_ap_pppp_info"                  -- cmm fun/data
  , "stg_ap_ppppp_info"                 -- cmm fun/data
  , "stg_ap_pppppp_info"                -- cmm fun/data
  , "stg_ap_0_fast"                     -- cmm fun
  , "stg_ap_v_fast"                     -- cmm fun
  , "stg_ap_f_fast"                     -- cmm fun
  , "stg_ap_d_fast"                     -- cmm fun
  , "stg_ap_l_fast"                     -- cmm fun
  , "stg_ap_v16_fast"                   -- cmm fun
  , "stg_ap_v32_fast"                   -- cmm fun
  , "stg_ap_v64_fast"                   -- cmm fun
  , "stg_ap_n_fast"                     -- cmm fun
  , "stg_ap_p_fast"                     -- cmm fun
  , "stg_ap_pv_fast"                    -- cmm fun
  , "stg_ap_pp_fast"                    -- cmm fun
  , "stg_ap_ppv_fast"                   -- cmm fun
  , "stg_ap_ppp_fast"                   -- cmm fun
  , "stg_ap_pppv_fast"                  -- cmm fun
  , "stg_ap_pppp_fast"                  -- cmm fun
  , "stg_ap_ppppp_fast"                 -- cmm fun
  , "stg_ap_pppppp_fast"                -- cmm fun
  , "stg_ap_1_upd_info"                 -- cmm fun/data
  , "stg_ap_2_upd_info"                 -- cmm fun/data
  , "stg_ap_3_upd_info"                 -- cmm fun/data
  , "stg_ap_4_upd_info"                 -- cmm fun/data
  , "stg_ap_5_upd_info"                 -- cmm fun/data
  , "stg_ap_6_upd_info"                 -- cmm fun/data
  , "stg_ap_7_upd_info"                 -- cmm fun/data

  -- primops: compact regions
  , "stg_compactAddWithSharingzh"       -- cmm fun
  , "stg_compactAddzh"                  -- cmm fun
  , "stg_compactNewzh"                  -- cmm fun
  , "stg_compactResizzezh"              -- cmm fun
  , "stg_compactContainszh"             -- cmm fun
  , "stg_compactContainsAnyzh"          -- cmm fun
  , "stg_compactGetFirstBlockzh"        -- cmm fun
  , "stg_compactGetNextBlockzh"         -- cmm fun
  , "stg_compactAllocateBlockzh"        -- cmm fun
  , "stg_compactFixupPointerszh"        -- cmm fun
  , "stg_compactSizzezh"                -- cmm fun

  -- primops: weak pointers
  , "stg_mkWeakzh"                      -- cmm fun
  , "stg_mkWeakNoFinalizzerzh"          -- cmm fun
  , "stg_addCFinalizzerToWeakzh"        -- cmm fun
  , "stg_finalizzeWeakzh"               -- cmm fun
  , "stg_deRefWeakzh"                   -- cmm fun

  -- primops: stable pointer
  , "stg_deRefStablePtrzh"              -- cmm fun
  , "stg_makeStablePtrzh"               -- cmm fun

  -- primops: stable name
  , "stg_makeStableNamezh"              -- cmm fun

  -- primops: STM
  , "stg_atomicallyzh"                  -- cmm fun
  , "stg_catchRetryzh"                  -- cmm fun
  , "stg_catchSTMzh"                    -- cmm fun
  , "stg_newTVarzh"                     -- cmm fun
  , "stg_readTVarzh"                    -- cmm fun
  , "stg_readTVarIOzh"                  -- cmm fun
  , "stg_writeTVarzh"                   -- cmm fun
  , "stg_retryzh"                       -- cmm fun

  -- primops: exceptions
  , "stg_getMaskingStatezh"             -- cmm fun
  , "stg_maskAsyncExceptionszh"         -- cmm fun
  , "stg_maskUninterruptiblezh"         -- cmm fun
  , "stg_catchzh"                       -- cmm fun
  , "stg_raisezh"                       -- cmm fun
  , "stg_raiseDivZZerozh"               -- cmm fun
  , "stg_raiseUnderflowzh"              -- cmm fun
  , "stg_raiseOverflowzh"               -- cmm fun
  , "stg_raiseIOzh"                     -- cmm fun
  , "stg_unmaskAsyncExceptionszh"       -- cmm fun

  -- primops: misc, etc
  , "stg_clearCCSzh"                    -- cmm fun
  , "stg_traceCcszh"                    -- cmm fun
  , "stg_traceEventzh"                  -- cmm fun
  , "stg_traceMarkerzh"                 -- cmm fun
  , "stg_traceBinaryEventzh"            -- cmm fun

  -- primops: double
  , "stg_decodeDoublezu2Intzh"          -- cmm fun
  , "stg_decodeDoublezuInt64zh"         -- cmm fun

  -- primops: float
  , "stg_decodeFloatzuIntzh"            -- cmm fun

  -- primops: delay wait
  , "stg_delayzh"                       -- cmm fun
  , "stg_waitReadzh"                    -- cmm fun
  , "stg_waitWritezh"                   -- cmm fun

  -- primops: concurrency
  , "stg_forkzh"                        -- cmm fun
  , "stg_forkOnzh"                      -- cmm fun
  , "stg_isCurrentThreadBoundzh"        -- cmm fun
  , "stg_killThreadzh"                  -- cmm fun
  , "stg_labelThreadzh"                 -- cmm fun
  , "stg_noDuplicatezh"                 -- cmm fun
  , "stg_threadStatuszh"                -- cmm fun
  , "stg_yieldzh"                       -- cmm fun

  -- primops: GHCi bytecode
  , "stg_unpackClosurezh"               -- cmm fun
  , "stg_closureSizzezh"                -- cmm fun
  , "stg_getApStackValzh"               -- cmm fun
  , "stg_mkApUpd0zh"                    -- cmm fun
  , "stg_newBCOzh"                      -- cmm fun

  -- primops: parallelism
  , "stg_getSparkzh"                    -- cmm fun
  , "stg_numSparkszh"                   -- cmm fun

  -- primops: mvar
  , "stg_isEmptyMVarzh"                 -- cmm fun
  , "stg_newMVarzh"                     -- cmm fun
  , "stg_putMVarzh"                     -- cmm fun
  , "stg_tryPutMVarzh"                  -- cmm fun
  , "stg_tryTakeMVarzh"                 -- cmm fun
  , "stg_tryReadMVarzh"                 -- cmm fun
  , "stg_takeMVarzh"                    -- cmm fun
  , "stg_readMVarzh"                    -- cmm fun

  -- primops: array
  , "stg_newArrayzh"                    -- cmm fun
  , "stg_copyArrayzh"                   -- cmm fun
  , "stg_copyMutableArrayzh"            -- cmm fun
  , "stg_cloneArrayzh"                  -- cmm fun
  , "stg_cloneMutableArrayzh"           -- cmm fun
  , "stg_freezzeArrayzh"                -- cmm fun
  , "stg_thawArrayzh"                   -- cmm fun
  , "stg_casArrayzh"                    -- cmm fun
  , "stg_copyArray_barrier"             -- cmm fun
  , "stg_unsafeThawArrayzh"             -- cmm fun

  -- primops: array array
  , "stg_copyArrayArrayzh"              -- cmm fun
  , "stg_copyMutableArrayArrayzh"       -- cmm fun
  , "stg_newArrayArrayzh"               -- cmm fun

  -- primops: small array
  , "stg_newSmallArrayzh"               -- cmm fun
  , "stg_unsafeThawSmallArrayzh"        -- cmm fun
  , "stg_cloneSmallArrayzh"             -- cmm fun
  , "stg_cloneSmallMutableArrayzh"      -- cmm fun
  , "stg_freezzeSmallArrayzh"           -- cmm fun
  , "stg_thawSmallArrayzh"              -- cmm fun
  , "stg_copySmallArrayzh"              -- cmm fun
  , "stg_copySmallMutableArrayzh"       -- cmm fun
  , "stg_casSmallArrayzh"               -- cmm fun
  , "stg_shrinkSmallMutableArrayzh"     -- cmm fun

  -- primops: bytearray
  , "stg_newByteArrayzh"                -- cmm fun
  , "stg_casIntArrayzh"                 -- cmm fun
  , "stg_newPinnedByteArrayzh"          -- cmm fun
  , "stg_newAlignedPinnedByteArrayzh"   -- cmm fun
  , "stg_isByteArrayPinnedzh"           -- cmm fun
  , "stg_isMutableByteArrayPinnedzh"    -- cmm fun
  , "stg_shrinkMutableByteArrayzh"      -- cmm fun
  , "stg_resizzeMutableByteArrayzh"     -- cmm fun

  -- primops: mutvar
  , "stg_newMutVarzh"                   -- cmm fun
  , "stg_atomicModifyMutVar2zh"         -- cmm fun
  , "stg_atomicModifyMutVarzuzh"        -- cmm fun
  , "stg_casMutVarzh"                   -- cmm fun

  -- primops: Synchronized I/O Ports
  , "stg_readIOPortzh"                  -- cmm fun
  , "stg_writeIOPortzh"                 -- cmm fun
  , "stg_newIOPortzh"                   -- cmm fun

  , "stg_paniczh"                       -- cmm fun
  , "stg_getThreadAllocationCounterzh"  -- cmm fun
  , "stg_setThreadAllocationCounterzh"  -- cmm fun
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
