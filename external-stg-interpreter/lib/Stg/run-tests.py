#!/usr/bin/env python3

import glob, os, os.path
import subprocess, shlex

skip_set = set(
  [ '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/allowinterrupt001.run/allowinterrupt001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/AtomicPrimops.run/AtomicPrimops.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc004.run/conc004.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc007.run/conc007.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc012.run/conc012.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc034.run/conc034.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc051.run/conc051.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc065.run/conc065.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc066.run/conc066.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/conc067.run/conc067.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/readMVar1.run/readMVar1.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T1980.run/T1980.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T3279.run/T3279.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T3429.run/T3429.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/throwto003.run/throwto003.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/tryReadMVar2.run/tryReadMVar2.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/deriving/should_run/T11535.run/T11535.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ffi/should_run/ffi020.run/ffi020.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ffi/should_run/fptr02.run/fptr02.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/process/tests/T8343.run/T8343.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/numeric/should_run/arith011.run/arith011.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/numeric/should_run/CarryOverflow.run/CarryOverflow.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/numeric/should_run/T8726.run/T8726.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/ArithInt16.run/ArithInt16.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/ArithInt8.run/ArithInt8.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/ArithWord16.run/ArithWord16.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/ArithWord8.run/ArithWord8.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/CmpInt8.run/CmpInt8.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/CmpWord8.run/CmpWord8.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/should_run/T10678.run/T10678.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/return_mem_to_os.run/return_mem_to_os.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/stack001.run/stack001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/stack002.run/stack002.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/T2783.run/T2783.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/simplrun004.run/simplrun004.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/simplrun005.run/simplrun005.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T3437.run/T3437.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T5920.run/T5920.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T5997.run/T5997.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/typecheck/should_run/StrictPats.run/StrictPats.o_ghc_stgapp'

  # to check

  # has stubs
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/cgrun078.run/cgrun078.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/driver/T16737.run/T16737.o_ghc_stgapp'

  # has program args

  # missing feature Static Pointers (cloud haskell) ; undefined symbol: hs_spt_lookup
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/CgStaticPointers.run/CgStaticPointers.o_ghc_stgapp'

  # timeout
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/array/should_run/arr017.run/arr017.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/callarity/unittest/CallArity1.run/CallArity1.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/cgrun022.run/cgrun022.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/cgrun058.run/cgrun058.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/compareByteArrays.run/compareByteArrays.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T15892.run/T15892.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T16846.run/T16846.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T3677.run/T3677.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T9001.run/T9001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/should_run/T9340.run/T9340.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/should_run/T367_letnoescape.run/T367_letnoescape.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/driver/T5313.run/T5313.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/Concurrent/Chan002.run/Chan002.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/Concurrent/Chan003.run/Chan003.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/CPUTime001.run/CPUTime001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/dynamic003.run/dynamic003.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/dynamic004.run/dynamic004.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/dynamic005.run/dynamic005.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/echo001.run/echo001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/inits.run/inits.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/encoding001.run/encoding001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/encoding004.run/encoding004.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/finalization001.run/finalization001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hGetBuf001.run/hGetBuf001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hSeek003.run/hSeek003.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hSetBuffering002.run/hSetBuffering002.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hSetBuffering003.run/hSetBuffering003.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/hSetBuffering004.run/hSetBuffering004.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/isEOF001.run/isEOF001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/IO/openFile008.run/openFile008.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/ioref001.run/ioref001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/length001.run/length001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/list003.run/list003.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/memo001.run/memo001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/stableptr001.run/stableptr001.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T13191.run/T13191.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T17499.run/T17499.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T3474.run/T3474.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T7653.run/T7653.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T8766.run/T8766.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T9532.run/T9532.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T9848.run/T9848.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/unicode002.run/unicode002.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/tests/T8108.run/T8108.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/parser/should_run/CountParserDeps.run/CountParserDeps.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/join_points/join002.run/join002.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/join_points/join003.run/join003.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/join_points/join007.run/join007.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/space_leaks/T4334.run/T4334.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/10queens/10queens.run/10queens.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/andy_cherry/andy_cherry.run/andy_cherry.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/barton-mangler-bug/barton-mangler-bug.run/barton-mangler-bug.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/galois_raytrace/galois_raytrace.run/galois_raytrace.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/jl_defaults/jl_defaults.run/jl_defaults.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/jules_xref2/jules_xref2.run/jules_xref2.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/jules_xref/jules_xref.run/jules_xref.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/life_space_leak/life_space_leak.run/life_space_leak.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/seward-space-leak/seward-space-leak.run/seward-space-leak.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/thurston-modular-arith/thurston-modular-arith.run/thurston-modular-arith.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quasiquotation/T7918.run/T7918.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/T3403.run/T3403.o_ghc_stgapp'

  # not enough memory
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/space_leaks/space_leak_001.run/space_leak_001.o_ghc_stgapp'

  # needs STM primops
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/tests/T12852.run/T12852.o_ghc_stgapp'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/stranal/should_run/T14171.run/T14171.o_ghc_stgapp'

  # needs RTS option parsing and heap mem limit support
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/should_run/simplrun010.run/simplrun010.o_ghc_stgapp'

  # needs detection of MVar blocked indefinitely
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/mdo/should_fail/mdofail006.run/mdofail006.o_ghc_stgapp'

  # bug
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/process/tests/process011.run/process011.o_ghc_stgapp'
  ])

################### OK (ALL)
#OK: 644  ALL: 714
test_paths = [
    '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ado/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/array/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/arrows/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/boxy/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/codeGen/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/cpranal/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/deriving/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/deSugar/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/determinism/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/driver/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/gadt/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/generics/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/indexed-types/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/lib/integer/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/array/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/process/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/mdo/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/numeric/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/overloadedlists/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/overloadedrecflds/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/overloadedstrings/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/parser/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/patsyn/'
#  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/perf/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/polykinds/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/primops/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/programs/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quantified-constraints/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quasiquotation/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/quotes/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rebindable/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/safeHaskell/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplCore/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/simplStg/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/stranal/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/th/'
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/typecheck/'

  #OK, but needs args; checked manually
  , '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/callarity/'
  ]

################ UNIMPLEMENTED / WILL NOT BE IMPLEMENTED

#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ghci/'
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/ghc-compact/'
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/ghc-heap/'
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/stm/'

################ BUGS

#OK: 21 FAIL: 46 (many bugs / unimplemented)
#test_paths = ['/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/concurrent/']

#OK: 147  FAIL: 23 (many bugs and unimplemented)
#test_paths = ['/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/base/']

#OK: 17  FAIL: 10 (many unimplemented)
#test_paths = ['/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/libraries/unix/']

# to check

#test_paths = ['/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/ffi/']
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/profiling/'
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/rts/'
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/unboxedsums/'


# this is the whole testsuite
#test_path = '/home/csaba/haskell/grin-compiler/ghc-whole-program-compiler-project/ghc-wpc/testsuite/tests/'

test_stderr_set = set()
for test_path in test_paths:
  print('Scanning:', test_path)
  test_stderr_set.update(set(glob.glob(test_path + "**/*.run.stderr", recursive=True)))

test_stderr_list = list(sorted(test_stderr_set))

print('Found:', len(test_stderr_list), 'tests\n')

ok_set = set()
fail_set = set()
timeout_set = set()

for test_stderr in test_stderr_list:
  test_run, _ = os.path.splitext(test_stderr)
  test_stdout = test_run + ".stdout"
  test_name, _ = os.path.splitext(test_run)
  test_stdin = test_run + ".stdin"
  test_args_path = test_run + ".args"

  ghcstgapp_path = ''
  stgapps = glob.glob(test_name + ".*_ghc_stgapp")
  if len(stgapps) == 1:
    ghcstgapp_path = stgapps[0]
  elif len(stgapps) == 0:
    print('missing ghc_stgapp:', test_name + ".*_ghc_stgapp")
    continue
  else:
    print('ambiguous ghc_stgapp:', stgapps)
    continue

  print("")
  print(ghcstgapp_path)

  if ghcstgapp_path in skip_set:
    print("SKIP: in skip set")
    continue

  test_args = ''
  if os.path.isfile(test_args_path):
    with open(test_args_path, "rt") as f:
      test_args = f.read()
      print('args:', test_args)

  test_input = None
  if os.path.isfile(test_stdin):
    with open(test_stdin, "rb") as f:
      test_input = f.read()

  with open(test_stdout, "rb") as f:
    expected_stdout = f.read()

  with open(test_stderr, "rb") as f:
    expected_stderr = f.read()

  # run ext-stg interpreter
  try:
    result = subprocess.run(['stack', 'exec', 'ext-stg-interpreter', '--', '-cwd', ghcstgapp_path, '+RTS', '-M2G', '-RTS'] + shlex.split(test_args),
                            input=test_input,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE,
                            timeout=60
                            )
  except subprocess.TimeoutExpired as e:
    print(e)
    timeout_set.add(ghcstgapp_path)
    continue

  stg_stdout = result.stdout
  stg_stderr = result.stderr

  ok = True
  if expected_stdout != stg_stdout:
    print('FAIL: stdout mistmatch')
    print(' expected: ', expected_stdout)
    print(' got     : ', stg_stdout)
    ok = False

  if expected_stderr != stg_stderr:
    print('FAIL: stderr mistmatch')
    print(' expected: ', expected_stderr)
    print(' got     : ', stg_stderr)
    ok = False

  if ok:
    print('OK')
    ok_set.add(ghcstgapp_path)
  else:
    fail_set.add(ghcstgapp_path)

print('\n\nSummary:')
print(' OK:      ', len(ok_set))
print(' FAIL:    ', len(fail_set))
print(' TIMEOUT: ', len(timeout_set))

print('\ntimeout list:')
for n in list(sorted(timeout_set)):
  print("  , '" + n + "'")


"""
  TODO:
    - some test have arguments, pass them to interpreter
    done - set the working directory where the tests are
"""
