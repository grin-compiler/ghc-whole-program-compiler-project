{-# LANGUAGE LambdaCase, QuasiQuotes, OverloadedStrings #-}
module Lambda.GHC.RtsAbstractModel where

import Lambda.Syntax
import Lambda.TH

rtsModel :: Program
rtsModel = [prog|

    primop effectful
      "newArray#" :: (T_Int64) @ t_624 -> %a_0 -> {"State#" %s_0} @ t_625 -> {"ghc-prim_GHC.Prim.Solo#" {"MutableArray#" %s_0 %a_0} @ t_626} @ t_627
      "unsafeFreezeArray#" :: {"MutableArray#" %s_5 %a_7} @ t_646 -> {"State#" %s_5} @ t_647 -> {"ghc-prim_GHC.Prim.Solo#" {"Array#" %a_7} @ t_648} @ t_649
      "raise#"    :: %b_1 -> %o_0

    constructors

      data base_GHC.Int.Int32
        "base_GHC.Int.I32#" Int64Rep

      data ghc-prim_GHC.Types.Int
        "ghc-prim_GHC.Types.I#" Int64Rep

      data base_GHC.Ptr.Ptr
        base_GHC.Ptr.Ptr AddrRep

      data "ghc-prim_GHC.Tuple.(,)"
        "ghc-prim_GHC.Tuple.(,)" LiftedRep LiftedRep

    ghc_rts_abstract_model =
      letS
        -- raise all exceptions possible from RTS

        r_ex00 = "raise#" $ "ghc-prim_GHC.Prim.Panic.absentSumFieldError"
        r_ex00 = "raise#" $ "ghc-prim_GHC.Prim.Exception.raiseUnderflow"
        r_ex00 = "raise#" $ "ghc-prim_GHC.Prim.Exception.raiseOverflow"
        r_ex00 = "raise#" $ "ghc-prim_GHC.Prim.Exception.raiseDivZero"
        r_ex01 = "raise#" $ base_Control.Exception.Base.nestedAtomically
        r_ex02 = "raise#" $ base_Control.Exception.Base.nonTermination
        r_ex03 = "raise#" $ base_GHC.Exception.Type.divZeroException
        r_ex04 = "raise#" $ base_GHC.Exception.Type.overflowException
        r_ex05 = "raise#" $ base_GHC.Exception.Type.underflowException
        r_ex06 = "raise#" $ base_GHC.IO.Exception.allocationLimitExceeded
        r_ex07 = "raise#" $ base_GHC.IO.Exception.blockedIndefinitelyOnMVar
        r_ex08 = "raise#" $ base_GHC.IO.Exception.blockedIndefinitelyOnSTM
        r_ex09 = "raise#" $ base_GHC.IO.Exception.cannotCompactFunction
        r_ex10 = "raise#" $ base_GHC.IO.Exception.cannotCompactMutable
        r_ex11 = "raise#" $ base_GHC.IO.Exception.cannotCompactPinned
        r_ex12 = "raise#" $ base_GHC.IO.Exception.heapOverflow
        r_ex13 = "raise#" $ base_GHC.IO.Exception.stackOverflow
        r_ex14 = "raise#" $ base_GHC.Event.Thread.blockedOnBadFD
        r_ex14 = "raise#" $ base_GHC.IOPort.doubleReadException

        -- create all data constuctors possible from RTS

        r_w64   = #T_Word64 0
        r_i64   = #T_Int64 0
        r_addr  = #T_Addr NullAddr
        r_f64   = #T_Double 0 % 1
        r_f32   = #T_Float 0 % 1

        r_con00 = "base_GHC.Int.I16#" $ r_i64
        r_con01 = "base_GHC.Int.I32#" $ r_i64
        r_con02 = "base_GHC.Int.I64#" $ r_i64
        r_con03 = "base_GHC.Int.I8#"  $ r_i64
        r_con04 = "base_GHC.Word.W16#" $ r_w64
        r_con05 = "base_GHC.Word.W32#" $ r_w64
        r_con06 = "base_GHC.Word.W64#" $ r_w64
        r_con07 = "base_GHC.Word.W8#"  $ r_w64
        r_con08 = "base_GHC.Ptr.FunPtr" $ r_addr
        r_con09 = "base_GHC.Ptr.Ptr"    $ r_addr
        r_con10 = "base_GHC.Stable.StablePtr" $ r_addr

        r_con11 = "ghc-prim_GHC.Types.C#" $ r_w64
        r_con12 = "ghc-prim_GHC.Types.D#" $ r_f64
        r_con13 = "ghc-prim_GHC.Types.F#" $ r_f32
        r_con14 = "ghc-prim_GHC.Types.I#" $ r_i64
        r_con15 = "ghc-prim_GHC.Types.W#" $ r_w64
        r_con16 = "ghc-prim_GHC.Types.False" $
        r_con17 = "ghc-prim_GHC.Types.True" $
        r_con18 = "ghc-prim_GHC.Tuple.()" $

        -- call all functions possible from RTS

        r_boxed_int32 = ["base_GHC.Int.I32#" r_i64]
        r_boxed_int   = ["ghc-prim_GHC.Types.I#" r_i64]
        r_ptr         = ["base_GHC.Ptr.Ptr" r_addr]
        r_void        = #T_Token "ghc-prim_GHC.Prim.void#"
        r_unboxed_tup0 = ["ghc-prim_GHC.Prim.(##)"]

      let
        r_closure_pure = \closure [] ->
          r_ptr
        r_closure_io = \closure [] r_arg00 ->
          r_ptr
        r_closure_io_ret_utup0 = \closure [] r_arg01 ->
          r_unboxed_tup0

      letS
        r_fun00 = "base_GHC.Conc.IO.ensureIOManagerIsRunning" $ r_void
        r_fun01 = "base_GHC.Conc.IO.ioManagerCapabilitiesChanged" $ r_void
        r_fun02 = "base_GHC.Conc.Signal.runHandlersPtr" $ r_ptr r_boxed_int32 r_void
        r_fun03 = "base_GHC.Conc.Sync.runSparks" $ r_void
        r_fun04 = "base_GHC.Pack.unpackCString" $ r_ptr
        r_fun05 = "base_GHC.TopHandler.flushStdHandles" $ r_void
        r_fun06 = "base_GHC.TopHandler.runIO" $ r_closure_io r_void
        r_fun07 = "base_GHC.TopHandler.runMainIO" $ r_closure_io r_void
        r_fun08 = "base_GHC.TopHandler.runNonIO"  $ r_closure_pure r_void
        --r_fun90 = "base_GHC.Event.Windows.processRemoteCompletion" $ r_void
        r_fun91 = "base_GHC.Conc.IO.interruptIOManager" $ r_void

        -- weak ptr related: needs more utility code

        r_realworld = #T_Token "ghc-prim_GHC.Prim.realWorld#"
        r_wp_arr01 = "newArray#" $ r_i64 r_closure_io_ret_utup0 r_realworld
        r_wp_res01 = case r_wp_arr01 of
          ("ghc-prim_GHC.Prim.Solo#" r_wp_arr02) @ r_wp_alt01 ->
            letS
              r_wp_arr03 = "unsafeFreezeArray#" $ r_wp_arr02 r_realworld
              r_wp_res02 = case r_wp_arr03 of
                ("ghc-prim_GHC.Prim.Solo#" r_wp_arr04) @ r_wp_alt02 ->
                  letS
                    r_fun09 = "base_GHC.Weak.runFinalizerBatch" $ r_boxed_int r_wp_arr04 r_void
                  r_fun09
            r_wp_res02

        -- call the main function
        r_fun10 = "main_:Main.main" $ r_void
      r_fun10

    -- code for tuple2Proj0 = \t -> case t of GHC.Tuple.(,) a b -> a
    tuple2Proj0 (p_arg0 : LiftedRep) =
      letS
        p_v00 = p_arg0 $
        p_result00 = case p_v00 of
          ("ghc-prim_GHC.Tuple.(,)" p_tup_arg_0 p_tup_arg_1) @ p_alt00 ->
            p_tup_arg_0
      p_result00

    -- top level values from ghc-prim_GHC.Prim module with VoidRep
    "ghc-prim_GHC.Prim.void#" =
      letS
        x01 = #T_Token "ghc-prim_GHC.Prim.void#"
      x01
    "ghc-prim_GHC.Prim.realWorld#" =
      letS
        x02 = #T_Token "ghc-prim_GHC.Prim.realWorld#"
      x02
    "ghc-prim_GHC.Prim.coercionToken#" =
      letS
        x03 = #T_Token "ghc-prim_GHC.Prim.coercionToken#"
      x03
    "ghc-prim_GHC.Prim.proxy#" =
      letS
        x04 = #T_Token "ghc-prim_GHC.Prim.proxy#"
      x04
    "ghc-prim_GHC.Prim.(##)" =
      letS
        x05 = #T_Token "ghc-prim_GHC.Prim.(##)"
      x05
  |]
