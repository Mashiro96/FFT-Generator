circuit FFT :
  module ComplexAdd :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>
  
    node _T = add(io_op1_re, io_op2_re) @[Modules.scala 19:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 19:26]
    node _T_2 = asSInt(_T_1) @[Modules.scala 19:26]
    node _T_3 = add(io_op1_im, io_op2_im) @[Modules.scala 20:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 20:26]
    node _T_5 = asSInt(_T_4) @[Modules.scala 20:26]
    io_res_re <= _T_2 @[Modules.scala 19:13]
    io_res_im <= _T_5 @[Modules.scala 20:13]

  module ComplexSub :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>
  
    node _T = sub(io_op1_re, io_op2_re) @[Modules.scala 32:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 32:26]
    node _T_2 = asSInt(_T_1) @[Modules.scala 32:26]
    node _T_3 = sub(io_op1_im, io_op2_im) @[Modules.scala 33:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 33:26]
    node _T_5 = asSInt(_T_4) @[Modules.scala 33:26]
    io_res_re <= _T_2 @[Modules.scala 32:13]
    io_res_im <= _T_5 @[Modules.scala 33:13]

  module ComplexTran :
    input clock : Clock
    input reset : UInt<1>
    input io_in_re : SInt<32>
    input io_in_im : SInt<32>
    output io_out_re : SInt<32>
    output io_out_im : SInt<32>
  
    node _T = sub(shl(asSInt(UInt<1>("h0")), 16), io_in_re) @[Modules.scala 49:16]
    node _T_1 = tail(_T, 1) @[Modules.scala 49:16]
    node _T_2 = asSInt(_T_1) @[Modules.scala 49:16]
    io_out_re <= io_in_im @[Modules.scala 48:13]
    io_out_im <= _T_2 @[Modules.scala 49:13]

  module ButterflyAdd :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_re : SInt<32>
    input io_in_0_im : SInt<32>
    input io_in_1_re : SInt<32>
    input io_in_1_im : SInt<32>
    input io_in_2_re : SInt<32>
    input io_in_2_im : SInt<32>
    input io_in_3_re : SInt<32>
    input io_in_3_im : SInt<32>
    output io_out_0_re : SInt<32>
    output io_out_0_im : SInt<32>
    output io_out_1_re : SInt<32>
    output io_out_1_im : SInt<32>
    output io_out_2_re : SInt<32>
    output io_out_2_im : SInt<32>
    output io_out_3_re : SInt<32>
    output io_out_3_im : SInt<32>
  
    inst ComplexAdd of ComplexAdd @[Modules.scala 24:22]
    inst ComplexAdd_1 of ComplexAdd @[Modules.scala 24:22]
    inst ComplexAdd_2 of ComplexAdd @[Modules.scala 24:22]
    inst ComplexAdd_3 of ComplexAdd @[Modules.scala 24:22]
    inst ComplexAdd_4 of ComplexAdd @[Modules.scala 24:22]
    inst ComplexSub of ComplexSub @[Modules.scala 37:22]
    inst ComplexSub_1 of ComplexSub @[Modules.scala 37:22]
    inst ComplexSub_2 of ComplexSub @[Modules.scala 37:22]
    inst ComplexTran of ComplexTran @[Modules.scala 53:22]
    inst ComplexAdd_5 of ComplexAdd @[Modules.scala 24:22]
    inst ComplexSub_3 of ComplexSub @[Modules.scala 37:22]
    inst ComplexSub_4 of ComplexSub @[Modules.scala 37:22]
    inst ComplexTran_1 of ComplexTran @[Modules.scala 53:22]
    inst ComplexSub_5 of ComplexSub @[Modules.scala 37:22]
    io_out_0_re <= ComplexAdd_2.io_res_re @[Modules.scala 88:15]
    io_out_0_im <= ComplexAdd_2.io_res_im @[Modules.scala 88:15]
    io_out_1_re <= ComplexSub.io_res_re @[Modules.scala 89:15]
    io_out_1_im <= ComplexSub.io_res_im @[Modules.scala 89:15]
    io_out_2_re <= ComplexAdd_5.io_res_re @[Modules.scala 90:15]
    io_out_2_im <= ComplexAdd_5.io_res_im @[Modules.scala 90:15]
    io_out_3_re <= ComplexSub_5.io_res_re @[Modules.scala 91:15]
    io_out_3_im <= ComplexSub_5.io_res_im @[Modules.scala 91:15]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io_op1_re <= io_in_0_re @[Modules.scala 25:17]
    ComplexAdd.io_op1_im <= io_in_0_im @[Modules.scala 25:17]
    ComplexAdd.io_op2_re <= io_in_2_re @[Modules.scala 26:17]
    ComplexAdd.io_op2_im <= io_in_2_im @[Modules.scala 26:17]
    ComplexAdd_1.clock <= clock
    ComplexAdd_1.reset <= reset
    ComplexAdd_1.io_op1_re <= io_in_1_re @[Modules.scala 25:17]
    ComplexAdd_1.io_op1_im <= io_in_1_im @[Modules.scala 25:17]
    ComplexAdd_1.io_op2_re <= io_in_3_re @[Modules.scala 26:17]
    ComplexAdd_1.io_op2_im <= io_in_3_im @[Modules.scala 26:17]
    ComplexAdd_2.clock <= clock
    ComplexAdd_2.reset <= reset
    ComplexAdd_2.io_op1_re <= ComplexAdd.io_res_re @[Modules.scala 25:17]
    ComplexAdd_2.io_op1_im <= ComplexAdd.io_res_im @[Modules.scala 25:17]
    ComplexAdd_2.io_op2_re <= ComplexAdd_1.io_res_re @[Modules.scala 26:17]
    ComplexAdd_2.io_op2_im <= ComplexAdd_1.io_res_im @[Modules.scala 26:17]
    ComplexAdd_3.clock <= clock
    ComplexAdd_3.reset <= reset
    ComplexAdd_3.io_op1_re <= io_in_0_re @[Modules.scala 25:17]
    ComplexAdd_3.io_op1_im <= io_in_0_im @[Modules.scala 25:17]
    ComplexAdd_3.io_op2_re <= io_in_2_re @[Modules.scala 26:17]
    ComplexAdd_3.io_op2_im <= io_in_2_im @[Modules.scala 26:17]
    ComplexAdd_4.clock <= clock
    ComplexAdd_4.reset <= reset
    ComplexAdd_4.io_op1_re <= io_in_1_re @[Modules.scala 25:17]
    ComplexAdd_4.io_op1_im <= io_in_1_im @[Modules.scala 25:17]
    ComplexAdd_4.io_op2_re <= io_in_3_re @[Modules.scala 26:17]
    ComplexAdd_4.io_op2_im <= io_in_3_im @[Modules.scala 26:17]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io_op1_re <= ComplexAdd_3.io_res_re @[Modules.scala 38:17]
    ComplexSub.io_op1_im <= ComplexAdd_3.io_res_im @[Modules.scala 38:17]
    ComplexSub.io_op2_re <= ComplexAdd_4.io_res_re @[Modules.scala 39:17]
    ComplexSub.io_op2_im <= ComplexAdd_4.io_res_im @[Modules.scala 39:17]
    ComplexSub_1.clock <= clock
    ComplexSub_1.reset <= reset
    ComplexSub_1.io_op1_re <= io_in_0_re @[Modules.scala 38:17]
    ComplexSub_1.io_op1_im <= io_in_0_im @[Modules.scala 38:17]
    ComplexSub_1.io_op2_re <= io_in_2_re @[Modules.scala 39:17]
    ComplexSub_1.io_op2_im <= io_in_2_im @[Modules.scala 39:17]
    ComplexSub_2.clock <= clock
    ComplexSub_2.reset <= reset
    ComplexSub_2.io_op1_re <= io_in_1_re @[Modules.scala 38:17]
    ComplexSub_2.io_op1_im <= io_in_1_im @[Modules.scala 38:17]
    ComplexSub_2.io_op2_re <= io_in_3_re @[Modules.scala 39:17]
    ComplexSub_2.io_op2_im <= io_in_3_im @[Modules.scala 39:17]
    ComplexTran.clock <= clock
    ComplexTran.reset <= reset
    ComplexTran.io_in_re <= ComplexSub_2.io_res_re @[Modules.scala 54:16]
    ComplexTran.io_in_im <= ComplexSub_2.io_res_im @[Modules.scala 54:16]
    ComplexAdd_5.clock <= clock
    ComplexAdd_5.reset <= reset
    ComplexAdd_5.io_op1_re <= ComplexSub_1.io_res_re @[Modules.scala 25:17]
    ComplexAdd_5.io_op1_im <= ComplexSub_1.io_res_im @[Modules.scala 25:17]
    ComplexAdd_5.io_op2_re <= ComplexTran.io_out_re @[Modules.scala 26:17]
    ComplexAdd_5.io_op2_im <= ComplexTran.io_out_im @[Modules.scala 26:17]
    ComplexSub_3.clock <= clock
    ComplexSub_3.reset <= reset
    ComplexSub_3.io_op1_re <= io_in_0_re @[Modules.scala 38:17]
    ComplexSub_3.io_op1_im <= io_in_0_im @[Modules.scala 38:17]
    ComplexSub_3.io_op2_re <= io_in_2_re @[Modules.scala 39:17]
    ComplexSub_3.io_op2_im <= io_in_2_im @[Modules.scala 39:17]
    ComplexSub_4.clock <= clock
    ComplexSub_4.reset <= reset
    ComplexSub_4.io_op1_re <= io_in_1_re @[Modules.scala 38:17]
    ComplexSub_4.io_op1_im <= io_in_1_im @[Modules.scala 38:17]
    ComplexSub_4.io_op2_re <= io_in_3_re @[Modules.scala 39:17]
    ComplexSub_4.io_op2_im <= io_in_3_im @[Modules.scala 39:17]
    ComplexTran_1.clock <= clock
    ComplexTran_1.reset <= reset
    ComplexTran_1.io_in_re <= ComplexSub_4.io_res_re @[Modules.scala 54:16]
    ComplexTran_1.io_in_im <= ComplexSub_4.io_res_im @[Modules.scala 54:16]
    ComplexSub_5.clock <= clock
    ComplexSub_5.reset <= reset
    ComplexSub_5.io_op1_re <= ComplexSub_3.io_res_re @[Modules.scala 38:17]
    ComplexSub_5.io_op1_im <= ComplexSub_3.io_res_im @[Modules.scala 38:17]
    ComplexSub_5.io_op2_re <= ComplexTran_1.io_out_re @[Modules.scala 39:17]
    ComplexSub_5.io_op2_im <= ComplexTran_1.io_out_im @[Modules.scala 39:17]

  module Switch :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_re : SInt<32>
    input io_in_0_im : SInt<32>
    input io_in_1_re : SInt<32>
    input io_in_1_im : SInt<32>
    input io_in_2_re : SInt<32>
    input io_in_2_im : SInt<32>
    input io_in_3_re : SInt<32>
    input io_in_3_im : SInt<32>
    output io_out_0_re : SInt<32>
    output io_out_0_im : SInt<32>
    output io_out_1_re : SInt<32>
    output io_out_1_im : SInt<32>
    output io_out_2_re : SInt<32>
    output io_out_2_im : SInt<32>
    output io_out_3_re : SInt<32>
    output io_out_3_im : SInt<32>
    input io_sel : UInt<2>
  
    node _T_1 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_2 = _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 139:71]
    node _T_4 = asSInt(_T_3) @[Modules.scala 139:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 139:71]
    node _T_6 = asSInt(_T_5) @[Modules.scala 139:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_9 = _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 139:71]
    node _T_11 = asSInt(_T_10) @[Modules.scala 139:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 139:71]
    node _T_13 = asSInt(_T_12) @[Modules.scala 139:71]
    node _T_15 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_16 = _T_15
    node _T_17 = bits(_T_16, 31, 0) @[Modules.scala 139:71]
    node _T_18 = asSInt(_T_17) @[Modules.scala 139:71]
    node _T_19 = bits(_T_16, 63, 32) @[Modules.scala 139:71]
    node _T_20 = asSInt(_T_19) @[Modules.scala 139:71]
    node _T_22 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_23 = _T_22
    node _T_24 = bits(_T_23, 31, 0) @[Modules.scala 139:71]
    node _T_25 = asSInt(_T_24) @[Modules.scala 139:71]
    node _T_26 = bits(_T_23, 63, 32) @[Modules.scala 139:71]
    node _T_27 = asSInt(_T_26) @[Modules.scala 139:71]
    reg _T_28_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_28_re) @[Reg.scala 15:16]
    reg _T_28_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_28_im) @[Reg.scala 15:16]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_1_im, _T_28_im) @[Reg.scala 16:19]
    node _GEN_1 = mux(UInt<1>("h1"), io_in_1_re, _T_28_re) @[Reg.scala 16:19]
    reg _T_29_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_29_re) @[Reg.scala 15:16]
    reg _T_29_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_29_im) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), _T_28_im, _T_29_im) @[Reg.scala 16:19]
    node _GEN_3 = mux(UInt<1>("h1"), _T_28_re, _T_29_re) @[Reg.scala 16:19]
    reg _T_30_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_30_re) @[Reg.scala 15:16]
    reg _T_30_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_30_im) @[Reg.scala 15:16]
    node _GEN_4 = mux(UInt<1>("h1"), _T_29_im, _T_30_im) @[Reg.scala 16:19]
    node _GEN_5 = mux(UInt<1>("h1"), _T_29_re, _T_30_re) @[Reg.scala 16:19]
    reg _T_31_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_31_re) @[Reg.scala 15:16]
    reg _T_31_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_31_im) @[Reg.scala 15:16]
    node _GEN_6 = mux(UInt<1>("h1"), _T_30_im, _T_31_im) @[Reg.scala 16:19]
    node _GEN_7 = mux(UInt<1>("h1"), _T_30_re, _T_31_re) @[Reg.scala 16:19]
    reg _T_32_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_32_re) @[Reg.scala 15:16]
    reg _T_32_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_32_im) @[Reg.scala 15:16]
    node _GEN_8 = mux(UInt<1>("h1"), io_in_2_im, _T_32_im) @[Reg.scala 16:19]
    node _GEN_9 = mux(UInt<1>("h1"), io_in_2_re, _T_32_re) @[Reg.scala 16:19]
    reg _T_33_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_33_re) @[Reg.scala 15:16]
    reg _T_33_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_33_im) @[Reg.scala 15:16]
    node _GEN_10 = mux(UInt<1>("h1"), _T_32_im, _T_33_im) @[Reg.scala 16:19]
    node _GEN_11 = mux(UInt<1>("h1"), _T_32_re, _T_33_re) @[Reg.scala 16:19]
    reg _T_34_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_34_re) @[Reg.scala 15:16]
    reg _T_34_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_34_im) @[Reg.scala 15:16]
    node _GEN_12 = mux(UInt<1>("h1"), _T_33_im, _T_34_im) @[Reg.scala 16:19]
    node _GEN_13 = mux(UInt<1>("h1"), _T_33_re, _T_34_re) @[Reg.scala 16:19]
    reg _T_35_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_35_re) @[Reg.scala 15:16]
    reg _T_35_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_35_im) @[Reg.scala 15:16]
    node _GEN_14 = mux(UInt<1>("h1"), _T_34_im, _T_35_im) @[Reg.scala 16:19]
    node _GEN_15 = mux(UInt<1>("h1"), _T_34_re, _T_35_re) @[Reg.scala 16:19]
    reg _T_36_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_36_re) @[Reg.scala 15:16]
    reg _T_36_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_36_im) @[Reg.scala 15:16]
    node _GEN_16 = mux(UInt<1>("h1"), _T_35_im, _T_36_im) @[Reg.scala 16:19]
    node _GEN_17 = mux(UInt<1>("h1"), _T_35_re, _T_36_re) @[Reg.scala 16:19]
    reg _T_37_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_37_re) @[Reg.scala 15:16]
    reg _T_37_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_37_im) @[Reg.scala 15:16]
    node _GEN_18 = mux(UInt<1>("h1"), _T_36_im, _T_37_im) @[Reg.scala 16:19]
    node _GEN_19 = mux(UInt<1>("h1"), _T_36_re, _T_37_re) @[Reg.scala 16:19]
    reg _T_38_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_38_re) @[Reg.scala 15:16]
    reg _T_38_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_38_im) @[Reg.scala 15:16]
    node _GEN_20 = mux(UInt<1>("h1"), _T_37_im, _T_38_im) @[Reg.scala 16:19]
    node _GEN_21 = mux(UInt<1>("h1"), _T_37_re, _T_38_re) @[Reg.scala 16:19]
    reg _T_39_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_39_re) @[Reg.scala 15:16]
    reg _T_39_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_39_im) @[Reg.scala 15:16]
    node _GEN_22 = mux(UInt<1>("h1"), _T_38_im, _T_39_im) @[Reg.scala 16:19]
    node _GEN_23 = mux(UInt<1>("h1"), _T_38_re, _T_39_re) @[Reg.scala 16:19]
    reg _T_40_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_40_re) @[Reg.scala 15:16]
    reg _T_40_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_40_im) @[Reg.scala 15:16]
    node _GEN_24 = mux(UInt<1>("h1"), io_in_3_im, _T_40_im) @[Reg.scala 16:19]
    node _GEN_25 = mux(UInt<1>("h1"), io_in_3_re, _T_40_re) @[Reg.scala 16:19]
    reg _T_41_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_41_re) @[Reg.scala 15:16]
    reg _T_41_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_41_im) @[Reg.scala 15:16]
    node _GEN_26 = mux(UInt<1>("h1"), _T_40_im, _T_41_im) @[Reg.scala 16:19]
    node _GEN_27 = mux(UInt<1>("h1"), _T_40_re, _T_41_re) @[Reg.scala 16:19]
    reg _T_42_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_42_re) @[Reg.scala 15:16]
    reg _T_42_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_42_im) @[Reg.scala 15:16]
    node _GEN_28 = mux(UInt<1>("h1"), _T_41_im, _T_42_im) @[Reg.scala 16:19]
    node _GEN_29 = mux(UInt<1>("h1"), _T_41_re, _T_42_re) @[Reg.scala 16:19]
    reg _T_43_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_43_re) @[Reg.scala 15:16]
    reg _T_43_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_43_im) @[Reg.scala 15:16]
    node _GEN_30 = mux(UInt<1>("h1"), _T_42_im, _T_43_im) @[Reg.scala 16:19]
    node _GEN_31 = mux(UInt<1>("h1"), _T_42_re, _T_43_re) @[Reg.scala 16:19]
    reg _T_44_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_44_re) @[Reg.scala 15:16]
    reg _T_44_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_44_im) @[Reg.scala 15:16]
    node _GEN_32 = mux(UInt<1>("h1"), _T_43_im, _T_44_im) @[Reg.scala 16:19]
    node _GEN_33 = mux(UInt<1>("h1"), _T_43_re, _T_44_re) @[Reg.scala 16:19]
    reg _T_45_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_45_re) @[Reg.scala 15:16]
    reg _T_45_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_45_im) @[Reg.scala 15:16]
    node _GEN_34 = mux(UInt<1>("h1"), _T_44_im, _T_45_im) @[Reg.scala 16:19]
    node _GEN_35 = mux(UInt<1>("h1"), _T_44_re, _T_45_re) @[Reg.scala 16:19]
    reg _T_46_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_46_re) @[Reg.scala 15:16]
    reg _T_46_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_46_im) @[Reg.scala 15:16]
    node _GEN_36 = mux(UInt<1>("h1"), _T_45_im, _T_46_im) @[Reg.scala 16:19]
    node _GEN_37 = mux(UInt<1>("h1"), _T_45_re, _T_46_re) @[Reg.scala 16:19]
    reg _T_47_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_47_re) @[Reg.scala 15:16]
    reg _T_47_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_47_im) @[Reg.scala 15:16]
    node _GEN_38 = mux(UInt<1>("h1"), _T_46_im, _T_47_im) @[Reg.scala 16:19]
    node _GEN_39 = mux(UInt<1>("h1"), _T_46_re, _T_47_re) @[Reg.scala 16:19]
    reg _T_48_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_48_re) @[Reg.scala 15:16]
    reg _T_48_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_48_im) @[Reg.scala 15:16]
    node _GEN_40 = mux(UInt<1>("h1"), _T_47_im, _T_48_im) @[Reg.scala 16:19]
    node _GEN_41 = mux(UInt<1>("h1"), _T_47_re, _T_48_re) @[Reg.scala 16:19]
    reg _T_49_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_49_re) @[Reg.scala 15:16]
    reg _T_49_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_49_im) @[Reg.scala 15:16]
    node _GEN_42 = mux(UInt<1>("h1"), _T_48_im, _T_49_im) @[Reg.scala 16:19]
    node _GEN_43 = mux(UInt<1>("h1"), _T_48_re, _T_49_re) @[Reg.scala 16:19]
    reg _T_50_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_50_re) @[Reg.scala 15:16]
    reg _T_50_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_50_im) @[Reg.scala 15:16]
    node _GEN_44 = mux(UInt<1>("h1"), _T_49_im, _T_50_im) @[Reg.scala 16:19]
    node _GEN_45 = mux(UInt<1>("h1"), _T_49_re, _T_50_re) @[Reg.scala 16:19]
    reg _T_51_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_51_re) @[Reg.scala 15:16]
    reg _T_51_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_51_im) @[Reg.scala 15:16]
    node _GEN_46 = mux(UInt<1>("h1"), _T_50_im, _T_51_im) @[Reg.scala 16:19]
    node _GEN_47 = mux(UInt<1>("h1"), _T_50_re, _T_51_re) @[Reg.scala 16:19]
    node _T_53 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_54 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_55 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node swdata_2_re = _T_39_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node swdata_3_re = _T_51_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_56_re = mux(_T_55, swdata_2_re, swdata_3_re) @[Modules.scala 129:64]
    node swdata_2_im = _T_39_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node swdata_3_im = _T_51_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_56_im = mux(_T_55, swdata_2_im, swdata_3_im) @[Modules.scala 129:64]
    node swdata_1_re = _T_31_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_57_re = mux(_T_54, swdata_1_re, _T_56_re) @[Modules.scala 129:40]
    node swdata_1_im = _T_31_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_57_im = mux(_T_54, swdata_1_im, _T_56_im) @[Modules.scala 129:40]
    node swdata_0_re = io_in_0_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_58_re = mux(_T_53, swdata_0_re, _T_57_re) @[Modules.scala 129:16]
    node swdata_0_im = io_in_0_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_58_im = mux(_T_53, swdata_0_im, _T_57_im) @[Modules.scala 129:16]
    reg _T_59_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_59_re) @[Reg.scala 15:16]
    reg _T_59_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_59_im) @[Reg.scala 15:16]
    node _T_52_im = _T_58_im @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_48 = mux(UInt<1>("h1"), _T_52_im, _T_59_im) @[Reg.scala 16:19]
    node _T_52_re = _T_58_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_49 = mux(UInt<1>("h1"), _T_52_re, _T_59_re) @[Reg.scala 16:19]
    reg _T_60_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_60_re) @[Reg.scala 15:16]
    reg _T_60_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_60_im) @[Reg.scala 15:16]
    node _GEN_50 = mux(UInt<1>("h1"), _T_59_im, _T_60_im) @[Reg.scala 16:19]
    node _GEN_51 = mux(UInt<1>("h1"), _T_59_re, _T_60_re) @[Reg.scala 16:19]
    reg _T_61_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_61_re) @[Reg.scala 15:16]
    reg _T_61_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_61_im) @[Reg.scala 15:16]
    node _GEN_52 = mux(UInt<1>("h1"), _T_60_im, _T_61_im) @[Reg.scala 16:19]
    node _GEN_53 = mux(UInt<1>("h1"), _T_60_re, _T_61_re) @[Reg.scala 16:19]
    reg _T_62_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_62_re) @[Reg.scala 15:16]
    reg _T_62_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_62_im) @[Reg.scala 15:16]
    node _GEN_54 = mux(UInt<1>("h1"), _T_61_im, _T_62_im) @[Reg.scala 16:19]
    node _GEN_55 = mux(UInt<1>("h1"), _T_61_re, _T_62_re) @[Reg.scala 16:19]
    reg _T_63_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_63_re) @[Reg.scala 15:16]
    reg _T_63_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_63_im) @[Reg.scala 15:16]
    node _GEN_56 = mux(UInt<1>("h1"), _T_62_im, _T_63_im) @[Reg.scala 16:19]
    node _GEN_57 = mux(UInt<1>("h1"), _T_62_re, _T_63_re) @[Reg.scala 16:19]
    reg _T_64_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_64_re) @[Reg.scala 15:16]
    reg _T_64_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_64_im) @[Reg.scala 15:16]
    node _GEN_58 = mux(UInt<1>("h1"), _T_63_im, _T_64_im) @[Reg.scala 16:19]
    node _GEN_59 = mux(UInt<1>("h1"), _T_63_re, _T_64_re) @[Reg.scala 16:19]
    reg _T_65_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_65_re) @[Reg.scala 15:16]
    reg _T_65_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_65_im) @[Reg.scala 15:16]
    node _GEN_60 = mux(UInt<1>("h1"), _T_64_im, _T_65_im) @[Reg.scala 16:19]
    node _GEN_61 = mux(UInt<1>("h1"), _T_64_re, _T_65_re) @[Reg.scala 16:19]
    reg _T_66_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_66_re) @[Reg.scala 15:16]
    reg _T_66_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_66_im) @[Reg.scala 15:16]
    node _GEN_62 = mux(UInt<1>("h1"), _T_65_im, _T_66_im) @[Reg.scala 16:19]
    node _GEN_63 = mux(UInt<1>("h1"), _T_65_re, _T_66_re) @[Reg.scala 16:19]
    reg _T_67_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_67_re) @[Reg.scala 15:16]
    reg _T_67_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_67_im) @[Reg.scala 15:16]
    node _GEN_64 = mux(UInt<1>("h1"), _T_66_im, _T_67_im) @[Reg.scala 16:19]
    node _GEN_65 = mux(UInt<1>("h1"), _T_66_re, _T_67_re) @[Reg.scala 16:19]
    reg _T_68_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_68_re) @[Reg.scala 15:16]
    reg _T_68_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_68_im) @[Reg.scala 15:16]
    node _GEN_66 = mux(UInt<1>("h1"), _T_67_im, _T_68_im) @[Reg.scala 16:19]
    node _GEN_67 = mux(UInt<1>("h1"), _T_67_re, _T_68_re) @[Reg.scala 16:19]
    reg _T_69_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_69_re) @[Reg.scala 15:16]
    reg _T_69_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_69_im) @[Reg.scala 15:16]
    node _GEN_68 = mux(UInt<1>("h1"), _T_68_im, _T_69_im) @[Reg.scala 16:19]
    node _GEN_69 = mux(UInt<1>("h1"), _T_68_re, _T_69_re) @[Reg.scala 16:19]
    reg _T_70_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_70_re) @[Reg.scala 15:16]
    reg _T_70_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_70_im) @[Reg.scala 15:16]
    node _GEN_70 = mux(UInt<1>("h1"), _T_69_im, _T_70_im) @[Reg.scala 16:19]
    node _GEN_71 = mux(UInt<1>("h1"), _T_69_re, _T_70_re) @[Reg.scala 16:19]
    node _T_72 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_73 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_74 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node _T_75_re = mux(_T_74, swdata_1_re, swdata_2_re) @[Modules.scala 129:64]
    node _T_75_im = mux(_T_74, swdata_1_im, swdata_2_im) @[Modules.scala 129:64]
    node _T_76_re = mux(_T_73, swdata_0_re, _T_75_re) @[Modules.scala 129:40]
    node _T_76_im = mux(_T_73, swdata_0_im, _T_75_im) @[Modules.scala 129:40]
    node _T_77_re = mux(_T_72, swdata_3_re, _T_76_re) @[Modules.scala 129:16]
    node _T_77_im = mux(_T_72, swdata_3_im, _T_76_im) @[Modules.scala 129:16]
    reg _T_78_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_78_re) @[Reg.scala 15:16]
    reg _T_78_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_78_im) @[Reg.scala 15:16]
    node _T_71_im = _T_77_im @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_72 = mux(UInt<1>("h1"), _T_71_im, _T_78_im) @[Reg.scala 16:19]
    node _T_71_re = _T_77_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_73 = mux(UInt<1>("h1"), _T_71_re, _T_78_re) @[Reg.scala 16:19]
    reg _T_79_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_79_re) @[Reg.scala 15:16]
    reg _T_79_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_79_im) @[Reg.scala 15:16]
    node _GEN_74 = mux(UInt<1>("h1"), _T_78_im, _T_79_im) @[Reg.scala 16:19]
    node _GEN_75 = mux(UInt<1>("h1"), _T_78_re, _T_79_re) @[Reg.scala 16:19]
    reg _T_80_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_80_re) @[Reg.scala 15:16]
    reg _T_80_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_80_im) @[Reg.scala 15:16]
    node _GEN_76 = mux(UInt<1>("h1"), _T_79_im, _T_80_im) @[Reg.scala 16:19]
    node _GEN_77 = mux(UInt<1>("h1"), _T_79_re, _T_80_re) @[Reg.scala 16:19]
    reg _T_81_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_81_re) @[Reg.scala 15:16]
    reg _T_81_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_81_im) @[Reg.scala 15:16]
    node _GEN_78 = mux(UInt<1>("h1"), _T_80_im, _T_81_im) @[Reg.scala 16:19]
    node _GEN_79 = mux(UInt<1>("h1"), _T_80_re, _T_81_re) @[Reg.scala 16:19]
    reg _T_82_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_82_re) @[Reg.scala 15:16]
    reg _T_82_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_82_im) @[Reg.scala 15:16]
    node _GEN_80 = mux(UInt<1>("h1"), _T_81_im, _T_82_im) @[Reg.scala 16:19]
    node _GEN_81 = mux(UInt<1>("h1"), _T_81_re, _T_82_re) @[Reg.scala 16:19]
    reg _T_83_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_83_re) @[Reg.scala 15:16]
    reg _T_83_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_83_im) @[Reg.scala 15:16]
    node _GEN_82 = mux(UInt<1>("h1"), _T_82_im, _T_83_im) @[Reg.scala 16:19]
    node _GEN_83 = mux(UInt<1>("h1"), _T_82_re, _T_83_re) @[Reg.scala 16:19]
    reg _T_84_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_84_re) @[Reg.scala 15:16]
    reg _T_84_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_84_im) @[Reg.scala 15:16]
    node _GEN_84 = mux(UInt<1>("h1"), _T_83_im, _T_84_im) @[Reg.scala 16:19]
    node _GEN_85 = mux(UInt<1>("h1"), _T_83_re, _T_84_re) @[Reg.scala 16:19]
    reg _T_85_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_85_re) @[Reg.scala 15:16]
    reg _T_85_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_85_im) @[Reg.scala 15:16]
    node _GEN_86 = mux(UInt<1>("h1"), _T_84_im, _T_85_im) @[Reg.scala 16:19]
    node _GEN_87 = mux(UInt<1>("h1"), _T_84_re, _T_85_re) @[Reg.scala 16:19]
    node _T_87 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_88 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_89 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node _T_90_re = mux(_T_89, swdata_0_re, swdata_1_re) @[Modules.scala 129:64]
    node _T_90_im = mux(_T_89, swdata_0_im, swdata_1_im) @[Modules.scala 129:64]
    node _T_91_re = mux(_T_88, swdata_3_re, _T_90_re) @[Modules.scala 129:40]
    node _T_91_im = mux(_T_88, swdata_3_im, _T_90_im) @[Modules.scala 129:40]
    node _T_92_re = mux(_T_87, swdata_2_re, _T_91_re) @[Modules.scala 129:16]
    node _T_92_im = mux(_T_87, swdata_2_im, _T_91_im) @[Modules.scala 129:16]
    reg _T_93_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_93_re) @[Reg.scala 15:16]
    reg _T_93_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_93_im) @[Reg.scala 15:16]
    node _T_86_im = _T_92_im @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_88 = mux(UInt<1>("h1"), _T_86_im, _T_93_im) @[Reg.scala 16:19]
    node _T_86_re = _T_92_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_89 = mux(UInt<1>("h1"), _T_86_re, _T_93_re) @[Reg.scala 16:19]
    reg _T_94_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_94_re) @[Reg.scala 15:16]
    reg _T_94_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_94_im) @[Reg.scala 15:16]
    node _GEN_90 = mux(UInt<1>("h1"), _T_93_im, _T_94_im) @[Reg.scala 16:19]
    node _GEN_91 = mux(UInt<1>("h1"), _T_93_re, _T_94_re) @[Reg.scala 16:19]
    reg _T_95_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_95_re) @[Reg.scala 15:16]
    reg _T_95_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_95_im) @[Reg.scala 15:16]
    node _GEN_92 = mux(UInt<1>("h1"), _T_94_im, _T_95_im) @[Reg.scala 16:19]
    node _GEN_93 = mux(UInt<1>("h1"), _T_94_re, _T_95_re) @[Reg.scala 16:19]
    reg _T_96_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_96_re) @[Reg.scala 15:16]
    reg _T_96_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_96_im) @[Reg.scala 15:16]
    node _GEN_94 = mux(UInt<1>("h1"), _T_95_im, _T_96_im) @[Reg.scala 16:19]
    node _GEN_95 = mux(UInt<1>("h1"), _T_95_re, _T_96_re) @[Reg.scala 16:19]
    node _T_98 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_99 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_100 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node _T_101_re = mux(_T_100, swdata_3_re, swdata_0_re) @[Modules.scala 129:64]
    node _T_101_im = mux(_T_100, swdata_3_im, swdata_0_im) @[Modules.scala 129:64]
    node _T_102_re = mux(_T_99, swdata_2_re, _T_101_re) @[Modules.scala 129:40]
    node _T_102_im = mux(_T_99, swdata_2_im, _T_101_im) @[Modules.scala 129:40]
    node _T_103_re = mux(_T_98, swdata_1_re, _T_102_re) @[Modules.scala 129:16]
    node _T_103_im = mux(_T_98, swdata_1_im, _T_102_im) @[Modules.scala 129:16]
    node _T_re = _T_6 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_im = _T_4 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_7_re = _T_13 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_7_im = _T_11 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_14_re = _T_20 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_14_im = _T_18 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_21_re = _T_27 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_21_im = _T_25 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_97_re = _T_103_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _T_97_im = _T_103_im @[Modules.scala 128:20 Modules.scala 129:10]
    io_out_0_re <= _T_70_re @[Modules.scala 145:15]
    io_out_0_im <= _T_70_im @[Modules.scala 145:15]
    io_out_1_re <= _T_85_re @[Modules.scala 146:15]
    io_out_1_im <= _T_85_im @[Modules.scala 146:15]
    io_out_2_re <= _T_96_re @[Modules.scala 147:15]
    io_out_2_im <= _T_96_im @[Modules.scala 147:15]
    io_out_3_re <= _T_97_re @[Modules.scala 148:15]
    io_out_3_im <= _T_97_im @[Modules.scala 148:15]
    _T_28_re <= _GEN_1 @[Reg.scala 16:23]
    _T_28_im <= _GEN_0 @[Reg.scala 16:23]
    _T_29_re <= _GEN_3 @[Reg.scala 16:23]
    _T_29_im <= _GEN_2 @[Reg.scala 16:23]
    _T_30_re <= _GEN_5 @[Reg.scala 16:23]
    _T_30_im <= _GEN_4 @[Reg.scala 16:23]
    _T_31_re <= _GEN_7 @[Reg.scala 16:23]
    _T_31_im <= _GEN_6 @[Reg.scala 16:23]
    _T_32_re <= _GEN_9 @[Reg.scala 16:23]
    _T_32_im <= _GEN_8 @[Reg.scala 16:23]
    _T_33_re <= _GEN_11 @[Reg.scala 16:23]
    _T_33_im <= _GEN_10 @[Reg.scala 16:23]
    _T_34_re <= _GEN_13 @[Reg.scala 16:23]
    _T_34_im <= _GEN_12 @[Reg.scala 16:23]
    _T_35_re <= _GEN_15 @[Reg.scala 16:23]
    _T_35_im <= _GEN_14 @[Reg.scala 16:23]
    _T_36_re <= _GEN_17 @[Reg.scala 16:23]
    _T_36_im <= _GEN_16 @[Reg.scala 16:23]
    _T_37_re <= _GEN_19 @[Reg.scala 16:23]
    _T_37_im <= _GEN_18 @[Reg.scala 16:23]
    _T_38_re <= _GEN_21 @[Reg.scala 16:23]
    _T_38_im <= _GEN_20 @[Reg.scala 16:23]
    _T_39_re <= _GEN_23 @[Reg.scala 16:23]
    _T_39_im <= _GEN_22 @[Reg.scala 16:23]
    _T_40_re <= _GEN_25 @[Reg.scala 16:23]
    _T_40_im <= _GEN_24 @[Reg.scala 16:23]
    _T_41_re <= _GEN_27 @[Reg.scala 16:23]
    _T_41_im <= _GEN_26 @[Reg.scala 16:23]
    _T_42_re <= _GEN_29 @[Reg.scala 16:23]
    _T_42_im <= _GEN_28 @[Reg.scala 16:23]
    _T_43_re <= _GEN_31 @[Reg.scala 16:23]
    _T_43_im <= _GEN_30 @[Reg.scala 16:23]
    _T_44_re <= _GEN_33 @[Reg.scala 16:23]
    _T_44_im <= _GEN_32 @[Reg.scala 16:23]
    _T_45_re <= _GEN_35 @[Reg.scala 16:23]
    _T_45_im <= _GEN_34 @[Reg.scala 16:23]
    _T_46_re <= _GEN_37 @[Reg.scala 16:23]
    _T_46_im <= _GEN_36 @[Reg.scala 16:23]
    _T_47_re <= _GEN_39 @[Reg.scala 16:23]
    _T_47_im <= _GEN_38 @[Reg.scala 16:23]
    _T_48_re <= _GEN_41 @[Reg.scala 16:23]
    _T_48_im <= _GEN_40 @[Reg.scala 16:23]
    _T_49_re <= _GEN_43 @[Reg.scala 16:23]
    _T_49_im <= _GEN_42 @[Reg.scala 16:23]
    _T_50_re <= _GEN_45 @[Reg.scala 16:23]
    _T_50_im <= _GEN_44 @[Reg.scala 16:23]
    _T_51_re <= _GEN_47 @[Reg.scala 16:23]
    _T_51_im <= _GEN_46 @[Reg.scala 16:23]
    _T_59_re <= _GEN_49 @[Reg.scala 16:23]
    _T_59_im <= _GEN_48 @[Reg.scala 16:23]
    _T_60_re <= _GEN_51 @[Reg.scala 16:23]
    _T_60_im <= _GEN_50 @[Reg.scala 16:23]
    _T_61_re <= _GEN_53 @[Reg.scala 16:23]
    _T_61_im <= _GEN_52 @[Reg.scala 16:23]
    _T_62_re <= _GEN_55 @[Reg.scala 16:23]
    _T_62_im <= _GEN_54 @[Reg.scala 16:23]
    _T_63_re <= _GEN_57 @[Reg.scala 16:23]
    _T_63_im <= _GEN_56 @[Reg.scala 16:23]
    _T_64_re <= _GEN_59 @[Reg.scala 16:23]
    _T_64_im <= _GEN_58 @[Reg.scala 16:23]
    _T_65_re <= _GEN_61 @[Reg.scala 16:23]
    _T_65_im <= _GEN_60 @[Reg.scala 16:23]
    _T_66_re <= _GEN_63 @[Reg.scala 16:23]
    _T_66_im <= _GEN_62 @[Reg.scala 16:23]
    _T_67_re <= _GEN_65 @[Reg.scala 16:23]
    _T_67_im <= _GEN_64 @[Reg.scala 16:23]
    _T_68_re <= _GEN_67 @[Reg.scala 16:23]
    _T_68_im <= _GEN_66 @[Reg.scala 16:23]
    _T_69_re <= _GEN_69 @[Reg.scala 16:23]
    _T_69_im <= _GEN_68 @[Reg.scala 16:23]
    _T_70_re <= _GEN_71 @[Reg.scala 16:23]
    _T_70_im <= _GEN_70 @[Reg.scala 16:23]
    _T_78_re <= _GEN_73 @[Reg.scala 16:23]
    _T_78_im <= _GEN_72 @[Reg.scala 16:23]
    _T_79_re <= _GEN_75 @[Reg.scala 16:23]
    _T_79_im <= _GEN_74 @[Reg.scala 16:23]
    _T_80_re <= _GEN_77 @[Reg.scala 16:23]
    _T_80_im <= _GEN_76 @[Reg.scala 16:23]
    _T_81_re <= _GEN_79 @[Reg.scala 16:23]
    _T_81_im <= _GEN_78 @[Reg.scala 16:23]
    _T_82_re <= _GEN_81 @[Reg.scala 16:23]
    _T_82_im <= _GEN_80 @[Reg.scala 16:23]
    _T_83_re <= _GEN_83 @[Reg.scala 16:23]
    _T_83_im <= _GEN_82 @[Reg.scala 16:23]
    _T_84_re <= _GEN_85 @[Reg.scala 16:23]
    _T_84_im <= _GEN_84 @[Reg.scala 16:23]
    _T_85_re <= _GEN_87 @[Reg.scala 16:23]
    _T_85_im <= _GEN_86 @[Reg.scala 16:23]
    _T_93_re <= _GEN_89 @[Reg.scala 16:23]
    _T_93_im <= _GEN_88 @[Reg.scala 16:23]
    _T_94_re <= _GEN_91 @[Reg.scala 16:23]
    _T_94_im <= _GEN_90 @[Reg.scala 16:23]
    _T_95_re <= _GEN_93 @[Reg.scala 16:23]
    _T_95_im <= _GEN_92 @[Reg.scala 16:23]
    _T_96_re <= _GEN_95 @[Reg.scala 16:23]
    _T_96_im <= _GEN_94 @[Reg.scala 16:23]

  module Switch_1 :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_re : SInt<32>
    input io_in_0_im : SInt<32>
    input io_in_1_re : SInt<32>
    input io_in_1_im : SInt<32>
    input io_in_2_re : SInt<32>
    input io_in_2_im : SInt<32>
    input io_in_3_re : SInt<32>
    input io_in_3_im : SInt<32>
    output io_out_0_re : SInt<32>
    output io_out_0_im : SInt<32>
    output io_out_1_re : SInt<32>
    output io_out_1_im : SInt<32>
    output io_out_2_re : SInt<32>
    output io_out_2_im : SInt<32>
    output io_out_3_re : SInt<32>
    output io_out_3_im : SInt<32>
    input io_sel : UInt<2>
  
    node _T_1 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_2 = _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 139:71]
    node _T_4 = asSInt(_T_3) @[Modules.scala 139:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 139:71]
    node _T_6 = asSInt(_T_5) @[Modules.scala 139:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_9 = _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 139:71]
    node _T_11 = asSInt(_T_10) @[Modules.scala 139:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 139:71]
    node _T_13 = asSInt(_T_12) @[Modules.scala 139:71]
    node _T_15 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_16 = _T_15
    node _T_17 = bits(_T_16, 31, 0) @[Modules.scala 139:71]
    node _T_18 = asSInt(_T_17) @[Modules.scala 139:71]
    node _T_19 = bits(_T_16, 63, 32) @[Modules.scala 139:71]
    node _T_20 = asSInt(_T_19) @[Modules.scala 139:71]
    node _T_22 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 139:71]
    node _T_23 = _T_22
    node _T_24 = bits(_T_23, 31, 0) @[Modules.scala 139:71]
    node _T_25 = asSInt(_T_24) @[Modules.scala 139:71]
    node _T_26 = bits(_T_23, 63, 32) @[Modules.scala 139:71]
    node _T_27 = asSInt(_T_26) @[Modules.scala 139:71]
    reg _T_28_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_28_re) @[Reg.scala 15:16]
    reg _T_28_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_28_im) @[Reg.scala 15:16]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_1_im, _T_28_im) @[Reg.scala 16:19]
    node _GEN_1 = mux(UInt<1>("h1"), io_in_1_re, _T_28_re) @[Reg.scala 16:19]
    reg _T_29_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_29_re) @[Reg.scala 15:16]
    reg _T_29_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_29_im) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), io_in_2_im, _T_29_im) @[Reg.scala 16:19]
    node _GEN_3 = mux(UInt<1>("h1"), io_in_2_re, _T_29_re) @[Reg.scala 16:19]
    reg _T_30_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_30_re) @[Reg.scala 15:16]
    reg _T_30_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_30_im) @[Reg.scala 15:16]
    node _GEN_4 = mux(UInt<1>("h1"), _T_29_im, _T_30_im) @[Reg.scala 16:19]
    node _GEN_5 = mux(UInt<1>("h1"), _T_29_re, _T_30_re) @[Reg.scala 16:19]
    reg _T_31_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_31_re) @[Reg.scala 15:16]
    reg _T_31_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_31_im) @[Reg.scala 15:16]
    node _GEN_6 = mux(UInt<1>("h1"), io_in_3_im, _T_31_im) @[Reg.scala 16:19]
    node _GEN_7 = mux(UInt<1>("h1"), io_in_3_re, _T_31_re) @[Reg.scala 16:19]
    reg _T_32_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_32_re) @[Reg.scala 15:16]
    reg _T_32_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_32_im) @[Reg.scala 15:16]
    node _GEN_8 = mux(UInt<1>("h1"), _T_31_im, _T_32_im) @[Reg.scala 16:19]
    node _GEN_9 = mux(UInt<1>("h1"), _T_31_re, _T_32_re) @[Reg.scala 16:19]
    reg _T_33_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_33_re) @[Reg.scala 15:16]
    reg _T_33_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_33_im) @[Reg.scala 15:16]
    node _GEN_10 = mux(UInt<1>("h1"), _T_32_im, _T_33_im) @[Reg.scala 16:19]
    node _GEN_11 = mux(UInt<1>("h1"), _T_32_re, _T_33_re) @[Reg.scala 16:19]
    node _T_35 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_36 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_37 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node swdata_2_re = _T_30_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node swdata_3_re = _T_33_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_38_re = mux(_T_37, swdata_2_re, swdata_3_re) @[Modules.scala 129:64]
    node swdata_2_im = _T_30_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node swdata_3_im = _T_33_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_38_im = mux(_T_37, swdata_2_im, swdata_3_im) @[Modules.scala 129:64]
    node swdata_1_re = _T_28_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_39_re = mux(_T_36, swdata_1_re, _T_38_re) @[Modules.scala 129:40]
    node swdata_1_im = _T_28_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_39_im = mux(_T_36, swdata_1_im, _T_38_im) @[Modules.scala 129:40]
    node swdata_0_re = io_in_0_re @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_40_re = mux(_T_35, swdata_0_re, _T_39_re) @[Modules.scala 129:16]
    node swdata_0_im = io_in_0_im @[Modules.scala 139:23 Modules.scala 139:23 Modules.scala 140:38]
    node _T_40_im = mux(_T_35, swdata_0_im, _T_39_im) @[Modules.scala 129:16]
    reg _T_41_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_41_re) @[Reg.scala 15:16]
    reg _T_41_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_41_im) @[Reg.scala 15:16]
    node _T_34_im = _T_40_im @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_12 = mux(UInt<1>("h1"), _T_34_im, _T_41_im) @[Reg.scala 16:19]
    node _T_34_re = _T_40_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_13 = mux(UInt<1>("h1"), _T_34_re, _T_41_re) @[Reg.scala 16:19]
    reg _T_42_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_42_re) @[Reg.scala 15:16]
    reg _T_42_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_42_im) @[Reg.scala 15:16]
    node _GEN_14 = mux(UInt<1>("h1"), _T_41_im, _T_42_im) @[Reg.scala 16:19]
    node _GEN_15 = mux(UInt<1>("h1"), _T_41_re, _T_42_re) @[Reg.scala 16:19]
    reg _T_43_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_43_re) @[Reg.scala 15:16]
    reg _T_43_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_43_im) @[Reg.scala 15:16]
    node _GEN_16 = mux(UInt<1>("h1"), _T_42_im, _T_43_im) @[Reg.scala 16:19]
    node _GEN_17 = mux(UInt<1>("h1"), _T_42_re, _T_43_re) @[Reg.scala 16:19]
    node _T_45 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_46 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_47 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node _T_48_re = mux(_T_47, swdata_1_re, swdata_2_re) @[Modules.scala 129:64]
    node _T_48_im = mux(_T_47, swdata_1_im, swdata_2_im) @[Modules.scala 129:64]
    node _T_49_re = mux(_T_46, swdata_0_re, _T_48_re) @[Modules.scala 129:40]
    node _T_49_im = mux(_T_46, swdata_0_im, _T_48_im) @[Modules.scala 129:40]
    node _T_50_re = mux(_T_45, swdata_3_re, _T_49_re) @[Modules.scala 129:16]
    node _T_50_im = mux(_T_45, swdata_3_im, _T_49_im) @[Modules.scala 129:16]
    reg _T_51_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_51_re) @[Reg.scala 15:16]
    reg _T_51_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_51_im) @[Reg.scala 15:16]
    node _T_44_im = _T_50_im @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_18 = mux(UInt<1>("h1"), _T_44_im, _T_51_im) @[Reg.scala 16:19]
    node _T_44_re = _T_50_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_19 = mux(UInt<1>("h1"), _T_44_re, _T_51_re) @[Reg.scala 16:19]
    reg _T_52_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_52_re) @[Reg.scala 15:16]
    reg _T_52_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_52_im) @[Reg.scala 15:16]
    node _GEN_20 = mux(UInt<1>("h1"), _T_51_im, _T_52_im) @[Reg.scala 16:19]
    node _GEN_21 = mux(UInt<1>("h1"), _T_51_re, _T_52_re) @[Reg.scala 16:19]
    node _T_54 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_55 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_56 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node _T_57_re = mux(_T_56, swdata_0_re, swdata_1_re) @[Modules.scala 129:64]
    node _T_57_im = mux(_T_56, swdata_0_im, swdata_1_im) @[Modules.scala 129:64]
    node _T_58_re = mux(_T_55, swdata_3_re, _T_57_re) @[Modules.scala 129:40]
    node _T_58_im = mux(_T_55, swdata_3_im, _T_57_im) @[Modules.scala 129:40]
    node _T_59_re = mux(_T_54, swdata_2_re, _T_58_re) @[Modules.scala 129:16]
    node _T_59_im = mux(_T_54, swdata_2_im, _T_58_im) @[Modules.scala 129:16]
    reg _T_60_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_60_re) @[Reg.scala 15:16]
    reg _T_60_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_60_im) @[Reg.scala 15:16]
    node _T_53_im = _T_59_im @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_22 = mux(UInt<1>("h1"), _T_53_im, _T_60_im) @[Reg.scala 16:19]
    node _T_53_re = _T_59_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _GEN_23 = mux(UInt<1>("h1"), _T_53_re, _T_60_re) @[Reg.scala 16:19]
    node _T_62 = eq(io_sel, UInt<1>("h0")) @[Modules.scala 129:22]
    node _T_63 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 129:46]
    node _T_64 = eq(io_sel, UInt<2>("h2")) @[Modules.scala 129:70]
    node _T_65_re = mux(_T_64, swdata_3_re, swdata_0_re) @[Modules.scala 129:64]
    node _T_65_im = mux(_T_64, swdata_3_im, swdata_0_im) @[Modules.scala 129:64]
    node _T_66_re = mux(_T_63, swdata_2_re, _T_65_re) @[Modules.scala 129:40]
    node _T_66_im = mux(_T_63, swdata_2_im, _T_65_im) @[Modules.scala 129:40]
    node _T_67_re = mux(_T_62, swdata_1_re, _T_66_re) @[Modules.scala 129:16]
    node _T_67_im = mux(_T_62, swdata_1_im, _T_66_im) @[Modules.scala 129:16]
    node _T_re = _T_6 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_im = _T_4 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_7_re = _T_13 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_7_im = _T_11 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_14_re = _T_20 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_14_im = _T_18 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_21_re = _T_27 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_21_im = _T_25 @[Modules.scala 139:71 Modules.scala 139:71]
    node _T_61_re = _T_67_re @[Modules.scala 128:20 Modules.scala 129:10]
    node _T_61_im = _T_67_im @[Modules.scala 128:20 Modules.scala 129:10]
    io_out_0_re <= _T_43_re @[Modules.scala 145:15]
    io_out_0_im <= _T_43_im @[Modules.scala 145:15]
    io_out_1_re <= _T_52_re @[Modules.scala 146:15]
    io_out_1_im <= _T_52_im @[Modules.scala 146:15]
    io_out_2_re <= _T_60_re @[Modules.scala 147:15]
    io_out_2_im <= _T_60_im @[Modules.scala 147:15]
    io_out_3_re <= _T_61_re @[Modules.scala 148:15]
    io_out_3_im <= _T_61_im @[Modules.scala 148:15]
    _T_28_re <= _GEN_1 @[Reg.scala 16:23]
    _T_28_im <= _GEN_0 @[Reg.scala 16:23]
    _T_29_re <= _GEN_3 @[Reg.scala 16:23]
    _T_29_im <= _GEN_2 @[Reg.scala 16:23]
    _T_30_re <= _GEN_5 @[Reg.scala 16:23]
    _T_30_im <= _GEN_4 @[Reg.scala 16:23]
    _T_31_re <= _GEN_7 @[Reg.scala 16:23]
    _T_31_im <= _GEN_6 @[Reg.scala 16:23]
    _T_32_re <= _GEN_9 @[Reg.scala 16:23]
    _T_32_im <= _GEN_8 @[Reg.scala 16:23]
    _T_33_re <= _GEN_11 @[Reg.scala 16:23]
    _T_33_im <= _GEN_10 @[Reg.scala 16:23]
    _T_41_re <= _GEN_13 @[Reg.scala 16:23]
    _T_41_im <= _GEN_12 @[Reg.scala 16:23]
    _T_42_re <= _GEN_15 @[Reg.scala 16:23]
    _T_42_im <= _GEN_14 @[Reg.scala 16:23]
    _T_43_re <= _GEN_17 @[Reg.scala 16:23]
    _T_43_im <= _GEN_16 @[Reg.scala 16:23]
    _T_51_re <= _GEN_19 @[Reg.scala 16:23]
    _T_51_im <= _GEN_18 @[Reg.scala 16:23]
    _T_52_re <= _GEN_21 @[Reg.scala 16:23]
    _T_52_im <= _GEN_20 @[Reg.scala 16:23]
    _T_60_re <= _GEN_23 @[Reg.scala 16:23]
    _T_60_im <= _GEN_22 @[Reg.scala 16:23]

  module FFT :
    input clock : Clock
    input reset : UInt<1>
    input io_dIn_0_re : SInt<32>
    input io_dIn_0_im : SInt<32>
    input io_dIn_1_re : SInt<32>
    input io_dIn_1_im : SInt<32>
    input io_dIn_2_re : SInt<32>
    input io_dIn_2_im : SInt<32>
    input io_dIn_3_re : SInt<32>
    input io_dIn_3_im : SInt<32>
    output io_dOut_0_re : SInt<32>
    output io_dOut_0_im : SInt<32>
    output io_dOut_1_re : SInt<32>
    output io_dOut_1_im : SInt<32>
    output io_dOut_2_re : SInt<32>
    output io_dOut_2_im : SInt<32>
    output io_dOut_3_re : SInt<32>
    output io_dOut_3_im : SInt<32>
    input io_din_valid : UInt<1>
    output io_dout_valid : UInt<1>
    output io_busy : UInt<1>
  
    inst ButterflyAdd of ButterflyAdd @[Modules.scala 96:22]
    inst Switch of Switch @[Modules.scala 153:22]
    inst Switch_1 of Switch_1 @[Modules.scala 153:22]
    reg cnt_0 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), cnt_0) @[FFT.scala 41:46]
    node busy = neq(cnt_0, UInt<1>("h0")) @[FFT.scala 45:22]
    node _T = or(io_din_valid, busy) @[FFT.scala 46:21]
    node _T_1 = eq(cnt_0, UInt<5>("h1f")) @[FFT.scala 47:26]
    node _T_2 = add(cnt_0, UInt<1>("h1")) @[FFT.scala 47:87]
    node _T_3 = tail(_T_2, 1) @[FFT.scala 47:87]
    node _T_4 = mux(_T_1, UInt<1>("h0"), _T_3) @[FFT.scala 47:18]
    node _GEN_0 = mux(_T, _T_4, cnt_0) @[FFT.scala 46:30]
    node _T_6 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_7 = _T_6
    node _T_8 = bits(_T_7, 31, 0) @[FFT.scala 53:113]
    node _T_9 = asSInt(_T_8) @[FFT.scala 53:113]
    node _T_10 = bits(_T_7, 63, 32) @[FFT.scala 53:113]
    node _T_11 = asSInt(_T_10) @[FFT.scala 53:113]
    node _T_13 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_14 = _T_13
    node _T_15 = bits(_T_14, 31, 0) @[FFT.scala 53:113]
    node _T_16 = asSInt(_T_15) @[FFT.scala 53:113]
    node _T_17 = bits(_T_14, 63, 32) @[FFT.scala 53:113]
    node _T_18 = asSInt(_T_17) @[FFT.scala 53:113]
    node _T_20 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_21 = _T_20
    node _T_22 = bits(_T_21, 31, 0) @[FFT.scala 53:113]
    node _T_23 = asSInt(_T_22) @[FFT.scala 53:113]
    node _T_24 = bits(_T_21, 63, 32) @[FFT.scala 53:113]
    node _T_25 = asSInt(_T_24) @[FFT.scala 53:113]
    node _T_27 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_28 = _T_27
    node _T_29 = bits(_T_28, 31, 0) @[FFT.scala 53:113]
    node _T_30 = asSInt(_T_29) @[FFT.scala 53:113]
    node _T_31 = bits(_T_28, 63, 32) @[FFT.scala 53:113]
    node _T_32 = asSInt(_T_31) @[FFT.scala 53:113]
    node _T_34 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_35 = _T_34
    node _T_36 = bits(_T_35, 31, 0) @[FFT.scala 53:113]
    node _T_37 = asSInt(_T_36) @[FFT.scala 53:113]
    node _T_38 = bits(_T_35, 63, 32) @[FFT.scala 53:113]
    node _T_39 = asSInt(_T_38) @[FFT.scala 53:113]
    node _T_41 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_42 = _T_41
    node _T_43 = bits(_T_42, 31, 0) @[FFT.scala 53:113]
    node _T_44 = asSInt(_T_43) @[FFT.scala 53:113]
    node _T_45 = bits(_T_42, 63, 32) @[FFT.scala 53:113]
    node _T_46 = asSInt(_T_45) @[FFT.scala 53:113]
    node _T_48 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_49 = _T_48
    node _T_50 = bits(_T_49, 31, 0) @[FFT.scala 53:113]
    node _T_51 = asSInt(_T_50) @[FFT.scala 53:113]
    node _T_52 = bits(_T_49, 63, 32) @[FFT.scala 53:113]
    node _T_53 = asSInt(_T_52) @[FFT.scala 53:113]
    node _T_55 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_56 = _T_55
    node _T_57 = bits(_T_56, 31, 0) @[FFT.scala 53:113]
    node _T_58 = asSInt(_T_57) @[FFT.scala 53:113]
    node _T_59 = bits(_T_56, 63, 32) @[FFT.scala 53:113]
    node _T_60 = asSInt(_T_59) @[FFT.scala 53:113]
    node _T_62 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_63 = _T_62
    node _T_64 = bits(_T_63, 31, 0) @[FFT.scala 53:113]
    node _T_65 = asSInt(_T_64) @[FFT.scala 53:113]
    node _T_66 = bits(_T_63, 63, 32) @[FFT.scala 53:113]
    node _T_67 = asSInt(_T_66) @[FFT.scala 53:113]
    node _T_69 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_70 = _T_69
    node _T_71 = bits(_T_70, 31, 0) @[FFT.scala 53:113]
    node _T_72 = asSInt(_T_71) @[FFT.scala 53:113]
    node _T_73 = bits(_T_70, 63, 32) @[FFT.scala 53:113]
    node _T_74 = asSInt(_T_73) @[FFT.scala 53:113]
    node _T_76 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_77 = _T_76
    node _T_78 = bits(_T_77, 31, 0) @[FFT.scala 53:113]
    node _T_79 = asSInt(_T_78) @[FFT.scala 53:113]
    node _T_80 = bits(_T_77, 63, 32) @[FFT.scala 53:113]
    node _T_81 = asSInt(_T_80) @[FFT.scala 53:113]
    node _T_83 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 53:113]
    node _T_84 = _T_83
    node _T_85 = bits(_T_84, 31, 0) @[FFT.scala 53:113]
    node _T_86 = asSInt(_T_85) @[FFT.scala 53:113]
    node _T_87 = bits(_T_84, 63, 32) @[FFT.scala 53:113]
    node _T_88 = asSInt(_T_87) @[FFT.scala 53:113]
    node _T_90 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_91 = _T_90
    node _T_92 = bits(_T_91, 31, 0) @[FFT.scala 54:117]
    node _T_93 = asSInt(_T_92) @[FFT.scala 54:117]
    node _T_94 = bits(_T_91, 63, 32) @[FFT.scala 54:117]
    node _T_95 = asSInt(_T_94) @[FFT.scala 54:117]
    node _T_97 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_98 = _T_97
    node _T_99 = bits(_T_98, 31, 0) @[FFT.scala 54:117]
    node _T_100 = asSInt(_T_99) @[FFT.scala 54:117]
    node _T_101 = bits(_T_98, 63, 32) @[FFT.scala 54:117]
    node _T_102 = asSInt(_T_101) @[FFT.scala 54:117]
    node _T_104 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_105 = _T_104
    node _T_106 = bits(_T_105, 31, 0) @[FFT.scala 54:117]
    node _T_107 = asSInt(_T_106) @[FFT.scala 54:117]
    node _T_108 = bits(_T_105, 63, 32) @[FFT.scala 54:117]
    node _T_109 = asSInt(_T_108) @[FFT.scala 54:117]
    node _T_111 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_112 = _T_111
    node _T_113 = bits(_T_112, 31, 0) @[FFT.scala 54:117]
    node _T_114 = asSInt(_T_113) @[FFT.scala 54:117]
    node _T_115 = bits(_T_112, 63, 32) @[FFT.scala 54:117]
    node _T_116 = asSInt(_T_115) @[FFT.scala 54:117]
    node _T_118 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_119 = _T_118
    node _T_120 = bits(_T_119, 31, 0) @[FFT.scala 54:117]
    node _T_121 = asSInt(_T_120) @[FFT.scala 54:117]
    node _T_122 = bits(_T_119, 63, 32) @[FFT.scala 54:117]
    node _T_123 = asSInt(_T_122) @[FFT.scala 54:117]
    node _T_125 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_126 = _T_125
    node _T_127 = bits(_T_126, 31, 0) @[FFT.scala 54:117]
    node _T_128 = asSInt(_T_127) @[FFT.scala 54:117]
    node _T_129 = bits(_T_126, 63, 32) @[FFT.scala 54:117]
    node _T_130 = asSInt(_T_129) @[FFT.scala 54:117]
    node _T_132 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_133 = _T_132
    node _T_134 = bits(_T_133, 31, 0) @[FFT.scala 54:117]
    node _T_135 = asSInt(_T_134) @[FFT.scala 54:117]
    node _T_136 = bits(_T_133, 63, 32) @[FFT.scala 54:117]
    node _T_137 = asSInt(_T_136) @[FFT.scala 54:117]
    node _T_139 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 54:117]
    node _T_140 = _T_139
    node _T_141 = bits(_T_140, 31, 0) @[FFT.scala 54:117]
    node _T_142 = asSInt(_T_141) @[FFT.scala 54:117]
    node _T_143 = bits(_T_140, 63, 32) @[FFT.scala 54:117]
    node _T_144 = asSInt(_T_143) @[FFT.scala 54:117]
    node _T_146 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 55:86]
    node _T_147 = _T_146
    node _T_148 = bits(_T_147, 31, 0) @[FFT.scala 55:86]
    node _T_149 = asSInt(_T_148) @[FFT.scala 55:86]
    node _T_150 = bits(_T_147, 63, 32) @[FFT.scala 55:86]
    node _T_151 = asSInt(_T_150) @[FFT.scala 55:86]
    node _T_153 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 55:86]
    node _T_154 = _T_153
    node _T_155 = bits(_T_154, 31, 0) @[FFT.scala 55:86]
    node _T_156 = asSInt(_T_155) @[FFT.scala 55:86]
    node _T_157 = bits(_T_154, 63, 32) @[FFT.scala 55:86]
    node _T_158 = asSInt(_T_157) @[FFT.scala 55:86]
    node _T_160 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 55:86]
    node _T_161 = _T_160
    node _T_162 = bits(_T_161, 31, 0) @[FFT.scala 55:86]
    node _T_163 = asSInt(_T_162) @[FFT.scala 55:86]
    node _T_164 = bits(_T_161, 63, 32) @[FFT.scala 55:86]
    node _T_165 = asSInt(_T_164) @[FFT.scala 55:86]
    node _T_167 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 55:86]
    node _T_168 = _T_167
    node _T_169 = bits(_T_168, 31, 0) @[FFT.scala 55:86]
    node _T_170 = asSInt(_T_169) @[FFT.scala 55:86]
    node _T_171 = bits(_T_168, 63, 32) @[FFT.scala 55:86]
    node _T_172 = asSInt(_T_171) @[FFT.scala 55:86]
    node _T_173 = bits(cnt_0, 3, 0) @[FFT.scala 60:26]
    node _T_174 = bits(cnt_0, 1, 0) @[FFT.scala 60:26]
    node _T_175 = bits(cnt_0, 3, 2) @[FFT.scala 90:31]
    node _T_176 = bits(cnt_0, 1, 0) @[FFT.scala 90:31]
    reg _T_177 : UInt<7>, clock with :
      reset => (UInt<1>("h0"), _T_177) @[FFT.scala 99:27]
    node _T_178 = eq(_T_177, UInt<5>("h1f")) @[FFT.scala 99:36]
    reg _T_179_0_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_0_re) @[FFT.scala 100:21]
    reg _T_179_0_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_0_im) @[FFT.scala 100:21]
    reg _T_179_1_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_1_re) @[FFT.scala 100:21]
    reg _T_179_1_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_1_im) @[FFT.scala 100:21]
    reg _T_179_2_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_2_re) @[FFT.scala 100:21]
    reg _T_179_2_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_2_im) @[FFT.scala 100:21]
    reg _T_179_3_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_3_re) @[FFT.scala 100:21]
    reg _T_179_3_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_179_3_im) @[FFT.scala 100:21]
    node _T_5_re = _T_11 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_5_im = _T_9 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_12_re = _T_18 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_12_im = _T_16 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_19_re = _T_25 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_19_im = _T_23 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_26_re = _T_32 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_26_im = _T_30 @[FFT.scala 53:113 FFT.scala 53:113]
    node data1_0_0_0_re = io_dIn_0_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node data1_0_0_0_im = io_dIn_0_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node data1_0_0_1_re = io_dIn_1_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node data1_0_0_1_im = io_dIn_1_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node data1_0_0_2_re = io_dIn_2_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node data1_0_0_2_im = io_dIn_2_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node data1_0_0_3_re = io_dIn_3_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node data1_0_0_3_im = io_dIn_3_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 96:17]
    node _T_33_re = _T_39 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_33_im = _T_37 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_40_re = _T_46 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_40_im = _T_44 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_47_re = _T_53 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_47_im = _T_51 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_54_re = _T_60 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_54_im = _T_58 @[FFT.scala 53:113 FFT.scala 53:113]
    node data1_1_0_0_re = Switch.io_out_0_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_1_0_0_im = Switch.io_out_0_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_1_0_1_re = Switch.io_out_1_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_1_0_1_im = Switch.io_out_1_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_1_0_2_re = Switch.io_out_2_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_1_0_2_im = Switch.io_out_2_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_1_0_3_re = Switch.io_out_3_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_1_0_3_im = Switch.io_out_3_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node _T_61_re = _T_67 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_61_im = _T_65 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_68_re = _T_74 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_68_im = _T_72 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_75_re = _T_81 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_75_im = _T_79 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_82_re = _T_88 @[FFT.scala 53:113 FFT.scala 53:113]
    node _T_82_im = _T_86 @[FFT.scala 53:113 FFT.scala 53:113]
    node data1_2_0_0_re = Switch_1.io_out_0_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_2_0_0_im = Switch_1.io_out_0_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_2_0_1_re = Switch_1.io_out_1_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_2_0_1_im = Switch_1.io_out_1_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_2_0_2_re = Switch_1.io_out_2_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_2_0_2_im = Switch_1.io_out_2_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_2_0_3_re = Switch_1.io_out_3_re @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node data1_2_0_3_im = Switch_1.io_out_3_im @[FFT.scala 53:65 FFT.scala 53:65 FFT.scala 91:54]
    node _T_89_re = _T_95 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_89_im = _T_93 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_96_re = _T_102 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_96_im = _T_100 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_103_re = _T_109 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_103_im = _T_107 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_110_re = _T_116 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_110_im = _T_114 @[FFT.scala 54:117 FFT.scala 54:117]
    node data2_0_0_0_re = _T_89_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_0_0_0_im = _T_89_im @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_0_0_1_re = _T_96_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_0_0_1_im = _T_96_im @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_0_0_2_re = _T_103_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_0_0_2_im = _T_103_im @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_0_0_3_re = _T_110_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_0_0_3_im = _T_110_im @[FFT.scala 54:69 FFT.scala 54:69]
    node _T_117_re = _T_123 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_117_im = _T_121 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_124_re = _T_130 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_124_im = _T_128 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_131_re = _T_137 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_131_im = _T_135 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_138_re = _T_144 @[FFT.scala 54:117 FFT.scala 54:117]
    node _T_138_im = _T_142 @[FFT.scala 54:117 FFT.scala 54:117]
    node data2_1_0_0_re = _T_117_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_1_0_0_im = _T_117_im @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_1_0_1_re = _T_124_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_1_0_1_im = _T_124_im @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_1_0_2_re = _T_131_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_1_0_2_im = _T_131_im @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_1_0_3_re = _T_138_re @[FFT.scala 54:69 FFT.scala 54:69]
    node data2_1_0_3_im = _T_138_im @[FFT.scala 54:69 FFT.scala 54:69]
    node _T_145_re = _T_151 @[FFT.scala 55:86 FFT.scala 55:86]
    node _T_145_im = _T_149 @[FFT.scala 55:86 FFT.scala 55:86]
    node _T_152_re = _T_158 @[FFT.scala 55:86 FFT.scala 55:86]
    node _T_152_im = _T_156 @[FFT.scala 55:86 FFT.scala 55:86]
    node _T_159_re = _T_165 @[FFT.scala 55:86 FFT.scala 55:86]
    node _T_159_im = _T_163 @[FFT.scala 55:86 FFT.scala 55:86]
    node _T_166_re = _T_172 @[FFT.scala 55:86 FFT.scala 55:86]
    node _T_166_im = _T_170 @[FFT.scala 55:86 FFT.scala 55:86]
    node datao_0_re = ButterflyAdd.io_out_0_re @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    node datao_0_im = ButterflyAdd.io_out_0_im @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    node datao_1_re = ButterflyAdd.io_out_1_re @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    node datao_1_im = ButterflyAdd.io_out_1_im @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    node datao_2_re = ButterflyAdd.io_out_2_re @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    node datao_2_im = ButterflyAdd.io_out_2_im @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    node datao_3_re = ButterflyAdd.io_out_3_re @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    node datao_3_im = ButterflyAdd.io_out_3_im @[FFT.scala 55:22 FFT.scala 55:22 FFT.scala 67:9]
    io_dOut_0_re <= _T_179_0_re @[FFT.scala 100:11]
    io_dOut_0_im <= _T_179_0_im @[FFT.scala 100:11]
    io_dOut_1_re <= _T_179_1_re @[FFT.scala 100:11]
    io_dOut_1_im <= _T_179_1_im @[FFT.scala 100:11]
    io_dOut_2_re <= _T_179_2_re @[FFT.scala 100:11]
    io_dOut_2_im <= _T_179_2_im @[FFT.scala 100:11]
    io_dOut_3_re <= _T_179_3_re @[FFT.scala 100:11]
    io_dOut_3_im <= _T_179_3_im @[FFT.scala 100:11]
    io_dout_valid <= _T_178 @[FFT.scala 99:17]
    io_busy <= busy @[FFT.scala 98:11]
    cnt_0 <= mux(reset, UInt<7>("h0"), _GEN_0) @[FFT.scala 47:12]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io_in_0_re <= data1_2_0_0_re @[Modules.scala 97:16]
    ButterflyAdd.io_in_0_im <= data1_2_0_0_im @[Modules.scala 97:16]
    ButterflyAdd.io_in_1_re <= data1_2_0_1_re @[Modules.scala 97:16]
    ButterflyAdd.io_in_1_im <= data1_2_0_1_im @[Modules.scala 97:16]
    ButterflyAdd.io_in_2_re <= data1_2_0_2_re @[Modules.scala 97:16]
    ButterflyAdd.io_in_2_im <= data1_2_0_2_im @[Modules.scala 97:16]
    ButterflyAdd.io_in_3_re <= data1_2_0_3_re @[Modules.scala 97:16]
    ButterflyAdd.io_in_3_im <= data1_2_0_3_im @[Modules.scala 97:16]
    Switch.clock <= clock
    Switch.reset <= reset
    Switch.io_in_0_re <= data2_0_0_0_re @[Modules.scala 154:16]
    Switch.io_in_0_im <= data2_0_0_0_im @[Modules.scala 154:16]
    Switch.io_in_1_re <= data2_0_0_1_re @[Modules.scala 154:16]
    Switch.io_in_1_im <= data2_0_0_1_im @[Modules.scala 154:16]
    Switch.io_in_2_re <= data2_0_0_2_re @[Modules.scala 154:16]
    Switch.io_in_2_im <= data2_0_0_2_im @[Modules.scala 154:16]
    Switch.io_in_3_re <= data2_0_0_3_re @[Modules.scala 154:16]
    Switch.io_in_3_im <= data2_0_0_3_im @[Modules.scala 154:16]
    Switch.io_sel <= _T_175 @[Modules.scala 155:17]
    Switch_1.clock <= clock
    Switch_1.reset <= reset
    Switch_1.io_in_0_re <= data2_1_0_0_re @[Modules.scala 154:16]
    Switch_1.io_in_0_im <= data2_1_0_0_im @[Modules.scala 154:16]
    Switch_1.io_in_1_re <= data2_1_0_1_re @[Modules.scala 154:16]
    Switch_1.io_in_1_im <= data2_1_0_1_im @[Modules.scala 154:16]
    Switch_1.io_in_2_re <= data2_1_0_2_re @[Modules.scala 154:16]
    Switch_1.io_in_2_im <= data2_1_0_2_im @[Modules.scala 154:16]
    Switch_1.io_in_3_re <= data2_1_0_3_re @[Modules.scala 154:16]
    Switch_1.io_in_3_im <= data2_1_0_3_im @[Modules.scala 154:16]
    Switch_1.io_sel <= _T_176 @[Modules.scala 155:17]
    _T_177 <= cnt_0 @[FFT.scala 99:27]
    _T_179_0_re <= datao_0_re @[FFT.scala 100:21]
    _T_179_0_im <= datao_0_im @[FFT.scala 100:21]
    _T_179_1_re <= datao_1_re @[FFT.scala 100:21]
    _T_179_1_im <= datao_1_im @[FFT.scala 100:21]
    _T_179_2_re <= datao_2_re @[FFT.scala 100:21]
    _T_179_2_im <= datao_2_im @[FFT.scala 100:21]
    _T_179_3_re <= datao_3_re @[FFT.scala 100:21]
    _T_179_3_im <= datao_3_im @[FFT.scala 100:21]
