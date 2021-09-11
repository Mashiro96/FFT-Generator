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
  
    node _T = add(io_op1_re, io_op2_re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asSInt(_T_1) @[Modules.scala 18:26]
    node _T_3 = add(io_op1_im, io_op2_im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asSInt(_T_4) @[Modules.scala 19:26]
    io_res_re <= _T_2 @[Modules.scala 18:13]
    io_res_im <= _T_5 @[Modules.scala 19:13]

  module ComplexSub :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>
  
    node _T = sub(io_op1_re, io_op2_re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asSInt(_T_1) @[Modules.scala 31:26]
    node _T_3 = sub(io_op1_im, io_op2_im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asSInt(_T_4) @[Modules.scala 32:26]
    io_res_re <= _T_2 @[Modules.scala 31:13]
    io_res_im <= _T_5 @[Modules.scala 32:13]

  module ButterflyAdd :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_re : SInt<32>
    input io_in_0_im : SInt<32>
    input io_in_1_re : SInt<32>
    input io_in_1_im : SInt<32>
    output io_out_0_re : SInt<32>
    output io_out_0_im : SInt<32>
    output io_out_1_re : SInt<32>
    output io_out_1_im : SInt<32>
  
    inst ComplexAdd of ComplexAdd @[Modules.scala 23:22]
    inst ComplexSub of ComplexSub @[Modules.scala 36:22]
    io_out_0_re <= ComplexAdd.io_res_re @[Modules.scala 84:15]
    io_out_0_im <= ComplexAdd.io_res_im @[Modules.scala 84:15]
    io_out_1_re <= ComplexSub.io_res_re @[Modules.scala 85:15]
    io_out_1_im <= ComplexSub.io_res_im @[Modules.scala 85:15]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io_op1_re <= io_in_0_re @[Modules.scala 24:17]
    ComplexAdd.io_op1_im <= io_in_0_im @[Modules.scala 24:17]
    ComplexAdd.io_op2_re <= io_in_1_re @[Modules.scala 25:17]
    ComplexAdd.io_op2_im <= io_in_1_im @[Modules.scala 25:17]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io_op1_re <= io_in_0_re @[Modules.scala 37:17]
    ComplexSub.io_op1_im <= io_in_0_im @[Modules.scala 37:17]
    ComplexSub.io_op2_re <= io_in_1_re @[Modules.scala 38:17]
    ComplexSub.io_op2_im <= io_in_1_im @[Modules.scala 38:17]

  module ComplexMul :
    input clock : Clock
    input reset : UInt<1>
    input io_op1_re : SInt<32>
    input io_op1_im : SInt<32>
    input io_op2_re : SInt<32>
    input io_op2_im : SInt<32>
    output io_res_re : SInt<32>
    output io_res_im : SInt<32>
  
    node _T = mul(io_op1_re, io_op2_re) @[Modules.scala 66:28]
    node _T_1 = mul(io_op1_im, io_op2_im) @[Modules.scala 66:52]
    node _T_2 = sub(_T, _T_1) @[Modules.scala 66:40]
    node _T_3 = tail(_T_2, 1) @[Modules.scala 66:40]
    node _T_4 = asSInt(_T_3) @[Modules.scala 66:40]
    node _T_5 = mul(io_op1_re, io_op2_im) @[Modules.scala 67:28]
    node _T_6 = mul(io_op1_im, io_op2_re) @[Modules.scala 67:52]
    node _T_7 = add(_T_5, _T_6) @[Modules.scala 67:40]
    node _T_8 = tail(_T_7, 1) @[Modules.scala 67:40]
    node _T_9 = asSInt(_T_8) @[Modules.scala 67:40]
    io_res_re <= asSInt(bits(shr(_T_4, 16), 31, 0)) @[Modules.scala 66:15]
    io_res_im <= asSInt(bits(shr(_T_9, 16), 31, 0)) @[Modules.scala 67:15]

  module ButterflyMul :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_re : SInt<32>
    input io_in_0_im : SInt<32>
    input io_in_1_re : SInt<32>
    input io_in_1_im : SInt<32>
    output io_out_0_re : SInt<32>
    output io_out_0_im : SInt<32>
    output io_out_1_re : SInt<32>
    output io_out_1_im : SInt<32>
    input io_wn_0_re : SInt<32>
    input io_wn_0_im : SInt<32>
  
    inst ButterflyAdd of ButterflyAdd @[Modules.scala 95:22]
    inst ComplexMul of ComplexMul @[Modules.scala 72:22]
    io_out_0_re <= ButterflyAdd.io_out_0_re @[Modules.scala 108:15]
    io_out_0_im <= ButterflyAdd.io_out_0_im @[Modules.scala 108:15]
    io_out_1_re <= ComplexMul.io_res_re @[Modules.scala 109:15]
    io_out_1_im <= ComplexMul.io_res_im @[Modules.scala 109:15]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io_in_0_re <= io_in_0_re @[Modules.scala 96:16]
    ButterflyAdd.io_in_0_im <= io_in_0_im @[Modules.scala 96:16]
    ButterflyAdd.io_in_1_re <= io_in_1_re @[Modules.scala 96:16]
    ButterflyAdd.io_in_1_im <= io_in_1_im @[Modules.scala 96:16]
    ComplexMul.clock <= clock
    ComplexMul.reset <= reset
    ComplexMul.io_op1_re <= ButterflyAdd.io_out_1_re @[Modules.scala 73:17]
    ComplexMul.io_op1_im <= ButterflyAdd.io_out_1_im @[Modules.scala 73:17]
    ComplexMul.io_op2_re <= io_wn_0_re @[Modules.scala 74:17]
    ComplexMul.io_op2_im <= io_wn_0_im @[Modules.scala 74:17]

  module Exchange :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_0_re : SInt<32>
    input io_in_0_0_im : SInt<32>
    input io_in_0_1_re : SInt<32>
    input io_in_0_1_im : SInt<32>
    input io_in_1_0_re : SInt<32>
    input io_in_1_0_im : SInt<32>
    input io_in_1_1_re : SInt<32>
    input io_in_1_1_im : SInt<32>
    output io_out_0_0_re : SInt<32>
    output io_out_0_0_im : SInt<32>
    output io_out_0_1_re : SInt<32>
    output io_out_0_1_im : SInt<32>
    output io_out_1_0_re : SInt<32>
    output io_out_1_0_im : SInt<32>
    output io_out_1_1_re : SInt<32>
    output io_out_1_1_im : SInt<32>
  
    io_out_0_0_re <= io_in_0_0_re @[Modules.scala 165:20]
    io_out_0_0_im <= io_in_0_0_im @[Modules.scala 165:20]
    io_out_0_1_re <= io_in_1_0_re @[Modules.scala 165:20]
    io_out_0_1_im <= io_in_1_0_im @[Modules.scala 165:20]
    io_out_1_0_re <= io_in_0_1_re @[Modules.scala 165:20]
    io_out_1_0_im <= io_in_0_1_im @[Modules.scala 165:20]
    io_out_1_1_re <= io_in_1_1_re @[Modules.scala 165:20]
    io_out_1_1_im <= io_in_1_1_im @[Modules.scala 165:20]

  module Switch :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_re : SInt<32>
    input io_in_0_im : SInt<32>
    input io_in_1_re : SInt<32>
    input io_in_1_im : SInt<32>
    output io_out_0_re : SInt<32>
    output io_out_0_im : SInt<32>
    output io_out_1_re : SInt<32>
    output io_out_1_im : SInt<32>
    input io_sel : UInt<1>
  
    node _T_1 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 138:71]
    node _T_2 = _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 138:71]
    node _T_4 = asSInt(_T_3) @[Modules.scala 138:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 138:71]
    node _T_6 = asSInt(_T_5) @[Modules.scala 138:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 138:71]
    node _T_9 = _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 138:71]
    node _T_11 = asSInt(_T_10) @[Modules.scala 138:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 138:71]
    node _T_13 = asSInt(_T_12) @[Modules.scala 138:71]
    reg _T_14_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_14_re) @[Reg.scala 15:16]
    reg _T_14_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_14_im) @[Reg.scala 15:16]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_1_im, _T_14_im) @[Reg.scala 16:19]
    node _GEN_1 = mux(UInt<1>("h1"), io_in_1_re, _T_14_re) @[Reg.scala 16:19]
    reg _T_15_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_15_re) @[Reg.scala 15:16]
    reg _T_15_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_15_im) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), _T_14_im, _T_15_im) @[Reg.scala 16:19]
    node _GEN_3 = mux(UInt<1>("h1"), _T_14_re, _T_15_re) @[Reg.scala 16:19]
    node _T_16 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 141:44]
    node swdata_1_re = _T_15_re @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node swdata_0_re = io_in_0_re @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node _T_17_re = mux(_T_16, swdata_1_re, swdata_0_re) @[Modules.scala 141:35]
    node swdata_1_im = _T_15_im @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node swdata_0_im = io_in_0_im @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node _T_17_im = mux(_T_16, swdata_1_im, swdata_0_im) @[Modules.scala 141:35]
    reg _T_18_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_18_re) @[Reg.scala 15:16]
    reg _T_18_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_18_im) @[Reg.scala 15:16]
    node _GEN_4 = mux(UInt<1>("h1"), _T_17_im, _T_18_im) @[Reg.scala 16:19]
    node _GEN_5 = mux(UInt<1>("h1"), _T_17_re, _T_18_re) @[Reg.scala 16:19]
    reg _T_19_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_19_re) @[Reg.scala 15:16]
    reg _T_19_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_19_im) @[Reg.scala 15:16]
    node _GEN_6 = mux(UInt<1>("h1"), _T_18_im, _T_19_im) @[Reg.scala 16:19]
    node _GEN_7 = mux(UInt<1>("h1"), _T_18_re, _T_19_re) @[Reg.scala 16:19]
    node _T_20 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 142:30]
    node _T_21_re = mux(_T_20, swdata_0_re, swdata_1_re) @[Modules.scala 142:21]
    node _T_21_im = mux(_T_20, swdata_0_im, swdata_1_im) @[Modules.scala 142:21]
    node _T_re = _T_6 @[Modules.scala 138:71 Modules.scala 138:71]
    node _T_im = _T_4 @[Modules.scala 138:71 Modules.scala 138:71]
    node _T_7_re = _T_13 @[Modules.scala 138:71 Modules.scala 138:71]
    node _T_7_im = _T_11 @[Modules.scala 138:71 Modules.scala 138:71]
    io_out_0_re <= _T_19_re @[Modules.scala 141:15]
    io_out_0_im <= _T_19_im @[Modules.scala 141:15]
    io_out_1_re <= _T_21_re @[Modules.scala 142:15]
    io_out_1_im <= _T_21_im @[Modules.scala 142:15]
    _T_14_re <= _GEN_1 @[Reg.scala 16:23]
    _T_14_im <= _GEN_0 @[Reg.scala 16:23]
    _T_15_re <= _GEN_3 @[Reg.scala 16:23]
    _T_15_im <= _GEN_2 @[Reg.scala 16:23]
    _T_18_re <= _GEN_5 @[Reg.scala 16:23]
    _T_18_im <= _GEN_4 @[Reg.scala 16:23]
    _T_19_re <= _GEN_7 @[Reg.scala 16:23]
    _T_19_im <= _GEN_6 @[Reg.scala 16:23]

  module Switch_2 :
    input clock : Clock
    input reset : UInt<1>
    input io_in_0_re : SInt<32>
    input io_in_0_im : SInt<32>
    input io_in_1_re : SInt<32>
    input io_in_1_im : SInt<32>
    output io_out_0_re : SInt<32>
    output io_out_0_im : SInt<32>
    output io_out_1_re : SInt<32>
    output io_out_1_im : SInt<32>
    input io_sel : UInt<1>
  
    node _T_1 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 138:71]
    node _T_2 = _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 138:71]
    node _T_4 = asSInt(_T_3) @[Modules.scala 138:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 138:71]
    node _T_6 = asSInt(_T_5) @[Modules.scala 138:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h0"))) @[Modules.scala 138:71]
    node _T_9 = _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 138:71]
    node _T_11 = asSInt(_T_10) @[Modules.scala 138:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 138:71]
    node _T_13 = asSInt(_T_12) @[Modules.scala 138:71]
    reg _T_14_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_14_re) @[Reg.scala 15:16]
    reg _T_14_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_14_im) @[Reg.scala 15:16]
    node _GEN_0 = mux(UInt<1>("h1"), io_in_1_im, _T_14_im) @[Reg.scala 16:19]
    node _GEN_1 = mux(UInt<1>("h1"), io_in_1_re, _T_14_re) @[Reg.scala 16:19]
    node _T_15 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 141:44]
    node swdata_1_re = _T_14_re @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node swdata_0_re = io_in_0_re @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node _T_16_re = mux(_T_15, swdata_1_re, swdata_0_re) @[Modules.scala 141:35]
    node swdata_1_im = _T_14_im @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node swdata_0_im = io_in_0_im @[Modules.scala 138:23 Modules.scala 138:23 Modules.scala 139:38]
    node _T_16_im = mux(_T_15, swdata_1_im, swdata_0_im) @[Modules.scala 141:35]
    reg _T_17_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_17_re) @[Reg.scala 15:16]
    reg _T_17_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_17_im) @[Reg.scala 15:16]
    node _GEN_2 = mux(UInt<1>("h1"), _T_16_im, _T_17_im) @[Reg.scala 16:19]
    node _GEN_3 = mux(UInt<1>("h1"), _T_16_re, _T_17_re) @[Reg.scala 16:19]
    node _T_18 = eq(io_sel, UInt<1>("h1")) @[Modules.scala 142:30]
    node _T_19_re = mux(_T_18, swdata_0_re, swdata_1_re) @[Modules.scala 142:21]
    node _T_19_im = mux(_T_18, swdata_0_im, swdata_1_im) @[Modules.scala 142:21]
    node _T_re = _T_6 @[Modules.scala 138:71 Modules.scala 138:71]
    node _T_im = _T_4 @[Modules.scala 138:71 Modules.scala 138:71]
    node _T_7_re = _T_13 @[Modules.scala 138:71 Modules.scala 138:71]
    node _T_7_im = _T_11 @[Modules.scala 138:71 Modules.scala 138:71]
    io_out_0_re <= _T_17_re @[Modules.scala 141:15]
    io_out_0_im <= _T_17_im @[Modules.scala 141:15]
    io_out_1_re <= _T_19_re @[Modules.scala 142:15]
    io_out_1_im <= _T_19_im @[Modules.scala 142:15]
    _T_14_re <= _GEN_1 @[Reg.scala 16:23]
    _T_14_im <= _GEN_0 @[Reg.scala 16:23]
    _T_17_re <= _GEN_3 @[Reg.scala 16:23]
    _T_17_im <= _GEN_2 @[Reg.scala 16:23]

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
  
    inst ButterflyMul of ButterflyMul @[Modules.scala 119:22]
    inst ButterflyMul_1 of ButterflyMul @[Modules.scala 119:22]
    inst ButterflyMul_2 of ButterflyMul @[Modules.scala 119:22]
    inst ButterflyMul_3 of ButterflyMul @[Modules.scala 119:22]
    inst ButterflyMul_4 of ButterflyMul @[Modules.scala 119:22]
    inst ButterflyMul_5 of ButterflyMul @[Modules.scala 119:22]
    inst ButterflyAdd of ButterflyAdd @[Modules.scala 95:22]
    inst ButterflyAdd_1 of ButterflyAdd @[Modules.scala 95:22]
    inst Exchange of Exchange @[Modules.scala 172:22]
    inst Switch of Switch @[Modules.scala 152:22]
    inst Switch_1 of Switch @[Modules.scala 152:22]
    inst Switch_2 of Switch_2 @[Modules.scala 152:22]
    inst Switch_3 of Switch_2 @[Modules.scala 152:22]
    reg cnt_0 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cnt_0) @[FFT.scala 37:46]
    reg cnt_1 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), cnt_1) @[FFT.scala 37:46]
    node busy = neq(cnt_0, UInt<1>("h0")) @[FFT.scala 41:21]
    node _T = or(io_din_valid, busy) @[FFT.scala 42:21]
    node _T_1 = eq(cnt_0, UInt<4>("h8")) @[FFT.scala 43:26]
    node _T_2 = add(cnt_0, UInt<1>("h1")) @[FFT.scala 43:87]
    node _T_3 = tail(_T_2, 1) @[FFT.scala 43:87]
    node _T_4 = mux(_T_1, UInt<1>("h0"), _T_3) @[FFT.scala 43:18]
    node _GEN_0 = mux(_T, _T_4, cnt_0) @[FFT.scala 42:30]
    node _T_6 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_7 = _T_6
    node _T_8 = bits(_T_7, 31, 0) @[FFT.scala 49:113]
    node _T_9 = asSInt(_T_8) @[FFT.scala 49:113]
    node _T_10 = bits(_T_7, 63, 32) @[FFT.scala 49:113]
    node _T_11 = asSInt(_T_10) @[FFT.scala 49:113]
    node _T_13 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_14 = _T_13
    node _T_15 = bits(_T_14, 31, 0) @[FFT.scala 49:113]
    node _T_16 = asSInt(_T_15) @[FFT.scala 49:113]
    node _T_17 = bits(_T_14, 63, 32) @[FFT.scala 49:113]
    node _T_18 = asSInt(_T_17) @[FFT.scala 49:113]
    node _T_20 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_21 = _T_20
    node _T_22 = bits(_T_21, 31, 0) @[FFT.scala 49:113]
    node _T_23 = asSInt(_T_22) @[FFT.scala 49:113]
    node _T_24 = bits(_T_21, 63, 32) @[FFT.scala 49:113]
    node _T_25 = asSInt(_T_24) @[FFT.scala 49:113]
    node _T_27 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_28 = _T_27
    node _T_29 = bits(_T_28, 31, 0) @[FFT.scala 49:113]
    node _T_30 = asSInt(_T_29) @[FFT.scala 49:113]
    node _T_31 = bits(_T_28, 63, 32) @[FFT.scala 49:113]
    node _T_32 = asSInt(_T_31) @[FFT.scala 49:113]
    node _T_34 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_35 = _T_34
    node _T_36 = bits(_T_35, 31, 0) @[FFT.scala 49:113]
    node _T_37 = asSInt(_T_36) @[FFT.scala 49:113]
    node _T_38 = bits(_T_35, 63, 32) @[FFT.scala 49:113]
    node _T_39 = asSInt(_T_38) @[FFT.scala 49:113]
    node _T_41 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_42 = _T_41
    node _T_43 = bits(_T_42, 31, 0) @[FFT.scala 49:113]
    node _T_44 = asSInt(_T_43) @[FFT.scala 49:113]
    node _T_45 = bits(_T_42, 63, 32) @[FFT.scala 49:113]
    node _T_46 = asSInt(_T_45) @[FFT.scala 49:113]
    node _T_48 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_49 = _T_48
    node _T_50 = bits(_T_49, 31, 0) @[FFT.scala 49:113]
    node _T_51 = asSInt(_T_50) @[FFT.scala 49:113]
    node _T_52 = bits(_T_49, 63, 32) @[FFT.scala 49:113]
    node _T_53 = asSInt(_T_52) @[FFT.scala 49:113]
    node _T_55 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_56 = _T_55
    node _T_57 = bits(_T_56, 31, 0) @[FFT.scala 49:113]
    node _T_58 = asSInt(_T_57) @[FFT.scala 49:113]
    node _T_59 = bits(_T_56, 63, 32) @[FFT.scala 49:113]
    node _T_60 = asSInt(_T_59) @[FFT.scala 49:113]
    node _T_62 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_63 = _T_62
    node _T_64 = bits(_T_63, 31, 0) @[FFT.scala 49:113]
    node _T_65 = asSInt(_T_64) @[FFT.scala 49:113]
    node _T_66 = bits(_T_63, 63, 32) @[FFT.scala 49:113]
    node _T_67 = asSInt(_T_66) @[FFT.scala 49:113]
    node _T_69 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_70 = _T_69
    node _T_71 = bits(_T_70, 31, 0) @[FFT.scala 49:113]
    node _T_72 = asSInt(_T_71) @[FFT.scala 49:113]
    node _T_73 = bits(_T_70, 63, 32) @[FFT.scala 49:113]
    node _T_74 = asSInt(_T_73) @[FFT.scala 49:113]
    node _T_76 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_77 = _T_76
    node _T_78 = bits(_T_77, 31, 0) @[FFT.scala 49:113]
    node _T_79 = asSInt(_T_78) @[FFT.scala 49:113]
    node _T_80 = bits(_T_77, 63, 32) @[FFT.scala 49:113]
    node _T_81 = asSInt(_T_80) @[FFT.scala 49:113]
    node _T_83 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_84 = _T_83
    node _T_85 = bits(_T_84, 31, 0) @[FFT.scala 49:113]
    node _T_86 = asSInt(_T_85) @[FFT.scala 49:113]
    node _T_87 = bits(_T_84, 63, 32) @[FFT.scala 49:113]
    node _T_88 = asSInt(_T_87) @[FFT.scala 49:113]
    node _T_90 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_91 = _T_90
    node _T_92 = bits(_T_91, 31, 0) @[FFT.scala 49:113]
    node _T_93 = asSInt(_T_92) @[FFT.scala 49:113]
    node _T_94 = bits(_T_91, 63, 32) @[FFT.scala 49:113]
    node _T_95 = asSInt(_T_94) @[FFT.scala 49:113]
    node _T_97 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_98 = _T_97
    node _T_99 = bits(_T_98, 31, 0) @[FFT.scala 49:113]
    node _T_100 = asSInt(_T_99) @[FFT.scala 49:113]
    node _T_101 = bits(_T_98, 63, 32) @[FFT.scala 49:113]
    node _T_102 = asSInt(_T_101) @[FFT.scala 49:113]
    node _T_104 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_105 = _T_104
    node _T_106 = bits(_T_105, 31, 0) @[FFT.scala 49:113]
    node _T_107 = asSInt(_T_106) @[FFT.scala 49:113]
    node _T_108 = bits(_T_105, 63, 32) @[FFT.scala 49:113]
    node _T_109 = asSInt(_T_108) @[FFT.scala 49:113]
    node _T_111 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 49:113]
    node _T_112 = _T_111
    node _T_113 = bits(_T_112, 31, 0) @[FFT.scala 49:113]
    node _T_114 = asSInt(_T_113) @[FFT.scala 49:113]
    node _T_115 = bits(_T_112, 63, 32) @[FFT.scala 49:113]
    node _T_116 = asSInt(_T_115) @[FFT.scala 49:113]
    node _T_118 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_119 = _T_118
    node _T_120 = bits(_T_119, 31, 0) @[FFT.scala 50:117]
    node _T_121 = asSInt(_T_120) @[FFT.scala 50:117]
    node _T_122 = bits(_T_119, 63, 32) @[FFT.scala 50:117]
    node _T_123 = asSInt(_T_122) @[FFT.scala 50:117]
    node _T_125 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_126 = _T_125
    node _T_127 = bits(_T_126, 31, 0) @[FFT.scala 50:117]
    node _T_128 = asSInt(_T_127) @[FFT.scala 50:117]
    node _T_129 = bits(_T_126, 63, 32) @[FFT.scala 50:117]
    node _T_130 = asSInt(_T_129) @[FFT.scala 50:117]
    node _T_132 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_133 = _T_132
    node _T_134 = bits(_T_133, 31, 0) @[FFT.scala 50:117]
    node _T_135 = asSInt(_T_134) @[FFT.scala 50:117]
    node _T_136 = bits(_T_133, 63, 32) @[FFT.scala 50:117]
    node _T_137 = asSInt(_T_136) @[FFT.scala 50:117]
    node _T_139 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_140 = _T_139
    node _T_141 = bits(_T_140, 31, 0) @[FFT.scala 50:117]
    node _T_142 = asSInt(_T_141) @[FFT.scala 50:117]
    node _T_143 = bits(_T_140, 63, 32) @[FFT.scala 50:117]
    node _T_144 = asSInt(_T_143) @[FFT.scala 50:117]
    node _T_146 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_147 = _T_146
    node _T_148 = bits(_T_147, 31, 0) @[FFT.scala 50:117]
    node _T_149 = asSInt(_T_148) @[FFT.scala 50:117]
    node _T_150 = bits(_T_147, 63, 32) @[FFT.scala 50:117]
    node _T_151 = asSInt(_T_150) @[FFT.scala 50:117]
    node _T_153 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_154 = _T_153
    node _T_155 = bits(_T_154, 31, 0) @[FFT.scala 50:117]
    node _T_156 = asSInt(_T_155) @[FFT.scala 50:117]
    node _T_157 = bits(_T_154, 63, 32) @[FFT.scala 50:117]
    node _T_158 = asSInt(_T_157) @[FFT.scala 50:117]
    node _T_160 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_161 = _T_160
    node _T_162 = bits(_T_161, 31, 0) @[FFT.scala 50:117]
    node _T_163 = asSInt(_T_162) @[FFT.scala 50:117]
    node _T_164 = bits(_T_161, 63, 32) @[FFT.scala 50:117]
    node _T_165 = asSInt(_T_164) @[FFT.scala 50:117]
    node _T_167 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_168 = _T_167
    node _T_169 = bits(_T_168, 31, 0) @[FFT.scala 50:117]
    node _T_170 = asSInt(_T_169) @[FFT.scala 50:117]
    node _T_171 = bits(_T_168, 63, 32) @[FFT.scala 50:117]
    node _T_172 = asSInt(_T_171) @[FFT.scala 50:117]
    node _T_174 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_175 = _T_174
    node _T_176 = bits(_T_175, 31, 0) @[FFT.scala 50:117]
    node _T_177 = asSInt(_T_176) @[FFT.scala 50:117]
    node _T_178 = bits(_T_175, 63, 32) @[FFT.scala 50:117]
    node _T_179 = asSInt(_T_178) @[FFT.scala 50:117]
    node _T_181 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_182 = _T_181
    node _T_183 = bits(_T_182, 31, 0) @[FFT.scala 50:117]
    node _T_184 = asSInt(_T_183) @[FFT.scala 50:117]
    node _T_185 = bits(_T_182, 63, 32) @[FFT.scala 50:117]
    node _T_186 = asSInt(_T_185) @[FFT.scala 50:117]
    node _T_188 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_189 = _T_188
    node _T_190 = bits(_T_189, 31, 0) @[FFT.scala 50:117]
    node _T_191 = asSInt(_T_190) @[FFT.scala 50:117]
    node _T_192 = bits(_T_189, 63, 32) @[FFT.scala 50:117]
    node _T_193 = asSInt(_T_192) @[FFT.scala 50:117]
    node _T_195 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 50:117]
    node _T_196 = _T_195
    node _T_197 = bits(_T_196, 31, 0) @[FFT.scala 50:117]
    node _T_198 = asSInt(_T_197) @[FFT.scala 50:117]
    node _T_199 = bits(_T_196, 63, 32) @[FFT.scala 50:117]
    node _T_200 = asSInt(_T_199) @[FFT.scala 50:117]
    node _T_202 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 51:86]
    node _T_203 = _T_202
    node _T_204 = bits(_T_203, 31, 0) @[FFT.scala 51:86]
    node _T_205 = asSInt(_T_204) @[FFT.scala 51:86]
    node _T_206 = bits(_T_203, 63, 32) @[FFT.scala 51:86]
    node _T_207 = asSInt(_T_206) @[FFT.scala 51:86]
    node _T_209 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 51:86]
    node _T_210 = _T_209
    node _T_211 = bits(_T_210, 31, 0) @[FFT.scala 51:86]
    node _T_212 = asSInt(_T_211) @[FFT.scala 51:86]
    node _T_213 = bits(_T_210, 63, 32) @[FFT.scala 51:86]
    node _T_214 = asSInt(_T_213) @[FFT.scala 51:86]
    node _T_216 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 51:86]
    node _T_217 = _T_216
    node _T_218 = bits(_T_217, 31, 0) @[FFT.scala 51:86]
    node _T_219 = asSInt(_T_218) @[FFT.scala 51:86]
    node _T_220 = bits(_T_217, 63, 32) @[FFT.scala 51:86]
    node _T_221 = asSInt(_T_220) @[FFT.scala 51:86]
    node _T_223 = asUInt(asSInt(UInt<64>("h0"))) @[FFT.scala 51:86]
    node _T_224 = _T_223
    node _T_225 = bits(_T_224, 31, 0) @[FFT.scala 51:86]
    node _T_226 = asSInt(_T_225) @[FFT.scala 51:86]
    node _T_227 = bits(_T_224, 63, 32) @[FFT.scala 51:86]
    node _T_228 = asSInt(_T_227) @[FFT.scala 51:86]
    node _T_229 = add(UInt<1>("h0"), cnt_0) @[FFT.scala 62:79]
    node _T_230 = tail(_T_229, 1) @[FFT.scala 62:79]
    node _T_233 = bits(_T_230, 2, 0)
    node _T_232_0 = asSInt(UInt<32>("h10000")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_1 = validif(eq(UInt<1>("h0"), _T_233), _T_232_0) @[FFT.scala 30:12]
    node _T_232_1 = asSInt(UInt<32>("hec83")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_2 = mux(eq(UInt<1>("h1"), _T_233), _T_232_1, _GEN_1) @[FFT.scala 30:12]
    node _T_232_2 = asSInt(UInt<32>("hb505")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_3 = mux(eq(UInt<2>("h2"), _T_233), _T_232_2, _GEN_2) @[FFT.scala 30:12]
    node _T_232_3 = asSInt(UInt<32>("h61f8")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_4 = mux(eq(UInt<2>("h3"), _T_233), _T_232_3, _GEN_3) @[FFT.scala 30:12]
    node _T_232_4 = asSInt(UInt<32>("h0")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_5 = mux(eq(UInt<3>("h4"), _T_233), _T_232_4, _GEN_4) @[FFT.scala 30:12]
    node _T_232_5 = asSInt(UInt<32>("hffff9e08")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_6 = mux(eq(UInt<3>("h5"), _T_233), _T_232_5, _GEN_5) @[FFT.scala 30:12]
    node _T_232_6 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_7 = mux(eq(UInt<3>("h6"), _T_233), _T_232_6, _GEN_6) @[FFT.scala 30:12]
    node _T_232_7 = asSInt(UInt<32>("hffff137d")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_8 = mux(eq(UInt<3>("h7"), _T_233), _T_232_7, _GEN_7) @[FFT.scala 30:12]
    node _T_235 = bits(_T_230, 2, 0)
    node _T_234_0 = asSInt(UInt<32>("h0")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_9 = validif(eq(UInt<1>("h0"), _T_235), _T_234_0) @[FFT.scala 31:12]
    node _T_234_1 = asSInt(UInt<32>("hffff9e08")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_10 = mux(eq(UInt<1>("h1"), _T_235), _T_234_1, _GEN_9) @[FFT.scala 31:12]
    node _T_234_2 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_11 = mux(eq(UInt<2>("h2"), _T_235), _T_234_2, _GEN_10) @[FFT.scala 31:12]
    node _T_234_3 = asSInt(UInt<32>("hffff137d")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_12 = mux(eq(UInt<2>("h3"), _T_235), _T_234_3, _GEN_11) @[FFT.scala 31:12]
    node _T_234_4 = asSInt(UInt<32>("hffff0000")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_13 = mux(eq(UInt<3>("h4"), _T_235), _T_234_4, _GEN_12) @[FFT.scala 31:12]
    node _T_234_5 = asSInt(UInt<32>("hffff137d")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_14 = mux(eq(UInt<3>("h5"), _T_235), _T_234_5, _GEN_13) @[FFT.scala 31:12]
    node _T_234_6 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_15 = mux(eq(UInt<3>("h6"), _T_235), _T_234_6, _GEN_14) @[FFT.scala 31:12]
    node _T_234_7 = asSInt(UInt<32>("hffff9e08")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_16 = mux(eq(UInt<3>("h7"), _T_235), _T_234_7, _GEN_15) @[FFT.scala 31:12]
    node _T_236 = add(UInt<3>("h4"), cnt_0) @[FFT.scala 62:79]
    node _T_237 = tail(_T_236, 1) @[FFT.scala 62:79]
    node _T_240 = bits(_T_237, 2, 0)
    node _T_239_0 = asSInt(UInt<32>("h10000")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_17 = validif(eq(UInt<1>("h0"), _T_240), _T_239_0) @[FFT.scala 30:12]
    node _T_239_1 = asSInt(UInt<32>("hec83")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_18 = mux(eq(UInt<1>("h1"), _T_240), _T_239_1, _GEN_17) @[FFT.scala 30:12]
    node _T_239_2 = asSInt(UInt<32>("hb505")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_19 = mux(eq(UInt<2>("h2"), _T_240), _T_239_2, _GEN_18) @[FFT.scala 30:12]
    node _T_239_3 = asSInt(UInt<32>("h61f8")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_20 = mux(eq(UInt<2>("h3"), _T_240), _T_239_3, _GEN_19) @[FFT.scala 30:12]
    node _T_239_4 = asSInt(UInt<32>("h0")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_21 = mux(eq(UInt<3>("h4"), _T_240), _T_239_4, _GEN_20) @[FFT.scala 30:12]
    node _T_239_5 = asSInt(UInt<32>("hffff9e08")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_22 = mux(eq(UInt<3>("h5"), _T_240), _T_239_5, _GEN_21) @[FFT.scala 30:12]
    node _T_239_6 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_23 = mux(eq(UInt<3>("h6"), _T_240), _T_239_6, _GEN_22) @[FFT.scala 30:12]
    node _T_239_7 = asSInt(UInt<32>("hffff137d")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_24 = mux(eq(UInt<3>("h7"), _T_240), _T_239_7, _GEN_23) @[FFT.scala 30:12]
    node _T_242 = bits(_T_237, 2, 0)
    node _T_241_0 = asSInt(UInt<32>("h0")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_25 = validif(eq(UInt<1>("h0"), _T_242), _T_241_0) @[FFT.scala 31:12]
    node _T_241_1 = asSInt(UInt<32>("hffff9e08")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_26 = mux(eq(UInt<1>("h1"), _T_242), _T_241_1, _GEN_25) @[FFT.scala 31:12]
    node _T_241_2 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_27 = mux(eq(UInt<2>("h2"), _T_242), _T_241_2, _GEN_26) @[FFT.scala 31:12]
    node _T_241_3 = asSInt(UInt<32>("hffff137d")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_28 = mux(eq(UInt<2>("h3"), _T_242), _T_241_3, _GEN_27) @[FFT.scala 31:12]
    node _T_241_4 = asSInt(UInt<32>("hffff0000")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_29 = mux(eq(UInt<3>("h4"), _T_242), _T_241_4, _GEN_28) @[FFT.scala 31:12]
    node _T_241_5 = asSInt(UInt<32>("hffff137d")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_30 = mux(eq(UInt<3>("h5"), _T_242), _T_241_5, _GEN_29) @[FFT.scala 31:12]
    node _T_241_6 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_31 = mux(eq(UInt<3>("h6"), _T_242), _T_241_6, _GEN_30) @[FFT.scala 31:12]
    node _T_241_7 = asSInt(UInt<32>("hffff9e08")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_32 = mux(eq(UInt<3>("h7"), _T_242), _T_241_7, _GEN_31) @[FFT.scala 31:12]
    node _T_243 = bits(cnt_1, 1, 0) @[FFT.scala 58:23]
    node _T_244 = add(UInt<1>("h0"), _T_243) @[FFT.scala 62:79]
    node _T_245 = tail(_T_244, 1) @[FFT.scala 62:79]
    node _T_247_0 = asSInt(UInt<32>("h10000")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_33 = validif(eq(UInt<1>("h0"), _T_245), _T_247_0) @[FFT.scala 30:12]
    node _T_247_1 = asSInt(UInt<32>("hb505")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_34 = mux(eq(UInt<1>("h1"), _T_245), _T_247_1, _GEN_33) @[FFT.scala 30:12]
    node _T_247_2 = asSInt(UInt<32>("h0")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_35 = mux(eq(UInt<2>("h2"), _T_245), _T_247_2, _GEN_34) @[FFT.scala 30:12]
    node _T_247_3 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_36 = mux(eq(UInt<2>("h3"), _T_245), _T_247_3, _GEN_35) @[FFT.scala 30:12]
    node _T_248_0 = asSInt(UInt<32>("h0")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_37 = validif(eq(UInt<1>("h0"), _T_245), _T_248_0) @[FFT.scala 31:12]
    node _T_248_1 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_38 = mux(eq(UInt<1>("h1"), _T_245), _T_248_1, _GEN_37) @[FFT.scala 31:12]
    node _T_248_2 = asSInt(UInt<32>("hffff0000")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_39 = mux(eq(UInt<2>("h2"), _T_245), _T_248_2, _GEN_38) @[FFT.scala 31:12]
    node _T_248_3 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_40 = mux(eq(UInt<2>("h3"), _T_245), _T_248_3, _GEN_39) @[FFT.scala 31:12]
    node _T_249 = add(UInt<1>("h0"), _T_243) @[FFT.scala 62:79]
    node _T_250 = tail(_T_249, 1) @[FFT.scala 62:79]
    node _T_252_0 = asSInt(UInt<32>("h10000")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_41 = validif(eq(UInt<1>("h0"), _T_250), _T_252_0) @[FFT.scala 30:12]
    node _T_252_1 = asSInt(UInt<32>("hb505")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_42 = mux(eq(UInt<1>("h1"), _T_250), _T_252_1, _GEN_41) @[FFT.scala 30:12]
    node _T_252_2 = asSInt(UInt<32>("h0")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_43 = mux(eq(UInt<2>("h2"), _T_250), _T_252_2, _GEN_42) @[FFT.scala 30:12]
    node _T_252_3 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_44 = mux(eq(UInt<2>("h3"), _T_250), _T_252_3, _GEN_43) @[FFT.scala 30:12]
    node _T_253_0 = asSInt(UInt<32>("h0")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_45 = validif(eq(UInt<1>("h0"), _T_250), _T_253_0) @[FFT.scala 31:12]
    node _T_253_1 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_46 = mux(eq(UInt<1>("h1"), _T_250), _T_253_1, _GEN_45) @[FFT.scala 31:12]
    node _T_253_2 = asSInt(UInt<32>("hffff0000")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_47 = mux(eq(UInt<2>("h2"), _T_250), _T_253_2, _GEN_46) @[FFT.scala 31:12]
    node _T_253_3 = asSInt(UInt<32>("hffff4afb")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_48 = mux(eq(UInt<2>("h3"), _T_250), _T_253_3, _GEN_47) @[FFT.scala 31:12]
    node _T_254 = bits(cnt_1, 0, 0) @[FFT.scala 58:23]
    node _T_255 = add(UInt<1>("h0"), _T_254) @[FFT.scala 62:79]
    node _T_256 = tail(_T_255, 1) @[FFT.scala 62:79]
    node _T_258_0 = asSInt(UInt<32>("h10000")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_49 = validif(eq(UInt<1>("h0"), _T_256), _T_258_0) @[FFT.scala 30:12]
    node _T_258_1 = asSInt(UInt<32>("h0")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_50 = mux(eq(UInt<1>("h1"), _T_256), _T_258_1, _GEN_49) @[FFT.scala 30:12]
    node _T_259_0 = asSInt(UInt<32>("h0")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_51 = validif(eq(UInt<1>("h0"), _T_256), _T_259_0) @[FFT.scala 31:12]
    node _T_259_1 = asSInt(UInt<32>("hffff0000")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_52 = mux(eq(UInt<1>("h1"), _T_256), _T_259_1, _GEN_51) @[FFT.scala 31:12]
    node _T_260 = add(UInt<1>("h0"), _T_254) @[FFT.scala 62:79]
    node _T_261 = tail(_T_260, 1) @[FFT.scala 62:79]
    node _T_263_0 = asSInt(UInt<32>("h10000")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_53 = validif(eq(UInt<1>("h0"), _T_261), _T_263_0) @[FFT.scala 30:12]
    node _T_263_1 = asSInt(UInt<32>("h0")) @[FFT.scala 26:12 FFT.scala 26:12]
    node _GEN_54 = mux(eq(UInt<1>("h1"), _T_261), _T_263_1, _GEN_53) @[FFT.scala 30:12]
    node _T_264_0 = asSInt(UInt<32>("h0")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_55 = validif(eq(UInt<1>("h0"), _T_261), _T_264_0) @[FFT.scala 31:12]
    node _T_264_1 = asSInt(UInt<32>("hffff0000")) @[FFT.scala 22:12 FFT.scala 22:12]
    node _GEN_56 = mux(eq(UInt<1>("h1"), _T_261), _T_264_1, _GEN_55) @[FFT.scala 31:12]
    reg _T_265_0_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_265_0_re) @[FFT.scala 85:67]
    reg _T_265_0_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_265_0_im) @[FFT.scala 85:67]
    reg _T_265_1_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_265_1_re) @[FFT.scala 85:67]
    reg _T_265_1_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_265_1_im) @[FFT.scala 85:67]
    reg _T_266_0_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_266_0_re) @[FFT.scala 85:67]
    reg _T_266_0_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_266_0_im) @[FFT.scala 85:67]
    reg _T_266_1_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_266_1_re) @[FFT.scala 85:67]
    reg _T_266_1_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_266_1_im) @[FFT.scala 85:67]
    node _T_267 = bits(cnt_1, 1, 1) @[FFT.scala 91:31]
    node _T_268 = bits(cnt_1, 0, 0) @[FFT.scala 91:31]
    reg _T_269 : UInt<4>, clock with :
      reset => (UInt<1>("h0"), _T_269) @[FFT.scala 100:27]
    node _T_270 = eq(_T_269, UInt<3>("h4")) @[FFT.scala 100:36]
    reg _T_271_0_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_0_re) @[FFT.scala 101:21]
    reg _T_271_0_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_0_im) @[FFT.scala 101:21]
    reg _T_271_1_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_1_re) @[FFT.scala 101:21]
    reg _T_271_1_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_1_im) @[FFT.scala 101:21]
    reg _T_271_2_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_2_re) @[FFT.scala 101:21]
    reg _T_271_2_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_2_im) @[FFT.scala 101:21]
    reg _T_271_3_re : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_3_re) @[FFT.scala 101:21]
    reg _T_271_3_im : SInt<32>, clock with :
      reset => (UInt<1>("h0"), _T_271_3_im) @[FFT.scala 101:21]
    node _T_5_re = _T_11 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_5_im = _T_9 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_12_re = _T_18 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_12_im = _T_16 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_0_0_0_re = io_dIn_0_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node data1_0_0_0_im = io_dIn_0_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node data1_0_0_1_re = io_dIn_1_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node data1_0_0_1_im = io_dIn_1_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node _T_19_re = _T_25 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_19_im = _T_23 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_26_re = _T_32 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_26_im = _T_30 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_0_1_0_re = io_dIn_2_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node data1_0_1_0_im = io_dIn_2_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node data1_0_1_1_re = io_dIn_3_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node data1_0_1_1_im = io_dIn_3_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 97:17]
    node _T_33_re = _T_39 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_33_im = _T_37 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_40_re = _T_46 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_40_im = _T_44 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_1_0_0_re = _T_265_0_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node data1_1_0_0_im = _T_265_0_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node data1_1_0_1_re = _T_265_1_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node data1_1_0_1_im = _T_265_1_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node _T_47_re = _T_53 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_47_im = _T_51 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_54_re = _T_60 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_54_im = _T_58 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_1_1_0_re = _T_266_0_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node data1_1_1_0_im = _T_266_0_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node data1_1_1_1_re = _T_266_1_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node data1_1_1_1_im = _T_266_1_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 85:57]
    node _T_61_re = _T_67 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_61_im = _T_65 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_68_re = _T_74 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_68_im = _T_72 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_2_0_0_re = Switch.io_out_0_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_2_0_0_im = Switch.io_out_0_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_2_0_1_re = Switch.io_out_1_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_2_0_1_im = Switch.io_out_1_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node _T_75_re = _T_81 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_75_im = _T_79 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_82_re = _T_88 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_82_im = _T_86 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_2_1_0_re = Switch_1.io_out_0_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_2_1_0_im = Switch_1.io_out_0_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_2_1_1_re = Switch_1.io_out_1_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_2_1_1_im = Switch_1.io_out_1_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node _T_89_re = _T_95 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_89_im = _T_93 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_96_re = _T_102 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_96_im = _T_100 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_3_0_0_re = Switch_2.io_out_0_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_3_0_0_im = Switch_2.io_out_0_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_3_0_1_re = Switch_2.io_out_1_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_3_0_1_im = Switch_2.io_out_1_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node _T_103_re = _T_109 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_103_im = _T_107 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_110_re = _T_116 @[FFT.scala 49:113 FFT.scala 49:113]
    node _T_110_im = _T_114 @[FFT.scala 49:113 FFT.scala 49:113]
    node data1_3_1_0_re = Switch_3.io_out_0_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_3_1_0_im = Switch_3.io_out_0_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_3_1_1_re = Switch_3.io_out_1_re @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node data1_3_1_1_im = Switch_3.io_out_1_im @[FFT.scala 49:65 FFT.scala 49:65 FFT.scala 92:54]
    node _T_117_re = _T_123 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_117_im = _T_121 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_124_re = _T_130 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_124_im = _T_128 @[FFT.scala 50:117 FFT.scala 50:117]
    node data2_0_0_0_re = ButterflyMul.io_out_0_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_0_0_0_im = ButterflyMul.io_out_0_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_0_0_1_re = ButterflyMul.io_out_1_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_0_0_1_im = ButterflyMul.io_out_1_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node _T_131_re = _T_137 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_131_im = _T_135 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_138_re = _T_144 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_138_im = _T_142 @[FFT.scala 50:117 FFT.scala 50:117]
    node data2_0_1_0_re = ButterflyMul_1.io_out_0_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_0_1_0_im = ButterflyMul_1.io_out_0_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_0_1_1_re = ButterflyMul_1.io_out_1_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_0_1_1_im = ButterflyMul_1.io_out_1_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node _T_145_re = _T_151 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_145_im = _T_149 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_152_re = _T_158 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_152_im = _T_156 @[FFT.scala 50:117 FFT.scala 50:117]
    node data2_1_0_0_re = ButterflyMul_2.io_out_0_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_1_0_0_im = ButterflyMul_2.io_out_0_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_1_0_1_re = ButterflyMul_2.io_out_1_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_1_0_1_im = ButterflyMul_2.io_out_1_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node _T_159_re = _T_165 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_159_im = _T_163 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_166_re = _T_172 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_166_im = _T_170 @[FFT.scala 50:117 FFT.scala 50:117]
    node data2_1_1_0_re = ButterflyMul_3.io_out_0_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_1_1_0_im = ButterflyMul_3.io_out_0_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_1_1_1_re = ButterflyMul_3.io_out_1_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_1_1_1_im = ButterflyMul_3.io_out_1_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node _T_173_re = _T_179 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_173_im = _T_177 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_180_re = _T_186 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_180_im = _T_184 @[FFT.scala 50:117 FFT.scala 50:117]
    node data2_2_0_0_re = ButterflyMul_4.io_out_0_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_2_0_0_im = ButterflyMul_4.io_out_0_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_2_0_1_re = ButterflyMul_4.io_out_1_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_2_0_1_im = ButterflyMul_4.io_out_1_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node _T_187_re = _T_193 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_187_im = _T_191 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_194_re = _T_200 @[FFT.scala 50:117 FFT.scala 50:117]
    node _T_194_im = _T_198 @[FFT.scala 50:117 FFT.scala 50:117]
    node data2_2_1_0_re = ButterflyMul_5.io_out_0_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_2_1_0_im = ButterflyMul_5.io_out_0_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_2_1_1_re = ButterflyMul_5.io_out_1_re @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node data2_2_1_1_im = ButterflyMul_5.io_out_1_im @[FFT.scala 50:69 FFT.scala 50:69 FFT.scala 63:19]
    node _T_201_re = _T_207 @[FFT.scala 51:86 FFT.scala 51:86]
    node _T_201_im = _T_205 @[FFT.scala 51:86 FFT.scala 51:86]
    node _T_208_re = _T_214 @[FFT.scala 51:86 FFT.scala 51:86]
    node _T_208_im = _T_212 @[FFT.scala 51:86 FFT.scala 51:86]
    node _T_215_re = _T_221 @[FFT.scala 51:86 FFT.scala 51:86]
    node _T_215_im = _T_219 @[FFT.scala 51:86 FFT.scala 51:86]
    node _T_222_re = _T_228 @[FFT.scala 51:86 FFT.scala 51:86]
    node _T_222_im = _T_226 @[FFT.scala 51:86 FFT.scala 51:86]
    node datao_0_re = ButterflyAdd.io_out_0_re @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node datao_0_im = ButterflyAdd.io_out_0_im @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node datao_1_re = ButterflyAdd.io_out_1_re @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node datao_1_im = ButterflyAdd.io_out_1_im @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node datao_2_re = ButterflyAdd_1.io_out_0_re @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node datao_2_im = ButterflyAdd_1.io_out_0_im @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node datao_3_re = ButterflyAdd_1.io_out_1_re @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node datao_3_im = ButterflyAdd_1.io_out_1_im @[FFT.scala 51:22 FFT.scala 51:22 FFT.scala 69:9]
    node _T_232_T_233 = _GEN_8 @[FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12]
    node _T_231_re = _T_232_T_233 @[FFT.scala 29:19 FFT.scala 30:12]
    node _T_234_T_235 = _GEN_16 @[FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12]
    node _T_231_im = _T_234_T_235 @[FFT.scala 29:19 FFT.scala 31:12]
    node _T_239_T_240 = _GEN_24 @[FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12]
    node _T_238_re = _T_239_T_240 @[FFT.scala 29:19 FFT.scala 30:12]
    node _T_241_T_242 = _GEN_32 @[FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12]
    node _T_238_im = _T_241_T_242 @[FFT.scala 29:19 FFT.scala 31:12]
    node _T_247_T_245 = _GEN_36 @[FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12]
    node _T_246_re = _T_247_T_245 @[FFT.scala 29:19 FFT.scala 30:12]
    node _T_248_T_245 = _GEN_40 @[FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12]
    node _T_246_im = _T_248_T_245 @[FFT.scala 29:19 FFT.scala 31:12]
    node _T_252_T_250 = _GEN_44 @[FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12]
    node _T_251_re = _T_252_T_250 @[FFT.scala 29:19 FFT.scala 30:12]
    node _T_253_T_250 = _GEN_48 @[FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12]
    node _T_251_im = _T_253_T_250 @[FFT.scala 29:19 FFT.scala 31:12]
    node _T_258_T_256 = _GEN_50 @[FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12]
    node _T_257_re = _T_258_T_256 @[FFT.scala 29:19 FFT.scala 30:12]
    node _T_259_T_256 = _GEN_52 @[FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12]
    node _T_257_im = _T_259_T_256 @[FFT.scala 29:19 FFT.scala 31:12]
    node _T_263_T_261 = _GEN_54 @[FFT.scala 30:12 FFT.scala 30:12 FFT.scala 30:12]
    node _T_262_re = _T_263_T_261 @[FFT.scala 29:19 FFT.scala 30:12]
    node _T_264_T_261 = _GEN_56 @[FFT.scala 31:12 FFT.scala 31:12 FFT.scala 31:12]
    node _T_262_im = _T_264_T_261 @[FFT.scala 29:19 FFT.scala 31:12]
    io_dOut_0_re <= _T_271_0_re @[FFT.scala 101:11]
    io_dOut_0_im <= _T_271_0_im @[FFT.scala 101:11]
    io_dOut_1_re <= _T_271_1_re @[FFT.scala 101:11]
    io_dOut_1_im <= _T_271_1_im @[FFT.scala 101:11]
    io_dOut_2_re <= _T_271_2_re @[FFT.scala 101:11]
    io_dOut_2_im <= _T_271_2_im @[FFT.scala 101:11]
    io_dOut_3_re <= _T_271_3_re @[FFT.scala 101:11]
    io_dOut_3_im <= _T_271_3_im @[FFT.scala 101:11]
    io_dout_valid <= _T_270 @[FFT.scala 100:17]
    io_busy <= busy @[FFT.scala 99:11]
    cnt_0 <= mux(reset, UInt<4>("h0"), _GEN_0) @[FFT.scala 43:12]
    cnt_1 <= mux(reset, UInt<4>("h0"), cnt_0) @[FFT.scala 39:16]
    ButterflyMul.clock <= clock
    ButterflyMul.reset <= reset
    ButterflyMul.io_in_0_re <= data1_0_0_0_re @[Modules.scala 120:16]
    ButterflyMul.io_in_0_im <= data1_0_0_0_im @[Modules.scala 120:16]
    ButterflyMul.io_in_1_re <= data1_0_0_1_re @[Modules.scala 120:16]
    ButterflyMul.io_in_1_im <= data1_0_0_1_im @[Modules.scala 120:16]
    ButterflyMul.io_wn_0_re <= _T_231_re @[Modules.scala 121:16]
    ButterflyMul.io_wn_0_im <= _T_231_im @[Modules.scala 121:16]
    ButterflyMul_1.clock <= clock
    ButterflyMul_1.reset <= reset
    ButterflyMul_1.io_in_0_re <= data1_0_1_0_re @[Modules.scala 120:16]
    ButterflyMul_1.io_in_0_im <= data1_0_1_0_im @[Modules.scala 120:16]
    ButterflyMul_1.io_in_1_re <= data1_0_1_1_re @[Modules.scala 120:16]
    ButterflyMul_1.io_in_1_im <= data1_0_1_1_im @[Modules.scala 120:16]
    ButterflyMul_1.io_wn_0_re <= _T_238_re @[Modules.scala 121:16]
    ButterflyMul_1.io_wn_0_im <= _T_238_im @[Modules.scala 121:16]
    ButterflyMul_2.clock <= clock
    ButterflyMul_2.reset <= reset
    ButterflyMul_2.io_in_0_re <= data1_1_0_0_re @[Modules.scala 120:16]
    ButterflyMul_2.io_in_0_im <= data1_1_0_0_im @[Modules.scala 120:16]
    ButterflyMul_2.io_in_1_re <= data1_1_0_1_re @[Modules.scala 120:16]
    ButterflyMul_2.io_in_1_im <= data1_1_0_1_im @[Modules.scala 120:16]
    ButterflyMul_2.io_wn_0_re <= _T_246_re @[Modules.scala 121:16]
    ButterflyMul_2.io_wn_0_im <= _T_246_im @[Modules.scala 121:16]
    ButterflyMul_3.clock <= clock
    ButterflyMul_3.reset <= reset
    ButterflyMul_3.io_in_0_re <= data1_1_1_0_re @[Modules.scala 120:16]
    ButterflyMul_3.io_in_0_im <= data1_1_1_0_im @[Modules.scala 120:16]
    ButterflyMul_3.io_in_1_re <= data1_1_1_1_re @[Modules.scala 120:16]
    ButterflyMul_3.io_in_1_im <= data1_1_1_1_im @[Modules.scala 120:16]
    ButterflyMul_3.io_wn_0_re <= _T_251_re @[Modules.scala 121:16]
    ButterflyMul_3.io_wn_0_im <= _T_251_im @[Modules.scala 121:16]
    ButterflyMul_4.clock <= clock
    ButterflyMul_4.reset <= reset
    ButterflyMul_4.io_in_0_re <= data1_2_0_0_re @[Modules.scala 120:16]
    ButterflyMul_4.io_in_0_im <= data1_2_0_0_im @[Modules.scala 120:16]
    ButterflyMul_4.io_in_1_re <= data1_2_0_1_re @[Modules.scala 120:16]
    ButterflyMul_4.io_in_1_im <= data1_2_0_1_im @[Modules.scala 120:16]
    ButterflyMul_4.io_wn_0_re <= _T_257_re @[Modules.scala 121:16]
    ButterflyMul_4.io_wn_0_im <= _T_257_im @[Modules.scala 121:16]
    ButterflyMul_5.clock <= clock
    ButterflyMul_5.reset <= reset
    ButterflyMul_5.io_in_0_re <= data1_2_1_0_re @[Modules.scala 120:16]
    ButterflyMul_5.io_in_0_im <= data1_2_1_0_im @[Modules.scala 120:16]
    ButterflyMul_5.io_in_1_re <= data1_2_1_1_re @[Modules.scala 120:16]
    ButterflyMul_5.io_in_1_im <= data1_2_1_1_im @[Modules.scala 120:16]
    ButterflyMul_5.io_wn_0_re <= _T_262_re @[Modules.scala 121:16]
    ButterflyMul_5.io_wn_0_im <= _T_262_im @[Modules.scala 121:16]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io_in_0_re <= data1_3_0_0_re @[Modules.scala 96:16]
    ButterflyAdd.io_in_0_im <= data1_3_0_0_im @[Modules.scala 96:16]
    ButterflyAdd.io_in_1_re <= data1_3_0_1_re @[Modules.scala 96:16]
    ButterflyAdd.io_in_1_im <= data1_3_0_1_im @[Modules.scala 96:16]
    ButterflyAdd_1.clock <= clock
    ButterflyAdd_1.reset <= reset
    ButterflyAdd_1.io_in_0_re <= data1_3_1_0_re @[Modules.scala 96:16]
    ButterflyAdd_1.io_in_0_im <= data1_3_1_0_im @[Modules.scala 96:16]
    ButterflyAdd_1.io_in_1_re <= data1_3_1_1_re @[Modules.scala 96:16]
    ButterflyAdd_1.io_in_1_im <= data1_3_1_1_im @[Modules.scala 96:16]
    Exchange.clock <= clock
    Exchange.reset <= reset
    Exchange.io_in_0_0_re <= data2_0_0_0_re @[Modules.scala 173:16]
    Exchange.io_in_0_0_im <= data2_0_0_0_im @[Modules.scala 173:16]
    Exchange.io_in_0_1_re <= data2_0_0_1_re @[Modules.scala 173:16]
    Exchange.io_in_0_1_im <= data2_0_0_1_im @[Modules.scala 173:16]
    Exchange.io_in_1_0_re <= data2_0_1_0_re @[Modules.scala 173:16]
    Exchange.io_in_1_0_im <= data2_0_1_0_im @[Modules.scala 173:16]
    Exchange.io_in_1_1_re <= data2_0_1_1_re @[Modules.scala 173:16]
    Exchange.io_in_1_1_im <= data2_0_1_1_im @[Modules.scala 173:16]
    _T_265_0_re <= Exchange.io_out_0_0_re @[FFT.scala 85:67]
    _T_265_0_im <= Exchange.io_out_0_0_im @[FFT.scala 85:67]
    _T_265_1_re <= Exchange.io_out_0_1_re @[FFT.scala 85:67]
    _T_265_1_im <= Exchange.io_out_0_1_im @[FFT.scala 85:67]
    _T_266_0_re <= Exchange.io_out_1_0_re @[FFT.scala 85:67]
    _T_266_0_im <= Exchange.io_out_1_0_im @[FFT.scala 85:67]
    _T_266_1_re <= Exchange.io_out_1_1_re @[FFT.scala 85:67]
    _T_266_1_im <= Exchange.io_out_1_1_im @[FFT.scala 85:67]
    Switch.clock <= clock
    Switch.reset <= reset
    Switch.io_in_0_re <= data2_1_0_0_re @[Modules.scala 153:16]
    Switch.io_in_0_im <= data2_1_0_0_im @[Modules.scala 153:16]
    Switch.io_in_1_re <= data2_1_0_1_re @[Modules.scala 153:16]
    Switch.io_in_1_im <= data2_1_0_1_im @[Modules.scala 153:16]
    Switch.io_sel <= _T_267 @[Modules.scala 154:17]
    Switch_1.clock <= clock
    Switch_1.reset <= reset
    Switch_1.io_in_0_re <= data2_1_1_0_re @[Modules.scala 153:16]
    Switch_1.io_in_0_im <= data2_1_1_0_im @[Modules.scala 153:16]
    Switch_1.io_in_1_re <= data2_1_1_1_re @[Modules.scala 153:16]
    Switch_1.io_in_1_im <= data2_1_1_1_im @[Modules.scala 153:16]
    Switch_1.io_sel <= _T_267 @[Modules.scala 154:17]
    Switch_2.clock <= clock
    Switch_2.reset <= reset
    Switch_2.io_in_0_re <= data2_2_0_0_re @[Modules.scala 153:16]
    Switch_2.io_in_0_im <= data2_2_0_0_im @[Modules.scala 153:16]
    Switch_2.io_in_1_re <= data2_2_0_1_re @[Modules.scala 153:16]
    Switch_2.io_in_1_im <= data2_2_0_1_im @[Modules.scala 153:16]
    Switch_2.io_sel <= _T_268 @[Modules.scala 154:17]
    Switch_3.clock <= clock
    Switch_3.reset <= reset
    Switch_3.io_in_0_re <= data2_2_1_0_re @[Modules.scala 153:16]
    Switch_3.io_in_0_im <= data2_2_1_0_im @[Modules.scala 153:16]
    Switch_3.io_in_1_re <= data2_2_1_1_re @[Modules.scala 153:16]
    Switch_3.io_in_1_im <= data2_2_1_1_im @[Modules.scala 153:16]
    Switch_3.io_sel <= _T_268 @[Modules.scala 154:17]
    _T_269 <= cnt_0 @[FFT.scala 100:27]
    _T_271_0_re <= datao_0_re @[FFT.scala 101:21]
    _T_271_0_im <= datao_0_im @[FFT.scala 101:21]
    _T_271_1_re <= datao_1_re @[FFT.scala 101:21]
    _T_271_1_im <= datao_1_im @[FFT.scala 101:21]
    _T_271_2_re <= datao_2_re @[FFT.scala 101:21]
    _T_271_2_im <= datao_2_im @[FFT.scala 101:21]
    _T_271_3_re <= datao_3_re @[FFT.scala 101:21]
    _T_271_3_im <= datao_3_im @[FFT.scala 101:21]
