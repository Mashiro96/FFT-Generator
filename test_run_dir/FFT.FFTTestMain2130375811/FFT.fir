;buildInfoPackage: chisel3, version: 3.2.8, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit FFT : 
  module ComplexAdd : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module ComplexMul : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = mul(io.op1.re, io.op2.re) @[Modules.scala 66:28]
    node _T_1 = mul(io.op1.im, io.op2.im) @[Modules.scala 66:52]
    node _T_2 = sub(_T, _T_1) @[Modules.scala 66:40]
    node _T_3 = tail(_T_2, 1) @[Modules.scala 66:40]
    node _T_4 = asFixedPoint(_T_3, 32) @[Modules.scala 66:40]
    io.res.re <= _T_4 @[Modules.scala 66:15]
    node _T_5 = mul(io.op1.re, io.op2.im) @[Modules.scala 67:28]
    node _T_6 = mul(io.op1.im, io.op2.re) @[Modules.scala 67:52]
    node _T_7 = add(_T_5, _T_6) @[Modules.scala 67:40]
    node _T_8 = tail(_T_7, 1) @[Modules.scala 67:40]
    node _T_9 = asFixedPoint(_T_8, 32) @[Modules.scala 67:40]
    io.res.im <= _T_9 @[Modules.scala 67:15]
    
  module ButterflyMul : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip wn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[1]}
    
    inst ButterflyAdd of ButterflyAdd @[Modules.scala 95:22]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io.in[0].im <= io.in[0].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[0].re <= io.in[0].re @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].im <= io.in[1].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].re <= io.in[1].re @[Modules.scala 96:16]
    io.out[0].im <= ButterflyAdd.io.out[0].im @[Modules.scala 108:15]
    io.out[0].re <= ButterflyAdd.io.out[0].re @[Modules.scala 108:15]
    inst ComplexMul of ComplexMul @[Modules.scala 72:22]
    ComplexMul.clock <= clock
    ComplexMul.reset <= reset
    ComplexMul.io.op1.im <= ButterflyAdd.io.out[1].im @[Modules.scala 73:17]
    ComplexMul.io.op1.re <= ButterflyAdd.io.out[1].re @[Modules.scala 73:17]
    ComplexMul.io.op2.im <= io.wn[0].im @[Modules.scala 74:17]
    ComplexMul.io.op2.re <= io.wn[0].re @[Modules.scala 74:17]
    io.out[1].im <= ComplexMul.io.res.im @[Modules.scala 109:15]
    io.out[1].re <= ComplexMul.io.res.re @[Modules.scala 109:15]
    
  module ComplexAdd_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd_1 @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub_1 @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module ComplexMul_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = mul(io.op1.re, io.op2.re) @[Modules.scala 66:28]
    node _T_1 = mul(io.op1.im, io.op2.im) @[Modules.scala 66:52]
    node _T_2 = sub(_T, _T_1) @[Modules.scala 66:40]
    node _T_3 = tail(_T_2, 1) @[Modules.scala 66:40]
    node _T_4 = asFixedPoint(_T_3, 32) @[Modules.scala 66:40]
    io.res.re <= _T_4 @[Modules.scala 66:15]
    node _T_5 = mul(io.op1.re, io.op2.im) @[Modules.scala 67:28]
    node _T_6 = mul(io.op1.im, io.op2.re) @[Modules.scala 67:52]
    node _T_7 = add(_T_5, _T_6) @[Modules.scala 67:40]
    node _T_8 = tail(_T_7, 1) @[Modules.scala 67:40]
    node _T_9 = asFixedPoint(_T_8, 32) @[Modules.scala 67:40]
    io.res.im <= _T_9 @[Modules.scala 67:15]
    
  module ButterflyMul_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip wn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[1]}
    
    inst ButterflyAdd of ButterflyAdd_1 @[Modules.scala 95:22]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io.in[0].im <= io.in[0].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[0].re <= io.in[0].re @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].im <= io.in[1].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].re <= io.in[1].re @[Modules.scala 96:16]
    io.out[0].im <= ButterflyAdd.io.out[0].im @[Modules.scala 108:15]
    io.out[0].re <= ButterflyAdd.io.out[0].re @[Modules.scala 108:15]
    inst ComplexMul of ComplexMul_1 @[Modules.scala 72:22]
    ComplexMul.clock <= clock
    ComplexMul.reset <= reset
    ComplexMul.io.op1.im <= ButterflyAdd.io.out[1].im @[Modules.scala 73:17]
    ComplexMul.io.op1.re <= ButterflyAdd.io.out[1].re @[Modules.scala 73:17]
    ComplexMul.io.op2.im <= io.wn[0].im @[Modules.scala 74:17]
    ComplexMul.io.op2.re <= io.wn[0].re @[Modules.scala 74:17]
    io.out[1].im <= ComplexMul.io.res.im @[Modules.scala 109:15]
    io.out[1].re <= ComplexMul.io.res.re @[Modules.scala 109:15]
    
  module ComplexAdd_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd_2 @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub_2 @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module ComplexMul_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = mul(io.op1.re, io.op2.re) @[Modules.scala 66:28]
    node _T_1 = mul(io.op1.im, io.op2.im) @[Modules.scala 66:52]
    node _T_2 = sub(_T, _T_1) @[Modules.scala 66:40]
    node _T_3 = tail(_T_2, 1) @[Modules.scala 66:40]
    node _T_4 = asFixedPoint(_T_3, 32) @[Modules.scala 66:40]
    io.res.re <= _T_4 @[Modules.scala 66:15]
    node _T_5 = mul(io.op1.re, io.op2.im) @[Modules.scala 67:28]
    node _T_6 = mul(io.op1.im, io.op2.re) @[Modules.scala 67:52]
    node _T_7 = add(_T_5, _T_6) @[Modules.scala 67:40]
    node _T_8 = tail(_T_7, 1) @[Modules.scala 67:40]
    node _T_9 = asFixedPoint(_T_8, 32) @[Modules.scala 67:40]
    io.res.im <= _T_9 @[Modules.scala 67:15]
    
  module ButterflyMul_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip wn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[1]}
    
    inst ButterflyAdd of ButterflyAdd_2 @[Modules.scala 95:22]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io.in[0].im <= io.in[0].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[0].re <= io.in[0].re @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].im <= io.in[1].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].re <= io.in[1].re @[Modules.scala 96:16]
    io.out[0].im <= ButterflyAdd.io.out[0].im @[Modules.scala 108:15]
    io.out[0].re <= ButterflyAdd.io.out[0].re @[Modules.scala 108:15]
    inst ComplexMul of ComplexMul_2 @[Modules.scala 72:22]
    ComplexMul.clock <= clock
    ComplexMul.reset <= reset
    ComplexMul.io.op1.im <= ButterflyAdd.io.out[1].im @[Modules.scala 73:17]
    ComplexMul.io.op1.re <= ButterflyAdd.io.out[1].re @[Modules.scala 73:17]
    ComplexMul.io.op2.im <= io.wn[0].im @[Modules.scala 74:17]
    ComplexMul.io.op2.re <= io.wn[0].re @[Modules.scala 74:17]
    io.out[1].im <= ComplexMul.io.res.im @[Modules.scala 109:15]
    io.out[1].re <= ComplexMul.io.res.re @[Modules.scala 109:15]
    
  module ComplexAdd_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd_3 @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub_3 @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module ComplexMul_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = mul(io.op1.re, io.op2.re) @[Modules.scala 66:28]
    node _T_1 = mul(io.op1.im, io.op2.im) @[Modules.scala 66:52]
    node _T_2 = sub(_T, _T_1) @[Modules.scala 66:40]
    node _T_3 = tail(_T_2, 1) @[Modules.scala 66:40]
    node _T_4 = asFixedPoint(_T_3, 32) @[Modules.scala 66:40]
    io.res.re <= _T_4 @[Modules.scala 66:15]
    node _T_5 = mul(io.op1.re, io.op2.im) @[Modules.scala 67:28]
    node _T_6 = mul(io.op1.im, io.op2.re) @[Modules.scala 67:52]
    node _T_7 = add(_T_5, _T_6) @[Modules.scala 67:40]
    node _T_8 = tail(_T_7, 1) @[Modules.scala 67:40]
    node _T_9 = asFixedPoint(_T_8, 32) @[Modules.scala 67:40]
    io.res.im <= _T_9 @[Modules.scala 67:15]
    
  module ButterflyMul_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip wn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[1]}
    
    inst ButterflyAdd of ButterflyAdd_3 @[Modules.scala 95:22]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io.in[0].im <= io.in[0].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[0].re <= io.in[0].re @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].im <= io.in[1].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].re <= io.in[1].re @[Modules.scala 96:16]
    io.out[0].im <= ButterflyAdd.io.out[0].im @[Modules.scala 108:15]
    io.out[0].re <= ButterflyAdd.io.out[0].re @[Modules.scala 108:15]
    inst ComplexMul of ComplexMul_3 @[Modules.scala 72:22]
    ComplexMul.clock <= clock
    ComplexMul.reset <= reset
    ComplexMul.io.op1.im <= ButterflyAdd.io.out[1].im @[Modules.scala 73:17]
    ComplexMul.io.op1.re <= ButterflyAdd.io.out[1].re @[Modules.scala 73:17]
    ComplexMul.io.op2.im <= io.wn[0].im @[Modules.scala 74:17]
    ComplexMul.io.op2.re <= io.wn[0].re @[Modules.scala 74:17]
    io.out[1].im <= ComplexMul.io.res.im @[Modules.scala 109:15]
    io.out[1].re <= ComplexMul.io.res.re @[Modules.scala 109:15]
    
  module ComplexAdd_4 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub_4 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd_4 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd_4 @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub_4 @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module ComplexMul_4 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = mul(io.op1.re, io.op2.re) @[Modules.scala 66:28]
    node _T_1 = mul(io.op1.im, io.op2.im) @[Modules.scala 66:52]
    node _T_2 = sub(_T, _T_1) @[Modules.scala 66:40]
    node _T_3 = tail(_T_2, 1) @[Modules.scala 66:40]
    node _T_4 = asFixedPoint(_T_3, 32) @[Modules.scala 66:40]
    io.res.re <= _T_4 @[Modules.scala 66:15]
    node _T_5 = mul(io.op1.re, io.op2.im) @[Modules.scala 67:28]
    node _T_6 = mul(io.op1.im, io.op2.re) @[Modules.scala 67:52]
    node _T_7 = add(_T_5, _T_6) @[Modules.scala 67:40]
    node _T_8 = tail(_T_7, 1) @[Modules.scala 67:40]
    node _T_9 = asFixedPoint(_T_8, 32) @[Modules.scala 67:40]
    io.res.im <= _T_9 @[Modules.scala 67:15]
    
  module ButterflyMul_4 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip wn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[1]}
    
    inst ButterflyAdd of ButterflyAdd_4 @[Modules.scala 95:22]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io.in[0].im <= io.in[0].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[0].re <= io.in[0].re @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].im <= io.in[1].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].re <= io.in[1].re @[Modules.scala 96:16]
    io.out[0].im <= ButterflyAdd.io.out[0].im @[Modules.scala 108:15]
    io.out[0].re <= ButterflyAdd.io.out[0].re @[Modules.scala 108:15]
    inst ComplexMul of ComplexMul_4 @[Modules.scala 72:22]
    ComplexMul.clock <= clock
    ComplexMul.reset <= reset
    ComplexMul.io.op1.im <= ButterflyAdd.io.out[1].im @[Modules.scala 73:17]
    ComplexMul.io.op1.re <= ButterflyAdd.io.out[1].re @[Modules.scala 73:17]
    ComplexMul.io.op2.im <= io.wn[0].im @[Modules.scala 74:17]
    ComplexMul.io.op2.re <= io.wn[0].re @[Modules.scala 74:17]
    io.out[1].im <= ComplexMul.io.res.im @[Modules.scala 109:15]
    io.out[1].re <= ComplexMul.io.res.re @[Modules.scala 109:15]
    
  module ComplexAdd_5 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub_5 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd_5 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd_5 @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub_5 @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module ComplexMul_5 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = mul(io.op1.re, io.op2.re) @[Modules.scala 66:28]
    node _T_1 = mul(io.op1.im, io.op2.im) @[Modules.scala 66:52]
    node _T_2 = sub(_T, _T_1) @[Modules.scala 66:40]
    node _T_3 = tail(_T_2, 1) @[Modules.scala 66:40]
    node _T_4 = asFixedPoint(_T_3, 32) @[Modules.scala 66:40]
    io.res.re <= _T_4 @[Modules.scala 66:15]
    node _T_5 = mul(io.op1.re, io.op2.im) @[Modules.scala 67:28]
    node _T_6 = mul(io.op1.im, io.op2.re) @[Modules.scala 67:52]
    node _T_7 = add(_T_5, _T_6) @[Modules.scala 67:40]
    node _T_8 = tail(_T_7, 1) @[Modules.scala 67:40]
    node _T_9 = asFixedPoint(_T_8, 32) @[Modules.scala 67:40]
    io.res.im <= _T_9 @[Modules.scala 67:15]
    
  module ButterflyMul_5 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip wn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[1]}
    
    inst ButterflyAdd of ButterflyAdd_5 @[Modules.scala 95:22]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io.in[0].im <= io.in[0].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[0].re <= io.in[0].re @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].im <= io.in[1].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].re <= io.in[1].re @[Modules.scala 96:16]
    io.out[0].im <= ButterflyAdd.io.out[0].im @[Modules.scala 108:15]
    io.out[0].re <= ButterflyAdd.io.out[0].re @[Modules.scala 108:15]
    inst ComplexMul of ComplexMul_5 @[Modules.scala 72:22]
    ComplexMul.clock <= clock
    ComplexMul.reset <= reset
    ComplexMul.io.op1.im <= ButterflyAdd.io.out[1].im @[Modules.scala 73:17]
    ComplexMul.io.op1.re <= ButterflyAdd.io.out[1].re @[Modules.scala 73:17]
    ComplexMul.io.op2.im <= io.wn[0].im @[Modules.scala 74:17]
    ComplexMul.io.op2.re <= io.wn[0].re @[Modules.scala 74:17]
    io.out[1].im <= ComplexMul.io.res.im @[Modules.scala 109:15]
    io.out[1].re <= ComplexMul.io.res.re @[Modules.scala 109:15]
    
  module ComplexAdd_6 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub_6 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd_6 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd_6 @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub_6 @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module ComplexAdd_7 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = add(io.op1.re, io.op2.re) @[Modules.scala 18:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 18:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 18:26]
    io.res.re <= _T_2 @[Modules.scala 18:13]
    node _T_3 = add(io.op1.im, io.op2.im) @[Modules.scala 19:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 19:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 19:26]
    io.res.im <= _T_5 @[Modules.scala 19:13]
    
  module ComplexSub_7 : 
    input clock : Clock
    input reset : Reset
    output io : {flip op1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, flip op2 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, res : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}}
    
    node _T = sub(io.op1.re, io.op2.re) @[Modules.scala 31:26]
    node _T_1 = tail(_T, 1) @[Modules.scala 31:26]
    node _T_2 = asFixedPoint(_T_1, 16) @[Modules.scala 31:26]
    io.res.re <= _T_2 @[Modules.scala 31:13]
    node _T_3 = sub(io.op1.im, io.op2.im) @[Modules.scala 32:26]
    node _T_4 = tail(_T_3, 1) @[Modules.scala 32:26]
    node _T_5 = asFixedPoint(_T_4, 16) @[Modules.scala 32:26]
    io.res.im <= _T_5 @[Modules.scala 32:13]
    
  module ButterflyAdd_7 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2]}
    
    inst ComplexAdd of ComplexAdd_7 @[Modules.scala 23:22]
    ComplexAdd.clock <= clock
    ComplexAdd.reset <= reset
    ComplexAdd.io.op1.im <= io.in[0].im @[Modules.scala 24:17]
    ComplexAdd.io.op1.re <= io.in[0].re @[Modules.scala 24:17]
    ComplexAdd.io.op2.im <= io.in[1].im @[Modules.scala 25:17]
    ComplexAdd.io.op2.re <= io.in[1].re @[Modules.scala 25:17]
    io.out[0].im <= ComplexAdd.io.res.im @[Modules.scala 84:15]
    io.out[0].re <= ComplexAdd.io.res.re @[Modules.scala 84:15]
    inst ComplexSub of ComplexSub_7 @[Modules.scala 36:22]
    ComplexSub.clock <= clock
    ComplexSub.reset <= reset
    ComplexSub.io.op1.im <= io.in[0].im @[Modules.scala 37:17]
    ComplexSub.io.op1.re <= io.in[0].re @[Modules.scala 37:17]
    ComplexSub.io.op2.im <= io.in[1].im @[Modules.scala 38:17]
    ComplexSub.io.op2.re <= io.in[1].re @[Modules.scala 38:17]
    io.out[1].im <= ComplexSub.io.res.im @[Modules.scala 85:15]
    io.out[1].re <= ComplexSub.io.res.re @[Modules.scala 85:15]
    
  module Exchange : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2][2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2][2]}
    
    io.out[0][0].im <= io.in[0][0].im @[Modules.scala 163:66]
    io.out[0][0].re <= io.in[0][0].re @[Modules.scala 163:66]
    io.out[0][1].im <= io.in[1][0].im @[Modules.scala 163:66]
    io.out[0][1].re <= io.in[1][0].re @[Modules.scala 163:66]
    io.out[1][0].im <= io.in[0][1].im @[Modules.scala 163:66]
    io.out[1][0].re <= io.in[0][1].re @[Modules.scala 163:66]
    io.out[1][1].im <= io.in[1][1].im @[Modules.scala 163:66]
    io.out[1][1].re <= io.in[1][1].re @[Modules.scala 163:66]
    
  module Switch : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip sel : UInt<1>}
    
    wire _T : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_1 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_2 : UInt<64>
    _T_2 <= _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 138:71]
    node _T_4 = asFixedPoint(_T_3, 16) @[Modules.scala 138:71]
    _T.im <= _T_4 @[Modules.scala 138:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 138:71]
    node _T_6 = asFixedPoint(_T_5, 16) @[Modules.scala 138:71]
    _T.re <= _T_6 @[Modules.scala 138:71]
    wire _T_7 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_9 : UInt<64>
    _T_9 <= _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 138:71]
    node _T_11 = asFixedPoint(_T_10, 16) @[Modules.scala 138:71]
    _T_7.im <= _T_11 @[Modules.scala 138:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 138:71]
    node _T_13 = asFixedPoint(_T_12, 16) @[Modules.scala 138:71]
    _T_7.re <= _T_13 @[Modules.scala 138:71]
    wire swdata : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[Modules.scala 138:23]
    swdata[0].im <= _T.im @[Modules.scala 138:23]
    swdata[0].re <= _T.re @[Modules.scala 138:23]
    swdata[1].im <= _T_7.im @[Modules.scala 138:23]
    swdata[1].re <= _T_7.re @[Modules.scala 138:23]
    swdata[0].im <= io.in[0].im @[Modules.scala 139:38]
    swdata[0].re <= io.in[0].re @[Modules.scala 139:38]
    reg _T_14 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_14.im <= io.in[1].im @[Reg.scala 16:23]
      _T_14.re <= io.in[1].re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg _T_15 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_15.im <= _T_14.im @[Reg.scala 16:23]
      _T_15.re <= _T_14.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    swdata[1].im <= _T_15.im @[Modules.scala 139:38]
    swdata[1].re <= _T_15.re @[Modules.scala 139:38]
    node _T_16 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 141:44]
    node _T_17 = mux(_T_16, swdata[1], swdata[0]) @[Modules.scala 141:35]
    reg _T_18 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_18.im <= _T_17.im @[Reg.scala 16:23]
      _T_18.re <= _T_17.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg _T_19 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_19.im <= _T_18.im @[Reg.scala 16:23]
      _T_19.re <= _T_18.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out[0].im <= _T_19.im @[Modules.scala 141:15]
    io.out[0].re <= _T_19.re @[Modules.scala 141:15]
    node _T_20 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 142:30]
    node _T_21 = mux(_T_20, swdata[0], swdata[1]) @[Modules.scala 142:21]
    io.out[1].im <= _T_21.im @[Modules.scala 142:15]
    io.out[1].re <= _T_21.re @[Modules.scala 142:15]
    
  module Switch_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip sel : UInt<1>}
    
    wire _T : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_1 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_2 : UInt<64>
    _T_2 <= _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 138:71]
    node _T_4 = asFixedPoint(_T_3, 16) @[Modules.scala 138:71]
    _T.im <= _T_4 @[Modules.scala 138:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 138:71]
    node _T_6 = asFixedPoint(_T_5, 16) @[Modules.scala 138:71]
    _T.re <= _T_6 @[Modules.scala 138:71]
    wire _T_7 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_9 : UInt<64>
    _T_9 <= _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 138:71]
    node _T_11 = asFixedPoint(_T_10, 16) @[Modules.scala 138:71]
    _T_7.im <= _T_11 @[Modules.scala 138:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 138:71]
    node _T_13 = asFixedPoint(_T_12, 16) @[Modules.scala 138:71]
    _T_7.re <= _T_13 @[Modules.scala 138:71]
    wire swdata : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[Modules.scala 138:23]
    swdata[0].im <= _T.im @[Modules.scala 138:23]
    swdata[0].re <= _T.re @[Modules.scala 138:23]
    swdata[1].im <= _T_7.im @[Modules.scala 138:23]
    swdata[1].re <= _T_7.re @[Modules.scala 138:23]
    swdata[0].im <= io.in[0].im @[Modules.scala 139:38]
    swdata[0].re <= io.in[0].re @[Modules.scala 139:38]
    reg _T_14 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_14.im <= io.in[1].im @[Reg.scala 16:23]
      _T_14.re <= io.in[1].re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg _T_15 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_15.im <= _T_14.im @[Reg.scala 16:23]
      _T_15.re <= _T_14.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    swdata[1].im <= _T_15.im @[Modules.scala 139:38]
    swdata[1].re <= _T_15.re @[Modules.scala 139:38]
    node _T_16 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 141:44]
    node _T_17 = mux(_T_16, swdata[1], swdata[0]) @[Modules.scala 141:35]
    reg _T_18 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_18.im <= _T_17.im @[Reg.scala 16:23]
      _T_18.re <= _T_17.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    reg _T_19 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_19.im <= _T_18.im @[Reg.scala 16:23]
      _T_19.re <= _T_18.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out[0].im <= _T_19.im @[Modules.scala 141:15]
    io.out[0].re <= _T_19.re @[Modules.scala 141:15]
    node _T_20 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 142:30]
    node _T_21 = mux(_T_20, swdata[0], swdata[1]) @[Modules.scala 142:21]
    io.out[1].im <= _T_21.im @[Modules.scala 142:15]
    io.out[1].re <= _T_21.re @[Modules.scala 142:15]
    
  module Switch_2 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip sel : UInt<1>}
    
    wire _T : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_1 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_2 : UInt<64>
    _T_2 <= _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 138:71]
    node _T_4 = asFixedPoint(_T_3, 16) @[Modules.scala 138:71]
    _T.im <= _T_4 @[Modules.scala 138:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 138:71]
    node _T_6 = asFixedPoint(_T_5, 16) @[Modules.scala 138:71]
    _T.re <= _T_6 @[Modules.scala 138:71]
    wire _T_7 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_9 : UInt<64>
    _T_9 <= _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 138:71]
    node _T_11 = asFixedPoint(_T_10, 16) @[Modules.scala 138:71]
    _T_7.im <= _T_11 @[Modules.scala 138:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 138:71]
    node _T_13 = asFixedPoint(_T_12, 16) @[Modules.scala 138:71]
    _T_7.re <= _T_13 @[Modules.scala 138:71]
    wire swdata : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[Modules.scala 138:23]
    swdata[0].im <= _T.im @[Modules.scala 138:23]
    swdata[0].re <= _T.re @[Modules.scala 138:23]
    swdata[1].im <= _T_7.im @[Modules.scala 138:23]
    swdata[1].re <= _T_7.re @[Modules.scala 138:23]
    swdata[0].im <= io.in[0].im @[Modules.scala 139:38]
    swdata[0].re <= io.in[0].re @[Modules.scala 139:38]
    reg _T_14 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_14.im <= io.in[1].im @[Reg.scala 16:23]
      _T_14.re <= io.in[1].re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    swdata[1].im <= _T_14.im @[Modules.scala 139:38]
    swdata[1].re <= _T_14.re @[Modules.scala 139:38]
    node _T_15 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 141:44]
    node _T_16 = mux(_T_15, swdata[1], swdata[0]) @[Modules.scala 141:35]
    reg _T_17 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_17.im <= _T_16.im @[Reg.scala 16:23]
      _T_17.re <= _T_16.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out[0].im <= _T_17.im @[Modules.scala 141:15]
    io.out[0].re <= _T_17.re @[Modules.scala 141:15]
    node _T_18 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 142:30]
    node _T_19 = mux(_T_18, swdata[0], swdata[1]) @[Modules.scala 142:21]
    io.out[1].im <= _T_19.im @[Modules.scala 142:15]
    io.out[1].re <= _T_19.re @[Modules.scala 142:15]
    
  module Switch_3 : 
    input clock : Clock
    input reset : Reset
    output io : {flip in : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], out : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], flip sel : UInt<1>}
    
    wire _T : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_1 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_2 : UInt<64>
    _T_2 <= _T_1
    node _T_3 = bits(_T_2, 31, 0) @[Modules.scala 138:71]
    node _T_4 = asFixedPoint(_T_3, 16) @[Modules.scala 138:71]
    _T.im <= _T_4 @[Modules.scala 138:71]
    node _T_5 = bits(_T_2, 63, 32) @[Modules.scala 138:71]
    node _T_6 = asFixedPoint(_T_5, 16) @[Modules.scala 138:71]
    _T.re <= _T_6 @[Modules.scala 138:71]
    wire _T_7 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[Modules.scala 138:71]
    node _T_8 = asUInt(asSInt(UInt<64>("h00"))) @[Modules.scala 138:71]
    wire _T_9 : UInt<64>
    _T_9 <= _T_8
    node _T_10 = bits(_T_9, 31, 0) @[Modules.scala 138:71]
    node _T_11 = asFixedPoint(_T_10, 16) @[Modules.scala 138:71]
    _T_7.im <= _T_11 @[Modules.scala 138:71]
    node _T_12 = bits(_T_9, 63, 32) @[Modules.scala 138:71]
    node _T_13 = asFixedPoint(_T_12, 16) @[Modules.scala 138:71]
    _T_7.re <= _T_13 @[Modules.scala 138:71]
    wire swdata : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[Modules.scala 138:23]
    swdata[0].im <= _T.im @[Modules.scala 138:23]
    swdata[0].re <= _T.re @[Modules.scala 138:23]
    swdata[1].im <= _T_7.im @[Modules.scala 138:23]
    swdata[1].re <= _T_7.re @[Modules.scala 138:23]
    swdata[0].im <= io.in[0].im @[Modules.scala 139:38]
    swdata[0].re <= io.in[0].re @[Modules.scala 139:38]
    reg _T_14 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_14.im <= io.in[1].im @[Reg.scala 16:23]
      _T_14.re <= io.in[1].re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    swdata[1].im <= _T_14.im @[Modules.scala 139:38]
    swdata[1].re <= _T_14.re @[Modules.scala 139:38]
    node _T_15 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 141:44]
    node _T_16 = mux(_T_15, swdata[1], swdata[0]) @[Modules.scala 141:35]
    reg _T_17 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}, clock @[Reg.scala 15:16]
    when UInt<1>("h01") : @[Reg.scala 16:19]
      _T_17.im <= _T_16.im @[Reg.scala 16:23]
      _T_17.re <= _T_16.re @[Reg.scala 16:23]
      skip @[Reg.scala 16:19]
    io.out[0].im <= _T_17.im @[Modules.scala 141:15]
    io.out[0].re <= _T_17.re @[Modules.scala 141:15]
    node _T_18 = eq(io.sel, UInt<1>("h01")) @[Modules.scala 142:30]
    node _T_19 = mux(_T_18, swdata[0], swdata[1]) @[Modules.scala 142:21]
    io.out[1].im <= _T_19.im @[Modules.scala 142:15]
    io.out[1].re <= _T_19.re @[Modules.scala 142:15]
    
  module FFT : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip dIn : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[4], dOut : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[4], flip din_valid : UInt<1>, dout_valid : UInt<1>, busy : UInt<1>}
    
    reg cnt_0 : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[FFT.scala 37:46]
    reg cnt_1 : UInt<4>, clock with : (reset => (reset, UInt<4>("h00"))) @[FFT.scala 37:46]
    cnt_1 <= cnt_0 @[FFT.scala 39:16]
    node busy = neq(cnt_0, UInt<1>("h00")) @[FFT.scala 41:21]
    node _T = or(io.din_valid, busy) @[FFT.scala 42:21]
    when _T : @[FFT.scala 42:30]
      node _T_1 = eq(cnt_0, UInt<4>("h08")) @[FFT.scala 43:26]
      node _T_2 = add(cnt_0, UInt<1>("h01")) @[FFT.scala 43:87]
      node _T_3 = tail(_T_2, 1) @[FFT.scala 43:87]
      node _T_4 = mux(_T_1, UInt<1>("h00"), _T_3) @[FFT.scala 43:18]
      cnt_0 <= _T_4 @[FFT.scala 43:12]
      skip @[FFT.scala 42:30]
    wire _T_5 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_6 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_7 : UInt<64>
    _T_7 <= _T_6
    node _T_8 = bits(_T_7, 31, 0) @[FFT.scala 49:113]
    node _T_9 = asFixedPoint(_T_8, 16) @[FFT.scala 49:113]
    _T_5.im <= _T_9 @[FFT.scala 49:113]
    node _T_10 = bits(_T_7, 63, 32) @[FFT.scala 49:113]
    node _T_11 = asFixedPoint(_T_10, 16) @[FFT.scala 49:113]
    _T_5.re <= _T_11 @[FFT.scala 49:113]
    wire _T_12 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_13 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_14 : UInt<64>
    _T_14 <= _T_13
    node _T_15 = bits(_T_14, 31, 0) @[FFT.scala 49:113]
    node _T_16 = asFixedPoint(_T_15, 16) @[FFT.scala 49:113]
    _T_12.im <= _T_16 @[FFT.scala 49:113]
    node _T_17 = bits(_T_14, 63, 32) @[FFT.scala 49:113]
    node _T_18 = asFixedPoint(_T_17, 16) @[FFT.scala 49:113]
    _T_12.re <= _T_18 @[FFT.scala 49:113]
    wire data1_0_0 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_0_0[0].im <= _T_5.im @[FFT.scala 49:65]
    data1_0_0[0].re <= _T_5.re @[FFT.scala 49:65]
    data1_0_0[1].im <= _T_12.im @[FFT.scala 49:65]
    data1_0_0[1].re <= _T_12.re @[FFT.scala 49:65]
    wire _T_19 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_20 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_21 : UInt<64>
    _T_21 <= _T_20
    node _T_22 = bits(_T_21, 31, 0) @[FFT.scala 49:113]
    node _T_23 = asFixedPoint(_T_22, 16) @[FFT.scala 49:113]
    _T_19.im <= _T_23 @[FFT.scala 49:113]
    node _T_24 = bits(_T_21, 63, 32) @[FFT.scala 49:113]
    node _T_25 = asFixedPoint(_T_24, 16) @[FFT.scala 49:113]
    _T_19.re <= _T_25 @[FFT.scala 49:113]
    wire _T_26 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_27 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_28 : UInt<64>
    _T_28 <= _T_27
    node _T_29 = bits(_T_28, 31, 0) @[FFT.scala 49:113]
    node _T_30 = asFixedPoint(_T_29, 16) @[FFT.scala 49:113]
    _T_26.im <= _T_30 @[FFT.scala 49:113]
    node _T_31 = bits(_T_28, 63, 32) @[FFT.scala 49:113]
    node _T_32 = asFixedPoint(_T_31, 16) @[FFT.scala 49:113]
    _T_26.re <= _T_32 @[FFT.scala 49:113]
    wire data1_0_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_0_1[0].im <= _T_19.im @[FFT.scala 49:65]
    data1_0_1[0].re <= _T_19.re @[FFT.scala 49:65]
    data1_0_1[1].im <= _T_26.im @[FFT.scala 49:65]
    data1_0_1[1].re <= _T_26.re @[FFT.scala 49:65]
    wire _T_33 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_34 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_35 : UInt<64>
    _T_35 <= _T_34
    node _T_36 = bits(_T_35, 31, 0) @[FFT.scala 49:113]
    node _T_37 = asFixedPoint(_T_36, 16) @[FFT.scala 49:113]
    _T_33.im <= _T_37 @[FFT.scala 49:113]
    node _T_38 = bits(_T_35, 63, 32) @[FFT.scala 49:113]
    node _T_39 = asFixedPoint(_T_38, 16) @[FFT.scala 49:113]
    _T_33.re <= _T_39 @[FFT.scala 49:113]
    wire _T_40 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_41 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_42 : UInt<64>
    _T_42 <= _T_41
    node _T_43 = bits(_T_42, 31, 0) @[FFT.scala 49:113]
    node _T_44 = asFixedPoint(_T_43, 16) @[FFT.scala 49:113]
    _T_40.im <= _T_44 @[FFT.scala 49:113]
    node _T_45 = bits(_T_42, 63, 32) @[FFT.scala 49:113]
    node _T_46 = asFixedPoint(_T_45, 16) @[FFT.scala 49:113]
    _T_40.re <= _T_46 @[FFT.scala 49:113]
    wire data1_1_0 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_1_0[0].im <= _T_33.im @[FFT.scala 49:65]
    data1_1_0[0].re <= _T_33.re @[FFT.scala 49:65]
    data1_1_0[1].im <= _T_40.im @[FFT.scala 49:65]
    data1_1_0[1].re <= _T_40.re @[FFT.scala 49:65]
    wire _T_47 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_48 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_49 : UInt<64>
    _T_49 <= _T_48
    node _T_50 = bits(_T_49, 31, 0) @[FFT.scala 49:113]
    node _T_51 = asFixedPoint(_T_50, 16) @[FFT.scala 49:113]
    _T_47.im <= _T_51 @[FFT.scala 49:113]
    node _T_52 = bits(_T_49, 63, 32) @[FFT.scala 49:113]
    node _T_53 = asFixedPoint(_T_52, 16) @[FFT.scala 49:113]
    _T_47.re <= _T_53 @[FFT.scala 49:113]
    wire _T_54 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_55 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_56 : UInt<64>
    _T_56 <= _T_55
    node _T_57 = bits(_T_56, 31, 0) @[FFT.scala 49:113]
    node _T_58 = asFixedPoint(_T_57, 16) @[FFT.scala 49:113]
    _T_54.im <= _T_58 @[FFT.scala 49:113]
    node _T_59 = bits(_T_56, 63, 32) @[FFT.scala 49:113]
    node _T_60 = asFixedPoint(_T_59, 16) @[FFT.scala 49:113]
    _T_54.re <= _T_60 @[FFT.scala 49:113]
    wire data1_1_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_1_1[0].im <= _T_47.im @[FFT.scala 49:65]
    data1_1_1[0].re <= _T_47.re @[FFT.scala 49:65]
    data1_1_1[1].im <= _T_54.im @[FFT.scala 49:65]
    data1_1_1[1].re <= _T_54.re @[FFT.scala 49:65]
    wire _T_61 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_62 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_63 : UInt<64>
    _T_63 <= _T_62
    node _T_64 = bits(_T_63, 31, 0) @[FFT.scala 49:113]
    node _T_65 = asFixedPoint(_T_64, 16) @[FFT.scala 49:113]
    _T_61.im <= _T_65 @[FFT.scala 49:113]
    node _T_66 = bits(_T_63, 63, 32) @[FFT.scala 49:113]
    node _T_67 = asFixedPoint(_T_66, 16) @[FFT.scala 49:113]
    _T_61.re <= _T_67 @[FFT.scala 49:113]
    wire _T_68 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_69 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_70 : UInt<64>
    _T_70 <= _T_69
    node _T_71 = bits(_T_70, 31, 0) @[FFT.scala 49:113]
    node _T_72 = asFixedPoint(_T_71, 16) @[FFT.scala 49:113]
    _T_68.im <= _T_72 @[FFT.scala 49:113]
    node _T_73 = bits(_T_70, 63, 32) @[FFT.scala 49:113]
    node _T_74 = asFixedPoint(_T_73, 16) @[FFT.scala 49:113]
    _T_68.re <= _T_74 @[FFT.scala 49:113]
    wire data1_2_0 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_2_0[0].im <= _T_61.im @[FFT.scala 49:65]
    data1_2_0[0].re <= _T_61.re @[FFT.scala 49:65]
    data1_2_0[1].im <= _T_68.im @[FFT.scala 49:65]
    data1_2_0[1].re <= _T_68.re @[FFT.scala 49:65]
    wire _T_75 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_76 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_77 : UInt<64>
    _T_77 <= _T_76
    node _T_78 = bits(_T_77, 31, 0) @[FFT.scala 49:113]
    node _T_79 = asFixedPoint(_T_78, 16) @[FFT.scala 49:113]
    _T_75.im <= _T_79 @[FFT.scala 49:113]
    node _T_80 = bits(_T_77, 63, 32) @[FFT.scala 49:113]
    node _T_81 = asFixedPoint(_T_80, 16) @[FFT.scala 49:113]
    _T_75.re <= _T_81 @[FFT.scala 49:113]
    wire _T_82 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_83 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_84 : UInt<64>
    _T_84 <= _T_83
    node _T_85 = bits(_T_84, 31, 0) @[FFT.scala 49:113]
    node _T_86 = asFixedPoint(_T_85, 16) @[FFT.scala 49:113]
    _T_82.im <= _T_86 @[FFT.scala 49:113]
    node _T_87 = bits(_T_84, 63, 32) @[FFT.scala 49:113]
    node _T_88 = asFixedPoint(_T_87, 16) @[FFT.scala 49:113]
    _T_82.re <= _T_88 @[FFT.scala 49:113]
    wire data1_2_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_2_1[0].im <= _T_75.im @[FFT.scala 49:65]
    data1_2_1[0].re <= _T_75.re @[FFT.scala 49:65]
    data1_2_1[1].im <= _T_82.im @[FFT.scala 49:65]
    data1_2_1[1].re <= _T_82.re @[FFT.scala 49:65]
    wire _T_89 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_90 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_91 : UInt<64>
    _T_91 <= _T_90
    node _T_92 = bits(_T_91, 31, 0) @[FFT.scala 49:113]
    node _T_93 = asFixedPoint(_T_92, 16) @[FFT.scala 49:113]
    _T_89.im <= _T_93 @[FFT.scala 49:113]
    node _T_94 = bits(_T_91, 63, 32) @[FFT.scala 49:113]
    node _T_95 = asFixedPoint(_T_94, 16) @[FFT.scala 49:113]
    _T_89.re <= _T_95 @[FFT.scala 49:113]
    wire _T_96 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_97 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_98 : UInt<64>
    _T_98 <= _T_97
    node _T_99 = bits(_T_98, 31, 0) @[FFT.scala 49:113]
    node _T_100 = asFixedPoint(_T_99, 16) @[FFT.scala 49:113]
    _T_96.im <= _T_100 @[FFT.scala 49:113]
    node _T_101 = bits(_T_98, 63, 32) @[FFT.scala 49:113]
    node _T_102 = asFixedPoint(_T_101, 16) @[FFT.scala 49:113]
    _T_96.re <= _T_102 @[FFT.scala 49:113]
    wire data1_3_0 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_3_0[0].im <= _T_89.im @[FFT.scala 49:65]
    data1_3_0[0].re <= _T_89.re @[FFT.scala 49:65]
    data1_3_0[1].im <= _T_96.im @[FFT.scala 49:65]
    data1_3_0[1].re <= _T_96.re @[FFT.scala 49:65]
    wire _T_103 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_104 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_105 : UInt<64>
    _T_105 <= _T_104
    node _T_106 = bits(_T_105, 31, 0) @[FFT.scala 49:113]
    node _T_107 = asFixedPoint(_T_106, 16) @[FFT.scala 49:113]
    _T_103.im <= _T_107 @[FFT.scala 49:113]
    node _T_108 = bits(_T_105, 63, 32) @[FFT.scala 49:113]
    node _T_109 = asFixedPoint(_T_108, 16) @[FFT.scala 49:113]
    _T_103.re <= _T_109 @[FFT.scala 49:113]
    wire _T_110 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 49:113]
    node _T_111 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 49:113]
    wire _T_112 : UInt<64>
    _T_112 <= _T_111
    node _T_113 = bits(_T_112, 31, 0) @[FFT.scala 49:113]
    node _T_114 = asFixedPoint(_T_113, 16) @[FFT.scala 49:113]
    _T_110.im <= _T_114 @[FFT.scala 49:113]
    node _T_115 = bits(_T_112, 63, 32) @[FFT.scala 49:113]
    node _T_116 = asFixedPoint(_T_115, 16) @[FFT.scala 49:113]
    _T_110.re <= _T_116 @[FFT.scala 49:113]
    wire data1_3_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 49:65]
    data1_3_1[0].im <= _T_103.im @[FFT.scala 49:65]
    data1_3_1[0].re <= _T_103.re @[FFT.scala 49:65]
    data1_3_1[1].im <= _T_110.im @[FFT.scala 49:65]
    data1_3_1[1].re <= _T_110.re @[FFT.scala 49:65]
    wire _T_117 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_118 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_119 : UInt<64>
    _T_119 <= _T_118
    node _T_120 = bits(_T_119, 31, 0) @[FFT.scala 50:117]
    node _T_121 = asFixedPoint(_T_120, 16) @[FFT.scala 50:117]
    _T_117.im <= _T_121 @[FFT.scala 50:117]
    node _T_122 = bits(_T_119, 63, 32) @[FFT.scala 50:117]
    node _T_123 = asFixedPoint(_T_122, 16) @[FFT.scala 50:117]
    _T_117.re <= _T_123 @[FFT.scala 50:117]
    wire _T_124 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_125 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_126 : UInt<64>
    _T_126 <= _T_125
    node _T_127 = bits(_T_126, 31, 0) @[FFT.scala 50:117]
    node _T_128 = asFixedPoint(_T_127, 16) @[FFT.scala 50:117]
    _T_124.im <= _T_128 @[FFT.scala 50:117]
    node _T_129 = bits(_T_126, 63, 32) @[FFT.scala 50:117]
    node _T_130 = asFixedPoint(_T_129, 16) @[FFT.scala 50:117]
    _T_124.re <= _T_130 @[FFT.scala 50:117]
    wire data2_0_0 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 50:69]
    data2_0_0[0].im <= _T_117.im @[FFT.scala 50:69]
    data2_0_0[0].re <= _T_117.re @[FFT.scala 50:69]
    data2_0_0[1].im <= _T_124.im @[FFT.scala 50:69]
    data2_0_0[1].re <= _T_124.re @[FFT.scala 50:69]
    wire _T_131 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_132 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_133 : UInt<64>
    _T_133 <= _T_132
    node _T_134 = bits(_T_133, 31, 0) @[FFT.scala 50:117]
    node _T_135 = asFixedPoint(_T_134, 16) @[FFT.scala 50:117]
    _T_131.im <= _T_135 @[FFT.scala 50:117]
    node _T_136 = bits(_T_133, 63, 32) @[FFT.scala 50:117]
    node _T_137 = asFixedPoint(_T_136, 16) @[FFT.scala 50:117]
    _T_131.re <= _T_137 @[FFT.scala 50:117]
    wire _T_138 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_139 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_140 : UInt<64>
    _T_140 <= _T_139
    node _T_141 = bits(_T_140, 31, 0) @[FFT.scala 50:117]
    node _T_142 = asFixedPoint(_T_141, 16) @[FFT.scala 50:117]
    _T_138.im <= _T_142 @[FFT.scala 50:117]
    node _T_143 = bits(_T_140, 63, 32) @[FFT.scala 50:117]
    node _T_144 = asFixedPoint(_T_143, 16) @[FFT.scala 50:117]
    _T_138.re <= _T_144 @[FFT.scala 50:117]
    wire data2_0_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 50:69]
    data2_0_1[0].im <= _T_131.im @[FFT.scala 50:69]
    data2_0_1[0].re <= _T_131.re @[FFT.scala 50:69]
    data2_0_1[1].im <= _T_138.im @[FFT.scala 50:69]
    data2_0_1[1].re <= _T_138.re @[FFT.scala 50:69]
    wire _T_145 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_146 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_147 : UInt<64>
    _T_147 <= _T_146
    node _T_148 = bits(_T_147, 31, 0) @[FFT.scala 50:117]
    node _T_149 = asFixedPoint(_T_148, 16) @[FFT.scala 50:117]
    _T_145.im <= _T_149 @[FFT.scala 50:117]
    node _T_150 = bits(_T_147, 63, 32) @[FFT.scala 50:117]
    node _T_151 = asFixedPoint(_T_150, 16) @[FFT.scala 50:117]
    _T_145.re <= _T_151 @[FFT.scala 50:117]
    wire _T_152 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_153 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_154 : UInt<64>
    _T_154 <= _T_153
    node _T_155 = bits(_T_154, 31, 0) @[FFT.scala 50:117]
    node _T_156 = asFixedPoint(_T_155, 16) @[FFT.scala 50:117]
    _T_152.im <= _T_156 @[FFT.scala 50:117]
    node _T_157 = bits(_T_154, 63, 32) @[FFT.scala 50:117]
    node _T_158 = asFixedPoint(_T_157, 16) @[FFT.scala 50:117]
    _T_152.re <= _T_158 @[FFT.scala 50:117]
    wire data2_1_0 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 50:69]
    data2_1_0[0].im <= _T_145.im @[FFT.scala 50:69]
    data2_1_0[0].re <= _T_145.re @[FFT.scala 50:69]
    data2_1_0[1].im <= _T_152.im @[FFT.scala 50:69]
    data2_1_0[1].re <= _T_152.re @[FFT.scala 50:69]
    wire _T_159 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_160 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_161 : UInt<64>
    _T_161 <= _T_160
    node _T_162 = bits(_T_161, 31, 0) @[FFT.scala 50:117]
    node _T_163 = asFixedPoint(_T_162, 16) @[FFT.scala 50:117]
    _T_159.im <= _T_163 @[FFT.scala 50:117]
    node _T_164 = bits(_T_161, 63, 32) @[FFT.scala 50:117]
    node _T_165 = asFixedPoint(_T_164, 16) @[FFT.scala 50:117]
    _T_159.re <= _T_165 @[FFT.scala 50:117]
    wire _T_166 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_167 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_168 : UInt<64>
    _T_168 <= _T_167
    node _T_169 = bits(_T_168, 31, 0) @[FFT.scala 50:117]
    node _T_170 = asFixedPoint(_T_169, 16) @[FFT.scala 50:117]
    _T_166.im <= _T_170 @[FFT.scala 50:117]
    node _T_171 = bits(_T_168, 63, 32) @[FFT.scala 50:117]
    node _T_172 = asFixedPoint(_T_171, 16) @[FFT.scala 50:117]
    _T_166.re <= _T_172 @[FFT.scala 50:117]
    wire data2_1_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 50:69]
    data2_1_1[0].im <= _T_159.im @[FFT.scala 50:69]
    data2_1_1[0].re <= _T_159.re @[FFT.scala 50:69]
    data2_1_1[1].im <= _T_166.im @[FFT.scala 50:69]
    data2_1_1[1].re <= _T_166.re @[FFT.scala 50:69]
    wire _T_173 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_174 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_175 : UInt<64>
    _T_175 <= _T_174
    node _T_176 = bits(_T_175, 31, 0) @[FFT.scala 50:117]
    node _T_177 = asFixedPoint(_T_176, 16) @[FFT.scala 50:117]
    _T_173.im <= _T_177 @[FFT.scala 50:117]
    node _T_178 = bits(_T_175, 63, 32) @[FFT.scala 50:117]
    node _T_179 = asFixedPoint(_T_178, 16) @[FFT.scala 50:117]
    _T_173.re <= _T_179 @[FFT.scala 50:117]
    wire _T_180 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_181 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_182 : UInt<64>
    _T_182 <= _T_181
    node _T_183 = bits(_T_182, 31, 0) @[FFT.scala 50:117]
    node _T_184 = asFixedPoint(_T_183, 16) @[FFT.scala 50:117]
    _T_180.im <= _T_184 @[FFT.scala 50:117]
    node _T_185 = bits(_T_182, 63, 32) @[FFT.scala 50:117]
    node _T_186 = asFixedPoint(_T_185, 16) @[FFT.scala 50:117]
    _T_180.re <= _T_186 @[FFT.scala 50:117]
    wire data2_2_0 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 50:69]
    data2_2_0[0].im <= _T_173.im @[FFT.scala 50:69]
    data2_2_0[0].re <= _T_173.re @[FFT.scala 50:69]
    data2_2_0[1].im <= _T_180.im @[FFT.scala 50:69]
    data2_2_0[1].re <= _T_180.re @[FFT.scala 50:69]
    wire _T_187 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_188 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_189 : UInt<64>
    _T_189 <= _T_188
    node _T_190 = bits(_T_189, 31, 0) @[FFT.scala 50:117]
    node _T_191 = asFixedPoint(_T_190, 16) @[FFT.scala 50:117]
    _T_187.im <= _T_191 @[FFT.scala 50:117]
    node _T_192 = bits(_T_189, 63, 32) @[FFT.scala 50:117]
    node _T_193 = asFixedPoint(_T_192, 16) @[FFT.scala 50:117]
    _T_187.re <= _T_193 @[FFT.scala 50:117]
    wire _T_194 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 50:117]
    node _T_195 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 50:117]
    wire _T_196 : UInt<64>
    _T_196 <= _T_195
    node _T_197 = bits(_T_196, 31, 0) @[FFT.scala 50:117]
    node _T_198 = asFixedPoint(_T_197, 16) @[FFT.scala 50:117]
    _T_194.im <= _T_198 @[FFT.scala 50:117]
    node _T_199 = bits(_T_196, 63, 32) @[FFT.scala 50:117]
    node _T_200 = asFixedPoint(_T_199, 16) @[FFT.scala 50:117]
    _T_194.re <= _T_200 @[FFT.scala 50:117]
    wire data2_2_1 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2] @[FFT.scala 50:69]
    data2_2_1[0].im <= _T_187.im @[FFT.scala 50:69]
    data2_2_1[0].re <= _T_187.re @[FFT.scala 50:69]
    data2_2_1[1].im <= _T_194.im @[FFT.scala 50:69]
    data2_2_1[1].re <= _T_194.re @[FFT.scala 50:69]
    wire _T_201 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 51:86]
    node _T_202 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 51:86]
    wire _T_203 : UInt<64>
    _T_203 <= _T_202
    node _T_204 = bits(_T_203, 31, 0) @[FFT.scala 51:86]
    node _T_205 = asFixedPoint(_T_204, 16) @[FFT.scala 51:86]
    _T_201.im <= _T_205 @[FFT.scala 51:86]
    node _T_206 = bits(_T_203, 63, 32) @[FFT.scala 51:86]
    node _T_207 = asFixedPoint(_T_206, 16) @[FFT.scala 51:86]
    _T_201.re <= _T_207 @[FFT.scala 51:86]
    wire _T_208 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 51:86]
    node _T_209 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 51:86]
    wire _T_210 : UInt<64>
    _T_210 <= _T_209
    node _T_211 = bits(_T_210, 31, 0) @[FFT.scala 51:86]
    node _T_212 = asFixedPoint(_T_211, 16) @[FFT.scala 51:86]
    _T_208.im <= _T_212 @[FFT.scala 51:86]
    node _T_213 = bits(_T_210, 63, 32) @[FFT.scala 51:86]
    node _T_214 = asFixedPoint(_T_213, 16) @[FFT.scala 51:86]
    _T_208.re <= _T_214 @[FFT.scala 51:86]
    wire _T_215 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 51:86]
    node _T_216 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 51:86]
    wire _T_217 : UInt<64>
    _T_217 <= _T_216
    node _T_218 = bits(_T_217, 31, 0) @[FFT.scala 51:86]
    node _T_219 = asFixedPoint(_T_218, 16) @[FFT.scala 51:86]
    _T_215.im <= _T_219 @[FFT.scala 51:86]
    node _T_220 = bits(_T_217, 63, 32) @[FFT.scala 51:86]
    node _T_221 = asFixedPoint(_T_220, 16) @[FFT.scala 51:86]
    _T_215.re <= _T_221 @[FFT.scala 51:86]
    wire _T_222 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 51:86]
    node _T_223 = asUInt(asSInt(UInt<64>("h00"))) @[FFT.scala 51:86]
    wire _T_224 : UInt<64>
    _T_224 <= _T_223
    node _T_225 = bits(_T_224, 31, 0) @[FFT.scala 51:86]
    node _T_226 = asFixedPoint(_T_225, 16) @[FFT.scala 51:86]
    _T_222.im <= _T_226 @[FFT.scala 51:86]
    node _T_227 = bits(_T_224, 63, 32) @[FFT.scala 51:86]
    node _T_228 = asFixedPoint(_T_227, 16) @[FFT.scala 51:86]
    _T_222.re <= _T_228 @[FFT.scala 51:86]
    wire datao : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[4] @[FFT.scala 51:22]
    datao[0].im <= _T_201.im @[FFT.scala 51:22]
    datao[0].re <= _T_201.re @[FFT.scala 51:22]
    datao[1].im <= _T_208.im @[FFT.scala 51:22]
    datao[1].re <= _T_208.re @[FFT.scala 51:22]
    datao[2].im <= _T_215.im @[FFT.scala 51:22]
    datao[2].re <= _T_215.re @[FFT.scala 51:22]
    datao[3].im <= _T_222.im @[FFT.scala 51:22]
    datao[3].re <= _T_222.re @[FFT.scala 51:22]
    node _T_229 = add(UInt<1>("h00"), cnt_0) @[FFT.scala 61:138]
    node _T_230 = tail(_T_229, 1) @[FFT.scala 61:138]
    wire _T_231 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 29:19]
    wire _T_232 : Fixed<32><<16>>[8] @[FFT.scala 26:12]
    _T_232[0] <= asFixedPoint(UInt<32>("h010000"), 16) @[FFT.scala 26:12]
    _T_232[1] <= asFixedPoint(UInt<32>("h0ec83"), 16) @[FFT.scala 26:12]
    _T_232[2] <= asFixedPoint(UInt<32>("h0b505"), 16) @[FFT.scala 26:12]
    _T_232[3] <= asFixedPoint(UInt<32>("h061f8"), 16) @[FFT.scala 26:12]
    _T_232[4] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 26:12]
    _T_232[5] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[FFT.scala 26:12]
    _T_232[6] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 26:12]
    _T_232[7] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[FFT.scala 26:12]
    node _T_233 = bits(_T_230, 2, 0)
    _T_231.re <= _T_232[_T_233] @[FFT.scala 30:12]
    wire _T_234 : Fixed<32><<16>>[8] @[FFT.scala 22:12]
    _T_234[0] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 22:12]
    _T_234[1] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[FFT.scala 22:12]
    _T_234[2] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_234[3] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[FFT.scala 22:12]
    _T_234[4] <= asFixedPoint(UInt<32>("h0ffff0000"), 16) @[FFT.scala 22:12]
    _T_234[5] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[FFT.scala 22:12]
    _T_234[6] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_234[7] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[FFT.scala 22:12]
    node _T_235 = bits(_T_230, 2, 0)
    _T_231.im <= _T_234[_T_235] @[FFT.scala 31:12]
    inst ButterflyMul of ButterflyMul @[Modules.scala 119:22]
    ButterflyMul.clock <= clock
    ButterflyMul.reset <= reset
    ButterflyMul.io.in[0].im <= data1_0_0[0].im @[Modules.scala 120:16]
    ButterflyMul.io.in[0].re <= data1_0_0[0].re @[Modules.scala 120:16]
    ButterflyMul.io.in[1].im <= data1_0_0[1].im @[Modules.scala 120:16]
    ButterflyMul.io.in[1].re <= data1_0_0[1].re @[Modules.scala 120:16]
    ButterflyMul.io.wn[0].im <= _T_231.im @[Modules.scala 121:16]
    ButterflyMul.io.wn[0].re <= _T_231.re @[Modules.scala 121:16]
    data2_0_0[0].im <= ButterflyMul.io.out[0].im @[FFT.scala 61:50]
    data2_0_0[0].re <= ButterflyMul.io.out[0].re @[FFT.scala 61:50]
    data2_0_0[1].im <= ButterflyMul.io.out[1].im @[FFT.scala 61:50]
    data2_0_0[1].re <= ButterflyMul.io.out[1].re @[FFT.scala 61:50]
    node _T_236 = add(UInt<3>("h04"), cnt_0) @[FFT.scala 61:138]
    node _T_237 = tail(_T_236, 1) @[FFT.scala 61:138]
    wire _T_238 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 29:19]
    wire _T_239 : Fixed<32><<16>>[8] @[FFT.scala 26:12]
    _T_239[0] <= asFixedPoint(UInt<32>("h010000"), 16) @[FFT.scala 26:12]
    _T_239[1] <= asFixedPoint(UInt<32>("h0ec83"), 16) @[FFT.scala 26:12]
    _T_239[2] <= asFixedPoint(UInt<32>("h0b505"), 16) @[FFT.scala 26:12]
    _T_239[3] <= asFixedPoint(UInt<32>("h061f8"), 16) @[FFT.scala 26:12]
    _T_239[4] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 26:12]
    _T_239[5] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[FFT.scala 26:12]
    _T_239[6] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 26:12]
    _T_239[7] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[FFT.scala 26:12]
    node _T_240 = bits(_T_237, 2, 0)
    _T_238.re <= _T_239[_T_240] @[FFT.scala 30:12]
    wire _T_241 : Fixed<32><<16>>[8] @[FFT.scala 22:12]
    _T_241[0] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 22:12]
    _T_241[1] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[FFT.scala 22:12]
    _T_241[2] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_241[3] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[FFT.scala 22:12]
    _T_241[4] <= asFixedPoint(UInt<32>("h0ffff0000"), 16) @[FFT.scala 22:12]
    _T_241[5] <= asFixedPoint(UInt<32>("h0ffff137d"), 16) @[FFT.scala 22:12]
    _T_241[6] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_241[7] <= asFixedPoint(UInt<32>("h0ffff9e08"), 16) @[FFT.scala 22:12]
    node _T_242 = bits(_T_237, 2, 0)
    _T_238.im <= _T_241[_T_242] @[FFT.scala 31:12]
    inst ButterflyMul_1 of ButterflyMul_1 @[Modules.scala 119:22]
    ButterflyMul_1.clock <= clock
    ButterflyMul_1.reset <= reset
    ButterflyMul_1.io.in[0].im <= data1_0_1[0].im @[Modules.scala 120:16]
    ButterflyMul_1.io.in[0].re <= data1_0_1[0].re @[Modules.scala 120:16]
    ButterflyMul_1.io.in[1].im <= data1_0_1[1].im @[Modules.scala 120:16]
    ButterflyMul_1.io.in[1].re <= data1_0_1[1].re @[Modules.scala 120:16]
    ButterflyMul_1.io.wn[0].im <= _T_238.im @[Modules.scala 121:16]
    ButterflyMul_1.io.wn[0].re <= _T_238.re @[Modules.scala 121:16]
    data2_0_1[0].im <= ButterflyMul_1.io.out[0].im @[FFT.scala 61:50]
    data2_0_1[0].re <= ButterflyMul_1.io.out[0].re @[FFT.scala 61:50]
    data2_0_1[1].im <= ButterflyMul_1.io.out[1].im @[FFT.scala 61:50]
    data2_0_1[1].re <= ButterflyMul_1.io.out[1].re @[FFT.scala 61:50]
    node _T_243 = bits(cnt_1, 1, 0) @[FFT.scala 58:23]
    node _T_244 = add(UInt<1>("h00"), _T_243) @[FFT.scala 61:138]
    node _T_245 = tail(_T_244, 1) @[FFT.scala 61:138]
    wire _T_246 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 29:19]
    wire _T_247 : Fixed<32><<16>>[4] @[FFT.scala 26:12]
    _T_247[0] <= asFixedPoint(UInt<32>("h010000"), 16) @[FFT.scala 26:12]
    _T_247[1] <= asFixedPoint(UInt<32>("h0b505"), 16) @[FFT.scala 26:12]
    _T_247[2] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 26:12]
    _T_247[3] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 26:12]
    _T_246.re <= _T_247[_T_245] @[FFT.scala 30:12]
    wire _T_248 : Fixed<32><<16>>[4] @[FFT.scala 22:12]
    _T_248[0] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 22:12]
    _T_248[1] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_248[2] <= asFixedPoint(UInt<32>("h0ffff0000"), 16) @[FFT.scala 22:12]
    _T_248[3] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_246.im <= _T_248[_T_245] @[FFT.scala 31:12]
    inst ButterflyMul_2 of ButterflyMul_2 @[Modules.scala 119:22]
    ButterflyMul_2.clock <= clock
    ButterflyMul_2.reset <= reset
    ButterflyMul_2.io.in[0].im <= data1_1_0[0].im @[Modules.scala 120:16]
    ButterflyMul_2.io.in[0].re <= data1_1_0[0].re @[Modules.scala 120:16]
    ButterflyMul_2.io.in[1].im <= data1_1_0[1].im @[Modules.scala 120:16]
    ButterflyMul_2.io.in[1].re <= data1_1_0[1].re @[Modules.scala 120:16]
    ButterflyMul_2.io.wn[0].im <= _T_246.im @[Modules.scala 121:16]
    ButterflyMul_2.io.wn[0].re <= _T_246.re @[Modules.scala 121:16]
    data2_1_0[0].im <= ButterflyMul_2.io.out[0].im @[FFT.scala 61:50]
    data2_1_0[0].re <= ButterflyMul_2.io.out[0].re @[FFT.scala 61:50]
    data2_1_0[1].im <= ButterflyMul_2.io.out[1].im @[FFT.scala 61:50]
    data2_1_0[1].re <= ButterflyMul_2.io.out[1].re @[FFT.scala 61:50]
    node _T_249 = add(UInt<1>("h00"), _T_243) @[FFT.scala 61:138]
    node _T_250 = tail(_T_249, 1) @[FFT.scala 61:138]
    wire _T_251 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 29:19]
    wire _T_252 : Fixed<32><<16>>[4] @[FFT.scala 26:12]
    _T_252[0] <= asFixedPoint(UInt<32>("h010000"), 16) @[FFT.scala 26:12]
    _T_252[1] <= asFixedPoint(UInt<32>("h0b505"), 16) @[FFT.scala 26:12]
    _T_252[2] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 26:12]
    _T_252[3] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 26:12]
    _T_251.re <= _T_252[_T_250] @[FFT.scala 30:12]
    wire _T_253 : Fixed<32><<16>>[4] @[FFT.scala 22:12]
    _T_253[0] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 22:12]
    _T_253[1] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_253[2] <= asFixedPoint(UInt<32>("h0ffff0000"), 16) @[FFT.scala 22:12]
    _T_253[3] <= asFixedPoint(UInt<32>("h0ffff4afb"), 16) @[FFT.scala 22:12]
    _T_251.im <= _T_253[_T_250] @[FFT.scala 31:12]
    inst ButterflyMul_3 of ButterflyMul_3 @[Modules.scala 119:22]
    ButterflyMul_3.clock <= clock
    ButterflyMul_3.reset <= reset
    ButterflyMul_3.io.in[0].im <= data1_1_1[0].im @[Modules.scala 120:16]
    ButterflyMul_3.io.in[0].re <= data1_1_1[0].re @[Modules.scala 120:16]
    ButterflyMul_3.io.in[1].im <= data1_1_1[1].im @[Modules.scala 120:16]
    ButterflyMul_3.io.in[1].re <= data1_1_1[1].re @[Modules.scala 120:16]
    ButterflyMul_3.io.wn[0].im <= _T_251.im @[Modules.scala 121:16]
    ButterflyMul_3.io.wn[0].re <= _T_251.re @[Modules.scala 121:16]
    data2_1_1[0].im <= ButterflyMul_3.io.out[0].im @[FFT.scala 61:50]
    data2_1_1[0].re <= ButterflyMul_3.io.out[0].re @[FFT.scala 61:50]
    data2_1_1[1].im <= ButterflyMul_3.io.out[1].im @[FFT.scala 61:50]
    data2_1_1[1].re <= ButterflyMul_3.io.out[1].re @[FFT.scala 61:50]
    node _T_254 = bits(cnt_1, 0, 0) @[FFT.scala 58:23]
    node _T_255 = add(UInt<1>("h00"), _T_254) @[FFT.scala 61:138]
    node _T_256 = tail(_T_255, 1) @[FFT.scala 61:138]
    wire _T_257 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 29:19]
    wire _T_258 : Fixed<32><<16>>[2] @[FFT.scala 26:12]
    _T_258[0] <= asFixedPoint(UInt<32>("h010000"), 16) @[FFT.scala 26:12]
    _T_258[1] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 26:12]
    _T_257.re <= _T_258[_T_256] @[FFT.scala 30:12]
    wire _T_259 : Fixed<32><<16>>[2] @[FFT.scala 22:12]
    _T_259[0] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 22:12]
    _T_259[1] <= asFixedPoint(UInt<32>("h0ffff0000"), 16) @[FFT.scala 22:12]
    _T_257.im <= _T_259[_T_256] @[FFT.scala 31:12]
    inst ButterflyMul_4 of ButterflyMul_4 @[Modules.scala 119:22]
    ButterflyMul_4.clock <= clock
    ButterflyMul_4.reset <= reset
    ButterflyMul_4.io.in[0].im <= data1_2_0[0].im @[Modules.scala 120:16]
    ButterflyMul_4.io.in[0].re <= data1_2_0[0].re @[Modules.scala 120:16]
    ButterflyMul_4.io.in[1].im <= data1_2_0[1].im @[Modules.scala 120:16]
    ButterflyMul_4.io.in[1].re <= data1_2_0[1].re @[Modules.scala 120:16]
    ButterflyMul_4.io.wn[0].im <= _T_257.im @[Modules.scala 121:16]
    ButterflyMul_4.io.wn[0].re <= _T_257.re @[Modules.scala 121:16]
    data2_2_0[0].im <= ButterflyMul_4.io.out[0].im @[FFT.scala 61:50]
    data2_2_0[0].re <= ButterflyMul_4.io.out[0].re @[FFT.scala 61:50]
    data2_2_0[1].im <= ButterflyMul_4.io.out[1].im @[FFT.scala 61:50]
    data2_2_0[1].re <= ButterflyMul_4.io.out[1].re @[FFT.scala 61:50]
    node _T_260 = add(UInt<1>("h00"), _T_254) @[FFT.scala 61:138]
    node _T_261 = tail(_T_260, 1) @[FFT.scala 61:138]
    wire _T_262 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>} @[FFT.scala 29:19]
    wire _T_263 : Fixed<32><<16>>[2] @[FFT.scala 26:12]
    _T_263[0] <= asFixedPoint(UInt<32>("h010000"), 16) @[FFT.scala 26:12]
    _T_263[1] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 26:12]
    _T_262.re <= _T_263[_T_261] @[FFT.scala 30:12]
    wire _T_264 : Fixed<32><<16>>[2] @[FFT.scala 22:12]
    _T_264[0] <= asFixedPoint(UInt<32>("h00"), 16) @[FFT.scala 22:12]
    _T_264[1] <= asFixedPoint(UInt<32>("h0ffff0000"), 16) @[FFT.scala 22:12]
    _T_262.im <= _T_264[_T_261] @[FFT.scala 31:12]
    inst ButterflyMul_5 of ButterflyMul_5 @[Modules.scala 119:22]
    ButterflyMul_5.clock <= clock
    ButterflyMul_5.reset <= reset
    ButterflyMul_5.io.in[0].im <= data1_2_1[0].im @[Modules.scala 120:16]
    ButterflyMul_5.io.in[0].re <= data1_2_1[0].re @[Modules.scala 120:16]
    ButterflyMul_5.io.in[1].im <= data1_2_1[1].im @[Modules.scala 120:16]
    ButterflyMul_5.io.in[1].re <= data1_2_1[1].re @[Modules.scala 120:16]
    ButterflyMul_5.io.wn[0].im <= _T_262.im @[Modules.scala 121:16]
    ButterflyMul_5.io.wn[0].re <= _T_262.re @[Modules.scala 121:16]
    data2_2_1[0].im <= ButterflyMul_5.io.out[0].im @[FFT.scala 61:50]
    data2_2_1[0].re <= ButterflyMul_5.io.out[0].re @[FFT.scala 61:50]
    data2_2_1[1].im <= ButterflyMul_5.io.out[1].im @[FFT.scala 61:50]
    data2_2_1[1].re <= ButterflyMul_5.io.out[1].re @[FFT.scala 61:50]
    inst ButterflyAdd of ButterflyAdd_6 @[Modules.scala 95:22]
    ButterflyAdd.clock <= clock
    ButterflyAdd.reset <= reset
    ButterflyAdd.io.in[0].im <= data1_3_0[0].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[0].re <= data1_3_0[0].re @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].im <= data1_3_0[1].im @[Modules.scala 96:16]
    ButterflyAdd.io.in[1].re <= data1_3_0[1].re @[Modules.scala 96:16]
    inst ButterflyAdd_1 of ButterflyAdd_7 @[Modules.scala 95:22]
    ButterflyAdd_1.clock <= clock
    ButterflyAdd_1.reset <= reset
    ButterflyAdd_1.io.in[0].im <= data1_3_1[0].im @[Modules.scala 96:16]
    ButterflyAdd_1.io.in[0].re <= data1_3_1[0].re @[Modules.scala 96:16]
    ButterflyAdd_1.io.in[1].im <= data1_3_1[1].im @[Modules.scala 96:16]
    ButterflyAdd_1.io.in[1].re <= data1_3_1[1].re @[Modules.scala 96:16]
    datao[0].im <= ButterflyAdd.io.out[0].im @[FFT.scala 65:9]
    datao[0].re <= ButterflyAdd.io.out[0].re @[FFT.scala 65:9]
    datao[1].im <= ButterflyAdd.io.out[1].im @[FFT.scala 65:9]
    datao[1].re <= ButterflyAdd.io.out[1].re @[FFT.scala 65:9]
    datao[2].im <= ButterflyAdd_1.io.out[0].im @[FFT.scala 65:9]
    datao[2].re <= ButterflyAdd_1.io.out[0].re @[FFT.scala 65:9]
    datao[3].im <= ButterflyAdd_1.io.out[1].im @[FFT.scala 65:9]
    datao[3].re <= ButterflyAdd_1.io.out[1].re @[FFT.scala 65:9]
    inst Exchange of Exchange @[Modules.scala 167:22]
    Exchange.clock <= clock
    Exchange.reset <= reset
    Exchange.io.in[0][0].im <= data2_0_0[0].im @[Modules.scala 168:16]
    Exchange.io.in[0][0].re <= data2_0_0[0].re @[Modules.scala 168:16]
    Exchange.io.in[0][1].im <= data2_0_0[1].im @[Modules.scala 168:16]
    Exchange.io.in[0][1].re <= data2_0_0[1].re @[Modules.scala 168:16]
    Exchange.io.in[1][0].im <= data2_0_1[0].im @[Modules.scala 168:16]
    Exchange.io.in[1][0].re <= data2_0_1[0].re @[Modules.scala 168:16]
    Exchange.io.in[1][1].im <= data2_0_1[1].im @[Modules.scala 168:16]
    Exchange.io.in[1][1].re <= data2_0_1[1].re @[Modules.scala 168:16]
    reg _T_265 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], clock @[FFT.scala 81:67]
    _T_265[0].im <= Exchange.io.out[0][0].im @[FFT.scala 81:67]
    _T_265[0].re <= Exchange.io.out[0][0].re @[FFT.scala 81:67]
    _T_265[1].im <= Exchange.io.out[0][1].im @[FFT.scala 81:67]
    _T_265[1].re <= Exchange.io.out[0][1].re @[FFT.scala 81:67]
    data1_1_0[0].im <= _T_265[0].im @[FFT.scala 81:57]
    data1_1_0[0].re <= _T_265[0].re @[FFT.scala 81:57]
    data1_1_0[1].im <= _T_265[1].im @[FFT.scala 81:57]
    data1_1_0[1].re <= _T_265[1].re @[FFT.scala 81:57]
    reg _T_266 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[2], clock @[FFT.scala 81:67]
    _T_266[0].im <= Exchange.io.out[1][0].im @[FFT.scala 81:67]
    _T_266[0].re <= Exchange.io.out[1][0].re @[FFT.scala 81:67]
    _T_266[1].im <= Exchange.io.out[1][1].im @[FFT.scala 81:67]
    _T_266[1].re <= Exchange.io.out[1][1].re @[FFT.scala 81:67]
    data1_1_1[0].im <= _T_266[0].im @[FFT.scala 81:57]
    data1_1_1[0].re <= _T_266[0].re @[FFT.scala 81:57]
    data1_1_1[1].im <= _T_266[1].im @[FFT.scala 81:57]
    data1_1_1[1].re <= _T_266[1].re @[FFT.scala 81:57]
    node _T_267 = bits(cnt_1, 1, 1) @[FFT.scala 87:31]
    inst Switch of Switch @[Modules.scala 152:22]
    Switch.clock <= clock
    Switch.reset <= reset
    Switch.io.in[0].im <= data2_1_0[0].im @[Modules.scala 153:16]
    Switch.io.in[0].re <= data2_1_0[0].re @[Modules.scala 153:16]
    Switch.io.in[1].im <= data2_1_0[1].im @[Modules.scala 153:16]
    Switch.io.in[1].re <= data2_1_0[1].re @[Modules.scala 153:16]
    Switch.io.sel <= _T_267 @[Modules.scala 154:17]
    data1_2_0[0].im <= Switch.io.out[0].im @[FFT.scala 88:54]
    data1_2_0[0].re <= Switch.io.out[0].re @[FFT.scala 88:54]
    data1_2_0[1].im <= Switch.io.out[1].im @[FFT.scala 88:54]
    data1_2_0[1].re <= Switch.io.out[1].re @[FFT.scala 88:54]
    inst Switch_1 of Switch_1 @[Modules.scala 152:22]
    Switch_1.clock <= clock
    Switch_1.reset <= reset
    Switch_1.io.in[0].im <= data2_1_1[0].im @[Modules.scala 153:16]
    Switch_1.io.in[0].re <= data2_1_1[0].re @[Modules.scala 153:16]
    Switch_1.io.in[1].im <= data2_1_1[1].im @[Modules.scala 153:16]
    Switch_1.io.in[1].re <= data2_1_1[1].re @[Modules.scala 153:16]
    Switch_1.io.sel <= _T_267 @[Modules.scala 154:17]
    data1_2_1[0].im <= Switch_1.io.out[0].im @[FFT.scala 88:54]
    data1_2_1[0].re <= Switch_1.io.out[0].re @[FFT.scala 88:54]
    data1_2_1[1].im <= Switch_1.io.out[1].im @[FFT.scala 88:54]
    data1_2_1[1].re <= Switch_1.io.out[1].re @[FFT.scala 88:54]
    node _T_268 = bits(cnt_1, 0, 0) @[FFT.scala 87:31]
    inst Switch_2 of Switch_2 @[Modules.scala 152:22]
    Switch_2.clock <= clock
    Switch_2.reset <= reset
    Switch_2.io.in[0].im <= data2_2_0[0].im @[Modules.scala 153:16]
    Switch_2.io.in[0].re <= data2_2_0[0].re @[Modules.scala 153:16]
    Switch_2.io.in[1].im <= data2_2_0[1].im @[Modules.scala 153:16]
    Switch_2.io.in[1].re <= data2_2_0[1].re @[Modules.scala 153:16]
    Switch_2.io.sel <= _T_268 @[Modules.scala 154:17]
    data1_3_0[0].im <= Switch_2.io.out[0].im @[FFT.scala 88:54]
    data1_3_0[0].re <= Switch_2.io.out[0].re @[FFT.scala 88:54]
    data1_3_0[1].im <= Switch_2.io.out[1].im @[FFT.scala 88:54]
    data1_3_0[1].re <= Switch_2.io.out[1].re @[FFT.scala 88:54]
    inst Switch_3 of Switch_3 @[Modules.scala 152:22]
    Switch_3.clock <= clock
    Switch_3.reset <= reset
    Switch_3.io.in[0].im <= data2_2_1[0].im @[Modules.scala 153:16]
    Switch_3.io.in[0].re <= data2_2_1[0].re @[Modules.scala 153:16]
    Switch_3.io.in[1].im <= data2_2_1[1].im @[Modules.scala 153:16]
    Switch_3.io.in[1].re <= data2_2_1[1].re @[Modules.scala 153:16]
    Switch_3.io.sel <= _T_268 @[Modules.scala 154:17]
    data1_3_1[0].im <= Switch_3.io.out[0].im @[FFT.scala 88:54]
    data1_3_1[0].re <= Switch_3.io.out[0].re @[FFT.scala 88:54]
    data1_3_1[1].im <= Switch_3.io.out[1].im @[FFT.scala 88:54]
    data1_3_1[1].re <= Switch_3.io.out[1].re @[FFT.scala 88:54]
    data1_0_0[0].im <= io.dIn[0].im @[FFT.scala 93:17]
    data1_0_0[0].re <= io.dIn[0].re @[FFT.scala 93:17]
    data1_0_0[1].im <= io.dIn[1].im @[FFT.scala 93:17]
    data1_0_0[1].re <= io.dIn[1].re @[FFT.scala 93:17]
    data1_0_1[0].im <= io.dIn[2].im @[FFT.scala 93:17]
    data1_0_1[0].re <= io.dIn[2].re @[FFT.scala 93:17]
    data1_0_1[1].im <= io.dIn[3].im @[FFT.scala 93:17]
    data1_0_1[1].re <= io.dIn[3].re @[FFT.scala 93:17]
    io.busy <= busy @[FFT.scala 95:11]
    reg _T_269 : UInt, clock @[FFT.scala 96:27]
    _T_269 <= cnt_0 @[FFT.scala 96:27]
    node _T_270 = eq(_T_269, UInt<3>("h04")) @[FFT.scala 96:36]
    io.dout_valid <= _T_270 @[FFT.scala 96:17]
    reg _T_271 : {re : Fixed<32><<16>>, im : Fixed<32><<16>>}[4], clock @[FFT.scala 97:21]
    _T_271[0].im <= datao[0].im @[FFT.scala 97:21]
    _T_271[0].re <= datao[0].re @[FFT.scala 97:21]
    _T_271[1].im <= datao[1].im @[FFT.scala 97:21]
    _T_271[1].re <= datao[1].re @[FFT.scala 97:21]
    _T_271[2].im <= datao[2].im @[FFT.scala 97:21]
    _T_271[2].re <= datao[2].re @[FFT.scala 97:21]
    _T_271[3].im <= datao[3].im @[FFT.scala 97:21]
    _T_271[3].re <= datao[3].re @[FFT.scala 97:21]
    io.dOut[0].im <= _T_271[0].im @[FFT.scala 97:11]
    io.dOut[0].re <= _T_271[0].re @[FFT.scala 97:11]
    io.dOut[1].im <= _T_271[1].im @[FFT.scala 97:11]
    io.dOut[1].re <= _T_271[1].re @[FFT.scala 97:11]
    io.dOut[2].im <= _T_271[2].im @[FFT.scala 97:11]
    io.dOut[2].re <= _T_271[2].re @[FFT.scala 97:11]
    io.dOut[3].im <= _T_271[3].im @[FFT.scala 97:11]
    io.dOut[3].re <= _T_271[3].re @[FFT.scala 97:11]
    
