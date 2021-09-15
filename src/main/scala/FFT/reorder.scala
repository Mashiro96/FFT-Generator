package FFT

import chisel3._
import chisel3.util._
import chisel3.experimental._

class reorder extends Module with Config {
  val io = IO(new Bundle{
    val dIn = Input(Vec(FFTparallel_r * radix, if(use_float) new IEEEComplex else new MyFixComplex))
    val dOut = Output(if(use_float) new IEEEComplex else new MyFixComplex)
    val din_valid = Input(new Bool)
    val dout_valid = Output(new Bool)
    val busy = Output(new Bool)
  })

  val FFTbit = log2Ceil(FFTlength)
  val databit = log2Ceil(datalength)
  val radixbit = log2Ceil(radix)

  val w_cnt = RegInit(0.U((databit + 1).W))
  val r_cnt = RegInit(0.U((FFTbit + 1).W))
  val do_write = io.din_valid || (w_cnt(databit) =/= 1.U && w_cnt =/= 0.U)
  val do_read = w_cnt(databit) === 1.U || (r_cnt(FFTbit) =/= 1.U && r_cnt =/= 0.U)
  val initValue = VecInit(Seq.fill(datalength)(VecInit(Seq.fill(FFTparallel_r * radix)(0.S((2*DataWidth).W).asTypeOf(if(use_float) new IEEEComplex else new MyFixComplex)))))
  val buffer = RegInit(initValue)
 // val buffer = Vec(datalength, VecInit(Seq.fill(FFTparallel_r * radix)(RegInit(0.S((2 * DataWidth).W).asTypeOf(if(use_float) new IEEEComplex else new MyFixComplex)))))
  //val buffer = Vec(datalength, Vec(FFTparallel_r * radix, Reg(if(use_float) new IEEEComplex else new MyFixComplex)))

  def bit_reverse(j: UInt): UInt = {
    var result: UInt = 0.U(FFTbit.W)
    val times = FFTbit
    for (i <- 0 until times) {
      var operator = j & (1.U(FFTbit.W) << i).asUInt
      result = result | ((operator >> i) << (times - 1 - i)).asUInt
    }
    result
  }
  def index(p: Int, r: Int, cnt: UInt): UInt = {
    val index_temp = (p * datalength).asUInt((FFTbit - radixbit).W) + cnt
    bit_reverse(Cat(index_temp,r.asUInt(radixbit.W)))
  }

  when(do_write) {
    w_cnt := w_cnt + 1.U
    buffer(w_cnt) := io.dIn
  }.otherwise {
    w_cnt := 0.U
  }
  when(do_read) {
    r_cnt := r_cnt + 1.U
  }.otherwise {
    r_cnt := 0.U
  }

  val addr = bit_reverse(r_cnt)
  io.dOut := buffer(addr(databit-1,0))(addr(FFTbit-1,databit))

  io.busy := do_write || do_read
  io.dout_valid := w_cnt(databit) === 1.U
  printf("%d,%d,%d,%d,%d,%d\n", do_write, w_cnt, {buffer(w_cnt)(0).re}, r_cnt, addr, {buffer(0)(0).re})
  //printf(p"$do_write, $w_cnt, ${io.dIn(0).re}, ${buffer(0)(0).re}\n")
}
