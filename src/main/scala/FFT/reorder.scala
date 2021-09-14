package FFT

import chisel3._
import chisel3.util._
import chisel3.experimental._

class reorder extends Module with Config {
  val io = IO(new Bundle{
    val dIn = Input(Vec(FFTparallel_r * radix, if(use_float) new IEEEComplex else new MyFixComplex))
    val dOut = Output(if(use_float) new IEEEComplex else new MyFixComplex)
    val din_valid = Input(new Bool)
    val do_valid = Output(new Bool)
    val busy = Output(new Bool)
  })

  val w_cnt = RegInit(0.U(log2Ceil(datalength).W))
  val r_cnt = RegInit(0.U(log2Ceil(FFTlength).W))
  val do_write = io.din_valid || w_cnt =/= 0.U
  val do_read = RegNext(w_cnt) === (datalength - 1).asUInt || r_cnt =/= 0.U
  val buffer = VecInit(Seq.fill(FFTlength)(RegInit(0.S((2 * DataWidth).W).asTypeOf(if (use_float) new IEEEComplex else new MyFixComplex))))

  def bit_reverse(j: Int): Int = {
    var result: Int = 0
    val times = log2Ceil(FFTlength)
    for (i <- 0 until times) {
      result = result | ((j >> i) << (times - 1 - i))
    }
    result
  }

  def addrVec(cnt: Int): Vec[UInt] = {
    val tempList = List.tabulate(FFTparallel_r, radix)((x,y) => datalength * radix * x + radix * cnt + y)
    val addList = tempList.flatten.map(x => bit_reverse(x).asUInt)
    VecInit(addList)
  }
  val addr = VecInit((0 until datalength).map(x => addrVec(x)).toList)

  when(do_write) {
    w_cnt := w_cnt + 1.U
    for (i <- 0 until FFTparallel_r * radix) {
      buffer(addr(w_cnt)(i)) := io.dIn(i)
    }
  }

  when(do_read) {
    r_cnt := r_cnt + 1.U
    io.dOut := buffer(r_cnt)
  }

  io.busy := do_write || do_read
  io.do_valid := RegNext(w_cnt) === (datalength - 1).asUInt
}
