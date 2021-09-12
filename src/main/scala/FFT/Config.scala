package FFT

import scala.math._

trait Config {
  def exp(f: Int) = f match {
    case 16 => 5
    case 32 => 8
    case 64 => 11
  }

  def sig(f: Int) = f match {
    case 16 => 11
    case 32 => 24
    case 64 => 53
  }

  val float_point_format = 32   // support 16, 32, 64
  val expWidth = exp(float_point_format)
  val sigWidth = sig(float_point_format)
  val use_float = true   //当use_float=true时使用浮点数, 否则使用定点数
//config of fixedpoint data format
  val DataWidth = 32
  val BinaryPoint = 16
//  val useFloat = false

//config of construct
// don't support all parallel data (datalength = 0)
// FFTstage - FFTparallel must > 1
  val radix = 4  //radix of the FFT, only supprot 2 or 4
  val FFTstage = 3 // FFT stages
  val FFTparallel = 2 // the really parallel is radix ^ FFTparallel
  val useGauss = true // whether use gauss multiplier
//  val useParallel = true // parallel input or serial input

//parameters
  val FFTlength = pow(radix, FFTstage).toInt
  val FFTparallel_r = pow(radix, FFTparallel).toInt
  val datalength = (FFTlength / (radix * FFTparallel_r))
}
