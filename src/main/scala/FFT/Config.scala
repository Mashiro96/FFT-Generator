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

  val float_point_format = 64   // support 32, 64
  val expWidth = exp(float_point_format)
  val sigWidth = sig(float_point_format)
  val use_float = true //当use_float=true时使用浮点数, 否则使用定点数
//config of fixedpoint data format
  val FixDataWidth = 32
  val BinaryPoint = 16
//  val useFloat = false

// config of construct
// don't support all parallel data (datalength = 1)
// FFTstage - FFTparallel must > 0
  val radix = 8  //radix of the FFT,supprot 2, 4, 8
  val FFTstage = 2 // FFT stages
  val FFTparallel = 1 // the really parallel is radix ^ FFTparallel
  val useGauss = false // whether use gauss multiplier
//  val useParallel = true // parallel input or serial input

//parameters
  val FFTlength = pow(radix, FFTstage).toInt
  val FFTparallel_r = pow(radix, FFTparallel).toInt
  val datalength = (FFTlength / (radix * FFTparallel_r))
  val DataWidth = if(use_float) float_point_format + 1 else FixDataWidth
}
