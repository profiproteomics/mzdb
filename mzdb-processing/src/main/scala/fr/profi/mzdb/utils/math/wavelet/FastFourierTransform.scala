package fr.profi.mzdb.utils.math.wavelet
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.complex.Complex
import scala.collection.mutable.Buffer

/**
 * static function performing fast fourier transform
 */
object fft {

  /**
   * if conjug == 1, do the multiplication with the conjugate in the fourier space
   * else keep normal stuff
   */
  def convfft(a: Buffer[Double], b: Array[Double], conjug: Int = -1): Buffer[Double] = {
    var c = new ArrayBuffer[Double]
    var N = a.length + b.length - 1
    var inp = new ArrayBuffer[Complex]
    var filt = new ArrayBuffer[Complex]

    for (i <- 0 until a.length) {
      var temp = a(i)
      inp += new Complex(temp, 0.)
    }

    for (i <- 0 until b.length) {
      var temp = b(i)
      filt += new Complex(temp, 0.)
    }
    fft(inp, 1, N)
    fft(filt, 1, N)
    var temp = new ArrayBuffer[Complex]

    var K = inp.length
    for (i <- 0 until K) {
      var mult: Complex = null;
      if (conjug == -1)
        mult = inp(i).multiply(filt(i))
      else
        mult = inp(i).multiply(filt(i).conjugate)
      temp += mult
    }
    fft(temp, -1, K)

    for (i <- 0 until N) {
      var temp1 = temp(i).getReal //math.sqrt(math.pow(temp(i).getReal, 2) + math.pow(temp(i).getImaginary, 2))
      c += temp1
    }
    c
  }
  
  /**complex version*/
  def convfft(a: Buffer[Complex], b: Array[Complex], conjug: Int): Buffer[Double] = {
    var c = new ArrayBuffer[Double]
    
    var N = a.length + b.length - 1
    var inp = new ArrayBuffer[Complex]
    var filt = new ArrayBuffer[Complex]
    
    for (i <- 0 until a.length) {
      var temp = a(i)
      inp += temp
    }

    for (i <- 0 until b.length) {
      var temp = b(i)
      filt += temp
    }
    
    fft(inp, 1, N)
    fft(filt, 1, N)
    var temp = new ArrayBuffer[Complex]

    var K = inp.length
    for (i <- 0 until K) {
      var mult: Complex = null;
      if (conjug == -1)
        mult = inp(i).multiply(filt(i))
      else
        mult = inp(i).multiply(filt(i).conjugate)
      temp += mult
    }
    fft(temp, -1, K)

    for (i <- 0 until N) {
      var temp1 = temp(i).getReal //math.sqrt(math.pow(temp(i).getReal, 2) + math.pow(temp(i).getImaginary, 2))
      c += temp1
    }
    c
  }
  

  def fft(data: ArrayBuffer[Complex], sign: Int, N: Int) = {
    var N_ = N
    var pi = -3.14159265358979;
    if (sign == 1 || sign == -1) {
      pi = sign * pi
    }
    var len = data.length

    if (len != N_) {
      var al = N_ - len;
      for (i <- 0 until al) {
        data += new Complex(0., 0.)
      }
    }

    var K = (math.pow(2.0, math.ceil(math.log10(N_.toDouble) / math.log10(2.0)))).toInt

    if (N_ < K) {

      var al = K - N_
      for (i <- 0 until al) {
        data += new Complex(0, 0)
      }
      N_ = K

    }

    bitreverse(data);

    //	 radix2(data);
    var iter = 1
    while (iter < N_) {
      var step = iter << 1;

      var theta = pi / iter.toDouble

      var wtemp = math.sin(theta * .5);
      //   Multipliers
      var wreal = -2 * wtemp * wtemp;
      var wimag = math.sin(theta);

      //   Factors
      var wr = 1.0;
      var wi = 0.0;
      //   Iteration through two loops

      for (m <- 0 until iter) {
        //   Iteration within m
        for (i <- m until N_ by step) {
          //   jth position
          var j = i + iter;

          var tempr = wr * data(j).getReal - wi * data(j).getImaginary()
          var tempi = wr * data(j).getImaginary + wi * data(j).getReal

          var temp = new Complex(tempr, tempi);
          data(j) = data(i).subtract(temp)
          data(i) = data(i).add(temp)

        }
        //   Twiddle Factors updated
        wtemp = wr
        wr += wr * wreal - wi * wimag
        wi += wi * wreal + wtemp * wimag
      }
      iter <<= 1
    }

    if (sign == -1) {
      var scale = 1.0 / N_.toDouble
      for (i <- 0 until N_) {
        data(i) = data(i).multiply(scale)
      }
    }

  }

  def bitreverse(sig: ArrayBuffer[Complex]) = {
    var len = sig.length
    var N = (math.pow(2.0, math.ceil(math.log10(len.toDouble) / math.log10(2.0)))).toInt
    var rev = 0;
    //   Processing Input Data
    for (iter <- 0 until N) {
      if (rev > iter) {
        //   Replacing current values with reversed values

        var tempr = sig(rev).getReal
        var tempi = sig(rev).getImaginary
        var temp = new Complex(tempr, tempi)
        sig(rev) = sig(iter)
        sig(iter) = temp

      }
      //   Using filter "filt" such that the value of reverse changes with each iteration
      var filt = N;
      filt >>= 1
      while ((rev & filt)!= 0) {
        rev &= ~filt
        filt >>= 1
      }
      rev |= filt
    }
  }

}