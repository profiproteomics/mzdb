package fr.profi.mzdb.util.math.wavelet

import scala.util.control.Breaks._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer
import scala.Numeric
import org.apache.commons.math3.transform.DftNormalization
import org.apache.commons.math3.transform.FastFourierTransformer
import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.exception.MathRuntimeException
import edu.emory.mathcs.jtransforms.fft.DoubleFFT_1D
import edu.emory.mathcs.jtransforms.fft.FloatFFT_1D
import org.apache.commons.math3.transform.TransformType

/**
 * static object performing continous wavelet transform
 */
object WaveletUtils {

  /** construct a fast fourier transformer */
  val transformer = new FastFourierTransformer(DftNormalization.STANDARD)

  /**
   * Make input length array to be power of 2
   * padd with zeros
   */
  def nextPowerOf2(a: Integer): Integer = {

    var b = 1;
    while (b < a) {
      b = b << 1;
    }
    if (b == 1 || b == 2)
      b = 4
    b;
  }

  /**
   * Check if n is a power of two
   */
  def checkIfItsPowerOf2(n: Integer): Boolean = {
    if (n == 0 || n == 1 || n == 2)
      return false
    ((n & -n) == n)
  }

  /**
   * make power of 2 an array padding with zero
   */
  def makePowerOf2(ydata: Array[Double]): Array[Double] = {
    if (!checkIfItsPowerOf2(ydata.length)) {
      val diff = nextPowerOf2(ydata.length) - ydata.length
      val ydatabuff = new ArrayBuffer[Double]()
      ydata.copyToBuffer[Double](ydatabuff)
      for (i <- 0 to diff - 1) {
        ydatabuff += 0
      }
      ydatabuff.toArray[Double]
    } else
      ydata
  }

  /**
   * make power of 2 an array of complex padding with zero
   */
  def makePowerOf2(ydata: Array[Complex]): Array[Complex] = {
    if (!checkIfItsPowerOf2(ydata.length)) {
      val diff = nextPowerOf2(ydata.length) - ydata.length
      val ydatabuff = new ArrayBuffer[Complex]()
      ydata.copyToBuffer[Complex](ydatabuff)
      for (i <- 0 to diff - 1) {
        ydatabuff += new Complex(0, 0)
      }
      ydatabuff.toArray[Complex]
    } else
      ydata
  }

  /**
   * to avoid boundary effect
   * tried to test but it is not concluent
   */
  def periodicExtend(sig: Buffer[Float], a: Int): Array[Float] = {
    val len = sig.length

    for (i <- 0 until a) {
      val temp1 = sig(2 * i)
      val temp2 = sig.last
      sig.+=:(temp2)
      sig += temp1
    }
    sig.toArray
  }

  /**
   * circular convolution using fft TODO:optimize 
   * @param y: its length must be a power of 2
   * @param wavelet: scaled wavelet, padded with zero to match y length
   * @return the circular convolution of y and wavelet
   */
  def convolveUsingFft(y: Array[Double], wavelet: Array[Double], useConjugate: Boolean=true): Array[Double] = {
    val initLength = y.length
    val ypower2 = this.makePowerOf2(y)
    val yfft = transformer.transform(ypower2, TransformType.FORWARD)
    
    val waveletsized = wavelet ++ ((for (i <- 0 until (ypower2.length - wavelet.length)) yield 0d))
    require(waveletsized.length == ypower2.length, s"ydata length: ${ypower2.length}, wavelet size: ${waveletsized.length}")
    
    val waveletfft = transformer.transform(waveletsized, TransformType.FORWARD).map( _.conjugate )//do not forget to take the conjugate
    
    //multiply
    val x = yfft.zip(waveletfft).map { case (a, b) => a.multiply(b) }
    return transformer.transform(x, TransformType.INVERSE).map(_.getReal).take(initLength)
  }
  
  /**for the cwt we need */
  def convolveUsingJtransform(y: Array[Float], wavelet: Array[Float], useConjugate: Boolean = true): Array[Float] = {
    val N = y.length
    //val fft = new DoubleFFT_1D(N)
    val fft = new FloatFFT_1D(N)
    
    val yfft = Array.fill[Float](N * 2)(0)
    for (j <- 0 until N ) {
      yfft(2 * j) = y(j)
    }
    fft.complexForward(yfft)

    val wavepadded = Array.fill[Float](N * 2)(0)

    val r = math.min(wavelet.length, N)
    for (i <- 0 until r ) {
      wavepadded(2 * i) = wavelet(i)
    }
    //do not forget to take the conjugate
    fft.complexForward(wavepadded)
    
    val x = Array.fill[Float](2 * N)(0)
    for (j <- 0 until N ) {
      val value = if (useConjugate) - wavepadded(2 * j + 1) else wavepadded(2 * j + 1)
      x(2 * j) =  yfft(2 * j) * wavepadded(2 * j) - yfft(2 * j + 1) * value 
      x(2 * j + 1) =  yfft(2 * j) * value +  wavepadded(2 * j) * yfft( 2 * j + 1) //tkae the conjugate
    }
    fft.complexInverse(x, true)
    x.zipWithIndex.collect{case (value, idx) if idx % 2 == 0 => value}
  }
  
  
  /**
   * compute the continous wavelet transform of input ydata
   *
   * @param ydata
   * @param wavelet: mother wavelet used, MexicanHat and Morlet implemented
   * @param scales, scales to perform cwt, scale belongs to R
   * @return Coefficients matrix
   */
  def cwt(ydata: Array[Float], wavelet: MotherWavelet = MexicanHat(), scales: Array[Float]): HashMap[Float, Array[Float]] = {

    //make y data periodic to avoid boundary effect
    val extendLength = wavelet.nbPoints / 3
    val ydataExtended = this.periodicExtend(ydata.toBuffer, extendLength) //.map( x =>0.0)

    //var ffdata = transformer.transform(makePowerOf2(ydata))

    var coeffs = new HashMap[Float, Array[Float]]//Array.ofDim[Double](scales.length, ydata.length)
    //val ydataExtended = makePowerOf2(ydata)
    val waveletValues = wavelet.values()

    val psiXval = wavelet.getPsiXval()
    val dxval = psiXval(1)
    val xmax = psiXval.last
    
    var prevExiting = scales.length
    breakable {
      for (scaleIndex <- 0 until scales.length) {//scale <- scales) {
        
        val scale = scales(scaleIndex)
        //build scales wavelet @see MassSpecWavelet, calc indexes of wavelet upSampling
        var f = ( for (i <- 0 until ydataExtended.length) yield 0f) toArray //ydata
        
        //println("buggy stuff:"  + (scale * xmax).toInt +", " + scale )
        val maxStuff = (scale * xmax).toInt
        val indexes: Array[Int] = (0 to maxStuff by 1).toArray.map { x => math.floor( x / (scale * dxval) ).toInt }
       
        
        val lenWave = indexes.length
  
        val waveletCoeffs = indexes.map { waveletValues(_) }
        
        val mean = waveletCoeffs.sum / waveletCoeffs.length
        val v = waveletCoeffs.reverse.map(_ - mean)
        for (i <- 0 until math.min(f.length, lenWave) ) {
          f(i) = v(i)
        }
        
        if (f.length > ydataExtended.length) {//ydata
          //logger.debug("exiting loop, scale was too large")
          prevExiting = scaleIndex//scale.toInt
          break
        }
        
        val convolvingResult : Array[Float] = this.convolveUsingJtransform(ydataExtended, f, true)
        val scaleogram = convolvingResult.map { x => (x * (1.0 / math.sqrt(scale))) toFloat }
  
        //flip an assign to row
        val p = ydataExtended.length - math.floor(lenWave / 2f).toInt //Extended
        //coeffs(scale) = ((scaleogram.slice(p, ydataExtended.length)) ++ (scaleogram.slice(0, p))).slice(0, ydata.length)
        coeffs(scale) = ( scaleogram.slice(p, ydataExtended.length) ++ scaleogram.slice(0, p) ).slice(extendLength, ydata.length + extendLength)
      }
    }
    if (prevExiting == scales.length) {
      return coeffs
    } else {
      return coeffs.slice(0, prevExiting)
    }
  }

  /**
   * denoise coefficients using a median percentile approach
   * should not be used
   * implemented in JSci
   */
  def denoiseCoeffs(coeffs: Array[Array[Double]], percent: Float = 0.25f) = {
    for (coeff <- coeffs) {
      var value = coeff.sortBy(x => x)
      var thresh = value((percent * coeff.length).toInt)
      for (i <- 0 until coeff.length) {
        if (coeff(i) < thresh)
          coeff(i) = 0
      }
    }
  }


  /**
   * possible optimization using periodic extension
   * see:http://code.google.com/p/wavelet1d/source/browse/trunk/src/wavelet.cpp
   */

//  def swt(signal: Array[Double], J: Int = 6, filter: WaveletFilter = Daub8): Array[Double] = {
//
//    // val transformer = new FastFourierTransformer()
//
//    var N = signal.length
//    var length = N;
//    var sig = signal
//    var lpd = filter.lp1
//    var hpd = filter.hp1
//
//    var swt_output = new ArrayBuffer[Double]
//
//    for (iter <- 0 until J) {
//      var len = sig.length
//      var low_pass: Array[Double] = null
//      var high_pass: Array[Double] = null
//
//      if (iter > 0) {
//
//        var M = math.pow(2.0, iter).toInt;
//        low_pass = upsamp(lpd, M)
//        high_pass = upsamp(hpd, M)
//
//      } else {
//        low_pass = lpd;
//        high_pass = hpd;
//      }
//
//      var len_filt = low_pass.length
//      sig = periodicExtend(sig.toBuffer, len_filt / 2)
//      //convolveUsingFft(y: Array[Double], wavelet: Array[Double]): Array[Double]
//      var cA = convolveUsingFft(sig, low_pass, useConjugate=false).toBuffer//fft.convfft(sig.toBuffer, low_pass)
//      var cD = convolveUsingFft(sig, high_pass, useConjugate=false).toBuffer//fft.convfft(sig.toBuffer, high_pass)
//
//      cA.trimStart(len_filt); cA.trimEnd(cA.length - N)
//      cD.trimStart(len_filt); cD.trimEnd(cD.length - N)
//
//      sig = cA.toArray;
//
//      if (iter == J - 1) {
//        cD.reverse.foreach(x => swt_output.insert(0, x))
//        cA.reverse.foreach(x => swt_output.insert(0, x))
//      } else {
//        cD.reverse.foreach(x => swt_output.insert(0, x))
//      }
//
//    }
//    swt_output.toArray
//  }
//
//  def upsamp(sig: Seq[Double], M: Int): Array[Double] = {
//    var len = sig.length;
//    var len_n = (math.ceil(len.toDouble * M.toDouble)).toInt;
//    var sig_u = new ArrayBuffer[Double]
//    for (i <- 0 until len_n) {
//      if (i % M == 0) {
//        var temp = sig(i / M);
//        sig_u += temp;
//
//      } else {
//        sig_u += 0;
//      }
//
//    }
//    sig_u.toArray
//  }
//
//  def downsamp(sig: Seq[Double], M: Int): Array[Double] = {
//    var len = sig.length
//    var len_n = math.ceil(len.toDouble / M.toDouble).toInt;
//    var sig_d = new ArrayBuffer[Double]
//    for (i <- 0 until len_n) {
//      var temp = sig(i * M);
//      sig_d += temp;
//    }
//    sig_d.toArray
//  }
//
//  /**
//   * compute the inverse transform
//   */
//  def iswt(swtop: Array[Double], J: Int = 6, filter: WaveletFilter = Daub8): Array[Double] = {
//
//    //var transformer = new FastFourierTransformer()
//    var N = swtop.length / (J + 1)
//
//    var lpr = filter.lp2
//    var hpr = filter.hp2
//
//    var appx_sig = new ArrayBuffer[Double]
//    var iswt_output = new ArrayBuffer[Double]
//
//    var low_pass = lpr;
//    var high_pass = hpr;
//    var lf = low_pass.length
//
//    for (iter <- 0 until J) {
//      var det_sig = new ArrayBuffer[Double];
//      if (iter == 0) {
//        for (i <- 0 until N) {
//          var temp = swtop(i);
//          appx_sig += temp;
//          var temp1 = swtop((iter + 1) * N + i)
//          det_sig += temp1
//        }
//      } else {
//        for (i <- 0 until N) {
//          var temp1 = swtop((iter + 1) * N + i);
//          det_sig += temp1
//        }
//      }
//
//      var value = math.pow(2.0, (J - 1 - iter).toDouble).toInt
//
//      iswt_output = new ArrayBuffer[Double]
//      for (i <- 0 until N) {
//        iswt_output += 0.0
//      }
//
//      for (count <- 0 until value) {
//        var appx1 = new ArrayBuffer[Double]
//        var det1 = new ArrayBuffer[Double]
//        for (index <- count until N by value) {
//          var temp = appx_sig(index);
//          appx1 += temp
//          var temp1 = det_sig(index)
//          det1 += temp1
//
//        }
//        var len = appx1.length
//
//        // Shift = 0
//
//        var appx2 = new ArrayBuffer[Double]
//        var det2 = new ArrayBuffer[Double]
//
//        for (index_shift <- 0 until len by 2) {
//          var temp = appx1(index_shift);
//          appx2 += temp
//          var temp1 = det1(index_shift);
//          det2 += temp1;
//        }
//
//        var U = 2; // Upsampling Factor
//
//        var cL0 = upsamp(appx2, U)
//        var cH0 = upsamp(det2, U)
//        cL0 = periodicExtend(cL0.toBuffer, lf / 2)
//        cH0 = periodicExtend(cH0.toBuffer, lf / 2)
//        
//        var oup00L = convolveUsingFft(cL0, low_pass, useConjugate=false).toBuffer//fft.convfft(cL0.toBuffer, low_pass)
//        var oup00H = convolveUsingFft(cH0, high_pass, useConjugate=false).toBuffer//fft.convfft(cH0.toBuffer, high_pass)
//
//        oup00L.trimStart(lf - 1); oup00L.trimEnd(oup00L.length - len)
//        oup00H.trimStart(lf - 1); oup00H.trimEnd(oup00H.length - len);
//
//        var oup00 = oup00L.zip(oup00H).map { case (a, b) => a + b }
//
//        // Shift = 1
//
//        var appx3 = new ArrayBuffer[Double]
//        var det3 = new ArrayBuffer[Double]
//
//        for (index_shift <- 1 until len by 2) {
//          var temp = appx1(index_shift)
//          appx3 += temp
//          var temp1 = det1(index_shift)
//          det3 += temp1
//        }
//
//        var cL1 = upsamp(appx3, U)
//        var cH1 = upsamp(det3, U)
//        cL1 = periodicExtend(cL1.toBuffer, lf / 2)
//        cH1 = periodicExtend(cH1.toBuffer, lf / 2)
//
//        var oup01L = convolveUsingFft(cL1, low_pass, useConjugate=false) toBuffer
//        var oup01H = convolveUsingFft(cH1, high_pass, useConjugate=false) toBuffer
//
//        oup01L.trimStart(lf - 1); oup01L.trimEnd(oup01L.length - len)
//        oup01H.trimStart(lf - 1); oup01H.trimEnd(oup01H.length - len)
//
//        var oup01 = oup01L.zip(oup01H).map { case (a, b) => a + b }.toBuffer[Double]
//        circshift(oup01, -1)
//
//        //   Continue
//        var index2 = 0;
//        for (index <- count until N by value) {
//          var temp = oup00(index2) + oup01(index2);
//          iswt_output(index) = temp / 2;
//          index2 += 1
//        }
//      }
//      appx_sig = iswt_output;
//    }
//    iswt_output.toArray
//  }
//  
//    def circshift(sig_cir: Buffer[Double], L: Int) = {
//    var K = L
//    if (math.abs(K) > sig_cir.length) {
//      K = sign(K) * (math.abs(K) % sig_cir.length)
//    }
//
//    if (K < 0) {
//      K = (sig_cir.length + K) % sig_cir.length
//    }
//    for (i <- 0 until K) {
//      sig_cir += sig_cir(0)
//      sig_cir.remove(0)
//    }
//
//  }
//
//  def sign(X: Int): Int = {
//    if (X >= 0)
//      return 1;
//    else
//      return -1;
//  }
//
//  def sign[T](x: T)(implicit X: Numeric[T]): Int = {
//    if (X.gt(x, X.zero))
//      return 1
//    return -1
//  }
//
//  ///////////////////////////////////////////////////////////////////////////////////////////////
//  /**
//   * coeff denoising Donoho et al., 1995, Coombes et al 2005
//   */
//  def denoiseSoft(coeffs: Array[Double], J: Int = 6) = {
//    var N = coeffs.length / (J + 1)
//    var cA = coeffs.slice(0, N)
//    //println("CA length : " + cA.length)
//    //cA.foreach(x=> print( x + "\t"));println()
//    var firstCD = coeffs.slice(coeffs.length - N, coeffs.length)
//    //firstCD.foreach(x=>print(x + "\t")); println()
//    //cA.zip(firstCD).foreach { case (a, b) => if (b > 0) print((a-b)+ "\t") else print(a+"\t")}
//    var sortedCD = firstCD.sortBy(x => x)
//    var med = firstCD((0.5 * firstCD.length).toInt)
//    var thresh = med / 0.6745 * math.sqrt(2 * math.log(N))
//
//    for (i <- 0 until coeffs.length) {
//      if (math.abs(coeffs(i)) < thresh) {
//        coeffs(i) = 0
//      } else if (coeffs(i) > thresh) {
//        coeffs(i) -= thresh
//      } else if (coeffs(i) < 0) {
//        coeffs(i) += thresh
//      } else {
//        println("Weird coeffs, warning")
//      }
//    }
//  }
//
//  /**
//   *
//   */
//  def denoiseHard(coeffs: Array[Double], J: Int) = {
//    var N = coeffs.length / (J + 1)
//    var cA = coeffs.slice(0, N)
//    var firstCD = coeffs.slice(coeffs.length - N, coeffs.length)
//    var sortedCD = firstCD.sortBy(x => x)
//    var med = firstCD((0.5 * firstCD.length).toInt)
//    var thresh = (med / 0.6745) * math.sqrt(2 * math.log(N))
//
//    for (i <- 0 until coeffs.length) {
//      if (coeffs(i) < thresh) {
//        coeffs(i) = 0.0
//      }
//    }
//  }



}
