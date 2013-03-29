package fr.profi.mzdb.algo.signal.detection

import java.awt.Color
import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import org.jfree.chart.ChartUtilities
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.GrayPaintScale
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.data.xy.DefaultXYZDataset
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.utils.math.wavelet.MotherWavelet
import fr.profi.mzdb.utils.math.wavelet.Ridge
import fr.profi.mzdb.utils.math.wavelet.RidgesFinder
import fr.profi.mzdb.utils.math.wavelet.WaveletUtils
import mr.go.sgfilter.SGFilter
import fr.profi.mzdb.model.ILcContext

/**
 * result of the algorithm
 */
case class CwtPeakel(val apex: Int,
                     val apexLcContext: ILcContext,
                     val minIdx: Int,
                     val startLcContext: ILcContext,
                     val maxIdx: Int,
                     val endLcContext: ILcContext, 
                     val snr: Float) {}

/**
 * Peak Width are 2 * scale where scale is the maxima point on the ridge
 */
class WaveletBasedPeakelFinder(var peaks: Seq[Peak],
                               var scales: Array[Float] = (1f to 64f by 1f).toArray[Float],
                               var wavelet: MotherWavelet = MexicanHat()) extends RidgesFinder {

  //by default MexicanHat is used since it has better results
  var ydata = peaks.map(_.getIntensity().toDouble)
  var coeffs: Array[Array[Double]] = WaveletUtils.cwt(ydata.toArray, wavelet, scales.toArray)
  var maxScale = coeffs.length

  //Debug
  this.coeffToImage(coeffs, "coeffs.jpg")
  this.maximaToImage(findMaximaRegion)
  this.maximaToImage(findMinimaRegion, "minima.jpg")

  /** recompute the coefficients */
  def computeCoeffs() { coeffs = WaveletUtils.cwt(ydata.toArray, MexicanHat(), scales.toArray) }

  //TODO
  /** compute the energy density of the wavelet */
  def energyDensity(): Array[Array[Double]] = { throw new Error("Not yet implemented") }

  /** smooth signal with a SG smoother */
  def smoothSignal(signal: Array[Double], times: Int = 3): Array[Double] = {
    import mr.go.sgfilter.SGFilter

    val (nl, nr, order) = (5, 5, 4)
    val polycoef = SGFilter.computeSGCoefficients(nl, nr, order)

    val sgFilter = new SGFilter(5, 5)
    var smoothedValues = signal
    for (i <- 1 to times) {
      smoothedValues = sgFilter.smooth(smoothedValues, polycoef)
    }
    smoothedValues
  }

  /**
   * Improved peak detection in mass spectrum by incorporating continuous wavelet transform-based pattern matching
   */
  def findMaxima(coeffs: Array[Array[Double]], winSize: Int = 5): Array[Array[Int]] = {

    var maximaIndexesPerScale = new ArrayBuffer[ArrayBuffer[Int]]
    var c = 0
    for (coeff <- coeffs) {
      val winsize = math.max(winSize, c * 2 + 1)
      var localMax = new ArrayBuffer[Int]
      //find maxima in each window of length winSize
      for (i <- 0 until coeff.length by winsize) {
        val maxi = coeff.slice(i, math.min(i + winsize, coeff.length)).max
        localMax += coeff.indexOf(maxi)
      }
      //shift by winSize /2
      for (i <- winsize / 2 until coeff.length by winsize) {
        val maxi = coeff.slice(i, math.min(i + winsize, coeff.length)).max
        localMax += coeff.indexOf(maxi)
      }
      //take the strongest maxima when 2 maxima indexes are close enough < to winSize
      for (i <- 0 until localMax.length - 1) {
        val (idx1, idx2) = (localMax(i), localMax(i + 1))
        if (math.abs(idx1 - idx2) <= winsize) {
          if (coeff(idx1) - coeff(idx2) <= 0) {
            localMax(i) = 0
          } else {
            localMax(i + 1) = 0
          }
        }
      }
      maximaIndexesPerScale += localMax.filter(_ != 0)
      c += 1
    }
    maximaIndexesPerScale.map(x => x.toArray[Int]).toArray[Array[Int]]
  }

  /**
   * try to find all maxima using kind of derivative approach
   * find too few maxima
   */
  def findStrictMaxima(coeffs: Array[Array[Double]]): Array[Array[Int]] = {
    var maximaIndexesPerScale = new ArrayBuffer[ArrayBuffer[Int]]

    for (coeff <- coeffs) {

      var maxima = new ArrayBuffer[Int]
      var apexIndex = 0;
      var peakHasBeenInserted = false;

      for (i <- 0 until coeff.length - 1) {

        if (coeff(i + 1) >= coeff(i)) {
          peakHasBeenInserted = false

        }
        if (coeff(i + 1) < coeff(i)) {
          if (!peakHasBeenInserted) {
            maxima += i
            println(i)
            peakHasBeenInserted = true;
          }
        }
      }
      maximaIndexesPerScale += maxima
    }
    maximaIndexesPerScale.map(x => x.toArray[Int]).toArray[Array[Int]]
  }

  /**
   * Reference:
   * A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   */
  def findMaximaRegion(coeffs: Array[Array[Double]], winSize: Int = 5): Array[Array[Int]] = {
    var maximaIndexesPerScale = new ArrayBuffer[ArrayBuffer[Int]]
    var c = 0
    for (coeff <- coeffs) {
      val winsize = math.max(winSize, c * 2 + 1)
      var localMax = new ArrayBuffer[Int]
      var coeffCloned = coeff.clone
      while (coeffCloned.length != 0) {
        val maxi = coeffCloned.max
        val maxIdxToCoeff = coeff.indexOf(maxi)
        val maxIdxToCoeffCloned = coeffCloned.indexOf(maxi)
        localMax += maxIdxToCoeff
        val (inf, sup) = (math.max(0, maxIdxToCoeffCloned - winsize / 2), math.min(coeffCloned.length, maxIdxToCoeffCloned + winsize / 2))
        coeffCloned = coeffCloned.diff(coeffCloned.slice(inf, sup))
      }
      maximaIndexesPerScale += localMax
      c += 1
    }
    maximaIndexesPerScale.map(x => x.toArray[Int]).toArray[Array[Int]]

  }

  /**
   * find minima by inversing wavelet coefficient,
   *  A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   */
  def findMinimaRegion(coeffs: Array[Array[Double]], winSize: Int = 5): Array[Array[Int]] = {
    val inverseCoeffs = coeffs map { x => x map { y => -y } }
    findMaximaRegion(inverseCoeffs, winSize)
  }

  /**
   * The CWT is performed on 32 scales (1 to 32) it would be better to
   * do 1 to 64 by step of 2
   * winLength: min Window acceptable
   * minRidgeLength : default mid 16 general half of scale number
   * scaleRange : maxCWT coeff must be in scale > 5
   * SNR = 3 by default could be less ?
   * the principal difficulty is that we need to readjust some parameters
   * regarding the length of the input array
   * Normally, we choose scale according to peaks width we want to find
   */
  def ridgeToPeaks(ridges: Array[Ridge],
                   minRidgeLength: Int = maxScale / 4, //this hard to defined 
                   minSNR: Float = 0.3f,
                   minPeakWidth:Float = 10f, // en seconds
                   maxPeakWidth : Float = 110f,
                   sizeNoise: Int = ydata.length / 20, //the same this hard to defined
                   skipBoundaries: Int = ydata.length / 20): Array[CwtPeakel] = { 

    var peakels = new ArrayBuffer[CwtPeakel]
     
    //filter against selected criteria
    var filteredRidges = ridges.filter(x => !x.isEnded && x.startingScale > 2 && x.length >= minRidgeLength) 
    
    //compute SNR for each ridge
    filteredRidges.foreach { x =>
      var (firstScale, maxIdxAtFirstScale, maxValueAtFirstScale) = x.firstScaleMaxCoeffPos
      var centroidValue = math.abs(maxValueAtFirstScale);
      var snrCoeffs = coeffs(0).map(math.abs(_)).slice(math.max(maxIdxAtFirstScale - sizeNoise, 0), 
            math.min(maxIdxAtFirstScale + sizeNoise, ydata.length - 1)).toList.sortBy(x => x)
      var noiseValue = snrCoeffs((0.9 * snrCoeffs.length) toInt)
      var estimatedSNR = centroidValue / noiseValue
      x.SNR = estimatedSNR.toFloat 
    }
    
    //filter against SNR criteria
    filteredRidges.filter(x => x.SNR > 3 )
    
    //peakWidth estimation
    filteredRidges.foreach { ridge =>
      var (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos
      var (firstScale, maxIdxAtFirstScale, maxValueAtFirstScale) = ridge.firstScaleMaxCoeffPos
      var demifwhm = (1.252 * maxScale) / 2
      val rt = peaks(maxIdxAtFirstScale).getLcContext().getElutionTime()
      val sortedPeaks_ = peaks.sortBy(x=> math.abs( x.getLcContext().getElutionTime() - (rt - demifwhm) )) toArray
      val sortedPeaks__ = peaks.sortBy(x=> math.abs( x.getLcContext().getElutionTime() - (rt + demifwhm) )).toArray
      if (demifwhm * 2 > minPeakWidth && demifwhm * 2 < maxPeakWidth) {
        peakels += CwtPeakel(apex = maxIdxAtFirstScale,
                             apexLcContext = peaks(maxIdxAtFirstScale).getLcContext,
                             minIdx = peaks.indexOf(sortedPeaks_(0)),
                             startLcContext = sortedPeaks_(0).getLcContext, 
                             maxIdx = peaks.indexOf(sortedPeaks__(0)),
                             endLcContext = sortedPeaks__(0).getLcContext,
                             snr = 0)
      }
    }
    peakels.toArray
  }

  /**
   * find local minima: basically to get the end of the peak
   * go the the left then to the right
   * since apex
   */
  def findLocalMinima(scale: Int = 0, maxIdx: Int): Pair[Int, Int] = {
    var coeffVal = coeffs(scale)(maxIdx)

    var i = maxIdx - 1 //maxIdx supposed to be always greater than zero
    var currVal = coeffs(scale)(i)
    var prevVal = coeffs(scale)(i - 1)
    //case previous point greater than maxValue
    //wrong position of the apex ?
    //to the left first
    while (i > 1 && currVal > prevVal) {
      i -= 1
      currVal = prevVal
      prevVal = coeffs(scale)(i - 1)
    }
    while (i > 1 && prevVal < currVal) {
      i -= 1
      currVal = prevVal
      prevVal = coeffs(scale)(i - 1)
      //currVal = coeffs(scale)(i)
    }
    var minIdx = i + 1

    //to the right of the peak
    i = maxIdx + 1
    currVal = coeffs(scale)(i)
    var nextVal = coeffs(scale)(i + 1)
    //if we did not find exactly the apex
    while (i < coeffs(scale).length - 2 && currVal < nextVal) {
      i += 1
      currVal = nextVal
      nextVal = coeffs(scale)(i + 1)
    }
    while (i < coeffs(scale).length - 2 && currVal > nextVal) {
      i += 1
      currVal = nextVal
      nextVal = coeffs(scale)(i + 1)
    }
    var lastIdx = i - 1

    (minIdx, lastIdx)
  }

  /**
   * return the indexes of the peakel found
   */
  def findPeakelsIndexes(smooth: String = "swt", //smoothing method could be 
                         winLength: Int = 5,
                         ridgeMethod: String = "maxima",
                         minRidgeLength: Int = 15,
                         minSNR: Float = 1.5f,
                         sizeNoise: Int = 200,
                         skipBoundaries: Int = (ydata.length * 2.5 / 100).toInt): Array[Pair[Int, Int]] = {

    val peakels = findCwtPeakels(smooth, winLength, ridgeMethod, minRidgeLength, minSNR, sizeNoise, skipBoundaries)
    peakels.map(x => (x.minIdx, x.maxIdx))
  }

  def findCwtPeakels(smooth: String = "swt", //smoothing method could be 
                     winLength: Int = 5,
                     ridgeMethod: String = "maxima",
                     minRidgeLength: Int = 15,
                     minSNR: Float = 2f,
                     sizeNoise: Int = 200,
                     skipBoundaries: Int = (ydata.length * 2.5 / 100).toInt): Array[CwtPeakel] = {

    var y_data: Array[Double] = null
    var swtSmoothing = false
    if (smooth == "swt") {
      var coefficients = WaveletUtils.swt(ydata.toArray[Double], 5)
      WaveletUtils.denoiseSoft(coefficients)
      y_data = WaveletUtils.iswt(coefficients)
      swtSmoothing = true
    } else if (smooth == "sg")
      y_data = smoothSignal(ydata.toArray, 3)
    else
      y_data = ydata.toArray

    val maxima = findMaximaRegion(coeffs, 10)
    val ridges = findRidges(maxima, coeffs, winLength)
    val peakels = ridgeToPeaks(ridges,
      minRidgeLength = minRidgeLength,
      minSNR = minSNR,
      sizeNoise = sizeNoise,
      skipBoundaries = skipBoundaries)
    peakels
  }

  /**
   * *********************************************************************************
   * //Plotting functions
   * *********************************************************************************
   */
  def coeffToImage(coeffs: Array[Array[Double]], file: String = "coeffs.jpg") = {
    var dataset = new DefaultXYZDataset()
    for (i <- 0 until coeffs.length) {
      var x = new ArrayBuffer[Double]
      var y = new ArrayBuffer[Double]
      for (j <- 0 until coeffs(i).length) {
        x += j + 1
        y += i + 1
      }
      var array = Array.ofDim[Double](3, ydata.length) //[Array[Double]]()
      array(0) = x.toArray
      array(1) = y.toArray
      array(2) = coeffs(i)
      dataset.addSeries(i, array)
    }
    var renderer = new XYBlockRenderer()
    var scale = new GrayPaintScale(-20, 10);
    renderer.setPaintScale(scale);

    var xAxis = new NumberAxis("X");
    var yAxis = new NumberAxis("Y");
    var plot = new XYPlot(dataset, xAxis, yAxis, renderer)
    plot.setBackgroundPaint(Color.lightGray);
    var chart = new JFreeChart("XYBlockChartDemo1", plot)
    ChartUtilities.saveChartAsJPEG(new java.io.File(file), chart, ydata.length, ydata.length);
  }

  def maximaToImage(f: (Array[Array[Double]], Int) => Array[Array[Int]], file: String = "maxima.jpg") {
    var maxima = f(coeffs, 10) //(winSize = coeffs(0).length / 15)
    var dataset = new DefaultXYZDataset()
    for (i <- 0 until maxima.length) {
      var x = new ArrayBuffer[Double]
      var y = new ArrayBuffer[Double]
      for (j <- 0 until coeffs(i).length) {
        x += j + 1
        y += i + 1
      }
      var array = Array.ofDim[Double](3, ydata.length) //[Array[Double]]()
      array(0) = x.toArray
      array(1) = y.toArray
      for (j <- maxima(i)) {
        array(2)(j) = 100
      }
      dataset.addSeries(i, array)
    }
    var renderer = new XYBlockRenderer()
    var scale = new GrayPaintScale(-100, 100);
    renderer.setPaintScale(scale);

    var xAxis = new NumberAxis("X");
    var yAxis = new NumberAxis("Y");
    var plot = new XYPlot(dataset, xAxis, yAxis, renderer)
    plot.setBackgroundPaint(Color.lightGray);
    var chart = new JFreeChart("XYBlockChartDemo1", plot)
    ChartUtilities.saveChartAsJPEG(new java.io.File(file), chart, ydata.length, ydata.length);

  }

  /**
   * read in Mass spectrometry data processing using zero-crossing lines in multi-scale of Gaussian derivative wavelet. Nguyen et al
   * equivalent of find maxima, looking for zero crosses of coefficients instead
   *
   * does not provide better result than maxima approach or bad implemented ?
   */
  def zeroCrossingsLines() { //: Array[Array[Int]] = 
    var zeroCross = new ArrayBuffer[ArrayBuffer[Int]]
    for (coeff <- coeffs) {
      var scaleZeroCross = new ArrayBuffer[Int]
      var counter = 0 //take the first index ?
      coeff.sliding(2).foreach { x =>
        if (WaveletUtils.sign[Double](x(0)) != WaveletUtils.sign[Double](x(1))) {
          scaleZeroCross += counter
        }
        counter += 1
      } //end foreach
      zeroCross += scaleZeroCross
    }
    zeroCross.map(x => x.toArray[Int]).toArray[Array[Int]]

    var dataset = new DefaultXYZDataset()
    for (i <- 0 until zeroCross.length) {
      var x = new ArrayBuffer[Double]
      var y = new ArrayBuffer[Double]
      for (j <- 0 until coeffs(i).length) {
        x += j + 1
        y += i + 1
      }
      var array = Array.ofDim[Double](3, ydata.length) //[Array[Double]]()
      array(0) = x.toArray
      array(1) = y.toArray
      for (j <- zeroCross(i)) {
        array(2)(j) = 100
      }
      dataset.addSeries(i, array)
    }
    var renderer = new XYBlockRenderer()
    var scale = new GrayPaintScale(-100, 100);
    renderer.setPaintScale(scale);

    var xAxis = new NumberAxis("X");
    var yAxis = new NumberAxis("Y");
    var plot = new XYPlot(dataset, xAxis, yAxis, renderer)
    plot.setBackgroundPaint(Color.lightGray);
    var chart = new JFreeChart("XYBlockChartDemo1", plot)
    ChartUtilities.saveChartAsJPEG(new java.io.File("zerocross.jpg"), chart, ydata.length, ydata.length);
  }

}//end class


