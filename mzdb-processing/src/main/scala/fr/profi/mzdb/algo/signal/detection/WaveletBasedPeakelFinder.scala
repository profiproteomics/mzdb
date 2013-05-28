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
import scala.collection.mutable.HashMap

/**
 * result of the algorithm
 */
case class CwtPeakel(val apex: Int,
                     val apexLcContext: ILcContext,
                     val minIdx: Int,
                     val startLcContext: ILcContext,
                     val maxIdx: Int,
                     val endLcContext: ILcContext,
                     val xMax: Float,
                     val intensityMax: Float, //
                     val centroid: Float, //x "centroid" value 
                     val snr: Float) {

  override def toString(): String = {
    "apex:" + apex + ", minIdx:" + minIdx + ", maxIdx:" + maxIdx + ", xmax:" + xMax + ", intensityMax:" + intensityMax + ", centroid:" + centroid + ", snr:" + snr
  }
}

object WaveletBasedPeakelFinder {
  /**
   * @author Marco
   * enumeration specifying the type of wanted smoothing
   *
   */
  object SmoothingMethod extends Enumeration {
    type SmoothingMethod = Value
    val SG, SWT, None = Value //Savitsky Golay, stationnary wavelet transform (heavier), None
  }
}

/**
 * @author Marco
 * Peak Width are 2 * scale where scale is the maxima point on the ridge
 * actually  the peak width estimation is a little bit tricky, differs a lot
 * between articles
 *
 */
class WaveletBasedPeakelFinder(var peaks: Seq[Peak],
                               var scales: Array[Float] = (5f to 64f by 1f).toArray[Float],
                               var wavelet: MotherWavelet = MexicanHat()) extends RidgesFinder {

  //by default MexicanHat is used since it has better results
  var ydata = peaks.map(_.getIntensity().toDouble)
  var coeffs: Array[Array[Double]] = WaveletUtils.cwt(ydata.toArray, wavelet, scales)
  var maxScale = coeffs.length

  //Debug
  /*this.coeffToImage(coeffs, "coeffs.jpg")
  this.maximaToImage(findMaximaRegion)
  this.maximaToImage(findMinimaRegion, "minima.jpg")*/

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
  protected def _findMaxima(coeffs: Array[Array[Double]], winSize: Int = 5): Array[Array[Int]] = {

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
  protected def _findStrictMaxima(coeffs: Array[Array[Double]]): Array[Array[Int]] = {
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
            //println(i)
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
  protected def _findMaximaRegion(coeffs: Array[Array[Double]], winSize: Int = 5): Array[Array[Int]] = {
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
  protected def _findMinimaRegion(coeffs: Array[Array[Double]], winSize: Int = 5): Array[Array[Int]] = {
    val inverseCoeffs = coeffs map { x => x map { y => -y } }
    _findMaximaRegion(inverseCoeffs, winSize)
  }

  /**
   * find local minima: basically to get the end of the peak
   * go the the left then to the right
   * since apex
   */

  protected def _findLocalMinima(scale: Int = 0, maxIdx: Int): Pair[Int, Int] = {
    var coeffVal = coeffs(scale)(maxIdx)

    var i = math.max(maxIdx - 1, 0) //maxIdx supposed to be always greater than zero
    var currVal = coeffs(scale)(i)
    var prevVal = coeffs(scale)(math.max(0, i - 1))
    //to the left of the peak
    while (i > 1 && prevVal < currVal) {
      i -= 1
      currVal = prevVal
      prevVal = coeffs(scale)(i - 1)
    }
    var minIdx = i + 1 //add cause we got to far

    //to the right of the peak
    i = math.min(maxIdx + 1, coeffs(scale).length - 1)
    currVal = coeffs(scale)(i)
    var nextVal = coeffs(scale)(math.min(i + 1, coeffs(scale).length - 1))
    while (i < coeffs(scale).length - 2 && currVal > nextVal) {
      i += 1
      currVal = nextVal
      nextVal = coeffs(scale)(i + 1)
    }
    var lastIdx = i - 1
    if (minIdx != lastIdx)
      return (minIdx, lastIdx)
    else
      return (0, 0)
  }

  ///////////////////////////////////////////////////////////
  //            HIGHER LEVEL FUNCTIONS
  //////////////////////////////////////////////////////////
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
                   minSNR: Float = 0.3f, //no SNR provided by default
                   minPeakWidth: Float = 10f, // en seconds
                   maxPeakWidth: Float = 110f,
                   sizeNoise: Int = ydata.length / 20, //the same this hard to defined
                   skipBoundaries: Int = ydata.length / 20): Array[CwtPeakel] = {

    var peakels = new ArrayBuffer[CwtPeakel]

    //filter against selected criteria
    // 1: the ridge do not have to be ended
    // 2: it must begin at a scale > 2
    // 3: its length must be > minRidgeLength
    var filteredRidges = ridges.filter { x => (!x.isEnded() && x.length >= minRidgeLength) } //&& x.startingScale > 2 => this hard to defined !

    //compute SNR for each ridge
    filteredRidges.foreach { ridge =>
      var (firstScale, maxIdxAtFirstScale, maxValueAtFirstScale) = ridge.firstScaleMaxCoeffPos
      var centroidValue = math.abs(maxValueAtFirstScale);
      var snrCoeffs = coeffs(0).map(math.abs(_)).slice(math.max(maxIdxAtFirstScale - sizeNoise, 0),
        math.min(maxIdxAtFirstScale + sizeNoise, ydata.length - 1)).toList.sortBy(x => x)
      var noiseValue = if (! snrCoeffs.isEmpty) snrCoeffs((0.9 * snrCoeffs.length) toInt) else 0
      var estimatedSNR = centroidValue / noiseValue
      ridge.SNR = estimatedSNR.toFloat
    }

    //filter against SNR criteria
    filteredRidges = filteredRidges.filter(ridge => ridge.SNR > minSNR)

    //peakWidth estimation
    filteredRidges.foreach { ridge =>
      var (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos
      var (firstScale, maxIdxAtFirstScale, maxValueAtFirstScale) = ridge.firstScaleMaxCoeffPos
      val (minIdx_, maxIdx_) = _findLocalMinima(maxScale, maxIdxAtMaxScale)
      if (minIdx_ != 0 && maxIdx_ != 0) {
        //println(minIdx_ + "\t" + maxIdx_)
        //centroid calculation
        val slicedPeaks = peaks.slice(minIdx_, maxIdx_)
        val intensities = slicedPeaks.map(_.getIntensity)
        val xvalues = slicedPeaks.map(_.getLcContext.getElutionTime)
        val b = (!xvalues.isEmpty && !intensities.isEmpty)
        val centroid = if (b)
                          xvalues.zip(intensities).map { case (x, y) => x * y }.reduceLeft(_ + _) / intensities.reduceLeft(_ + _)
                       else 0
        val intensityMax = if (b) intensities.max else 0
        val xmax = if (b) xvalues.max else 0

        //Seen in the thesis (biblio)
        //var demifwhm = (1.252 * maxScale) / 2
        //val rt = peaks(maxIdxAtFirstScale).getLcContext().getElutionTime()
        //val sortedPeaks_ = peaks.sortBy(x=> math.abs( x.getLcContext().getElutionTime() - (rt - demifwhm) )) toArray
        //val sortedPeaks__ = peaks.sortBy(x=> math.abs( x.getLcContext().getElutionTime() - (rt + demifwhm) )).toArray
        //if (demifwhm * 2 > minPeakWidth && demifwhm * 2 < maxPeakWidth) {
        peakels += CwtPeakel(apex = maxIdxAtFirstScale,
          apexLcContext = peaks(maxIdxAtFirstScale).getLcContext,
          minIdx = minIdx_, //peaks.indexOf(sortedPeaks_(0)),
          startLcContext = peaks(minIdx_).getLcContext, //sortedPeaks_(0).getLcContext, 
          maxIdx = maxIdx_, //peaks.indexOf(sortedPeaks__(0)),
          endLcContext = peaks(maxIdx_).getLcContext, //sortedPeaks__(0).getLcContext,
          xMax = xmax toFloat,
          intensityMax = intensityMax toFloat,
          centroid = centroid toFloat,
          snr = ridge.SNR)
      }

    }
    //filter overlapping peakels tkae the largest
    peakels = peakels.filter( x => math.abs(x.maxIdx - x.minIdx) >= 3) // filter the two small peakel
    peakels = filterOverlappingPeakels(peakels); // remove a detected peakel which is contained in another peakel
    
    //peakels.foreach(x => println(x))
    peakels.toArray
  }
  
  
  def filterOverlappingPeakels(peakels: ArrayBuffer[CwtPeakel]) : ArrayBuffer[CwtPeakel] = {
    var overlappingPeakelsByMasterPeakel = new HashMap[CwtPeakel, ArrayBuffer[CwtPeakel]]
    for (i <- 0 until peakels.length ) {
      overlappingPeakelsByMasterPeakel(peakels(i)) = new ArrayBuffer[CwtPeakel]
      for (j <- 0 until peakels.length) {
        if (i != j) {
          //complete overlapping peakels
          if ( (peakels(j).minIdx >= peakels(i).minIdx && peakels(j).maxIdx <= peakels(i).maxIdx) ){ // || 
              //(peakels(j).minIdx < peakels(i).maxIdx && peakels(j).maxIdx > peakels(i).minIdx) ||
              //(peakels(j).maxIdx < peakels(i).maxIdx && peakels(j).minIdx < peakels(i).minIdx) ) {
            overlappingPeakelsByMasterPeakel(peakels(i)) += peakels(j)
          }      
        }
      }
    }
    var finalPeakels = new ArrayBuffer[CwtPeakel]
    overlappingPeakelsByMasterPeakel.map{ case (k, v) => 
      if ( ! v.isEmpty) {
        v += k
        v.sortBy(x => x.maxIdx - x.minIdx)
        finalPeakels += v.last
      } else {
        finalPeakels += k
      }
    }
    finalPeakels.distinct
  }


  /**
   * return the indexes of the peakel found
   */
  def findPeakelsIndexes(smooth: WaveletBasedPeakelFinder.SmoothingMethod.Value = WaveletBasedPeakelFinder.SmoothingMethod.SWT, //smoothing method could be sg or swt or none
                         winLength: Int = 5,
                         ridgeMethod: String = "maxima",
                         minRidgeLength: Int = maxScale / 4,
                         minSNR: Float = 0.3f,
                         sizeNoise: Int = ydata.length / 20,
                         skipBoundaries: Int = ydata.length / 20): Array[Pair[Int, Int]] = {

    val peakels = findCwtPeakels(smooth, winLength, ridgeMethod, minRidgeLength, minSNR, sizeNoise, skipBoundaries)
    peakels.map(x => (x.minIdx, x.maxIdx))
  }

  /**
   * return the peakels
   */
  def findCwtPeakels(smooth: WaveletBasedPeakelFinder.SmoothingMethod.Value = WaveletBasedPeakelFinder.SmoothingMethod.SWT, //smoothing method could be 
                     winLength: Int = 5,
                     ridgeMethod: String = "maxima",
                     minRidgeLength: Int = maxScale / 4,
                     minSNR: Float = 0.3f,
                     sizeNoise: Int = ydata.length / 20,
                     skipBoundaries: Int = ydata.length / 20): Array[CwtPeakel] = {

    var y_data: Array[Double] = null
    var swtSmoothing = false
    if (smooth == WaveletBasedPeakelFinder.SmoothingMethod.SWT) {
      var coefficients = WaveletUtils.swt(ydata.toArray[Double], 5)
      WaveletUtils.denoiseSoft(coefficients)
      y_data = WaveletUtils.iswt(coefficients)
      swtSmoothing = true
    } else if (smooth == WaveletBasedPeakelFinder.SmoothingMethod.SG) {
      y_data = smoothSignal(ydata.toArray, 3)
    } else {
      y_data = ydata.toArray
    } 

    val maxima = _findMaximaRegion(coeffs, 10)
    val (ridges, orphanRidges) = findRidges(maxima, coeffs, winLength, 4)
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


