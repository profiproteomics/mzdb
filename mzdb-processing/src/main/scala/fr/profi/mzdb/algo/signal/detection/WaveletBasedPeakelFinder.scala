package fr.profi.mzdb.algo.signal.detection

import java.awt.Color
import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.chart.renderer.GrayPaintScale
import org.jfree.chart.ChartUtilities
import org.jfree.chart.JFreeChart
import org.jfree.data.xy.DefaultXYZDataset
import fr.profi.mzdb.utils.math.cwt.MexicanHat
import fr.profi.mzdb.utils.math.cwt.MotherWavelet
import fr.profi.mzdb.utils.math.cwt.cwt
import fr.profi.mzdb.utils.math.cwt.swt
import fr.profi.mzdb.model.Peak


/**
 * result of the algorithm
 */
case class CwtPeakel (val apex: Int, 
					  val scanID:Int,
					  val minIdx:Int, 
					  val minScanID: Int, 
					  val maxIdx: Int, 
					  val maxScanID:Int, 
					  val snr: Float) {}






trait RidgesFinder {
  
  /**
   * FindRidges : find Ridge among cwt coefficients start from biggest scale
   * find local maxima then go up among scale finding local maximum in a given
   * window
   *
   * winLength: minWindow window is proportionnal to the scale: scale * 2 + 1
   */
  
  def findRidges(maximaIndexesPerScale: Array[Array[Int]], winLength: Int = 5): Array[Ridge] = {

    //var maximaIndexesPerScale = findMaxima3() //seems to have better performance than zerocrossing lines

    var lastMaximaRow = maximaIndexesPerScale.last
    var ridges = new ArrayBuffer[Ridge]
    //println("lastmaximaRow length", lastMaximaRow.length)
    for (m <- lastMaximaRow) { //.sortBy(x=> coeffs(coeffs.length - 1)(x)).slice(coeffs.length -50, coeffs.length - 1)) {
      var r = Ridge()
      //println("m" + m)
      r.add(maximaIndexesPerScale.length - 1, Some(m))
      ridges += r
    }
    
    for (i <- maximaIndexesPerScale.length - 2 to 0 by -1) {
      var currentRow = maximaIndexesPerScale(i)
      var winSize: Int = if ((i * 2 + 1).toInt > winLength) (i * 2 + 1).toInt else winLength
      //println("winsize: " + winSize)
      var treatedIdx = new ArrayBuffer[Int]()

      for (ridge <- ridges if !ridge.isEnded()) {
        var prevMaxIndex = ridge.maximaIndexPerScale(i + 1)

        if (prevMaxIndex == None) {
          var k = i + 2
          while (ridge.maximaIndexPerScale(k) == None)
            k += 1
          prevMaxIndex = ridge.maximaIndexPerScale(k)
        }

        var closestMax = currentRow.minBy(x => math.abs(prevMaxIndex.get - x)) //sortBy(x => math.abs(prevMaxIndex.get - x))

        if (math.abs(prevMaxIndex.get - closestMax) < winSize) { // /2
          ridge.add(i, Some(closestMax))
          ridge.initGap()
          treatedIdx += closestMax

        } else {
          ridge.incGap()
          ridge.add(i, None)
        }

      }

      for (maxIdx <- currentRow if !treatedIdx.contains(maxIdx)) {
        var r_ = Ridge()
        r_.add(i, Some(maxIdx))
        ridges += r_
        //println("found treated peak")
      }
    }
    //remove no good ridgeLines
    ridges = ridges.filter(x => !x.isEnded && x.startLevel > 2) //filer ridge line with gap >= maxAllowedGap and ridge starting at level <= 2

    //remove different ridge lines with the same apex take the longest

    var ridgesPerMaxIndexAtFirstScale = HashMap[Int, ArrayBuffer[Ridge]]()
    ridges.foreach { r => ridgesPerMaxIndexAtFirstScale.getOrElseUpdate(r.maxIdxAtFirstScale._2, new ArrayBuffer[Ridge]()) += r }
    var lastOfLastRidges = new ArrayBuffer[Ridge]()

    ridgesPerMaxIndexAtFirstScale.foreach { case (u, v) => lastOfLastRidges += v.maxBy { x => x.length() } }

    lastOfLastRidges.toArray[Ridge]
  }
}



object WaveletBasedPeakelFinder extends RidgesFinder {
  
}


/**
 * Peak Width are 2 * scale where scale is the maxima point on the ridge
 */
class WaveletBasedPeakelFinder(peaks : Seq[Peak],
                               var scales: Array[Float] = (1f to 64f by 1f).toArray[Float], 
                               var wavelet: MotherWavelet = MexicanHat()) { //((1f to 1f by 1f) ++ (2f to 30f by 2f) ++ (32f to 64f by 4f)).toArray[Float]){//

  //by default MexicanHat is used since it has better results
  var ydata = peaks.map(_.getIntensity().toDouble)
  var coeffs: Array[Array[Double]] = cwt.transform(ydata.toArray, wavelet, scales.toArray)

  def computeCoeffs() = {
    coeffs = cwt.transform(ydata.toArray, MexicanHat(), scales.toArray)
  }

  def apply(): Array[Array[Double]] = {
    coeffs
  }

  def energyDensity(): Array[Array[Double]] = {
    throw new Error("Not yet implemented")
  }

  /**
   * tried to implement original implementation @see MassSpecWavelet
   *
   */
  def findMaxima(winLength: Int = 5): Array[Array[Int]] = {

    var maximaIndexesPerScale = new ArrayBuffer[ArrayBuffer[Int]]

    for (scale <- 0 until coeffs.length) {
      var windowSize = (scale + 1) * 2 toInt
      var indexes = new ArrayBuffer[Int]

      for (coeffArray <- coeffs(scale).sliding(windowSize, windowSize)) {
        var maxval = coeffArray.max //[Double]
        if (maxval > coeffs(scale)(0) && maxval > coeffs(scale)(windowSize) && maxval != 0) {
          var index = coeffs(scale).indexOf(maxval)
          indexes += index
        }
      }
      //shift
      for (coeffArray <- coeffs(scale).slice(windowSize / 2, coeffs(scale).length).sliding(windowSize, windowSize)) {
        var maxval = coeffArray.max
        if (maxval > coeffs(scale)(0) && maxval > coeffs(scale)(windowSize) && maxval != 0) {
          var index = coeffs(scale).indexOf(maxval)
          indexes += index
        }
      }
      var newIndexes = new ArrayBuffer[Int]
      (indexes.sortBy(x => x)).sliding(2).foreach { arr =>
        println(arr(0), arr(1))
        if (math.abs(arr(0) - arr(1)) < windowSize) {
          var i = 0
          if (ydata(arr(0)) > ydata(arr(1)))
            i = arr(0)
          else
            i = arr(1)
          indexes += i
        } else {
          indexes += arr(0)
          indexes += arr(1)
        }
      }
      maximaIndexesPerScale += indexes
    }
    maximaIndexesPerScale.map(x => x.toArray[Int]).toArray[Array[Int]]
  }

  /**
   * try to find all maxima using kind of derivative approach
   */
  def findMaxima3(): Array[Array[Int]] = {
    var maximaIndexesPerScale = new ArrayBuffer[ArrayBuffer[Int]]

    for (coeff <- coeffs) {
      /*
      val m = coeff.max[Double] * 0.1 / 100f
      var coef = new ArrayBuffer[Double]
      for (i <- 0 until coeff.length) {
        if (coeff(i) >= m)
          coef += coeff(i)
          else
            coef += 0
      }*/

      var maxima = new ArrayBuffer[Int]
      var apexIndex = 0;
      var peakHasBeenInserted = false;

      //if (s->defaultArrayLength == 0)
      //return results;

      for (i <- 0 until coeff.length - 1) {
        if (coeff(i) > 0) {
          if (coeff(i + 1) >= coeff(i)) {
            peakHasBeenInserted = false
            //if (peakHasBeenInserted) {
            //  maxima += apexIndex
            //  peakHasBeenInserted = false;
            //}

          }
          //setting one peak
          //apexIndex = i;

          if (coeff(i + 1) < coeff(i)) { //&& intensities->data[i+1] > _threshold) {
            //apexIndex = i;
            if (!peakHasBeenInserted) {
              maxima += i
              peakHasBeenInserted = true;
            }
          }
        }
      }
      maximaIndexesPerScale += maxima
    }
    maximaIndexesPerScale.map(x => x.toArray[Int]).toArray[Array[Int]]
  }

  

  /**
   * The CWT is performed on 32 scales (1 to 32) it would be better to
   * do 1 to 64 by step of 2
   * winLength: min Window acceptable
   * minRidgeLength : default mid 16 general half of scale number
   * scaleRange : maxCWT coeff must be in scale > 5
   * SNR = 3 by default could be less ?
   */
  def ridgeToPeaks(ridges: Array[Ridge],
    minRidgeLength: Int = 15,
    minSNR: Float = 2f,
    sizeNoise: Int = 500,
    skipBoundaries: Int = 100): Array[CwtPeakel] = { // scaleRange: Pair[Int, Int] = (0, 64), not relevant parameter

    //var peakels = new ArrayBuffer[Peakel]
    //var ridges = findRidges(winLength)
    //println("len ridges: " + ridges.length)

    //val sizeNoise = 500
    //val skipBoundaries = 100

    /*
    var values = new ArrayBuffer[Double]
    for (i <- 0 until ydata.length)
      values += 0
    */
    //var indexes = new ArrayBuffer[Pair[Int, Int]]
    var peakels = new ArrayBuffer[CwtPeakel]		
    
    for (ridge <- ridges if ridge.length() >= minRidgeLength) {
      //println(ridge.length)
      var v = ridge.maxIdxAtFirstScale
      //println("***" + v._2)
      var t = ridge.maxIndex
      var maxScale = t._1
      var maxCwtIdx = t._2
      if (v._2 > skipBoundaries && v._2 < ydata.length - skipBoundaries) {
        var centroidValue = math.abs(coeffs(maxScale)(maxCwtIdx))
        var snrCoeffs = coeffs(0).map(math.abs(_)).slice(math.max(v._2 - sizeNoise, 0), math.min(v._2 + sizeNoise, ydata.length - 1)).toList.sortBy(x => x)
        var noiseValue = math.max(snrCoeffs((0.005 * snrCoeffs.length) toInt), 0.001)
        var estimatedSNR = centroidValue / noiseValue

        if (estimatedSNR > minSNR) {
          var i = findLocalMinima(scale = maxScale, maxCwtIdx)
         
          peakels += CwtPeakel(apex= v._2, 
              				   scanID = peaks(v._2).getLcContext.getScanId,
              				   minIdx = i._1, 
              				   minScanID = peaks(i._1).getLcContext.getScanId, 
              				   maxIdx = i._2, 
              				   maxScanID = peaks(i._2).getLcContext.getScanId, 
              				   snr = estimatedSNR.toFloat)
          //indexes += i
          //DEBUG values(v._2) = ydata(v._2)
          //DEBUG println("apex:" + v._2 + " maxCoefficentIdx: " + t._2 + " ridgeLength: " + ridge.length + " SNR:" + estimatedSNR + " begin:"+indexes._1 + " end:"+indexes._2 + " peakValue:" + coeffs(maxScale)(maxCwtIdx))
        }
      }
    }
    /*DEBUG
    for (v <- values)
      print(v + "\t")
    println
    */

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
    while (i > 0 && currVal > prevVal) {
      i -= 1
      currVal = prevVal
      prevVal = coeffs(scale)(i - 1)
    }
    while (i > 0 && prevVal < currVal) {
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
    while (i < coeffs(scale).length - 1 && currVal < nextVal) {
      i += 1
      currVal = nextVal
      nextVal = coeffs(scale)(i + 1)
    }
    while (i < coeffs(scale).length - 1 && currVal > nextVal) {
      i += 1
      currVal = nextVal
      nextVal = coeffs(scale)(i + 1)
    }
    var lastIdx = i - 1

    (minIdx, lastIdx)
  }

  //***********************************************************************************
  //Plotting functions
  //***********************************************************************************
  def coeffToImage(file: String = "coeffs.jpg") = {
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

  def maximaToImage(file: String = "maxima.jpg") = {
    var maxima = findMaxima3()
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
  def zeroCrossingsLines(): Array[Array[Int]] = {
    var zeroCross = new ArrayBuffer[ArrayBuffer[Int]]
    for (coeff <- coeffs) {
      var scaleZeroCross = new ArrayBuffer[Int]
      var counter = 0 //take the first index ?
      coeff.sliding(2).foreach { x =>
        if (swt.sign[Double](x(0)) != swt.sign[Double](x(1))) {
          scaleZeroCross += counter
        }
        counter += 1
      } //end foreach
      zeroCross += scaleZeroCross
    }
    zeroCross.map(x => x.toArray[Int]).toArray[Array[Int]]
  }

  /**
   *
   */
  def findPeakelsIndexes(smooth: String = "swt", //smoothing method could be 
                         winLength: Int = 5,
                         ridgeMethod: String = "maxima",
                         minRidgeLength: Int = 15,
                         minSNR: Float = 2f,
                         sizeNoise:Int = 200,
                         skipBoundaries:Int = (ydata.length * 2.5 / 100).toInt): Array[Pair[Int, Int]] = {

    val peakels = findCwtPeakels(smooth, winLength, ridgeMethod, minRidgeLength, minSNR, sizeNoise, skipBoundaries)
    peakels.map(x=> (x.minIdx, x.maxIdx))
  }
  
  
  
  def findCwtPeakels(smooth: String = "swt", //smoothing method could be 
                         winLength: Int = 5,
                         ridgeMethod: String = "maxima",
                         minRidgeLength: Int = 15,
                         minSNR: Float = 2f,
                         sizeNoise:Int = 200,
                         skipBoundaries:Int = (ydata.length * 2.5 / 100).toInt) : Array[CwtPeakel] = {

    var y_data: Array[Double] = null
    var swtSmoothing = false
    if (smooth == "swt") {
      var coefficients = swt.transform(ydata.toArray[Double], 5)
      swt.denoiseSoft(coefficients)
      y_data = swt.inverseTransform(coefficients)
      swtSmoothing = true
    } else if (smooth == "sg")
      y_data = cwt.smoothSignal(ydata.toArray, 3)
    else
      y_data = ydata.toArray
    
    
    val maxima = findMaxima3()
    val ridges = WaveletBasedPeakelFinder.findRidges(maxima, winLength)
    val peakels = ridgeToPeaks(ridges, 
    						   minRidgeLength = minRidgeLength, 
    						   minSNR = minSNR, 
    						   sizeNoise = sizeNoise, 
    						   skipBoundaries= skipBoundaries)
    peakels
  }
  
  
  
  

}

/**
 * class for modelize Ridge
 * internal map storing scale for key  and maxIdx of cwt coefficient
 * at the consdidered scale
 */
case class Ridge(var gap: Int = 0) {
  var maximaIndexPerScale = HashMap[Int, Option[Int]]()
  val maxGap = 3

  def isEnded() = {
    gap == maxGap //|| maximaIndexPerScale.get(0) != None 
  }

  /**
   * in theory the peak centroid
   */
  lazy val maxIndex: Pair[Int, Int] = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._2.get)
    (v._1, v._2.get)
  }

  /**
   * in theory the peak apex
   */
  lazy val maxIdxAtFirstScale: Pair[Int, Int] = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).minBy(x => x._1)
    (v._1, v._2.get)
  }
  
  
  lazy val maxIdxAtLastScale: Pair[Int, Int] = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._1)
    (v._1, v._2.get)
  }

  def add(scaleIdx: Int, maximaIdx: Option[Int]) = {
    maximaIndexPerScale(scaleIdx) = maximaIdx
  }

  def hasScale(scaleIdx: Int): Boolean = {
    maximaIndexPerScale.get(scaleIdx) == None
  }

  def get(scaleIdx: Int): Option[Int] = {
    //if (! hasScale(scaleIdx))
    //  throw new Exception("Error, never supposed to happen")
    maximaIndexPerScale.get(scaleIdx).get
  }

  def incGap() = {
    gap += 1
  }

  def initGap() = {
    gap = 0
  }

  def length(): Int = {
    maximaIndexPerScale.size
  }

  def startLevel(): Int = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._1)
    v._1
  }

  lazy val totalGaps: Int = {
    var c = 0
    maximaIndexPerScale.values.foreach(x => if (x == None) c += 1)
    c
  }

}
