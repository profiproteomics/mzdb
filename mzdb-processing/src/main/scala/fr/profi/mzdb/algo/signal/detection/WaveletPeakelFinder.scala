package fr.profi.mzdb.algo.signal.detection

import java.awt.Color

import scala.Array.canBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._

import org.jfree.chart.ChartUtilities
import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.LookupPaintScale
import org.jfree.chart.renderer.xy.XYBlockRenderer
import org.jfree.data.xy.DefaultXYZDataset

import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.utils.math.wavelet._


/** Peak Width are ~= 2 * scale where scale is the maxima point on the ridge */
abstract class AbstractWaveletPeakelFinder(peaks: Seq[Peak]) extends IWaveletDetectionMethod with Logging {
  
  require(peaks != null && !peaks.isEmpty, "provided peaks array is empty or null")
  //var ydata: Array[Double] = null
  var coeffs : HashMap[Float, Array[Float]] = null
  /*automatic parameters, could be changed by users */
  var minScale : Float = -1f
  var maxScale : Float = -1f  
  /*baseline and noise estimation, based on CentWave*/
  var baseline :Double = -1
  var noise :Double = -1
    
  var cwtParams : CwtParameters = null
  var ridgeFilteringParams: RidgeFilteringParameters = null
  var maximaParams = MaximaFinder()
  
  var ydata = peaks.map(_.getIntensity).toArray//data //zeroPadData(data, n= 50)
  
  this.setCwtParametersParams()
  this.setRidgesFilteringParams()
  
  /** */
  def zeroPadData(ydata: Array[Double], n: Int = 100) : Array[Double] = {
    val ydat = ydata.toBuffer
    for ( i <- 0 until n) {
      ydat.+=:(1d)
      ydat.+=(1d)
    }
    return ydat.toArray
  }
  
  /** compute the coefficients */
  def computeCoeffs() {
     coeffs = WaveletUtils.cwt(this.ydata, this.cwtParams.wavelet, this.cwtParams.scales)
     //this.coeffToImage(coeffs)
  }
 
  /** A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008 */
  protected def _findMaximaWee(coeffs: HashMap[Float, Array[Double]], minWinSize: Int = 5): HashMap[Float, Array[Int]] = {
    val maximaIndexesPerScale = new HashMap[Float, Array[Int]]
    coeffs.foreach{  case (scale, coeffsRow) =>
      //not going to use it ?
      val winsize = math.max(minWinSize, scale * 2 + 1).toInt
      
      val localMax = new ArrayBuffer[Int]
      var coeffCloned = coeffsRow.clone
      while (coeffCloned.length != 0) {
        val maxi = coeffCloned.max
        val maxIdxToCoeff = coeffsRow.indexOf(maxi)
        val maxIdxToCoeffCloned = coeffCloned.indexOf(maxi)
        localMax += maxIdxToCoeff
        val (inf, sup) = (math.max(0, maxIdxToCoeffCloned - winsize / 2), math.min(coeffCloned.length, maxIdxToCoeffCloned + winsize / 2))
        coeffCloned = coeffCloned.diff(coeffCloned.slice(inf, sup))
      }
      maximaIndexesPerScale(scale) = localMax toArray
    }
    maximaIndexesPerScale
  }

  /*protected def _findMaximaNaive(win: Int = 0): HashMap[Float, Array[Int]] = {
    val indexes = new HashMap[Float, Array[Int]]
    this.coeffs.foreach {
      case (scale, coeffsRow) =>
        val maxs = new ArrayBuffer[Int]
        for (i <- 0 until coeffsRow.length) {
          if (i == 0) {
            if (coeffsRow(i + 1) < coeffsRow(i))
              maxs += i;
          } else if (i == coeffsRow.length - 1) {
            if (coeffsRow(i - 1) < coeffsRow(i))
              maxs += i;
          } else {
            if (coeffsRow(i - 1) < coeffsRow(i) && coeffsRow(i + 1) < coeffsRow(i))
              maxs += i;
          }
        }
        indexes += (scale -> maxs.toArray)
    }
    indexes
  }*/

   /* find minima by inversing wavelet coefficient,
   *  A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   **/
  protected def _findMinimaWee(coeffs: HashMap[Float, Array[Double]], winSize: Int = 5): HashMap[Float, Array[Int]] = {
    val inverseCoeffs = coeffs map { case ( s, c) => 
        s-> c.map { y => -y } }
    this._findMaximaWee(inverseCoeffs, winSize)
  }
  
   /* pair a maxima Ridge to a minima Ridge, to be used with the gaussian first derivative
   *  A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   
  protected def _pairMaximaMinimaRidges( maximaRidges:Array[Ridge], minimaRidges: Array[Ridge], winSize: Int = 5) : Array[Pair[Ridge, Ridge]] = {
    //idify ridges very ugly stuff, better reimplement equals or hashcode in ridge
    for (i <- 0 until maximaRidges.length) {
      maximaRidges(i).id = i + 1
    }
    var i = maximaRidges.length
    for (j <- 0 until minimaRidges.length) {
      minimaRidges(j).id = i
      i+=1
    }
    
    val result = new ArrayBuffer[Pair[Ridge, Ridge]]
    var alreadyTaken = Set[Int]() //will contains minimaRidge associated to a maxima ridge
    for (maximaRidge <- maximaRidges) {//i <- 0 until maximaRidges.length) {
      //val maximaRidge = maximaRidges(i)
      val sortedRidges = minimaRidges.filter{x=> 
                                              val diff = x.firstScaleMaxCoeffPos._2 - maximaRidge.firstScaleMaxCoeffPos._2 //index difference
                                              diff > 0 && diff < winSize * 2 //filter ridges where min is on the left of max && filter the ridge to far away
                                            }
      
      if ( sortedRidges.length > 1) {
        //group by length
        val ridgesByLength = sortedRidges.groupBy(_.length)
        //take the group with closest length
        val keys =  ridgesByLength.keys.toBuffer.sortBy[Int]( x => math.abs(x - maximaRidge.length))
        var closestLength = keys.head
        var i = 0
        while ( ridgesByLength(closestLength).isEmpty && i < keys.length - 1) {
          i+=1
          closestLength = keys(i)
        }
        val closestRidgesInLength = ridgesByLength(closestLength)//weird needed sortBy[Int]
        val bestPair = closestRidgesInLength.sortBy(_.maxCoeffPos._3).last //the best pair the highest PeakValue
        if (! alreadyTaken.contains(bestPair.id)) {
          //does not contain pair yet
          bestPair.associatedRidge = Some(maximaRidge)
          alreadyTaken += bestPair.id
          result += Pair(maximaRidge, bestPair)
        } else {
          //resolve conflict, already contains  bestPair
          if (maximaRidge.length > bestPair.associatedRidge.get.length || maximaRidge.maxCoeffPos._1 > bestPair.associatedRidge.get.maxCoeffPos._1) {
          //if (math.abs(bestPair.length - maximaRidge.length) < math.abs(bestPair.length - bestPair.associatedRidge.get.length)) {
            bestPair.associatedRidge = Some(maximaRidge)
            result += Pair(maximaRidge, bestPair)
          }
        }
      } else if (sortedRidges.length == 1) {
      
        if (! alreadyTaken.contains(sortedRidges(0).id)) {
          sortedRidges(0).associatedRidge = Some(maximaRidge)
          alreadyTaken += sortedRidges(0).id
          result += Pair(maximaRidge, sortedRidges(0))
        } else {
            if (maximaRidge.length > sortedRidges(0).associatedRidge.get.length || maximaRidge.maxCoeffPos._1 > sortedRidges(0).associatedRidge.get.maxCoeffPos._1) {
          //if (math.abs(bestPair.length - maximaRidge.length) < math.abs(bestPair.length - bestPair.associatedRidge.get.length)) {
              sortedRidges(0).associatedRidge = Some(maximaRidge)
              result += Pair(maximaRidge, sortedRidges(0))
            }        
        }
      } else {
        //do nothing does not contribute to a peak
      }
    } 
    result.toArray
  }*/
  



  /**
   * find local minima: basically to get the end of the peak
   * go the the left then to the right
   * since apex
   */
  protected def _findLocalMinima(scale:Float = 0, maxIdx :Int): Option[Pair[Int, Int]] = {    
    val coeffsAtScale = this.coeffs(scale)
    
    if (coeffsAtScale.isEmpty)
      return Option.empty[Pair[Int, Int]]
    
    var leftIndex = maxIdx//math.max(maxIdx - 1, 0)
    breakable {
      for (i <- leftIndex until 0 by -1) {
        if ( coeffsAtScale(i-1) > coeffsAtScale(i) ) {
          leftIndex = i
          break
        }
      }
    }
    
    var rightIndex = maxIdx//math.min(maxIdx + 1, coeffsAtScale.length - 1)
    breakable {
      for (i <- rightIndex until coeffsAtScale.length - 1) {
        if ( coeffsAtScale(i+1) > coeffsAtScale(i)) {
          rightIndex = i
          break
        }
      }
    }
    if (leftIndex != rightIndex) Some(Pair(leftIndex, rightIndex)) else Option.empty[Pair[Int, Int]]
  }
  
  
  /**
   * taken from centwave algorithm
   */
  def _findPeakBoundariesXCMS(scale:Float, maxIndex:Int) : Option[Pair[Int, Int]] = {
    val coeffsAtScale = this.coeffs(scale)
    //println("scale:" + scale+ ", len ydata :" + this.ydata.length + ", len coeffs:" + coeffsAtScale.length )
    val lwpos = math.max(0, maxIndex - scale)
    val rwpos = math.min( maxIndex + scale, coeffsAtScale.length - 1)
    
    var l = lwpos.toInt
    var r = rwpos.toInt
    var outl = 0
    var vpos = 0
    var opos = 0
    while( (l > 1) && this.ydata(l) > 0 && outl <= 7 ) {
      if (outl > 0) vpos = opos else vpos = l
      if (this.ydata(l-1) > this.ydata(l)) outl +=1 else outl = 0
      if (outl == 1) opos = l
      l-=1
    }
    if (outl > 0) l = l + outl
    
    outl = 0
    vpos = 0
    opos = 0
    while( (r < this.ydata.length - 1) && this.ydata(r) > 0 && outl <= (15/2).toInt ) {
      if (outl > 0) vpos = opos else vpos = l
      if (this.ydata(r+1) > this.ydata(r)) outl +=1 else outl = 0
      if (outl == 1) opos = r
      r+=1
    }
    if (outl > 0) r = r - outl
    
    Some((l, r))
    
  }
  
   /**
   * find local minima: basically to get the end of the peak
   * go the the left then to the right
   * since apex
   */
  protected def _findLocalMinimaRealSpaceFromWaveletBounds(minIdx: Int, maxIdx: Int): Pair[Int, Int] = {    
    var leftIndex = minIdx//math.max(maxIdx - 1, 0)
    breakable {
      for (i <- leftIndex until 0 by -1) {
        if ( this.ydata(i-1) > this.ydata(i) ) {
          leftIndex = i
          break
        }
      }
    }
    
    var rightIndex = maxIdx//math.min(maxIdx + 1, this.ydata.length - 1)
    breakable {
      for (i <- rightIndex until this.ydata.length - 1) {
        if ( this.ydata(i+1) > this.ydata(i)) {
          rightIndex = i
          break
        }
      }
    }
    Pair(leftIndex, rightIndex)
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

  
   /** first method using the first derivative of gaussian as wavelet*/
  /*protected def _ridgeToPeaks(ridges: Array[Pair[Ridge, Ridge]]): Array[CwtPeakel] = {
    //TODO test if we can remove this filter
    val (minRidgeLength, sizeNoise, minSNR) = (ridgeFilteringParams.minRidgeLength, ridgeFilteringParams.sizeNoise, ridgeFilteringParams.minSNR)
    var validPairs = ridges.filter { case (a, b) => (! a.isEnded(maxGap=6))  && a.length >= minRidgeLength && (! b.isEnded(maxGap=6)) && b.length >= minRidgeLength }
    //logger.warn("validPairs first filter (isEnded):" + validPairs.length)
    
    
    //compute SNR thresholds on both
    validPairs.foreach { case( a, b) =>
       //_computeSNR(a, scale=0, sizeNoise=sizeNoise)
       //_computeSNR(b, scale=0, sizeNoise=sizeNoise)
      
    }
    
    //not sur if the second element need to be > to minSNR since it is already a minimum
    //validPairs = validPairs.filter{ case (a, b) => a.SNR >= minSNR && math.abs(b.SNR) >= minSNR}
    
    //logger.warn("validPairs second filter (SNR):" + validPairs.length)

    
    var peakels = new ArrayBuffer[CwtPeakel]() ++ validPairs.map{ pair => 
      val (minScale, maxIdxAtFirstScale, maxValueAtFirstScale) = pair._1.maxCoeffPos; //correspond to the max ridge
      val (minScale_, minIdxAtFirstScale, minValueAtFirstScale) = pair._2.maxCoeffPos //to the min ridge 
      //println(""+ maxIdxAtFirstScale + "\t" + minIdxAtFirstScale)
      if (maxIdxAtFirstScale !=0 && minIdxAtFirstScale != 0) {
        val slicedPeaks = peaks.slice(maxIdxAtFirstScale, minIdxAtFirstScale)
        val intensities = slicedPeaks.map(_.getIntensity)
        val maxIndex = intensities.indexOf(intensities.max)
        
        val xvalues = slicedPeaks.map(_.getLcContext.getElutionTime)
        val b = (!xvalues.isEmpty && !intensities.isEmpty)
        var (centroid, intensityMax) = (0f,0f)
        if (b) {
          centroid = xvalues.zip(intensities).map { case (x, y) => x * y }.reduceLeft(_ + _) / intensities.reduceLeft(_ + _)
          intensityMax = intensities.max
        }
        
        //we may need to refined min and max indexes TODO
        CwtPeakel(index = intensities.indexOf(intensities.max),
                  peaks = slicedPeaks.map(Some(_)) toArray,
                  apexLcContext = peaks(maxIndex).getLcContext,
                  minIdx = maxIdxAtFirstScale, 
                  startLcContext = peaks(maxIdxAtFirstScale).getLcContext, 
                  maxIdx = minIdxAtFirstScale, 
                  endLcContext = peaks(minIdxAtFirstScale).getLcContext, 
                  xMax = xvalues(maxIndex) toFloat,
                  intensityMax = intensityMax toFloat,
                  centroid = centroid toFloat,
                  snr = (pair._1.SNR + pair._2.SNR) / 2f)
      } else
        null
    }.filter(_ != null)
    
    //// filter the two small peakel
    //peakels = peakels.filter{ x=> math.abs(x.endLcContext.getElutionTime - x.startLcContext.getElutionTime) >= ridgeFilteringParams.minPeakWidth} 
    
    // remove a detected peakel which is contained in another peakel
    peakels = _filterOverlappingPeakels(peakels); 
    
    //fill peaks data
    peakels.toArray
  }*/
  
  protected def _filterOverlappingPeakels(peakels: Seq[CwtPeakel], mergePeakels: Boolean = false) : Array[CwtPeakel] = {
    val overlappingPeakelsByMasterPeakel = new HashMap[CwtPeakel, ArrayBuffer[CwtPeakel]]
    for (i <- 0 until peakels.length ) {
      val peakel = peakels(i)
      for (j <- 0 until peakels.length) {
        if (i != j) {
          val otherPeakel = peakels(j)
          //complete overlapping peakels
          if ( (otherPeakel.startLcContext.getElutionTime >= peakel.startLcContext.getElutionTime && 
                otherPeakel.endLcContext.getElutionTime <= peakel.endLcContext.getElutionTime) ){
            //println("hey found a bad guy over here")
            overlappingPeakelsByMasterPeakel.getOrElseUpdate(peakel, new ArrayBuffer[CwtPeakel]) += otherPeakel
          }      
        }
      }
    }
    val v = overlappingPeakelsByMasterPeakel.map{ case (peakel, badPeakels) => badPeakels }
                                            .toArray
                                            .flatten
                                            .toSet
    peakels.toSet.diff(v).toArray                                            
  }
  
  /** to be used after filtering peakels */
  protected def _mergeOverlappingPeakels(peakels: Seq[CwtPeakel] ) : Array[CwtPeakel] = {
    /** */
    def merge(p1: CwtPeakel, p2: CwtPeakel): CwtPeakel = {
      val minIdx = math.min(p1.minIdx, p2.minIdx)
      val maxIdx = math.max(p1.maxIdx, p2.maxIdx)
      val apexIndex = if (p1.intensityMax > p2.intensityMax) p1.apexIndex else p2.apexIndex
      val peakApex = this.peaks(apexIndex)
      
      new CwtPeakel(
        peaks = peaks.toArray,
        apexIndex = apexIndex, //index on the original ydata
        apexLcContext = peakApex.getLcContext(),
        minIdx = minIdx,
        startLcContext = peaks.head.getLcContext(),
        maxIdx = maxIdx,
        endLcContext = peaks.last.getLcContext(),
        xMax = peakApex.getLcContext().getElutionTime,
        intensityMax = peakApex.getIntensity(), //
        centroid = peakApex.getMz().toFloat,  
        snr = Array(p1, p2).map(_.snr).sum / 2
      )
    }
    
    var sortedPeakelsByMinTime = peakels.sortBy(_.getFirstLcContext().getElutionTime).toBuffer
    var i = 0
    var N = sortedPeakelsByMinTime.length
    breakable {
      while (i < N - 1) {
        val (p1, p2) = (sortedPeakelsByMinTime(i), sortedPeakelsByMinTime(i + 1))
        val (minrt, maxrt) = (p1.startLcContext.getElutionTime, p1.endLcContext.getElutionTime)
        val (minrt_, maxrt_) = (p2.startLcContext.getElutionTime, p2.endLcContext.getElutionTime)
        
        // Test inclusion
        if (minrt_ > minrt && minrt_ < maxrt && maxrt_ > maxrt) {
          val merged = merge(p1, p2)
          // remove both front
          for (i <- 0 to 1) sortedPeakelsByMinTime.remove(0)
          sortedPeakelsByMinTime.prepend(merged)
          N -= 1
          sortedPeakelsByMinTime = sortedPeakelsByMinTime.sortBy(_.getFirstLcContext().getElutionTime)
          i = 0
        }
        i += 1
      }
    }
    sortedPeakelsByMinTime.toArray
  }

  
  /**
   * return the indexes of the peakel found
   */
  def findPeakelsIndexes(asScanId:Boolean=false): Array[Pair[Int, Int]] = {

    val peakels = this.findCwtPeakels()
    if (! asScanId)
      return peakels.map(x => (x.minIdx, x.maxIdx))
    peakels.map(x=> (x.startLcContext.getScanId(), x.endLcContext.getScanId()))
  }
  
  /**
   * percentile approach to determine the noise
   */
  private def _noiseValue(value:Float = 1f/3) :Float = {
    val ydata = this.ydata.sortBy(x=>x)
    ydata((value * ydata.length).toInt).toFloat
  }

  /** return the peakels */
  def findCwtPeakels(): Array[CwtPeakel] // = {
    /*check all the parameters*/
    //logger.debug("Detected CutOff:" + cutOff)
    
    /*dont know what the following code is doing*/
    /*var (min_, max_) = (0f,0f)
    if (cwtParams.scales.min != ridgeFilteringParams.minPeakWidth)
      min_ = math.max(0,ridgeFilteringParams.minPeakWidth - 2)
      
    if (cwtParams.scales.max != ridgeFilteringParams.maxPeakWidth)
      max_ = ridgeFilteringParams.maxPeakWidth + 2
    
    if (min_ != 0 || max_ != 0 )
      cwtParams.scales = (min_ to max_ by 1f).toArray[Float]*/
     
    
    //var y_data: Array[Double] = null
    
    //Preprocessing and cwt transform
    /*cwtParams.smooth match {
      case SWT => 
        try {
        y_data = SmoothingUtils.smooth(new UWTSmoother(ydata))
        } catch {
          case e :Exception => 
            logger.warn("SWT smooth failed, fall back mode: SG smoothing\n");
            y_data = SmoothingUtils.smooth(new SGSmoother(ydata))
        }
      case SG => y_data = SmoothingUtils.smooth(new SGSmoother(ydata))//, new scala.collection.immutable.HashMap[String, Float]())
      case None => y_data = ydata
    }
    logger.debug("Smooth ended")*/
    
    
    
    
    
//    computeCoeffs //compute coeffs with cwtParameters
//    //logger.debug("CWT ended")
//    
//    method match {
//      case Du =>
//        /*check we have the good wavelet for performing the Du method*/
//        if (! cwtParams.wavelet.isInstanceOf[MexicanHat]) {
//          println("Du method was selectionned, setting wavelet to MexicanHat (it was a GaussianFirstDerivative)")
//          cwtParams.wavelet =  MexicanHat()
//        }
//        
//        val maximaIndexesByScale = this._findMaximaNaive(0)//this._findMaxima(1) //
//        //maxima.foreach(x=> x.foreach(println(_)))
//        //this.maximaToImage(this._findMaximaNaive)
//        val (ridges, orphanRidges) = this.findRidges(maximaIndexesByScale, winLength = 5, maxGap = 3) //maximaParams.winLength, maxGap = 4)
//        val peakels = this._ridgeToPeaks(ridges)
//        peakels
//      
//        
//        /*case Coombes =>
//        //check we have the good wavelet for performing the Du method
//        if (! cwtParams.wavelet.isInstanceOf[GaussianFirstDerivative]) {
//          println("Setting wavelet to GaussianFirstDerivative")
//          cwtParams.wavelet =  GaussianFirstDerivative()
//        }
//        
//        //launch 2 threads
//        val (maxima, toDel) = findRidges(this._findMaximaRegion(coeffs, 5), coeffs, maximaParams.winLength, maxGap = 6)
//        //logger.debug("Count maximaRidges:" + maxima.length)
//        
//        val invCoeffs = coeffs.map(array => array.map(-_))
//        val (minima, toDel_) = findRidges(_findMinimaRegion(coeffs, 5), invCoeffs, maximaParams.winLength, maxGap = 6)
//        //logger.debug("Count minimaRidges:" + minima.length)
//        val pairs =  _pairMaximaMinimaRidges(maxima, minima, 10)
//        //logger.debug("Count pairs:" + pairs.length)
//
//        val peakels = _ridgeToPeaks(pairs).distinct.filter(_.intensityMax > cutOff)
//        peakels*/
//    //}
//      
//  }

  /**
   * *********************************************************************************
   * //Plotting functions
   * *********************************************************************************
   */
  def coeffToImage(coeffs: Array[Array[Double]], file: String = "coeffs.jpg") = {
    val dataset = new DefaultXYZDataset()
    for (i <- 0 until coeffs.length) {
      val x = new ArrayBuffer[Double]
      val y = new ArrayBuffer[Double]
      for (j <- 0 until coeffs(i).length) {
        x += j+ 1//this.peaks(j).getLcContext().getElutionTime()//j + 1
        y += i + 1//minScale + i * 2//i + 1
      }
      val array = Array.ofDim[Double](3, ydata.length) //[Array[Double]]()
      //println(x.length + ", " + y.length + ", " + coeffs(i).length + ", " + ydata.length)
      array(0) = x.toArray
      array(1) = y.toArray
      array(2) = coeffs(i)
      dataset.addSeries(i, array)
    }
    val renderer = new XYBlockRenderer()
    //val paintScale = new GrayPaintScale(-20, 10);
    //renderer.setPaintScale(paintScale);
    //val paintScale = new LookupPaintScale();
    val (minVal, maxVal) = (coeffs.flatten.min, coeffs.flatten.max)
    //println(minVal + ", " + maxVal)
    //paintScale.add(minVal, Color.blue);
    //paintScale.add(minVal + maxVal / 2.0, Color.green);
    //paintScale.add(maxVal, Color.yellow);
     val ps = new LookupPaintScale(0,101,Color.lightGray);
   // ps.add(minVal, Color.green);
    //ps.add(maxVal, Color.yellow);
    //ps.add(20, Color.red);
    renderer.setPaintScale(ps);
    val xAxis = new NumberAxis("X");
    val yAxis = new NumberAxis("Y");
    val plot = new XYPlot(dataset, xAxis, yAxis, renderer)
    plot.setBackgroundPaint(Color.lightGray);
    val chart = new JFreeChart("XYBlockChartDemo1", plot)
    ChartUtilities.saveChartAsJPEG(new java.io.File(file), chart, 600, 400)//ydata.length);
  }

  def maximaToImage(f: (Array[Array[Double]], Int) => Array[Array[Int]], file: String = "maxima.jpg") {
    /*val maxima = f(coeffs, 10) //(winSize = coeffs(0).length / 15)
    val dataset = new DefaultXYZDataset()
    for (i <- 0 until maxima.length) {
      val x = new ArrayBuffer[Double]
      val y = new ArrayBuffer[Double]
      for (j <- 0 until coeffs(i).length) {
        x += j + 1//this.peaks(j).getLcContext().getElutionTime()//j + 1
        y += i + 1//minScale + i * 2//i + 1
      }
      val array = Array.ofDim[Double](3, ydata.length) //[Array[Double]]()
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
    ChartUtilities.saveChartAsJPEG(new java.io.File(file), chart, ydata.length, ydata.length);*/

  }

  /**
   * read in Mass spectrometry data processing using zero-crossing lines in multi-scale of Gaussian derivative wavelet. Nguyen et al
   * equivalent of find maxima, looking for zero crosses of coefficients instead
   *
   * does not provide better result than maxima approach or bad implemented ?
   */
  def zeroCrossingsLines() { //: Array[Array[Int]] = 
    /*var zeroCross = new ArrayBuffer[ArrayBuffer[Int]]
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
    ChartUtilities.saveChartAsJPEG(new java.io.File("zerocross.jpg"), chart, ydata.length, ydata.length);*/
  }

}//end class


