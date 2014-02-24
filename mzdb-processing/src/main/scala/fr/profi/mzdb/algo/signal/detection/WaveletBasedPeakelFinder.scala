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
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.utils.math.wavelet.MotherWavelet
import fr.profi.mzdb.utils.math.wavelet.Ridge
import fr.profi.mzdb.utils.math.wavelet.RidgesFinder
import fr.profi.mzdb.utils.math.wavelet.WaveletUtils
import mr.go.sgfilter.SGFilter
import fr.profi.mzdb.model.ILcContext
import scala.collection.mutable.HashMap
import fr.profi.mzdb.utils.math.UWTSmoother
import fr.profi.mzdb.utils.math.SGSmoother
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.utils.math.wavelet.GaussianFirstDerivative
import scala.util.control.Breaks._
import org.apache.commons.math.stat.descriptive.moment.StandardDeviation
import org.jfree.chart.renderer.LookupPaintScale
import scala.beans.BeanProperty


/**
 * result of the algorithm
 * too many attributes, no ? LcContextSummary 
 */
class CwtPeakel( override val index:Int,
                 override val peaks: Array[Peak],
                 val apexLcContext: ILcContext,
                 val minIdx: Int,
                 val startLcContext: ILcContext,
                 val maxIdx: Int,
                 val endLcContext: ILcContext,
                 val xMax: Float,
                 val intensityMax: Float, //
                 val centroid: Float,  
                 val snr: Float,
                 val coeffsAtMaxScale: Array[Pair[Peak, Double]]= null) extends Peakel( index:Int, peaks:Array[Peak]) {
  
  
  override def toString(): String = {
    "apex:" + index + ", minIdx:" + minIdx + ", maxIdx:" + maxIdx + ", xmax:" + xMax + ", intensityMax:" + intensityMax + ", centroid:" + centroid + ", snr:" + snr + ", minTime:"+ startLcContext.getElutionTime() + ", maxtime:" + endLcContext.getElutionTime() 
  }
}


object SmoothingMethod extends Enumeration {
  type SmoothingMethod = Value
  val SG, SWT, None = Value //Savitsky Golay, stationnary wavelet transform (heavier), None
}
import SmoothingMethod._


object Method extends Enumeration {
  type Method = Value
  val Du, Coombes = Value
}
import Method._

case class CwtParameters (var smooth: SmoothingMethod = SWT, var scales: Array[Float], var wavelet: MotherWavelet)

case class RidgeFilteringParameters( var minRidgeLength: Int , //this hard to defined 
                                     var minSNR: Float, //no SNR provided by default
                                     var minPeakWidth: Float, // en seconds
                                     var maxPeakWidth: Float,
                                     var sizeNoise: Int, //the same this hard to defined
                                     var skipBoundaries: Int)

case class MaximaFinder(var winLength: Int = 5, var ridgeMethod: String = "maxima")

/**
 * @author Marc Dubois
 * Peak Width are ~= 2 * scale where scale is the maxima point on the ridge
 *
 */
class WaveletBasedPeakelFinder( var peaks: Seq[Peak] = null ) extends RidgesFinder with Logging {
 
  var ydata: Array[Double] = null
  var coeffs : HashMap[Float, Array[Double]] = null 

  /*automatic parameters, could be changed by users */
  var minScale : Float = -1f
  var maxScale : Float = -1f
  
  //logger.info("mean of the rt difference:" + scanTimeDiffMean)
  
  /*baseline and noise estimation, based on CentWave*/
  var baseline :Double = -1
  var noise :Double = -1
    
  var cwtParams : CwtParameters = null
  var ridgeFilteringParams: RidgeFilteringParameters = null
  
  var maximaParams = MaximaFinder()
                                                     
  if (peaks != null)
    this.setPeaks(peaks)
  
  /**
   * 
   */
  def zeroPadData(ydata: Array[Double], n: Int = 100) : Array[Double] = {
    val ydat = ydata.toBuffer
    for ( i <- 0 until n) {
      ydat.+=:(1d)
      ydat.+=(1d)
    }
    return ydat.toArray
  }
  
  /**
   * 
   */
  def setPeaks(  peaks_ : Seq[Peak])= {
    require(peaks_.length > 1)
    
    this.peaks = peaks_
    val data = peaks_.map(_.getIntensity.toDouble) toArray
        
    /*automatic parameters, could be changed by users */
    val scanTimeDiff = peaks_.map(_.getLcContext().getElutionTime()).sliding(2).withFilter(_.length == 2).map{ x=>x(1) - x(0)} toArray
    val scanTimeDiffSum = scanTimeDiff.sum
    
    if (scanTimeDiffSum == 0)
      throw new Exception("Error trying to compute scales")
    
    val scanTimeDiffMean = scanTimeDiffSum / scanTimeDiff.length;
   
    //NOTE: also the default in centwave
    this.minScale = math.round( ( 20/ scanTimeDiffMean ) / 2f ) 
    this.maxScale=  math.round( ( 50 / scanTimeDiffMean ) / 2f ) 
    
    /*baseline and noise estimation, based on CentWave*/
    val toBeTrimmed = math.round(0.1 * data.length) toInt
    val trimmedYdata = data.sorted.slice(toBeTrimmed, data.length - toBeTrimmed)
    
    this.baseline = trimmedYdata.sum / trimmedYdata.length
    this.noise = new StandardDeviation().evaluate(trimmedYdata, 0, trimmedYdata.length)
      
    this.cwtParams = CwtParameters(scales = ( minScale to maxScale by 2f ) toArray, wavelet =  MexicanHat() )
    
    
    this.ridgeFilteringParams = RidgeFilteringParameters(minRidgeLength = 0,  
                                                       minSNR =  1.5f, 
                                                       minPeakWidth = 15, 
                                                       maxPeakWidth = 150, 
                                                       sizeNoise = data.length / 10, 
                                                       skipBoundaries = data.length / 10)
    this.ydata = data //zeroPadData(data, n= 50)
  }
  
  
  
  /** recompute the coefficients */
  def computeCoeffs() {
     coeffs = WaveletUtils.cwt(ydata.toArray, cwtParams.wavelet, cwtParams.scales)
     //this.coeffToImage(coeffs)
  }

  /** compute the energy density of the wavelet */
  def energyDensity(): Array[Array[Double]] = { 
    throw new Error("Not yet implemented") 
 }
  
 
  
  /**
   * Improved peak detection in mass spectrum by incorporating continuous wavelet transform-based pattern matching
   * Du et al 2005
   */
  protected def _findMaxima(winSize: Int = 5): HashMap[Float, Array[Int]] = {

    val maximaIndexesPerScale = new HashMap[Float, Array[Int]]
    //var c = 0
    this.coeffs.foreach{ case(scale, coeff)  =>
      
      val winsize = math.max(winSize, scale * 2 + 1).toInt
      val localMax = new ArrayBuffer[Int]
      
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
      maximaIndexesPerScale(scale) = localMax.filter(_ != 0).toArray      
    }
    maximaIndexesPerScale
  }
  
  /**
   * Implemented in order to lead less false positive, peak cut
   */
  protected def _findMaximaNaive( win :Int = 0): HashMap[Float, Array[Int]] = {
    val indexes = new HashMap[Float, Array[Int]]
    this.coeffs.foreach{  case (scale, coeffsRow) =>
                           val maxs= new ArrayBuffer[Int]
                           for (i <- 0 until coeffsRow.length) {
                              if ( i == 0) {
                                  if (coeffsRow(i + 1) < coeffsRow(i))
                                      maxs += i;
                              } else if ( i == coeffsRow.length - 1) {
                                  if ( coeffsRow(i - 1) < coeffsRow(i) )
                                      maxs += i;
                              } else {
                                  if (coeffsRow(i - 1) < coeffsRow(i) && coeffsRow(i + 1) < coeffsRow(i))
                                      maxs += i;
                              }
                           }
                           indexes += (scale -> maxs.toArray)
                        }
    indexes
  }

 
  /**
   * Reference:
   * A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   * a fixed window is used here
   */
  protected def _findMaximaRegion(coeffs: HashMap[Float, Array[Double]],minWinSize: Int = 5): HashMap[Float, Array[Int]] = {
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
  

   /* find minima by inversing wavelet coefficient,
   *  A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   **/
  protected def _findMinimaRegion(coeffs: HashMap[Float, Array[Double]], winSize: Int = 5): HashMap[Float, Array[Int]] = {
    val inverseCoeffs = coeffs map { case ( s, c) => 
        s-> c.map { y => -y } }
    this._findMaximaRegion(inverseCoeffs, winSize)
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
  

  /** compute a SNR Dhu et al, at a given scale usually the first scale, index =0 */
  protected def _computeSNR(r: Ridge, scale: Int = 0,  sizeNoise:Int) {
    val (firstScale, maxIdxAtFirstScale, maxValueAtFirstScale) = r.firstScaleMaxCoeffPos
    val centroidValue = math.abs(maxValueAtFirstScale);
    val snrCoeffs = coeffs(scale).map(math.abs(_)).slice(math.max(maxIdxAtFirstScale - sizeNoise, 0), math.min(maxIdxAtFirstScale + sizeNoise, ydata.length - 1)).toList.sortBy(x => x)
    val noiseValue = if (! snrCoeffs.isEmpty) snrCoeffs((0.95 * snrCoeffs.length) toInt) else 0
    val estimatedSNR = centroidValue / noiseValue
    r.SNR = estimatedSNR.toFloat
  }

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
    
    
    /*var i = math.max( maxIdx - 1, 0)
    
    var currVal = coeffsAtScale(i)
   
    //to the left of the peak
    var passed = false
    breakable {
      while (i > 0 ) {
        i -= 1
        val prevVal = coeffsAtScale(i)
        if (prevVal >= currVal) {
          passed = true
          break
        }
        currVal = prevVal
      }
    }
    val minIdx = if (passed) i + 1 else i
    
    passed = false    
    
    //to the right of the peak
    var j = math.min(maxIdx + 1, coeffsAtScale.length - 1)
    currVal = coeffsAtScale(j)
    breakable {
      while (j < coeffsAtScale.length - 1 ) {
        j += 1
        val nextVal = coeffsAtScale(j)
        if (nextVal >= currVal) {
          passed = true
          break
        }
        currVal = nextVal
      }
    }
    val lastIdx = if (passed) j - 1 else j*/
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
    /*var i = math.max( minIdx - 1, 0)
    
    var currVal = this.ydata(i)
   
    //to the left of the peak
    var passed = false
    breakable {
      while (i > 0 ) {
        i -= 1
        val prevVal = this.ydata(i)
        if (prevVal >= currVal) {
          passed = true
          break
        }
        currVal = prevVal
      }
    }
    val minIndex = if (passed) i + 1 else i
    passed = false    
    
    //to the right of the peak
    var j = math.min(maxIdx + 1, this.ydata.length - 1)
    currVal = this.ydata(j)
    breakable {
      while (j < this.ydata.length - 1 ) {
        j += 1
        val nextVal = this.ydata(j)
        if (nextVal >= currVal) {
          passed = true
          break
        }
        currVal = nextVal
      }
    }
    val lastIdx = if (passed) j - 1 else j
    Pair(minIndex, lastIdx)  */
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
  protected def _ridgeToPeaks(ridges: Array[Ridge]): Array[CwtPeakel] = {
    val (minRidgeLength, sizeNoise, minSNR) = (ridgeFilteringParams.minRidgeLength, ridgeFilteringParams.sizeNoise, ridgeFilteringParams.minSNR)
    var peakels = new ArrayBuffer[CwtPeakel]
     //logger.warn("sizeNoise :" + sizeNoise + ", minSNR: " + minSNR)
    
     /*filter against selected criteria
    ** 1: the ridge do not have to be ended
    ** 2: it must begin at a scale > 2
    ** 3: its length must be > minRidgeLength*/
    var filteredRidges = ridges.filter { x => (!x.isEnded() && x.length >= 0) } //minRidgeLength) }
    //logger.warn("ridges length after removing ended ones :" + filteredRidges.length)
    
    /*group ridges by the max at first scale*/
   val ridgesByMaxIndexAtMaxScale = new HashMap[Pair[Float, Int], ArrayBuffer[Ridge]]()
    filteredRidges.foreach{ r => val pair = (r.maxCoeffPos._1, r.maxCoeffPos._2)// (maxScale, maxIndexAtMaxScale)
                                  ridgesByMaxIndexAtMaxScale.getOrElseUpdate(pair, new ArrayBuffer[Ridge]) += r
    }
    filteredRidges = ridgesByMaxIndexAtMaxScale.map{ case (i, ridges) => ridges.maxBy( _.length)} toArray
    
    //logger.warn("ridges length after removing those pointing to the same max :" + filteredRidges.length)
    
    
    /*compute SNR for each ridge*/
    filteredRidges.foreach { ridge =>
      val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos//firstScaleMaxCoeffPos
      val sideIndexesAsOpt = this._findLocalMinima(maxScale, maxIdxAtMaxScale) // WAZRNING BEFORE maxScale
      
      if (sideIndexesAsOpt.isDefined) {
        val (minIdx, maxIdx) = sideIndexesAsOpt.get
        if ( minIdx < maxIdx) {
          val sliced = this.ydata.slice(minIdx, math.min(maxIdx + 1, ydata.length))
          if (!sliced.isEmpty)
            ridge.SNR = ( sliced.max - this.baseline ) / this.noise toFloat 
            else
              ridge.SNR = -1000
        } else {
          ridge.SNR = -1000
        }
            
      } else {
          ridge.SNR = -1000
      }
    }

    /*filter against SNR criteria*/
    filteredRidges = filteredRidges.filter(ridge => ridge.SNR > minSNR)
    //logger.warn("ridges passing SNR filter: :" + filteredRidges.length)

    /*filter against maxScale position*/
    //filteredRidges = filteredRidges.filter(_.maxCoeffPos._1 >= 4) //put in parameters ?
    
    /*peakWidth estimation*/
    filteredRidges.foreach { ridge =>
     
      val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos  
      
      val sideIndexesAsOpt  = this._findLocalMinima(maxScale, maxIdxAtMaxScale) // WAZRNING BEFORE maxScale 
      //val sideIndexesAsOpt  = this._findPeakBoundariesXCMS(maxScale, maxIdxAtMaxScale)
      
      if (sideIndexesAsOpt.isDefined) {
        // go back into real data, centroid calculation
        val sidesIndexes = sideIndexesAsOpt.get
        var (minIdx, maxIdx) = (math.max(sidesIndexes._1, 0), math.min(sidesIndexes._2, peaks.length - 1 ))
        //val p = this._findLocalMinimaRealSpaceFromWaveletBounds(minIdx, maxIdx)
        //minIdx = p._1
        //maxIdx = p._2
        val slicedPeaks = peaks.slice(minIdx, math.min(maxIdx + 1, peaks.length))
        val intensities = slicedPeaks.map(_.getIntensity)
        val xvalues = slicedPeaks.map(_.getLcContext.getElutionTime)
        val centroid =xvalues.zip(intensities).map { case (x, y) => x * y }.reduceLeft(_ + _) / intensities.reduceLeft(_ + _)
        val intensityMax = intensities.max 
        val xmax =  xvalues(intensities.indexOf(intensityMax)) 
        
        peakels += new CwtPeakel(index = intensities.indexOf(intensityMax),
                                  peaks = slicedPeaks toArray, 
                                  apexLcContext = this.peaks(intensities.indexOf(intensityMax)).getLcContext,
                                  minIdx = minIdx, 
                                  startLcContext = peaks(minIdx).getLcContext,  
                                  maxIdx = maxIdx, 
                                  endLcContext = peaks(maxIdx).getLcContext,
                                  xMax = xmax toFloat,
                                  intensityMax = intensityMax toFloat,
                                  centroid = centroid toFloat,
                                  snr = ridge.SNR)
                            //coeffsAtMaxScale = r)
      }

    }
    /*filter peakels really too small*/
    //peakels = peakels.filter( x => math.abs(x.maxIdx - x.minIdx) >= 6) // filter the two small peakel
    
    /*remove a detected peakel which is contained in another peakel*/
    peakels = this._filterOverlappingPeakels(peakels);
    val mergedPeakels = this._mergeOverlappingPeakels(peakels)
    
    //peakels.toArray
    mergedPeakels
  }
  
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
  
  private def _filterOverlappingPeakels(peakels: ArrayBuffer[CwtPeakel], mergePeakels: Boolean = false) : ArrayBuffer[CwtPeakel] = {
    val overlappingPeakelsByMasterPeakel = new HashMap[CwtPeakel, ArrayBuffer[CwtPeakel]]
    for (i <- 0 until peakels.length ) {
      overlappingPeakelsByMasterPeakel(peakels(i)) = new ArrayBuffer[CwtPeakel]
      for (j <- 0 until peakels.length) {
        if (i != j) {
          //complete overlapping peakels
          if ( (peakels(j).startLcContext.getElutionTime >= peakels(i).startLcContext.getElutionTime && 
                peakels(j).endLcContext.getElutionTime <= peakels(i).endLcContext.getElutionTime) ){ 
            overlappingPeakelsByMasterPeakel(peakels(i)) += peakels(j)
          }      
        }
      }
    }
    var toAvoid = Set[CwtPeakel]()
    overlappingPeakelsByMasterPeakel.map{ case (k, v) => 
      if ( ! v.isEmpty) {
        v.foreach(toAvoid += _)
      }
    }
    //finalPeakels.distinct
    val newPeakels = peakels.filter(! toAvoid.contains(_))
    //merge
    newPeakels
   
  }
  
  /**
   * to be used after filtering peakels
   */
  private def _mergeOverlappingPeakels(peakels: ArrayBuffer[CwtPeakel] ) : Array[CwtPeakel] = {
    //
    var toAvoid = Set[CwtPeakel]()
    val overlappingPeakelsByMasterPeakel = new HashMap[CwtPeakel, ArrayBuffer[CwtPeakel]]
    
    for (i <- 0 until peakels.length ) {
      
      overlappingPeakelsByMasterPeakel(peakels(i)) = new ArrayBuffer[CwtPeakel]
      //
      if (! toAvoid.contains(peakels(i))) {
        for (j <- 0 until peakels.length) {
          if (i != j) {
            val minrt  = peakels(i).apexLcContext.getElutionTime
            val maxrt = peakels(i).apexLcContext.getElutionTime
            val minrt_ = peakels(j).apexLcContext.getElutionTime
            val maxrt_ = peakels(j).apexLcContext.getElutionTime
            
            //test
            if ( ( minrt < minrt_ && maxrt < maxrt_ && maxrt > minrt_) || 
                 ( minrt > minrt_ && maxrt > maxrt_ &&  minrt < maxrt_) ) {
              
              overlappingPeakelsByMasterPeakel(peakels(i)) += peakels(j)
              toAvoid += peakels(j)
            }
          }
        }
      }
    }
    
    //val newPeakels = new ArrayBuffer[CwtPeakel]
   
    
    overlappingPeakelsByMasterPeakel.map{ case (cwtPeakel, ovlCwtPeakels) => 
      var all = ovlCwtPeakels += cwtPeakel
      all = all.sortBy(_.endLcContext.getElutionTime)
      val peaks = new ArrayBuffer[Peak]
      val scanIds = Set[Int]()
      for ( cwtPeak <- all) {
        for ( p <- cwtPeak.peaks ) {
          if (p !=null && ! scanIds.contains(p.getLcContext().getScanId()))
            peaks += p
        }
      }
      
      val minIdx = all.map(_.minIdx).toArray.minBy[Int](x=>x)
      val maxIdx = all.map(_.maxIdx).toArray.maxBy[Int](x=>x)
      
      val peakApex = peaks.maxBy(_.getIntensity())
      val apexIndex = ydata.indexOf( peakApex.getIntensity())

      new CwtPeakel(index = apexIndex, //index on the original ydata
                    peaks = peaks.toArray,
                    apexLcContext = peakApex.getLcContext(),
                    minIdx = minIdx,
                    startLcContext = peaks.head.getLcContext(),
                    maxIdx = maxIdx,
                    endLcContext = peaks.last.getLcContext(),
                    xMax = peakApex.getLcContext().getElutionTime,
                    intensityMax = peakApex.getIntensity(), //
                    centroid = peakApex.getMz().toFloat,  
                    snr = all.map(_.snr).sum / all.length )
    } toArray
    
  }

  
  /**
   * return the indexes of the peakel found
   */
  def findPeakelsIndexes(method: Method = Du, asScanId:Boolean=false): Array[Pair[Int, Int]] = {

    val peakels = findCwtPeakels(method)
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

  /**
   * return the peakels
   */
  def findCwtPeakels(method: Method = Du, cutOff: Float = _noiseValue() ): Array[CwtPeakel] = {
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
    
    
    
    
    
    computeCoeffs //compute coeffs with cwtParameters
    //logger.debug("CWT ended")
    
    method match {
      case Du =>
        /*check we have the good wavelet for performing the Du method*/
        if (! cwtParams.wavelet.isInstanceOf[MexicanHat]) {
          println("Du method was selectionned, setting wavelet to MexicanHat (it was a GaussianFirstDerivative)")
          cwtParams.wavelet =  MexicanHat()
        }
        
        val maximaIndexesByScale = this._findMaximaNaive(0)//this._findMaxima(1) //
        //maxima.foreach(x=> x.foreach(println(_)))
        //this.maximaToImage(this._findMaximaNaive)
        val (ridges, orphanRidges) = this.findRidges(maximaIndexesByScale, winLength = 5, maxGap = 3) //maximaParams.winLength, maxGap = 4)
        val peakels = this._ridgeToPeaks(ridges)
        peakels
      
        
        /*case Coombes =>
        //check we have the good wavelet for performing the Du method
        if (! cwtParams.wavelet.isInstanceOf[GaussianFirstDerivative]) {
          println("Setting wavelet to GaussianFirstDerivative")
          cwtParams.wavelet =  GaussianFirstDerivative()
        }
        
        //launch 2 threads
        val (maxima, toDel) = findRidges(this._findMaximaRegion(coeffs, 5), coeffs, maximaParams.winLength, maxGap = 6)
        //logger.debug("Count maximaRidges:" + maxima.length)
        
        val invCoeffs = coeffs.map(array => array.map(-_))
        val (minima, toDel_) = findRidges(_findMinimaRegion(coeffs, 5), invCoeffs, maximaParams.winLength, maxGap = 6)
        //logger.debug("Count minimaRidges:" + minima.length)
        val pairs =  _pairMaximaMinimaRidges(maxima, minima, 10)
        //logger.debug("Count pairs:" + pairs.length)

        val peakels = _ridgeToPeaks(pairs).distinct.filter(_.intensityMax > cutOff)
        peakels*/
    }
      
  }

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


