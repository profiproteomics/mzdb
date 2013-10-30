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
import fr.profi.mzdb.utils.math.SmoothingUtils
import fr.profi.mzdb.utils.math.UWTSmoother
import fr.profi.mzdb.utils.math.SGSmoother
import com.weiglewilczek.slf4s.Logging
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.utils.math.wavelet.GaussianFirstDerivative
import scala.util.control.Breaks._
import org.apache.commons.math.stat.descriptive.moment.StandardDeviation
import org.jfree.chart.renderer.LookupPaintScale
import scala.reflect.BeanProperty


/**
 * result of the algorithm
 * too many attributes, no ? LcContextSummary 
 */
case class CwtPeakel( override val index:Int,
                      override val peaks: Array[Option[Peak]],
                      apexLcContext: ILcContext,
                      minIdx: Int,
                      startLcContext: ILcContext,
                      maxIdx: Int,
                      endLcContext: ILcContext,
                      xMax: Float,
                      intensityMax: Float, //
                      centroid: Float,  
                      snr: Float,
                      coeffsAtMaxScale: Array[Pair[Peak, Double]]= null) 
                      extends Peakel( index:Int, 
                                      peaks:Array[Option[Peak]]) {
  
  
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
 * @author Marco
 * Peak Width are 2 * scale where scale is the maxima point on the ridge
 * actually  the peak width estimation is a little bit tricky, differs a lot
 * between articles
 *
 */
class WaveletBasedPeakelFinder(val peaks: Seq[Peak] ) extends RidgesFinder with Logging{
   
  val ydata = peaks.map(_.getIntensity.toDouble) toArray
  var coeffs : Array[Array[Double]] = null
  
  /*automatic parameters, could be changed by users */
  val scanTimeDiffMean = peaks.map(_.getLcContext().getElutionTime()).sliding(2).withFilter(_.length == 2).map{ x => x(1) - x(0) }.sum / peaks.length;
  val minScale = math.max(1, math.round( (10 / scanTimeDiffMean) / 2f )) toFloat
  val maxScale=  math.max(6, math.round( (200 / scanTimeDiffMean) / 2f ) ) toFloat
  
  logger.info("mean of the rt difference:" + scanTimeDiffMean)
  println("minScale:" + minScale + ",  maxScale:" + maxScale)
  
  
  /*baseline and noise estimation, based on CentWave*/
  val toBeTrimmed = math.round(0.1 * ydata.length) toInt
  val trimmedYdata = ydata.sorted.slice(toBeTrimmed, ydata.length - toBeTrimmed)
  val baseline = trimmedYdata.sum / trimmedYdata.length
  val noise = new StandardDeviation().evaluate(trimmedYdata, 0, trimmedYdata.length)
    
  var cwtParams = CwtParameters(scales = ( minScale to maxScale by 1f ) toArray, wavelet =  MexicanHat() )//GaussianFirstDerivative()) //(0.5f to 10 by 0.5f) ++ ( 11f to 63f by 2f)
  var ridgeFilteringParams = RidgeFilteringParameters(minRidgeLength = 0, //(120f - 15f) / 4 toInt,  
                                                       minSNR =  3f, 
                                                       minPeakWidth = 15, 
                                                       maxPeakWidth = 150, 
                                                       sizeNoise = ydata.length / 10, 
                                                       skipBoundaries = ydata.length / 10)
  var maximaParams = MaximaFinder()
  
  
  /** recompute the coefficients */
  def computeCoeffs() {
     coeffs = WaveletUtils.cwt(ydata.toArray, cwtParams.wavelet, cwtParams.scales)//.filter(x=> x.forall(_!=0)); 
     //coeffs.foreach{x=> x.foreach(println(_)); println("\n")}
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
   * Implemented in order to lead less false positive, peak cut
   */
  protected def _findMaximaNaive(coeffs:Array[Array[Double]], win :Int = 0): Array[Array[Int]] = {
    val indexes = new ArrayBuffer[ArrayBuffer[Int]]
    coeffs.foreach{  row =>
                     val maxs= new ArrayBuffer[Int]
                     for (i <- 0 until row.length) {
                        if ( i == 0) {
                            if (row(i + 1) < row(i))
                                maxs += i;
                        } else if ( i == row.length - 1) {
                            if ( row(i - 1) < row(i) )
                                maxs += i;
                        } else {
                            if (row(i - 1) < row(i) && row(i + 1) < row(i))
                                maxs += i;
                        }
                     }
                     indexes += maxs
                  }
    indexes.map(_.toArray).toArray
  }

 
  /**
   * Reference:
   * A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   * a fixed window is used here
   */
  protected def _findMaximaRegion(coeffs: Array[Array[Double]], winSize: Int = 5): Array[Array[Int]] = {
    val maximaIndexesPerScale = new ArrayBuffer[ArrayBuffer[Int]]
    var c = 0
    for ( (coeff, i) <- coeffs.zipWithIndex) {
      //not going to use it ?
      val winsize = math.max(winSize, this.cwtParams.scales(i) / 2 + 1)
      
      val localMax = new ArrayBuffer[Int]
      var coeffCloned = coeff.clone
      while (coeffCloned.length != 0) {
        val maxi = coeffCloned.max
        val maxIdxToCoeff = coeff.indexOf(maxi)
        val maxIdxToCoeffCloned = coeffCloned.indexOf(maxi)
        localMax += maxIdxToCoeff
        val (inf, sup) = (math.max(0, maxIdxToCoeffCloned - winSize / 2), math.min(coeffCloned.length, maxIdxToCoeffCloned + winSize / 2))
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
   * pair a maxima Ridge to a minima Ridge, to be used with the gaussian first derivative
   *  A continuous wavelet transform algorithm for peak detection, Andrew Wee et al, 2008
   */
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
  }
  

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
  protected def _findLocalMinima(scale:Int = 0, maxIdx :Int): Option[Pair[Int, Int]] = {    
    var i = math.max( maxIdx - 1, 0)
    val coeffsAtScale = this.coeffs(scale)
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
    val lastIdx = if (passed) j - 1 else j
    if (minIdx != lastIdx) Some(Pair(minIdx, lastIdx))  else Option.empty[Pair[Int, Int]] 
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
     logger.warn("sizeNoise :" + sizeNoise + ", minSNR: " + minSNR)
    
     /*filter against selected criteria
    ** 1: the ridge do not have to be ended
    ** 2: it must begin at a scale > 2
    ** 3: its length must be > minRidgeLength*/
    var filteredRidges = ridges.filter { x => (!x.isEnded() && x.length >= 0) } //minRidgeLength) }
    logger.warn("ridges length after removing ended ones :" + filteredRidges.length)
    
    /*group ridges by the max at first scale*/
   val ridgesByMaxIndexAtMaxScale = new HashMap[Pair[Int, Int], ArrayBuffer[Ridge]]()
    filteredRidges.foreach{ r => val pair = (r.maxCoeffPos._1, r.maxCoeffPos._2)// (maxScale, maxIndexAtMaxScale)
                                  ridgesByMaxIndexAtMaxScale.getOrElseUpdate(pair, new ArrayBuffer[Ridge]) += r
    }
    filteredRidges = ridgesByMaxIndexAtMaxScale.map{ case (i, ridges) => ridges.maxBy( _.length)} toArray
    
    logger.warn("ridges length after removing those pointing to the same max :" + filteredRidges.length)
    
    
    /*compute SNR for each ridge*/
    filteredRidges.foreach { ridge =>
      val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos//firstScaleMaxCoeffPos
      val sideIndexesAsOpt = this._findLocalMinima(maxScale, maxIdxAtMaxScale) // WAZRNING BEFORE maxScale
      
      if (sideIndexesAsOpt.isDefined) {
        val (minIdx, maxIdx) = sideIndexesAsOpt.get
        ridge.SNR = ( this.ydata.slice(minIdx, maxIdx).max - this.baseline ) / this.noise toFloat //estimatedSNR.toFloat
      } else {
          ridge.SNR = -1000
      }
    }

    /*filter against SNR criteria*/
    filteredRidges = filteredRidges.filter(ridge => ridge.SNR > minSNR)
    logger.warn("ridges passing SNR filter: :" + filteredRidges.length)

    /*filter against maxScale position*/
    filteredRidges = filteredRidges.filter(_.maxCoeffPos._1 >= 2) //put in parameters ?
    
    /*peakWidth estimation*/
    filteredRidges.foreach { ridge =>
      //DEBUG purpose
      /*if (ridge.SNR > 40) {
         val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos  
         println("Max Scale : " + this.cwtParams.scales(maxScale) + ", maxIdxAtMaxScale : " + maxIdxAtMaxScale)
         coeffs(maxScale).foreach(println(_))
         //println(maxIdxAtMaxScale + ", " + maxScale)
      }*/
      val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos  
      
      val sideIndexesAsOpt  = _findLocalMinima(maxScale, maxIdxAtMaxScale) // WAZRNING BEFORE maxScale 
      if (sideIndexesAsOpt.isDefined) {
        // go back into real data
        //centroid calculation
        val (minIdx, maxIdx) = sideIndexesAsOpt.get
        val slicedPeaks = peaks.slice(minIdx, maxIdx)
        val intensities = slicedPeaks.map(_.getIntensity)
        val xvalues = slicedPeaks.map(_.getLcContext.getElutionTime)
        val centroid =xvalues.zip(intensities).map { case (x, y) => x * y }.reduceLeft(_ + _) / intensities.reduceLeft(_ + _)
        val intensityMax = intensities.max 
        val xmax =  xvalues(intensities.indexOf(intensityMax)) 
        val r = this.peaks.slice(minIdx, maxIdx + 1).zip(coeffs(maxScale).slice(minIdx, maxIdx)).toArray
        peakels += CwtPeakel(index = intensities.indexOf(intensityMax),
                            peaks = slicedPeaks.map(Some(_)) toArray,
                            apexLcContext = this.peaks(intensities.indexOf(intensityMax)).getLcContext,
                            minIdx = minIdx, 
                            startLcContext = peaks(minIdx).getLcContext,  
                            maxIdx = maxIdx, 
                            endLcContext = peaks(maxIdx).getLcContext,
                            xMax = xmax toFloat,
                            intensityMax = intensityMax toFloat,
                            centroid = centroid toFloat,
                            snr = ridge.SNR,
                            coeffsAtMaxScale = r)
      }

    }
    /*filter peakels really too small*/
    peakels = peakels.filter( x => math.abs(x.maxIdx - x.minIdx) >= 6) // filter the two small peakel
    
    /*remove a detected peakel which is contained in another peakel*/
    peakels = this._filterOverlappingPeakels(peakels); 
    
    peakels.toArray
  }
  
   /** first method using the first derivative of gaussian as wavelet*/
  protected def _ridgeToPeaks(ridges: Array[Pair[Ridge, Ridge]]): Array[CwtPeakel] = {
    //TODO test if we can remove this filter
    val (minRidgeLength, sizeNoise, minSNR) = (ridgeFilteringParams.minRidgeLength, ridgeFilteringParams.sizeNoise, ridgeFilteringParams.minSNR)
    var validPairs = ridges.filter { case (a, b) => (! a.isEnded(maxGap=6))  && a.length >= minRidgeLength && (! b.isEnded(maxGap=6)) && b.length >= minRidgeLength }
    logger.warn("validPairs first filter (isEnded):" + validPairs.length)
    
    
    //compute SNR thresholds on both
    validPairs.foreach { case( a, b) =>
       //_computeSNR(a, scale=0, sizeNoise=sizeNoise)
       //_computeSNR(b, scale=0, sizeNoise=sizeNoise)
      
    }
    
    //not sur if the second element need to be > to minSNR since it is already a minimum
    //validPairs = validPairs.filter{ case (a, b) => a.SNR >= minSNR && math.abs(b.SNR) >= minSNR}
    
    logger.warn("validPairs second filter (SNR):" + validPairs.length)

    
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
  }
  
  private def _filterOverlappingPeakels(peakels: ArrayBuffer[CwtPeakel], mergePeakels: Boolean = false) : ArrayBuffer[CwtPeakel] = {
    var overlappingPeakelsByMasterPeakel = new HashMap[CwtPeakel, ArrayBuffer[CwtPeakel]]
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
    //if (mergePeakels) 
    /*val finalPeakels = new ArrayBuffer[CwtPeakel]
    val s = Set[CwtPeakel]()
    for (i <- 0 until newPeakels.length ) {
      overlappingPeakelsByMasterPeakel(peakels(i)) = new ArrayBuffer[CwtPeakel]
      for (j <- 0 until peakels.length) {
        if (i != j) {
          if ( newPeakels(i).startLcContext.getElutionTime > peakels.
        }
      }
     }
    null*/
  }

  
  /**
   * return the indexes of the peakel found
   */
  def findPeakelsIndexes(): Array[Pair[Int, Int]] = {

    val peakels = findCwtPeakels()
    peakels.map(x => (x.minIdx, x.maxIdx))
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
  def findCwtPeakels(method: Method = Coombes, cutOff: Float = _noiseValue() ): Array[CwtPeakel] = {
    /*check all the parameters*/
    logger.debug("Detected CutOff:" + cutOff)
    
    /*dont know what the following code is doing*/
    /*var (min_, max_) = (0f,0f)
    if (cwtParams.scales.min != ridgeFilteringParams.minPeakWidth)
      min_ = math.max(0,ridgeFilteringParams.minPeakWidth - 2)
      
    if (cwtParams.scales.max != ridgeFilteringParams.maxPeakWidth)
      max_ = ridgeFilteringParams.maxPeakWidth + 2
    
    if (min_ != 0 || max_ != 0 )
      cwtParams.scales = (min_ to max_ by 1f).toArray[Float]*/
     
    
    var y_data: Array[Double] = null
    
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
    logger.debug("CWT ended")
    
    method match {
      case Du =>
        /*check we have the good wavelet for performing the Du method*/
        if (! cwtParams.wavelet.isInstanceOf[MexicanHat]) {
          println("Du method was selectionned, setting wavelet to MexicanHat (it was a GaussianFirstDerivative)")
          cwtParams.wavelet =  MexicanHat()
        }
        
        val maxima = this._findMaximaNaive(coeffs, 0).map(x => x.filter(_ != 0))//this._findMaximaRegion(coeffs, 5)
        //maxima.foreach(x=> x.foreach(println(_)))
        //this.maximaToImage(this._findMaximaNaive)
        val (ridges, orphanRidges) = this.findRidges(maxima, coeffs, winLength = 5, maxGap = 3) //maximaParams.winLength, maxGap = 4)
        val peakels = this._ridgeToPeaks(ridges)
        peakels
      case Coombes =>
        //check we have the good wavelet for performing the Du method
        if (! cwtParams.wavelet.isInstanceOf[GaussianFirstDerivative]) {
          println("Setting wavelet to GaussianFirstDerivative")
          cwtParams.wavelet =  GaussianFirstDerivative()
        }
        
        //launch 2 threads
        val (maxima, toDel) = findRidges(_findMaximaRegion(coeffs, 5), coeffs, maximaParams.winLength, maxGap = 6)
        logger.debug("Count maximaRidges:" + maxima.length)
        
        val invCoeffs = coeffs.map(array => array.map(-_))
        val (minima, toDel_) = findRidges(_findMinimaRegion(coeffs, 5), invCoeffs, maximaParams.winLength, maxGap = 6)
        logger.debug("Count minimaRidges:" + minima.length)
        val pairs =  _pairMaximaMinimaRidges(maxima, minima, 10)
        logger.debug("Count pairs:" + pairs.length)

        val peakels = _ridgeToPeaks(pairs).distinct.filter(_.intensityMax > cutOff)
        peakels
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
    println(minVal + ", " + maxVal)
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
    val maxima = f(coeffs, 10) //(winSize = coeffs(0).length / 15)
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


