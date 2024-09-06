package fr.profi.mzdb.algo.signal.detection

import fr.profi.mzdb.model.ILcContext
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.util.math.wavelet.MotherWavelet

/** result of the algorithm */
case class CwtPeakel(
  /*override val lcContexts: Array[ILcContext],
  override val mzValues: Array[Double],
  override val intensityValues: Array[Float],
  override val leftHwhmValues: Array[Float] = null,
  override val rightHwhmValues: Array[Float] = null,*/
  val peaks: Array[Peak],
  
  val apexIndex: Int,
  val apexLcContext: ILcContext,
 
  val minIdx: Int,
  val startLcContext: ILcContext,
 
  val maxIdx: Int,
  val endLcContext: ILcContext,
 
  val xMax: Float,
 
  val intensityMax: Float, //
   
  val centroid: Float,  
   
  val snr: Float,
   
  val coeffsAtMaxScale: Array[Tuple2[Peak, Double]] = null
) {//extends Peakel( lcContexts, mzValues, intensityValues, leftHwhmValues, rightHwhmValues ) {
  
  def getFirstLcContext() = peaks.head.getLcContext()
  def getLastLcContext() = peaks.last.getLcContext()
  
  override def toString(): String = {
    "minIdx:" + minIdx + ", maxIdx:" + maxIdx + ", xmax:" + 
    xMax + ", intensityMax:" + intensityMax + ", centroid:" + centroid + ", snr:" + snr + 
    ", minTime:" + startLcContext.getElutionTime() + ", maxtime:" + endLcContext.getElutionTime() 
  }
}

/** Savitsky Golay, stationnary wavelet transform (UDWT == SWT), None */
object SmoothingMethod extends Enumeration {
  type SmoothingMethod = Value
  val SG, SWT, None = Value 
}
import SmoothingMethod._

/** Du et al. Improved peak detection in mass spectrum by incorporating continuous wavelet transform-based pattern matching.2006
 *  Wee et al. A continuous wavelet transform algorithm for peak detection 2008
 */
object Method extends Enumeration {
  type Method = Value
  val Du, Wee = Value
}
import Method._


/** */
case class CwtParameters (
  var smooth: SmoothingMethod = None, 
  var scales: Array[Float], 
  var wavelet: MotherWavelet
)

/** */
case class RidgeFilteringParameters( 
  var minRidgeLength: Int , //this hard to defined 
  var minSNR: Float,        //no SNR provided by default
  var minPeakWidth: Float,  // en seconds
  var maxPeakWidth: Float,
  var sizeNoise: Int,       //the same this hard to defined
  var skipBoundaries: Int
)

/** */
case class MaximaFinder(
  var winLength: Int = 5,
  var ridgeMethod: String = "maxima"
)