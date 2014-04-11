package fr.profi.mzdb.algo.signal.detection

import fr.profi.mzdb.model.ILcContext
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.utils.math.wavelet.MotherWavelet

/** result of the algorithm */
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
                 
                 val coeffsAtMaxScale: Array[Pair[Peak, Double]]= null
               ) extends Peakel( index:Int, peaks:Array[Peak]) {
  
  override def toString(): String = {
    "apex:" + index + ", minIdx:" + minIdx + ", maxIdx:" + maxIdx + ", xmax:" + 
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
case class CwtParameters (var smooth: SmoothingMethod = None, 
                          var scales: Array[Float], 
                          var wavelet: MotherWavelet)

/** */
case class RidgeFilteringParameters( var minRidgeLength: Int , //this hard to defined 
                                     var minSNR: Float,        //no SNR provided by default
                                     var minPeakWidth: Float,  // en seconds
                                     var maxPeakWidth: Float,
                                     var sizeNoise: Int,       //the same this hard to defined
                                     var skipBoundaries: Int)

/** */
case class MaximaFinder(var winLength: Int = 5, var ridgeMethod: String = "maxima")