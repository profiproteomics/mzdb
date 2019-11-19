package fr.profi.mzdb.algo.signal.detection

import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.util.math.wavelet.RidgesFinder
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.util.math.wavelet.Ridge
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

trait IWaveletDetectionMethod extends RidgesFinder {
  
  /** */
  def setCwtParametersParams()
  
  /** */
  def setRidgesFilteringParams()
  
  /** */
  def computeSNR(r: Ridge)
  
  /** */
  def findMaxima(minWinSize: Int = 5) : HashMap[Float, Array[Int]]
  
  
  /** Implemented in order to lead less false positive, peak cut */
  protected def _findMaximaNaive( win :Int = 0): HashMap[Float, Array[Int]] = {
    val maxIndexesByScale = new HashMap[Float, Array[Int]]
    
    this.coeffs.foreach{case (scale, coeffsRow) =>
      
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
       maxIndexesByScale += (scale -> maxs.toArray)
    }
    
    maxIndexesByScale
  }
  
  def ridgesToPeaks(ridges: Array[Ridge]): Array[CwtPeakel]
  
}