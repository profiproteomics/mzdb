/**
 *
 */
package fr.profi.mzdb.utils.math
import mr.go.sgfilter.SGFilter
import fr.profi.mzdb.utils.math.wavelet.WaveletUtils
import scala.collection.immutable.HashMap
import scala.beans.BeanProperty


trait Smoother {
  def smooth(data:Array[Double]) : Array[Double]
}

/** UWT equivalent to SWT*/
object UWTSmoother extends Smoother{
  
  @BeanProperty var smoothMethod = "Soft"
  @BeanProperty var nbIter = 6

  def smooth(data:Array[Double]): Array[Double] = {
    val coeffs = WaveletUtils.swt(data) //, args.getOrElse("iter", 6))
    WaveletUtils.denoiseSoft(coeffs) //, 6)
    WaveletUtils.iswt(coeffs) // , 6)
  }
}

/** smooth signal with a SG smoother */
object SGSmoother extends Smoother {
  
  def smooth(data:Array[Double]): Array[Double] = {  
    val (nl, nr, order) = (5, 5, 4)
    val polycoef = SGFilter.computeSGCoefficients(nl, nr, order)

    val sgFilter = new SGFilter(5, 5)
    var smoothedValues = data
    for (i <- 1 to 3) {
      smoothedValues = sgFilter.smooth(smoothedValues, polycoef)
    }
    smoothedValues
  }
}