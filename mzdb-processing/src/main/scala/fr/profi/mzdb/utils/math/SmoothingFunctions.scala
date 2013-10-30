/**
 *
 */
package fr.profi.mzdb.utils.math
import mr.go.sgfilter.SGFilter
import fr.profi.mzdb.utils.math.wavelet.WaveletUtils
import scala.collection.immutable.HashMap


trait Smoother {
  def smooth(args:Map[String, Float]) : Array[Double]
}

abstract class AbstractSmoother(data: Array[Double]) extends Smoother

object SmoothingUtils{
  def smooth(s: Smoother, args:Map[String, Float] = new HashMap[String, Float]): Array[Double] = {
    s.smooth(args)
  }
}

/** UWT equivalent to SWT*/
class UWTSmoother( data: Array[Double]) extends AbstractSmoother(data) {
  
  /*object PARAMS extends Enumeration {
    type PARAMS = Value
    val Iter = "iter"
    val filter = "filter"
  }*/
  
  def smooth(args: Map[String, Float]): Array[Double] = {
    val coeffs = WaveletUtils.swt(data) //, args.getOrElse("iter", 6))
    WaveletUtils.denoiseSoft(coeffs) //, 6)
    WaveletUtils.iswt(coeffs) // , 6)
  }
}

/** smooth signal with a SG smoother */
class SGSmoother( data: Array[Double]) extends AbstractSmoother(data) {
  
  def smooth(args:Map[String, Float]): Array[Double] = {  
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