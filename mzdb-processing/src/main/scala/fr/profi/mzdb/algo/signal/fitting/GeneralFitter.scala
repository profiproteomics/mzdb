/**
 *
 */
package fr.profi.mzdb.algo.signal.fitting

import scala.collection.mutable.ArrayBuffer
import scala.beans.BeanProperty
import org.apache.commons.math.analysis.DifferentiableMultivariateVectorialFunction
import org.apache.commons.math.optimization.fitting.ParametricRealFunction
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import org.apache.commons.math.optimization.fitting.WeightedObservedPoint
import org.apache.commons.math.optimization.fitting.GaussianParametersGuesser
import fr.profi.mzdb.utils.math.pdf.ParametricGaussApprox
import org.apache.commons.math.optimization.fitting.ParametricGaussianFunction
import fr.profi.mzdb.utils.math.pdf.ParametricPolynomial

/*trait SpecifiedParametricRealFunction extends ParametricRealFunction {
  def getNbParameters(): Int
  def getApexParameterIndex(): Int
  //the last two parameters are sigma left and right
}*/

/**
 * @author Marco
 *
 */
class GeneralFitter(@BeanProperty val x: Array[Double], //mz
                    @BeanProperty val y: Array[Double], //intensity
                    @BeanProperty var peaks: ArrayBuffer[PeakShape],
                    peakType: PeakType.Value) {
  def fit(iter:Int = 100, 
          optimizer:AbstractLeastSquaresOptimizer = new LevenbergMarquardtOptimizer, 
          weights:Array[Double] = (for (i <- 0 until y.length) yield 1d) toArray) : VectorialPointValuePair = {
    
    peakType match {
      case PeakType.GAUSS =>
        val gaussFitter = new GaussFitter(x, y, peaks)
        return gaussFitter.optimize(iter, optimizer, weights)
      case PeakType.GAUSSLORENTZ =>
        val gaussLorentzFitter = new GaussLorentzFitter(x, y, peaks)
        return gaussLorentzFitter.optimize(iter, optimizer, weights)
      case PeakType.PARABOLA =>
        val parabolaFitter = new PolyFitter(x, y, peaks)
        return parabolaFitter.optimize(iter, optimizer, weights)
    }
    
  }
 
  
    
}

