package fr.profi.mzdb.algo.signal.fitting

import org.apache.commons.math.analysis.DifferentiableMultivariateVectorialFunction
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import scala.reflect.BeanProperty
import org.apache.commons.math.optimization.fitting.GaussianFunction
import org.apache.commons.math.analysis.polynomials.PolynomialFunction
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.utils.math.pdf.LorentzianFunction

object PeakType extends Enumeration {
  type PeakType = Value
  val GAUSS, APPROX_GAUSS, PARABOLA, GAUSSLORENTZ = Value
}

case class PeakShape(var y_shift: Double,
                     var mz: Double,
                     var intensity: Float,
                     var sigmaLeft: Float = 0.05f,
                     var sigmaRight: Float = 0.05f) {
  var fittedMz = 0d
  var fittedIntensity = 0f
  var fittedSigmaLeft = 0f
  var fittedSigmaRight = 0f
  var fittedYShift = 0d
  //parameter of the first parabola
  @BeanProperty var alpha = 0d
  @BeanProperty var beta = 0d
  @BeanProperty var gamma = 0d
  //parameter of the second parabola
  @BeanProperty var alpha2 = 0d
  @BeanProperty var beta2 = 0d
  @BeanProperty var gamma2 = 0d

  @BeanProperty var peakType = PeakType.GAUSS

  def getFittedY(x: Array[Double]): Array[Double] = {
    val r = new ArrayBuffer[Double](x.length)
    if (peakType == PeakType.GAUSS) {
      val func1 = new GaussianFunction(fittedYShift, fittedIntensity.toDouble, fittedMz, fittedSigmaLeft.toDouble)
      val func2 = new GaussianFunction(fittedYShift, fittedIntensity.toDouble, fittedMz, fittedSigmaRight.toDouble)
      for (i <- 0 until x.length) {
        if (x(i) <= fittedMz) {
          r += func1.value(x(i))
        } else {
          r += func2.value(x(i))
        }
      }
    } else if (peakType == PeakType.PARABOLA) {
      val func_ = new PolynomialFunction(Array[Double](gamma, beta, alpha))
      //val func_ = new GaussianFunction(fittedYShift, fittedIntensity.toDouble, fittedMz, fittedSigmaLeft.toDouble)

      for (i <- 0 until x.length) {
        r += func_.value(x(i))
      }
    } else {
      val func_1 = new GaussianFunction(fittedYShift, fittedIntensity.toDouble, fittedMz, fittedSigmaLeft.toDouble)
      val func_2 = new LorentzianFunction(fittedIntensity.toDouble, fittedMz, fittedSigmaRight.toDouble)
      for (i <- 0 until x.length) {
        if (x(i) <= fittedMz) {
          r += func_1.value(x(i))
        } else {
          r += func_2.value(x(i))
        }
      }
    }
    r.toArray
  }

}

trait IFitter extends DifferentiableMultivariateVectorialFunction {
  def getY(): Array[Double]
  def getX(): Array[Double]
  def optimize(iteration: Int,
               optimizer: AbstractLeastSquaresOptimizer,
               weights: Array[Double]): VectorialPointValuePair
}