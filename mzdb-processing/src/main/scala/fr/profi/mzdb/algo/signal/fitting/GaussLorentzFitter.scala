package fr.profi.mzdb.algo.signal.fitting

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import org.apache.commons.math.optimization.fitting.WeightedObservedPoint
import org.apache.commons.math.optimization.fitting.GaussianParametersGuesser
import org.apache.commons.math.optimization.fitting.ParametricGaussianFunction
import fr.profi.mzdb.utils.math.pdf.ParametricLorentzian

class GaussLorentzFitter(@BeanProperty val x: Array[Double], //mz
                          @BeanProperty val y: Array[Double], //intensity
                          @BeanProperty var peaks: ArrayBuffer[PeakShape] = new ArrayBuffer[PeakShape]) extends IFitter {
  val gauss = new ParametricGaussianFunction
  val lorentz = new ParametricLorentzian

  def value(variables: Array[Double]): Array[Double] = {
    val v = new Array[Double](x.length)
    for (i <- 0 until v.length) {
      val moz = x(i)
      var calcInt = 0.0
      for (j <- 0 until peaks.length) {
        val (a, b, c, d, e) = (variables(i * j), variables(i * j + 1), 
                               variables(i * j + 2), variables(i * j + 3), 
                               variables(i * j + 4))
        if (x(i) < c)
          calcInt += gauss.value(x(i), Array[Double](a, b, c, d))
        else
          calcInt += lorentz.value(x(i), Array[Double](b, c, e))
      }
      v(i) = calcInt
    }
    return v
  }

  def jacobian(variables: Array[Double]): Array[Array[Double]] = {
    val jacobian = Array.ofDim[Double](x.length, GaussFitter.NB_PARAMETER * peaks.length)
    for (i <- 0 until x.length) {
      for (j <- 0 until peaks.length) {
        val (a, b, c, d, e) = (variables(i * j), variables(i * j + 1), variables(i * j + 2), variables(i * j + 3), variables(i * j + 4))
        var params = new ArrayBuffer[Double](5)
        if (x(i) < c) {
          params = params ++ gauss.gradient(x(i), Array[Double](a, b, c, d))
          params += 0d
        } else {
          params = params ++ lorentz.gradient(x(i), Array[Double](b, c, e))
          params.insert(0, 0d)
          params.insert(3, 0d)
        }
        jacobian(i) = params.toArray
      }
    }
    return jacobian
  }

  def jacobian(): MultivariateMatrixFunction = {
    return new MultivariateMatrixFunction() {

      def value(point: Array[Double]): Array[Array[Double]] = {
        return jacobian(point);
      }
    };
  }

  def optimize(iteration: Int = 1000,
               optimizer: AbstractLeastSquaresOptimizer = new LevenbergMarquardtOptimizer,
               weights: Array[Double] = (for (i <- 0 until y.length) yield 1d) toArray): VectorialPointValuePair = {

    var optimum: VectorialPointValuePair = null
    if (peaks.isEmpty) {
      val observations = x.zip(y) map { case (x_, y_) => new WeightedObservedPoint(1d, x_, y_) } toArray
      val initGuess = new GaussianParametersGuesser(observations).guess()
      peaks += new PeakShape(y_shift = initGuess(0),
                             mz = initGuess(2),
                             intensity = initGuess(1) toFloat,
                             sigmaLeft = initGuess(3) toFloat,
                             sigmaRight = initGuess(3) toFloat)
    }
    var initialGuess = new ArrayBuffer[Double]()
    for (p <- peaks) {
      initialGuess += p.y_shift
      initialGuess += p.intensity
      initialGuess += p.mz
      initialGuess += p.sigmaLeft
      initialGuess += p.sigmaRight
    }

    optimum = optimizer.optimize(this, y, weights, initialGuess.toArray)

    val refPoint = optimum.getPointRef()
    for (i <- 0 until peaks.length) {
      peaks(i).setPeakType(PeakType.GAUSS)
      peaks(i).fittedYShift = refPoint(i * GaussFitter.NB_PARAMETER)
      peaks(i).fittedIntensity = refPoint(i * GaussFitter.NB_PARAMETER + 1).toFloat
      peaks(i).fittedMz = refPoint(i * GaussFitter.NB_PARAMETER + 2)
      peaks(i).fittedSigmaLeft = refPoint(i * GaussFitter.NB_PARAMETER + 3).toFloat
      peaks(i).fittedSigmaRight = refPoint(i * GaussFitter.NB_PARAMETER + 4).toFloat
    }
    optimum
  }

}