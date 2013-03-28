package fr.profi.mzdb.algo.signal.fitting

import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import org.apache.commons.math.optimization.fitting.WeightedObservedPoint
import org.apache.commons.math.optimization.fitting.GaussianParametersGuesser

/** Here we consider that lwhm and rwhm are the same */
class PolyFitter(@BeanProperty val x: Array[Double], //mz
                 @BeanProperty val y: Array[Double], //intensity
                 @BeanProperty var peaks: ArrayBuffer[PeakShape] = new ArrayBuffer[PeakShape]()) //basically peaks detected after smoothing and local maxima detection
  extends IFitter { //DifferentiableMultivariateVectorialFunction {
  /* calculate data */
  def value(variables: Array[Double]): Array[Double] = {
    val v = new Array[Double](x.length)

    for (i <- 0 until v.length) {
      val moz = x(i)
      var calcInt = 0.0
      for (j <- 0 until peaks.length) {
        val (a, b, c) = (variables(i * j), variables(i * j + 1), variables(i * j + 2))//, variables(i * j + 3))
        calcInt += a * moz * moz + b * moz + c //(-1.0 / 2.0 * sigma2) * (moz * moz) + (b / sigma2) * moz - (b * b) / (2.0 * sigma2) + math.log(a) //
      }
      v(i) = calcInt
    }
    return v
  }

  def calcJacobian(variables: Array[Double]): Array[Array[Double]] = {
    val jacobian = Array.ofDim[Double](x.length, 3 * peaks.length)
    for (i <- 0 until x.length) {
      for (j <- 0 until peaks.length) {
        val (a, b, c) = (variables(i * j), variables(i * j + 1), variables(i * j + 2))
        jacobian(i)(i * j) = x(i) * x(i)
        jacobian(i)(i * j + 1) = x(i)
        jacobian(i)(i * j + 2) = 1.0
      }
    }
    return jacobian
  }

  /** interface method of DifferentiableMultivariateVectorialFunction */
  def jacobian(): MultivariateMatrixFunction = {
    return new MultivariateMatrixFunction() {

      def value(point: Array[Double]): Array[Array[Double]] = {
        return calcJacobian(point);
      }
    };
  }

  def optimize(iteration: Int = 1000,
               optimizer: AbstractLeastSquaresOptimizer = new LevenbergMarquardtOptimizer): VectorialPointValuePair = {
    var weights = (for (i <- 0 until y.length) yield 1d) toArray
    /*val maxIdx = y.indexOf(y.max)
    for (i <- maxIdx - 5 until maxIdx + 5) {
      weights(i) += 10.0d
    }*/

    var optimum: VectorialPointValuePair = null
    if (peaks.isEmpty) {
      val observations = x.zip(y) map { case (x_, y_) => new WeightedObservedPoint(1d, x_, y_) } toArray
      val initGuess = new GaussianParametersGuesser(observations).guess()
      peaks += new PeakShape(initGuess(0),
        initGuess(1),
        initGuess(2) toFloat,
        initGuess(3) toFloat,
        initGuess(3) toFloat)
    }
    var initialGuess = new ArrayBuffer[Double]()
    for (i <- 0 until peaks.length) {
      //initialGuess += peaks(i).y_shift
      initialGuess += - 1.0 /  ( 2.0 * peaks(i).sigmaRight * peaks(i).sigmaRight)//peaks(i).intensity
      initialGuess += peaks(i).mz / (peaks(i).sigmaRight * peaks(i).sigmaRight)
      initialGuess += peaks(i).sigmaRight
      //initialGuess += peaks(i).sigmaRight
    }
    optimum = optimizer.optimize(this, y, weights, initialGuess.toArray)
    val refPoint = optimum.getPointRef()
    for (i <- 0 until peaks.length) {
      val c = refPoint(i * 4 + 2).toFloat
      val b = refPoint(i * 4 + 1)
      val a = refPoint(i * 4)
      
      //println("" + a + "\t" + b + "\t" + c)
      peaks(i).setAlpha(a )
      peaks(i).setBeta( b)
      peaks(i).setGamma(c)
      peaks(i).setPeakType(PeakType.PARABOLA)
      peaks(i).fittedIntensity = (- ( (b * b) - 4 * a * c )  / (4 * a)).toFloat //refPoint(i * 4).toFloat
      peaks(i).fittedMz = - b / (2 * a)
      val d1 = (b * b) - 4 * a * c
      val d2 = (b * b) - 4 * a *( c + d1 / (8 * a) )
      val x1 = (-b - math.sqrt(d2)) / (2 * a )
      val x2 = (-b + math.sqrt(d2)) / (2 * a )
      val sigma = math.abs(x1 - x2) / 2.35482
      println("" + peaks(i).fittedIntensity + "\t" + peaks(i).fittedMz + "\t" + sigma)
      peaks(i).fittedSigmaLeft = sigma.toFloat//0.0//refPoint(i * 4 + 2).toFloat
      peaks(i).fittedSigmaRight = sigma.toFloat//refPoint(i * 4 + 2).toFloat
    }
    optimum
  }
}