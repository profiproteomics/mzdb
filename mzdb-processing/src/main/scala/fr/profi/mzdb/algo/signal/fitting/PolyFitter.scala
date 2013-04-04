package fr.profi.mzdb.algo.signal.fitting

import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import org.apache.commons.math.optimization.fitting.WeightedObservedPoint
import org.apache.commons.math.optimization.fitting.GaussianParametersGuesser
import org.apache.commons.math.analysis.DifferentiableMultivariateVectorialFunction
import fr.profi.mzdb.utils.math.pdf.ParametricPolynomial

/** Here we consider that lwhm and rwhm are the same => update :Nope we fit 2 parabola...
 *  do not know if it is necessary...but we try it*/
class PolyFitter(@BeanProperty val x: Array[Double], //xdata
                 @BeanProperty val y: Array[Double], //intensity
                 @BeanProperty var peaks: ArrayBuffer[PeakShape] = new ArrayBuffer[PeakShape]()) //basically peaks detected after smoothing and local maxima detection
  extends IFitter {

  val poly = new ParametricPolynomial

  def value(variables: Array[Double]): Array[Double] = {
    val v = new Array[Double](x.length)
    for (i <- 0 until v.length) {
      val moz = x(i)
      var calcInt = 0.0
      for (j <- 0 until peaks.length) {
        var (c, b, a) = (variables(i * j), variables(i * j + 1), 
                         variables(i * j + 2))//, variables(i * j + 3), 
                                     //variables(i * j + 4), variables(i * j + 5))
        //val apex = -b / (2 * a)
        //if (moz < apex) {
          calcInt += poly.value(moz,  Array[Double](c, b, a))
        /*} else {
          calcInt += poly.value(moz, Array[Double](cp, bp, ap))
        }*/
      }
      v(i) = calcInt
    }
    return v
  }

  def jacobian(variables: Array[Double]): Array[Array[Double]] = {
    val jacobian = Array.ofDim[Double](x.length, 3 * peaks.length)
    for (i <- 0 until x.length) {
      val moz = x(i)
      for (j <- 0 until peaks.length) {
        var (c, b, a) = (variables(i * j), variables(i * j + 1), 
                         variables(i * j + 2))//, variables(i * j + 3),
                                     //variables(i * j + 4), variables(i * j + 5))
        var params = new ArrayBuffer[Double](6)
        /*val apex = -b / (2 * a)
        if (moz < apex) {
          params = params ++ poly.gradient(moz, Array[Double](c, b, a)) 
          params = params ++ Array[Double](0d, 0d, 0d)
        } else {
          params = params ++ Array[Double](0d, 0d, 0d) 
          params = params ++ poly.gradient(moz, Array[Double](cp, bp, ap))  
        }*/
        jacobian(i) = poly.gradient(moz, Array[Double](c, b, a))
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
      //at the beginning sigmaLeft = sigmaRight
      peaks += new PeakShape(y_shift = initGuess(0),
                             intensity = initGuess(1) toFloat,
                             mz = initGuess(2),
                             sigmaLeft = initGuess(3) toFloat,
                             sigmaRight = initGuess(3) toFloat)
    }
    var initialGuess = new ArrayBuffer[Double]()
    for (i <- 0 until peaks.length) {
      val sigmaConsensus = (peaks(i).sigmaLeft + peaks(i).sigmaRight) / 2.0
      val sigmaConsensus2 = sigmaConsensus * sigmaConsensus
      //for (j <- 0 until 2) {
        initialGuess += 0d //- (peaks(i).mz * peaks(i).mz) / (2.0 * sigmaConsensus2) + math.log(peaks(i).intensity)
        initialGuess += 0d //peaks(i).mz / sigmaConsensus2
        initialGuess += 0d //- 1.0 / (2.0 * sigmaConsensus2)
      //}
    }
    
    optimum = optimizer.optimize(this, y, weights, initialGuess.toArray)
    
    val refPoint = optimum.getPointRef()
    for (i <- 0 until peaks.length) {
      val c = refPoint(i * 3) // cst term
      val b = refPoint(i * 3 + 1) // x term
      val a = refPoint(i * 3 + 2) // x2 term
      /*val cp = refPoint(i * 6 + 3)
      val bp = refPoint(i * 6 + 4)
      val ap = refPoint(i * 6 + 5)*/

      peaks(i).setAlpha(a)
      peaks(i).setBeta(b)
      peaks(i).setGamma(c)
      
      /*peaks(i).setAlpha2(ap)
      peaks(i).setBeta2(bp)
      peaks(i).setGamma2(cp)*/
      
      peaks(i).setPeakType(PeakType.PARABOLA)
      
      //to the first parabola, may do the mean with the second par
      //val (b2, bp2) = (b * b, bp * bp)
      val b2 = b * b
      val d1 = b2 - 4 * a * c
      //val d2 = bp2 - (4 * ap * cp)
      peaks(i).fittedIntensity = (- d1 / (4 * a)).toFloat 
      peaks(i).fittedMz = -b / (2 * a)

      val d3 = b2 - 4 * a * (c + d1 / (8 * a))
      //val d4 = bp2 - 4 * ap * ( cp + d2 / (8 * ap))
      val x1 = (-b - math.sqrt(d3)) / (2 * a)
      val x2 = (-b + math.sqrt(d3)) / (2 * a)
      
      val sigma = math.abs(x1 - x2) / 2.35482
      peaks(i).fittedSigmaLeft = sigma.toFloat 
      peaks(i).fittedSigmaRight = sigma.toFloat 
    }
    optimum
  }

}