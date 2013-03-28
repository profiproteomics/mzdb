package fr.profi.mzdb.algo.signal.fitting

import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import org.apache.commons.math.optimization.fitting.GaussianParametersGuesser
import org.apache.commons.math.optimization.fitting.WeightedObservedPoint
import org.apache.commons.math.optimization.fitting.ParametricGaussianFunction
import org.apache.commons.math.optimization.fitting.ParametricRealFunction


object GaussFitter {
  val SIGMA_FACTOR = 2.354820045
  val  NB_PARAMETER = 5
}
 
  /* test mz fit or peakel */
class GaussFitter ( @BeanProperty val x: Array[Double], //mz
				    @BeanProperty val y:Array[Double], //intensity
					@BeanProperty var peaks: ArrayBuffer[PeakShape] = new ArrayBuffer[PeakShape]()) //basically peaks detected after smoothing and local maxima detection
					   extends IFitter {
   
	val paramGauss = new ParametricGaussianFunction
	
   /* calculate data */
	def value(variables: Array[Double]): Array[Double] = {
	  val v = new Array[Double](x.length)
	  for (i <- 0 until v.length) {
	    val moz = x(i)
	    var calcInt = 0.0
	    for (j <- 0 until peaks.length) {
	      var vari:Array[Double] = null
	      if (x(i) < variables(2))
	        vari = Array[Double](variables(i * j), variables(i * j + 1), variables(i * j) + 2, variables(i * j + 3))
	      else
	        vari = Array[Double](variables(i * j), variables(i * j + 1), variables(i * j) + 2, variables(i * j + 4))
	      
	      calcInt += paramGauss.value(x(i), vari)
	    }
	    v(i) = calcInt
	  }
	  return v
	}
	
	
	def jacobian(variables: Array[Double] ) : Array[Array[Double]]= {
		val jacobian =  Array.ofDim[Double](x.length, GaussFitter.NB_PARAMETER * peaks.length)		
		for (i <- 0 until x.length) {
		  for (j <- 0 until peaks.length) {
			var jac = jacobian(i).toBuffer
			var vari:Array[Double] = null
		    if (x(i) < variables(2))
		      vari = Array[Double](variables(i * j), variables(i * j + 1), variables(i * j) + 2, variables(i * j + 3))
		    else
		      vari = Array[Double](variables(i * j), variables(i * j + 1), variables(i * j) + 2, variables(i * j + 4))
			jac++ paramGauss.gradient(x(i), vari);
		    jacobian(i) = jac.toArray//paramGauss.gradient(x(i), variables);
		  }
		}
		return jacobian
	}
	
  def jacobian() : MultivariateMatrixFunction = {
		  return new MultivariateMatrixFunction() {
		    
		    def value(point: Array[Double]) : Array[Array[Double]] = {
                return jacobian(point);
		    }
		  };
  }
  
   def optimize(iteration:Int = 1000, 
		   		optimizer:AbstractLeastSquaresOptimizer=new LevenbergMarquardtOptimizer): VectorialPointValuePair = {
     
     val weights = (for (i <- 0 until y.length) yield 1d) toArray
     val maxIdx = y.indexOf(y.max)
     for (i <- maxIdx - 5 to maxIdx + 5) {
       weights(i) += 5d
     }
     
     var optimum : VectorialPointValuePair = null
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
    	 initialGuess += peaks(i).y_shift
    	 initialGuess += peaks(i).intensity
         initialGuess += peaks(i).mz
         initialGuess += peaks(i).sigmaLeft
         initialGuess += peaks(i).sigmaRight
     }
     optimum = optimizer.optimize(this, y, weights, initialGuess.toArray)
     val refPoint = optimum.getPointRef()
     for ( i<- 0 until peaks.length ) {
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