package fr.profi.mzdb.algo.signal.fitting

import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair

/** Here we consider that lwhm and rwhm are the same */
class PolyFitter ( @BeanProperty val x: Array[Double], //mz
				   @BeanProperty val y:Array[Double], //intensity
				   @BeanProperty var peaks: ArrayBuffer[PeakShape]= new ArrayBuffer[PeakShape]()) //basically peaks detected after smoothing and local maxima detection
				   extends IFitter {//DifferentiableMultivariateVectorialFunction {
    /* calculate data */
	def value(variables: Array[Double]): Array[Double] = {
	  val v = new Array[Double](x.length)
	  
	  for (i <- 0 until v.length) {
	    val moz = x(i)
	    var calcInt = 0.0
	    for (j <- 0 until peaks.length) {
	    	val (a, b, c) = (variables(i * j), variables(i * j + 1), variables(i * j + 2))
	    	//val sigma = c//if (moz <= b) c else d
	    	//val sigma2 = sigma * sigma
	    	calcInt += a * moz * moz + b * moz + c//(-1.0 / (2.0 * sigma2)) * moz * moz + (b / sigma2) * moz - (b * b) / (2.0 * sigma2) + math.log(a)
	    }
	    v(i) = calcInt
	  }
	  return v
	}
	
	
	def calcJacobian(variables: Array[Double] ) : Array[Array[Double]]= {
		val jacobian =  Array.ofDim[Double](x.length, 3 * peaks.length)		
		for (i <- 0 until x.length) {
		  for (j <- 0 until peaks.length) {
			  val (a, b, c) = (variables(i * j), variables(i * j + 1), variables(i * j + 2))
			  //val sigma = c//if (x(i) <= b) c else d
		      //val sigma2 = sigma * sigma
		      val dx = x(i) - b
			  jacobian(i)(i * j) = x(i) * x(i)//1.0 / a
			  jacobian(i)(i * j + 1) = x(i) //dx / sigma2
			  jacobian(i)(i * j + 2) =  1.0//(dx * dx) / (sigma2 * sigma)
			  //jacobian(i)(i * j + 3) = if (x(i) > b) dx * dx / (sigma2 * sigma) else 0
		  }
		}
		return jacobian
	}

	/** interface method of DifferentiableMultivariateVectorialFunction */
  def jacobian() : MultivariateMatrixFunction = {
		  return new MultivariateMatrixFunction() {
		    
		    def value(point: Array[Double]) : Array[Array[Double]] = {
                return calcJacobian(point);
		    }
		  };
  }
  
  def optimize(iteration:Int = 1000, 
		   		optimizer:AbstractLeastSquaresOptimizer=new LevenbergMarquardtOptimizer): VectorialPointValuePair = {
     var weights = (for (i <- 0 until y.length) yield 1d) toArray
     val maxIdx = y.indexOf(y.max)
     for ( i <- maxIdx - 5 until maxIdx + 5) {
       weights(i) += 10.0d
     }
     
     var optimum : VectorialPointValuePair = null
     if (peaks.isEmpty) {
       val sigmaApprox =  (x.last - x.first) / 4.0
       peaks += new PeakShape(y_shift = 0, 
               		   		  mz = x(y.indexOf(y.max)), 
    		   				  intensity = y.max toFloat, 
    		   				  sigmaLeft = sigmaApprox toFloat,
    		   				  sigmaRight = sigmaApprox toFloat)
     } 
    var initialGuess = new ArrayBuffer[Double]()
     for (i <- 0 until peaks.length) {
    	 initialGuess += 1.0 //peaks(i).intensity
         initialGuess += 1.0 //peaks(i).mz
         initialGuess += 1.0 //peaks(i).sigmaLeft
         //initialGuess += peaks(i).sigmaRight
     }
     optimum = optimizer.optimize(this, y, weights.toArray, initialGuess.toArray)
     val refPoint = optimum.getPointRef()
     for ( i<- 0 until peaks.length ) {
       peaks(i).setPeakType(PeakType.PARABOLA)
       peaks(i).alpha = refPoint(i)
       peaks(i).beta = refPoint(i + 1)
       peaks(i).gamma = refPoint(i + 2)
       peaks(i).fittedSigmaLeft = math.sqrt(-1.0d / 2.0d * peaks(i).alpha).toFloat
       peaks(i).fittedSigmaRight = math.sqrt(-1.0d / 2.0d * peaks(i).alpha).toFloat
       peaks(i).fittedMz = -peaks(i).beta / 2.0 * peaks(i).alpha
       peaks(i).fittedIntensity = (peaks(i).beta * peaks(i).beta - 4 * peaks(i).alpha * peaks(i).gamma) / (4 * peaks(i).alpha) toFloat
     }
     optimum
   }
}