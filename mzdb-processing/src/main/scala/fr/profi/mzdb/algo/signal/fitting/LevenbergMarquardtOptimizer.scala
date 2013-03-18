package fr.profi.mzdb.algo.signal.fitting
 import scala.collection.JavaConversions._
import org.apache.commons.math.analysis.DifferentiableMultivariateVectorialFunction
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import scala.reflect.BeanProperty
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer

case class PeakShape (var mz:Double, 
					  var intensity:Float,
					  var lwhm:Float = 0.05f, 
					  var rwhm:Float = 0.05f) {}

trait IFittingType extends DifferentiableMultivariateVectorialFunction {
  def getY(): Array[Double]
  def getX(): Array[Double]
}

/* basically this used to fit mz Peak */
class ParabolaFitting ( @BeanProperty val x: Array[Double], //mz
					    @BeanProperty val y:Array[Double], //intensity
					    @BeanProperty var peaks: Array[PeakShape]) //basically peaks detected after smoothing and local maxima detection
					    extends IFittingType {//DifferentiableMultivariateVectorialFunction {
    /* calculate data */
	def value(variables: Array[Double]): Array[Double] = {
	  val v = new Array[Double](x.length)
	  
	  for (i <- 0 until v.length) {
	    val moz = x(i)
	    var calcInt = 0.0
	    for (j <- 0 until peaks.length) {
	    	val (a, b, c) = (variables(i * j), variables(i * j + 1), variables(i * j + 2));
	    	calcInt += -1 / 2 * c*c * moz * moz + b / c*c* moz - (b * b) / 2 * c*c + math.log(a)
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
			  jacobian(i)(i * j) = 1. / a
			  jacobian(i)(i * j + 1) = (x(i) - b) / (c * c)
			  jacobian(i)(i * j + 2) = math.pow(( x(i) -  b), 2) / math.pow(c, 3)
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
}

  /* test mz fit or peakel */
 class GaussianFitting ( @BeanProperty val x: Array[Double], //mz
					      @BeanProperty val y:Array[Double], //intensity
					      @BeanProperty var peaks: Array[PeakShape]) //basically peaks detected after smoothing and local maxima detection
					      extends IFittingType {
    
    /* calculate data */
	def value(variables: Array[Double]): Array[Double] = {
	  val v = new Array[Double](x.length)

	  for (i <- 0 until v.length) {
	    val moz = x(i)
	    var calcInt = 0.0
	    for (j <- 0 until peaks.length) {
       	  val (a, b, c, d) = (variables(i * j), variables(i * j + 1), variables( i *j + 2), variables(i * j + 3));
       	  val sigma = if (x(i) < b) 2 * c else 2 * d
	      calcInt += a * math.exp(- math.pow( x(i) - b , 2) / 2 * sigma * sigma)
	    }
	    v(i) = calcInt
	  }
	  return v
	}
	
	
	def jacobian(variables: Array[Double] ) : Array[Array[Double]]= {
		val jacobian =  Array.ofDim[Double](x.length, 3 * peaks.length)		
		for (i <- 0 until x.length) {
		  for (j <- 0 until peaks.length) {
			  val (a, b, c, d) = (variables(i * j), variables(i * j + 1), variables(i * j + 2), variables(i * j + 3))
			  val sigma = if (x(i) < b) 2 * c else 2 * d
			  val dfdy = math.exp(- math.pow( x(i) - b, 2) / 2 * sigma * sigma)
			  jacobian(i)(i * j) = dfdy
			  jacobian(i)(i * j + 1) = a * (x(i) - b) / sigma * sigma * dfdy  
			  jacobian(i)(i * j + 2) = if (x(i) < b) a * math.pow(( x(i) -  b), 2) / math.pow(sigma, 3) else 0
			  jacobian(i)(i * j + 3) = if (x(i) >= b) a * math.pow(( x(i) -  b), 2) / math.pow(sigma, 3) else 0
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
  }
 
 
 //the idea is to fit on the first part a gaussian and on the second part a lorentzian
 //this would be particulary adapted for fitting peakels
 abstract class GaussLorentzFitting( @BeanProperty val x: Array[Double], //mz
					        @BeanProperty val y:Array[Double], //intensity
					        @BeanProperty var peaks: Array[PeakShape]) //basically peaks detected after smoothing and local maxima detection
					        extends IFittingType{
  
  }
   
 

 object Optimizer {
    
     def optimize(iteration: Int = 100, 
    		 	  optimizer:AbstractLeastSquaresOptimizer = new LevenbergMarquardtOptimizer, // could be GaussNewton to but LM generally is more performant
    		 	  function: IFittingType, 
    		 	  weights:Array[Double], // may put more wheigths on points around the apex !
    		 	  initialSolution: Array[Double]): VectorialPointValuePair = {

       val optimum = optimizer.optimize(function, function.getY, weights, initialSolution)
       optimum
     }
  }
    

