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
					  var lwhm:Float, 
					  var rwhm:Float) {}


class ParabolaFitting ( @BeanProperty var x: Array[Double], 
					    @BeanProperty var y:Array[Double], 
					    @BeanProperty var peaks: Array[PeakShape]) extends DifferentiableMultivariateVectorialFunction {
    /* calculate data */
	def value(variables: Array[Double]): Array[Double] = {
	  val v = new Array[Double](x.length)
	  val (a, b, c) = (variables(0), variables(1), variables(2));
	  val c_squared = c * c
	  
	  for (i <- 0 to v.length) {
	    val moz = x(i)
	    v(i) = -1 / 2 * c_squared * moz * moz + b / c_squared * moz - (b * b) / 2 * c_squared + math.log(a) 
	  }
	  return v
	}
	
	
	def jacobian(variables: Array[Double] ) : Array[Array[Double]]= {
		val jacobian =  Array.ofDim[Double](x.length, 3)
		val (a, b, c) = (variables(0), variables(1), variables(2))
		
		for (i <- 0 to x.length) {
		  jacobian(i)(0) = 1. / a
		  jacobian(i)(1) = (x(i) - b) / (c * c)
		  jacobian(i)(2) = math.pow(( x(i) -  b), 2) / math.pow(c, 3)
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

  
  object Optimizer {
    
     def optimize(iteration: Int = 100, 
    		 	  optimizer:AbstractLeastSquaresOptimizer = new LevenbergMarquardtOptimizer, 
    		 	  function: ParabolaFitting, //DifferentiableMultivariateVectorialFunction, 
    		 	  weights:Array[Double], 
    		 	  initialSolution: Array[Double]): VectorialPointValuePair = {
       /*public VectorialPointValuePair optimize(final DifferentiableMultivariateVectorialFunction f,
                                            final double[] target, final double[] weights,
                                            final double[] startPoint)*/
       
       val optimum = optimizer.optimize(function, function.getY, weights, initialSolution)
       //optimizer.getConvergenceChecker().
       optimum
     }
  }
    

}