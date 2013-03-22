package fr.profi.mzdb.algo.signal.fitting

import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.analysis.MultivariateMatrixFunction
import org.apache.commons.math.optimization.general.AbstractLeastSquaresOptimizer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import org.apache.commons.math.optimization.VectorialPointValuePair
import org.apache.commons.math.optimization.fitting.WeightedObservedPoint
import org.apache.commons.math.optimization.fitting.GaussianParametersGuesser


 abstract class GaussLorentzFitting( @BeanProperty val x: Array[Double], //mz
					        @BeanProperty val y:Array[Double], //intensity
					        @BeanProperty var peaks: Array[PeakShape]) //basically peaks detected after smoothing and local maxima detection
					        extends IFitter{
  
  }