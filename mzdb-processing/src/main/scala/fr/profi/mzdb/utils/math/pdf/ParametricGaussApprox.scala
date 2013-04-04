package fr.profi.mzdb.utils.math.pdf
import math.log
import org.apache.commons.math.optimization.fitting.ParametricRealFunction

/**
 * alpha^2x + betax + gamma
 * In case of modelizing apex of gaussain
 * alpha = -1 / 2c^2 where c is sigma
 * beta = b / c^2 where b is deviation
 * gamma = - b^2 / 2c^2 + ln(a) where a is the apex
 * 
 * This is particulary true around the detected apex
 */
class ParametricGaussApprox extends ParametricRealFunction {
  
  /**
   * return the value with associated params 
   */
  def value(x: Double, params: Array[Double]): Double = {
    val a = params(0)
    val b = params(1)
    val c = params(2)
    val c2 = c * c
    return (-1.0 / (2.0 * c2)) * (x * x) + (b / c2) * x - ((b * b) / (2.0 * c2)) + log(a)
  }

  /**
   * return the partial derivatives (to a, b, and c) at point x
   */
  def gradient(x: Double, params: Array[Double]): Array[Double] = {
    val a = params(0)
    val b = params(1)
    val c = params(2)
    Array[Double](1.0 / a, (x - b) / (c * c), ((x - b) * (x - b)) / (c * c * c))
  }

}