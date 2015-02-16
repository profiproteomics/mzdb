package fr.profi.mzdb.utils.math.pdf

import math.{ exp, log, sqrt }
import org.apache.commons.math3.analysis.ParametricUnivariateFunction

/**
 * alpha^2x + betax + gamma
 * In case of modelizing apex of gaussain
 * alpha = -1 / 2c^2 where c is sigma
 * beta = b / c^2 where b is deviation
 * gamma = - b^2 / 2c^2 + ln(a) where a is the apex
 */
class ParametricParabola(
  val a: Double,
  val b: Double,
  val c: Double
) extends ParametricUnivariateFunction {

  def this(params: Array[Double]) = {
    this(params(0), params(1), params(3))
  }

  def value(x: Double): Double = {
    val c2 = c * c
    return (-1.0 / (2.0 * c2)) * (x * x) + (b / c2) * x - ((b * b) / (2.0 * c2)) + log(a)
  }

  def value(x: Double, params: Double*): Double = {
    val c2 = c * c
    return -1.0 / 2.0 * c2 * x * x + b / c2 * x - (b * b) / 2.0 * c2 + log(a)
  }

  /**
   * return the partial derivatives (to a, b, and c) at point x
   */
  def gradient(x: Double, parameters: Double*): Array[Double] = {
    val a = parameters(0)
    val b = parameters(1)
    val c = parameters(2)
    Array[Double](1.0 / a, (x - b) / c * c, (x - b) * (x - b) / c * c * c)
  }

}