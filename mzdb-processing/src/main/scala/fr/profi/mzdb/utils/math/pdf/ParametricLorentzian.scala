package fr.profi.mzdb.utils.math.pdf

import org.apache.commons.math3.analysis.ParametricUnivariateFunction

/**
 * @author Marco
 * y = a * ( c^2 / ((x -b)^2 + c^2))
 * c = hwhm represents the half width at half maximum
 * fwhm = 2 * hwhm
 */
class ParametricLorentzian extends ParametricUnivariateFunction {
  
  /**
   * return the value with associated params 
   */
  def value(x: Double, params: Double*): Double = {
    val a = params(0)
    val b = params(1)
    val c = params(2)
    val c2 = c * c
    a * (c2 / ((x - b) * (x - b) + c2)) 
  }

  /**
   * return the partial derivatives (to a, b, and c) at point x
   */
  def gradient(x: Double, params: Double*): Array[Double] = {
    val a = params(0)
    val b = params(1)
    val c = params(2)
    val c2 = c * c
    val dx = x - b
    Array[Double](c2 / (dx * dx + c2), 
                  a * ( (2 * c2 * dx)/ math.pow(dx * dx + c2, 2) ), 
                  a * ( (dx * dx * c * (2 - c) ) / math.pow(dx * dx + c2, 2) ) )
  }
}