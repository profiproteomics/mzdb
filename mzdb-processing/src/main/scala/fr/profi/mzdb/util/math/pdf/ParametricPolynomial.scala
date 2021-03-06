package fr.profi.mzdb.util.math.pdf

import org.apache.commons.math3.analysis.ParametricUnivariateFunction

/**
 * @author Marco
 * have to defined there is a general implementation in commons math
 * but it is private, and inaccessible
 *
 */
class ParametricPolynomial extends ParametricUnivariateFunction {

  def gradient(x: Double, parameters: Double*): Array[Double] = {
    val gradient = new Array[Double](parameters.length);
    var xn = 1.0d;
    for (i <- 0 until parameters.length) {
      gradient(i) = xn;
      xn *= x;
    }
    gradient;
  }

  def value(x: Double, parameters: Double*): Double = {
    var y = 0d;
    for (i <- parameters.length - 1 to 0 by -1) {
      y = y * x + parameters(i);
    }
    y;
  }
}
    
/*
class ParametricPoly2 extends ParametricUnivariateFunction {

  def gradient(x:Double, params: Double*) : Array[Double] = {
    val grad = new Array[Double](params.length)
    grad(0) = 1.0
    grad(1) = x
    grad(2) = x * x
    grad
  }
  
  def value(x:Double, params: Double*): Double = {
    val a = params(0)
    val b = params(1)
    val c = params(2)
    c * x * x + b * x + a
  }

}*/