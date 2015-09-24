package fr.profi.mzdb.utils.math.pdf

import scala.beans.BeanProperty
import org.apache.commons.math3.analysis.UnivariateFunction

/**
 * @author Marco
 *
 */
class LorentzianFunction(
  @BeanProperty val intensity:Double, 
  @BeanProperty val center:Double, 
  @BeanProperty val demiWidth:Double
) extends UnivariateFunction {
  
  def this(params:Array[Double]) {
    this(params(0), params(1), params(3))
  }
  
  /** a * ( c^2 / ((x -b)^2 + c^2)) */
  def value(x: Double) :Double = {
        val dx = x - demiWidth;
        val c2 = demiWidth * demiWidth
        intensity * (c2 / ( dx * dx + c2))
    }

}