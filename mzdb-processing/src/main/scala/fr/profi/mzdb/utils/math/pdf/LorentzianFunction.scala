/**
 *
 */
package fr.profi.mzdb.utils.math.pdf

import org.apache.commons.math.analysis.DifferentiableUnivariateRealFunction
import scala.beans.BeanProperty

/**
 * @author Marco
 *
 */
class LorentzianFunction (@BeanProperty val intensity:Double, 
                          @BeanProperty val center:Double, 
                          @BeanProperty val demiWidth:Double) {
  
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