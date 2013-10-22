/**
 *
 */
package fr.profi.mzdb.utils.math

import org.apache.commons.math.distribution.NormalDistributionImpl
import org.apache.commons.math.MathException

/**
 * @author Marco
 *
 */
object StatisticsConversion {
  private val dist = new NormalDistributionImpl(0, 1); //mean 0 , std 1
  
  def zscoreToPvalue(aZValue: Double): Double = {
    
        var lQuantile = 0d;
        try {
            lQuantile = dist.cumulativeProbability(aZValue);
        } catch {
          case me :MathException => me.printStackTrace()
        }
        
        return 1 - lQuantile;
    }

}