package fr.profi.mzdb.utils.math

import org.apache.commons.math3.distribution.NormalDistribution

/**
 * @author Marco
 *
 */
object StatisticsConversion {
  private val dist = new NormalDistribution(0, 1); //mean 0 , std 1
  
  def zscoreToPvalue(aZValue: Double): Double = {    
    val lQuantile = dist.cumulativeProbability(aZValue);
    return 1 - lQuantile;
  }

}