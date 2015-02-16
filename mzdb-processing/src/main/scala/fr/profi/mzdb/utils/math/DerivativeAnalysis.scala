package fr.profi.mzdb.utils.math

object DerivativeAnalysis {
  
  def calcTernarySlopes( values: Array[Double], derivativeLevel: Int ): Array[Double] = {

    val signums = values.sliding(2).map { buffer =>
      val diff = (buffer(1) - buffer(0))
      if( diff == 0 ) 0.0 else diff.signum.toDouble
    } toArray
    
    if( derivativeLevel == 1 ) signums
    else calcTernarySlopes(signums, derivativeLevel - 1)
  }

}