package fr.profi.mzdb.algo.signal.filtering

import mr.go.sgfilter.SGFilterMath3

/**
 * @author David Bouyssie
 *
 */
class SavitzkyGolaySmoother( smoothingConfig: SavitzkyGolaySmoothingConfig ) extends ISignalSmoother {
  
  def smoothTimeIntensityPairs(rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)] = {
    
    val nbPoints = smoothingConfig.nbPoints
    val polyOrder = smoothingConfig.polyOrder
    val times = smoothingConfig.iterationCount
    
    val(nl,nr,order) = (nbPoints,nbPoints,polyOrder)
    val coeffs = SGFilterMath3.computeSGCoefficients(nl,nr,order)

    val sgFilter = new SGFilterMath3(nbPoints,nbPoints)
    var smoothedValues = rtIntPairs.map(_._2)
    val maxBeforeSG = smoothedValues.max
    
    for( i <- 1 to times ) {
      smoothedValues = sgFilter.smooth(smoothedValues,coeffs)
    }
    
    // Re-scale value (they are underestimated after SG filter)      
    //val maxAfterSG = smoothedValues.max
    //val scalingFactor = maxAfterSG / maxBeforeSG
    //val rescaledSmoothedValues = smoothedValues.map( _ * scalingFactor)
  
    val smoothedRtIntPairs = for( i <- rtIntPairs.indices )
      yield rtIntPairs(i)._1 -> smoothedValues(i)
    
    smoothedRtIntPairs.toArray
  }

}

case class SavitzkyGolaySmoothingConfig(
  nbPoints: Int = 5,
  polyOrder: Int = 4,
  iterationCount: Int = 1
) extends ISmoothingConfig