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
    val peaksCount = rtIntPairs.length
    var smoothedValues = new Array[Double](peaksCount)
    
    var i = 0
    while (i < peaksCount) {
      smoothedValues(i) = rtIntPairs(i)._2
      i += 1
    }
    //val maxBeforeSG = smoothedValues.max
    
    var time = 1
    while( time <= times ) {
      smoothedValues = sgFilter.smooth(smoothedValues,coeffs)
      time += 1
    }
    
    // Re-scale value (they are underestimated after SG filter)
    //val maxAfterSG = smoothedValues.max
    //val scalingFactor = maxAfterSG / maxBeforeSG
    //val rescaledSmoothedValues = smoothedValues.map( _ * scalingFactor)
  
    val smoothedRtIntPairs = new Array[(Float,Double)](peaksCount)
    
    i = 0
    while (i < peaksCount) {
      smoothedRtIntPairs(i) = (rtIntPairs(i)._1, smoothedValues(i))
      i += 1
    }
    
    smoothedRtIntPairs
  }

}

case class SavitzkyGolaySmoothingConfig(
  nbPoints: Int = 5,
  polyOrder: Int = 4,
  iterationCount: Int = 1
) extends ISmoothingConfig