package fr.profi.mzdb.algo.signal.detection

import mr.go.sgfilter.SGFilter
import fr.profi.mzdb.model.IPeakelData
import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
case class SmoothingConfig(
  nbPoints: Int = 5,
  polyOrder: Int = 4,
  times: Int = 3
)

trait IPeakelFinder {
  
  def findPeakelsIndices(peaks: Seq[Peak]): Array[Tuple2[Int,Int]]
  def findPeakelsIndices(peakel: IPeakelData): Array[Tuple2[Int,Int]]
  
  protected def smoothValues(values: Array[Double], smoothingConfig: SmoothingConfig ): Array[Double] = {

    val nbPoints = smoothingConfig.nbPoints
    val polyOrder = smoothingConfig.polyOrder
    val times = smoothingConfig.times
      
    val(nl,nr,order) = (nbPoints,nbPoints,polyOrder)
    val coeffs = SGFilter.computeSGCoefficients(nl,nr,order)

    val sgFilter = new SGFilter(nbPoints,nbPoints)
    var smoothedValues = values
    for( i <- 1 to times ) {
      smoothedValues = sgFilter.smooth(smoothedValues,coeffs)
    }
    
    smoothedValues
  }

}