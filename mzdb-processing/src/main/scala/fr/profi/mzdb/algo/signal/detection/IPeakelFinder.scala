package fr.profi.mzdb.algo.signal.detection

import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
trait IPeakelFinder {
  
  def findPeakelsIndices(peaks: Seq[Peak] ): Array[Tuple2[Int,Int]]
  
  protected def smoothValues(values: Array[Double], times: Int = 3 ): Array[Double] = {
    
    import mr.go.sgfilter.SGFilter
    
    // TODO: static values ???
    val(nl,nr,order) = (5,5,4)
    val coeffs = SGFilter.computeSGCoefficients(nl,nr,order)

    val sgFilter = new SGFilter(5,5)
    var smoothedValues = values
    for( i <- 1 to times ) {
      smoothedValues = sgFilter.smooth(smoothedValues,coeffs)
    }
    
    smoothedValues
  }

}