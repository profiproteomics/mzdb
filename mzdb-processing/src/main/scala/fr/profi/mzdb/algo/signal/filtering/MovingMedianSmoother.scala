package fr.profi.mzdb.algo.signal.filtering

import fr.profi.util.math.getMedianObject
import fr.profi.util.math.median

/**
 * @author bouyssie
 *
 */
class MovingMedianSmoother {
  
  def smoothTimeIntensityPairs(rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)] = {
    
    // Applying a moving median filter while keeping first and last values
    val filteredRtIntPairs = Array(rtIntPairs.head) ++ rtIntPairs.sliding(3).map { buffer =>
      getMedianObject( buffer, { (a: (Float,Double), b: (Float,Double)) =>
        a._2 > b._2
      } )
    } ++ Array(rtIntPairs.last)
    require( filteredRtIntPairs.length == rtIntPairs.length, "filtered array must have the same length than unfiltered one")
    
    filteredRtIntPairs
  }
  
  /*def smoothIntensities(intensities: Array[Double] ): Array[Double] = {
    
    // Applying a moving median filter while keeping first and last values
    val filteredIntensities = Array(intensities.head) ++ intensities.sliding(3).map { buffer =>
      median( buffer )
    } ++ Array(intensities.last)
    require( filteredIntensities.length == intensities.length, "filtered array must have the same length than unfiltered one")
    
    filteredIntensities
  }*/

}