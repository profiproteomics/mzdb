package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.algo.signal.filtering._
import fr.profi.mzdb.model.IPeakelData
import fr.profi.mzdb.model.Peak
import fr.profi.util.stat._
import fr.profi.mzdb.utils.math.DerivativeAnalysis

/**
 * @author David Bouyssie
 *
 */
object SmartPeakelFinder extends IPeakelFinder {
  
  var minPeaksCount = 5
  var miniMaxiDistanceThresh = 3
  var expectedBinDataPointsCount = 10
  var maxOscillationFactor = 10
  
  // gapTolerance set to 1 means that ponctual intensity hole won't be removed
  val baselineRemover = new BaselineRemover( gapTolerance = 1 )
  val psgSmoother = new PartialSavitzkyGolaySmoother(SavitzkyGolaySmoothingConfig(iterationCount = 1))
  
  def findPeakelsIndices(rtIntPairs: Array[(Float,Double)] ): Array[(Int,Int)] = {
    
    // Check we have at least 5 peaks before the filtering
    val peaksCount = rtIntPairs.length
    if( peaksCount < minPeaksCount ) return Array()
    
    // Compute the oscillation factor
    val oscillationFactor = calcOscillationFactor(rtIntPairs)
    
    // If the oscillationFactor is high, the SavitskyGolay filter will not provide a good result
    if( oscillationFactor >= maxOscillationFactor ) {
      // TODO: should we apply the HistogramBasedPeakelFinder ???
      
      // Then we only apply a baseline filter
      val noiseThreshold = baselineRemover.calcNoiseThreshold(rtIntPairs)
      return baselineRemover.findNoiseFreePeakGroupsIndices(rtIntPairs, noiseThreshold)
    }
    
    // Smooth intensities
    val smoothedRtIntPairs = psgSmoother.smoothTimeIntensityPairs(rtIntPairs)
    val smoothedIntensities = smoothedRtIntPairs.map(_._2)
    
    // Look for significant minima and maxima in the smoothed signal
    val miniMaxi = DerivativeAnalysis.findSignificantMiniMaxi(
      smoothedIntensities,
      miniMaxiDistanceThresh,
      0.66f
    )
    
    // Return empty array of no mini/maxi found
    if( miniMaxi.isEmpty ) {
      //println(smoothedIntensities.toArray)
      return Array()
    }
    
    // Convert mini/maxi into peakel indices
    val tmpPeakelsIndices = miniMaxi.filter(_.isMaximum == false).sliding(2).map { buffer =>
      buffer(0).index -> buffer(1).index
    }
    
    // Refine peakels using BaselineRemover algorithm
    val refinedPeakelsIndices = tmpPeakelsIndices.map { tmpPeakelIndices =>
      
      // Retrieve peakel time/intensity pairs
      val( firstIndex, lastIndex ) = tmpPeakelIndices
      val peakelRtIntPairs = rtIntPairs.slice( firstIndex, lastIndex + 1 )
      
      // Compute the noise threshold
      val noiseThreshold = baselineRemover.calcNoiseThreshold(peakelRtIntPairs)
      
      // Find peakels indices above noise threshold
      val noiseFreePeakelsIndices = baselineRemover.findNoiseFreePeakGroupsIndices(peakelRtIntPairs, noiseThreshold)
      
      // Keep the biggest peak group above the noise threshold
      if( noiseFreePeakelsIndices.isEmpty ) {
        firstIndex -> lastIndex
      } else {
        val refinedFirstIndex = firstIndex + noiseFreePeakelsIndices.head._1
        val refinedLastIndex = firstIndex + noiseFreePeakelsIndices.last._2
        
        refinedFirstIndex -> refinedLastIndex
      }
    }
    
    refinedPeakelsIndices.toArray
  }
  
  def calcOscillationFactor(rtIntPairs: Array[(Float,Double)]): Double = { 
    val intensities = rtIntPairs.map(_._2)
    sumDeltaIntensities(rtIntPairs) / (intensities.max - intensities.min)
  }
  
  def sumDeltaIntensities(rtIntPairs: Array[(Float,Double)]): Double = {        
    rtIntPairs.sliding(2).foldLeft(0.0) { (sum,buffer) =>
      sum + math.abs(buffer(1)._2 - buffer(0)._2)
    }
  }

}
