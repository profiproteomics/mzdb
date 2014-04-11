package fr.profi.mzdb.algo.feature.extraction

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.algo.signal.detection.waveletImpl._
import fr.profi.mzdb.model._

object FeatureExtractionUtils {

  /** Detects peakels */ 
  def findPeakelsIndexes(
    peaks: Array[Peak],
    detectionAlgorithm: DetectionAlgorithm.Value, 
    minSNR: Float = 1.0f
  ): Array[Pair[Int, Int]] = {
    
    var peakelIndexes: Array[(Int, Int)] = null 
    
    if ( detectionAlgorithm == DetectionAlgorithm.BASIC ) {
      peakelIndexes = BasicPeakelFinder.findPeakelsIndexes(peaks.map(_.getIntensity toDouble), 2) 
    } else {
      val wpf = new WaveletPeakelFinderNeumann(peaks)
      wpf.ridgeFilteringParams.minSNR = minSNR
      peakelIndexes = wpf.findPeakelsIndexes(asScanId = false) 
    }
    
    require(peakelIndexes != null, "peakelIndexes is null")
    
    peakelIndexes
  }
  
 /** naive local maxima finder*/
  def findMaximaNaive(data: Array[Float]): Array[Int] = {
    
    val maxs = new ArrayBuffer[Int]
    
    var i = 0
    while ( i < data.length) {
      
      if (i == 0) {
        
        if (data(i + 1) < data(i))
          maxs += i
        
      } else if (i == data.length - 1) {
        
        if (data(i - 1) < data(i))
          maxs += i
          
      } else if (data(i - 1) < data(i) && data(i + 1) < data(i)) {
          maxs += i
      }
      
      i += 1
    }
    
    maxs.toArray
  }

  
  /** no plans to use it */
  /*def _nbGapInMaxPeakelRespectful(ft: Feature, maxPredictedPeakelIndex: Int, maxConsecutiveGaps: Int): Boolean = {
    
    //should never happened
    if (ft.peakelsCount == 0)
      return false

    val maxIntensityPeakel = if (ft.peakelsCount > maxPredictedPeakelIndex)
      ft.peakels(maxPredictedPeakelIndex)
    else
      ft.peakels.maxBy(_.getIntensity)
    
    var gap = 0
    for (p <- maxIntensityPeakel.peaks) {
      if (p == null)
        gap += 1
      else
        gap = 0
      if (gap > maxConsecutiveGaps)
        return false
    }
    
    return true
  }*/

}