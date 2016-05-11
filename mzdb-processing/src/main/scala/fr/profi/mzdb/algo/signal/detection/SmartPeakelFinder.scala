package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.algo.signal.filtering._
import fr.profi.mzdb.model.IPeakelData
import fr.profi.mzdb.model.Peak
import fr.profi.util.stat._
import fr.profi.mzdb.utils.math.DerivativeAnalysis
import scala.beans.BeanProperty
import com.typesafe.scalalogging.LazyLogging

/**
 * @author David Bouyssie
 *
 */

class SmartPeakelFinder(
  var minPeaksCount: Int = 5,
  var miniMaxiDistanceThresh: Int = 3,
  var maxIntensityRelThresh: Float = 0.66f,
  var useOscillationFactor: Boolean = false,
  var maxOscillationFactor: Int = 10,
  var usePartialSGSmoother: Boolean = false,
  var useBaselineRemover: Boolean = false,
  var useSmoothing: Boolean = true
) extends IPeakelFinder with LazyLogging {


  // gapTolerance set to 1 means that ponctual intensity hole won't be removed
  val baselineRemover = new BaselineRemover(gapTolerance = 1)

  def findPeakelsIndices(rtIntPairs: Array[(Float, Double)]): Array[(Int, Int)] = {

    // Check we have at least 5 peaks before the filtering
    val peaksCount = rtIntPairs.length
    if (peaksCount < minPeaksCount) return Array()

    // Compute the oscillation factor
    // If the oscillationFactor is high, the SavitskyGolay filter will not provide a good result
    if (useOscillationFactor && (calcOscillationFactor(rtIntPairs) >= maxOscillationFactor)) {
      // TODO: should we apply the HistogramBasedPeakelFinder ???

      // Then we only apply a baseline filter
      val noiseThreshold = baselineRemover.calcNoiseThreshold(rtIntPairs)
      return baselineRemover.findNoiseFreePeakGroupsIndices(rtIntPairs, noiseThreshold)
    }
    
    // Smooth intensities
    val smoothedRtIntPairs = if( useSmoothing == false ) rtIntPairs
    else {
      
      val smoother = { 
        if (usePartialSGSmoother) {
          new PartialSavitzkyGolaySmoother(SavitzkyGolaySmoothingConfig(iterationCount = 1))
        } else {
          val nbSmoothingPoints = { if (peaksCount <= 20) 5 else if (peaksCount <=50) 7 else 11}
          new SavitzkyGolaySmoother(SavitzkyGolaySmoothingConfig(nbPoints = nbSmoothingPoints, polyOrder = 2, iterationCount = 1))
        }
      }
      
      smoother.smoothTimeIntensityPairs(rtIntPairs)
    }

    val smoothedIntensities = smoothedRtIntPairs.map(_._2)

    // Look for significant minima and maxima in the smoothed signal
    val miniMaxi = DerivativeAnalysis.findSignificantMiniMaxi(
      smoothedIntensities,
      miniMaxiDistanceThresh,
      maxIntensityRelThresh
    )
    
    // Return empty array of no mini/maxi found
    if (miniMaxi.isEmpty) {
      //println(smoothedIntensities.toArray)
      return Array()
    }

    // Convert mini/maxi into peakel indices
    val tmpPeakelsIndices = miniMaxi.filter(_.isMaximum == false).sliding(2).map { buffer =>
      buffer(0).index -> buffer(1).index
    }

    // Refine peakels using BaselineRemover algorithm
    val refinedPeakelsIndices = if (useBaselineRemover) {
        tmpPeakelsIndices.map { tmpPeakelIndices =>

        // Retrieve peakel time/intensity pairs
        val (firstIndex, lastIndex) = tmpPeakelIndices
        val peakelRtIntPairs = rtIntPairs.slice(firstIndex, lastIndex + 1)

        // Compute the noise threshold
        val noiseThreshold = baselineRemover.calcNoiseThreshold(peakelRtIntPairs)

        // Find peakels indices above noise threshold
        val noiseFreePeakelsIndices = baselineRemover.findNoiseFreePeakGroupsIndices(peakelRtIntPairs, noiseThreshold)

        // Keep the biggest peak group above the noise threshold
        // NON : j'ai plutot l'impression que l'on garde tous les points entre le 1er groupe au dessus du threshold 
        // et le dernier groupe au dessus du threshold. Seuls les pieds a gauche et a droite sont coupes non ?
        if (noiseFreePeakelsIndices.isEmpty) {
          firstIndex -> lastIndex
        } else {
          val refinedFirstIndex = firstIndex + noiseFreePeakelsIndices.head._1
          val refinedLastIndex = firstIndex + noiseFreePeakelsIndices.last._2

          refinedFirstIndex -> refinedLastIndex
        }
      }
    } else {
      tmpPeakelsIndices
    }
    refinedPeakelsIndices.toArray
  }

  def calcOscillationFactor(rtIntPairs: Array[(Float, Double)]): Double = {
    val intensities = rtIntPairs.map(_._2)
    sumDeltaIntensities(rtIntPairs) / (intensities.max - intensities.min)
  }

  def sumDeltaIntensities(rtIntPairs: Array[(Float, Double)]): Double = {
    rtIntPairs.sliding(2).foldLeft(0.0) { (sum, buffer) =>
      sum + math.abs(buffer(1)._2 - buffer(0)._2)
    }
  }

}