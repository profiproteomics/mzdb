package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer

import fr.profi.mzdb.algo.signal.filtering._
import fr.profi.mzdb.model.Peak
import fr.profi.util.stat._

/**
 * @author David Bouyssie
 *
 */
class HistogramBasedPeakelFinder(
  var sameSlopeCountThreshold:Int = 2,
  var expectedBinDataPointsCount:Int = 5
) extends IPeakelFinder {
 
  val basicPeakelFinder = new BasicPeakelFinder(sameSlopeCountThreshold)
  
  def findPeakelsIndices(rtIntPairs: Array[(Float,Double)] ): Array[(Int,Int)] = {
    
    // Check we have at least 5 peaks before the filtering
    if( rtIntPairs.length < 5 ) return Array()
    
    val xicBinner = new XicBinner( XicBinnerConfig(expectedBinDataPointsCount) )
    val extendedBins = xicBinner.calcBins(rtIntPairs)
    val nbBins = extendedBins.length

    // Check we have at least 3 peaks after the binning
    if( nbBins < 3 ) return Array()
    
    // Add some padding to the bins (needed for smoothing operation)
    val paddedBins = Array( extendedBins.head.copy( sum = rtIntPairs.head._2 ) ) ++ 
      extendedBins ++ 
    Array( extendedBins.last.copy( sum = rtIntPairs.last._2 ) )
    
    // Convert bins into array and add paddings
    val binnedRtIntPairs = paddedBins.map( eb => eb.bin.center.toFloat -> eb.sum )
    
    // Smooth binned intensities
    val nbSmoothingPoints = math.sqrt(nbBins).toInt
    val sgMoother = new SavitzkyGolaySmoother(
      SavitzkyGolaySmoothingConfig(
        nbPoints = 3,
        iterationCount = 1
      )
    )
    val smoothedRtIntPairs = sgMoother.smoothTimeIntensityPairs(binnedRtIntPairs)
    
    val smoothedIntensities = smoothedRtIntPairs.map(_._2)
    
    // Make left to right analysis
    val leftToRightBinIndices = basicPeakelFinder.findPeakelsIndicesFromSmoothedIntensities(smoothedIntensities)
    
    // Make right to left analysis
    val maxBinIdx = nbBins - 1
    val rightToLeftBinIndices = basicPeakelFinder.findPeakelsIndicesFromSmoothedIntensities(smoothedIntensities.reverse) map { rightToLeftBinIdx =>
      // Reverse bin indices
      ( maxBinIdx - rightToLeftBinIdx._2, maxBinIdx - rightToLeftBinIdx._1 )
    }
    
    // Check we have the same number of bins using left and right analyses
    val finalBinIndices = if( leftToRightBinIndices.length != rightToLeftBinIndices.length ) leftToRightBinIndices.toArray
    else {
      // Compute intersections of detected peakel indices using both analyses
      val allBinIndicesSorted = (leftToRightBinIndices ++ rightToLeftBinIndices).sortBy(_._1)
      val binIndicesIntersections = new ArrayBuffer[(Int, Int)]
      
      // Compute intersections of detected peakel indices using both analyses
      for( leftToRightBinIdx <- leftToRightBinIndices; rightToLeftBinIdx <- rightToLeftBinIndices ) {
        if( rightToLeftBinIdx._1 >= leftToRightBinIdx._1 && rightToLeftBinIdx._1 < leftToRightBinIdx._2 ) {
          val firstBinIdx = ( math.max(leftToRightBinIdx._1, rightToLeftBinIdx._1) )
          val lastBinIdx = ( math.min(leftToRightBinIdx._2, rightToLeftBinIdx._2) )
          
          require( lastBinIdx > firstBinIdx, "error during computation of peakel indices intersection")
          
          binIndicesIntersections += ( firstBinIdx -> lastBinIdx )
        }
      }
      
      binIndicesIntersections.toArray
    }
    
    // Convert peakel indices of binned values into indices of input values
    val peakelIndices = new ArrayBuffer[Tuple2[Int,Int]]( finalBinIndices.length )
    val rtInPairsWithIndex = rtIntPairs.zipWithIndex
    
    for( binIdx <- finalBinIndices ) {
      val(firstBin,lastBin) = (paddedBins(binIdx._1).bin, paddedBins(binIdx._2).bin)
      val firstIdx = rtInPairsWithIndex.find( _._1._1 >= firstBin.lowerBound ).get._2
      val lastIdx = rtInPairsWithIndex.find( _._1._1 >= lastBin.upperBound ).map(_._2).getOrElse( rtIntPairs.length - 1 )
      
      // TODO: remove lowest peaks from peakels ??? (threshold on relative difference ?)
      
      peakelIndices += (firstIdx -> lastIdx)
    }
    
    peakelIndices.toArray
  }
  


}
