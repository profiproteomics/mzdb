package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peak
import fr.profi.util.stat._

/**
 * @author David Bouyssie
 *
 */
object HistogramBasedPeakelFinder extends IPeakelFinder {

  def findPeakelsIndexes(peaks: Seq[Peak] ): Array[Tuple2[Int,Int]] = {
    findPeakelsIndexes( peaks.map( p => (p.getLcContext.getElutionTime , p.getIntensity.toDouble) ).toArray )
  }
  
  protected def findPeakelsIndexes(rtIntPairs: Array[(Float,Double)], consNbTimesThresh: Int = 2, binSize: Float = 10f ): Array[Tuple2[Int,Int]] = {
    
    val tmpPeakelIndexes = new ArrayBuffer[Tuple2[Int,Int]]
    if( rtIntPairs.length < 5 ) return tmpPeakelIndexes.toArray
    
    var peakDetectionBegin = false
    var afterMinimum = true
    var afterMaximum = false
    var nbConsecutiveTimes = 1
    var prevSlope = 0
    var prevMaxValue = 0.0
    var prevMinIdx = 0
    
    var peakIdx = 0
    val binnedRtIntPairs = _binRtIntPairs(rtIntPairs,binSize)
    
    // TODO: factorize this code with the one from BasicPeakelFinder
    // or replace the BasciPeakel finder by this one
    binnedRtIntPairs.map( _._2 ).sliding(2).foreach { buffer =>
      val prevValue = buffer(0)
      val curValue = buffer(1)
      val curSlope = (curValue - prevValue).signum
      
      // Small hack to start peak detection when signal is increasing
      if( peakDetectionBegin == false) {
        if( curSlope == 1 ) {
          peakDetectionBegin = true
          prevMinIdx = peakIdx
        }
      }
      
      if( peakDetectionBegin ) {
        
        if( afterMaximum && curValue > prevMaxValue ) {
          //afterMaximum = false
          //afterMinimum = true
          prevMaxValue = curValue
        }
        
        if( curSlope != prevSlope ) {
          if( nbConsecutiveTimes >= consNbTimesThresh ) {
            if( prevSlope == 1 && afterMinimum ) {
              prevMaxValue = prevValue
              afterMaximum = true
              afterMinimum = false
            }
            else if( prevSlope == -1 && afterMaximum && prevValue < prevMaxValue/2 ) {
              tmpPeakelIndexes += Tuple2(prevMinIdx,peakIdx)
              prevMinIdx = peakIdx
              afterMaximum = false
              afterMinimum = true
            }
          }
          
          nbConsecutiveTimes = 1
        }
        else if ( curSlope != 0 ) nbConsecutiveTimes += 1
        
      }
      
      prevSlope = curSlope
      peakIdx += 1
    }
    
    if( afterMaximum ) tmpPeakelIndexes += Tuple2(prevMinIdx,binnedRtIntPairs.length-1) //|| peaks.length == 0 
    
    // Convert peakel indexes of binned values into indexes of input values    
    val peakelIndexes = new ArrayBuffer[Tuple2[Int,Int]]( tmpPeakelIndexes.length )
    val rtInPairsWithIndex = rtIntPairs.zipWithIndex
    
    for( peakelIdx <- tmpPeakelIndexes ) {
      val(firstBin,lastBin) = (binnedRtIntPairs(peakelIdx._1),binnedRtIntPairs(peakelIdx._2))
      val firstIdx = rtInPairsWithIndex.find( _._1._1 >= firstBin._1 ).get._2
      val lastIdx = rtInPairsWithIndex.find( _._1._1 >= lastBin._1 ).get._2
      peakelIndexes += (firstIdx -> lastIdx)
    }
    
    peakelIndexes.toArray
  }
  
  private def _binRtIntPairs( rtIntPairs: Array[(Float,Double)], binSize: Float ): Array[(Float,Double)] = {
    
    // Instantiate an histogram computer
    val histoComputer = new EntityHistogramComputer(rtIntPairs, { rtIntPair: (Float,Double) => rtIntPair._1.toDouble } )
    
    // Compute the number of bins
    val timeRange = rtIntPairs.last._1 - rtIntPairs.head._1
    val nbBins = (timeRange / binSize).toInt
    
    val rtIntPairsHisto = histoComputer.calcHistogram(nbBins)
    
    val newRtIntPairs = rtIntPairsHisto.map { case (bin, dataPoints) =>
      val intSum = dataPoints.foldLeft(0.0) { (s,dp) => s + dp._2 }
      (bin.lowerBound.toFloat,intSum)
    }
    
    newRtIntPairs
  }

}