package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer

import fr.profi.mzdb.algo.signal.filtering._
import fr.profi.mzdb.model.IPeakelData
import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
object BasicPeakelFinder extends IPeakelFinder {
  
  var sameSlopeCountThreshold = 2
  
  def findPeakelsIndices(rtIntPairs: Array[(Float,Double)] ): Array[Tuple2[Int,Int]] = {
    
    if( rtIntPairs.length < 5 )
      return Array()
    
    val sgMoother = new SavitzkyGolaySmoother(SavitzkyGolaySmoothingConfig(iterationCount = 3))
    val smoothedRtIntPairs = sgMoother.smoothTimeIntensityPairs(rtIntPairs)
    findPeakelsIndicesFromSmoothedIntensities( smoothedRtIntPairs.map(_._2) ).toArray
      
    /*var peakDetectionBegin = false
    var afterMinimum = true
    var afterMaximum = false
    var nbConsecutiveTimes = 1
    var prevSlope = 0
    var prevMaxValue = 0.0
    var prevMinIdx = 0
    
    var peakIdx = 0
    smoothValues(values).sliding(2).foreach { buffer =>
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
            // Detects minimum local with the constraint of being lower than half of previous maximum
            else if( prevSlope == -1 && afterMaximum  && prevValue < prevMaxValue / 2 ) {
              peakelIndices += Tuple2(prevMinIdx,peakIdx)
              prevMinIdx = peakIdx
              afterMaximum = false
              afterMinimum = true
            }
          }
          nbConsecutiveTimes = 1
        }
        else if ( curSlope != 0 ) 
          nbConsecutiveTimes += 1
      }
      
      prevSlope = curSlope
      peakIdx += 1
    }//end sliding foreach
    
    if( afterMaximum ) 
      peakelIndices += Tuple2(prevMinIdx,values.length-1) //|| peaks.length == 0 
    
    peakelIndices.toArray*/
  }
  
  def findPeakelsIndicesFromSmoothedIntensities(
    smoothedIntensities: Array[Double],
    sameSlopeCountThresh: Int = BasicPeakelFinder.sameSlopeCountThreshold
  ): ArrayBuffer[(Int,Int)] = {
    
    var peakIdx = 0
    var prevMinIdx = peakIdx
    var prevSlope = 0
    var prevMaxValue = 0.0
    var sameSlopeCount = 1
    var peakDetectionBegin = false
    var afterMinimum = true
    var afterMaximum = false
    
    val peakelIndices = new ArrayBuffer[(Int,Int)]
    
    smoothedIntensities.sliding(2).foreach { buffer =>
      val prevValue = buffer(0)
      val curValue = buffer(1)
      val curDiff = (curValue - prevValue)
      val curSlope = if( curDiff == 0 ) 0 else curDiff.signum
      
      // Small hack to start peak detection when signal is increasing
      if( peakDetectionBegin == false) {
        if( curSlope == 1 ) {
          peakDetectionBegin = true
          prevMinIdx = peakIdx
        }
      }
      
      if( peakDetectionBegin ) {
        
        if( afterMaximum && curValue > prevMaxValue ) {
          prevMaxValue = curValue
        }
        
        if( curSlope != prevSlope ) {
          if( sameSlopeCount >= sameSlopeCountThresh ) {
            if( prevSlope == 1 && afterMinimum ) {
              prevMaxValue = prevValue
              afterMaximum = true
              afterMinimum = false
            }
            // Detects minimum local with the constraint of being lower than 66% of previous maximum
            else if( prevSlope == -1 && afterMaximum && prevValue < prevMaxValue * 0.66 ) {
              
              peakelIndices += (prevMinIdx -> peakIdx)
              prevMinIdx = peakIdx
              afterMaximum = false
              afterMinimum = true
            }
          }
          
          sameSlopeCount = 1
        }
        else sameSlopeCount += 1
        
      }
      
      prevSlope = curSlope
      peakIdx += 1
    } //end sliding foreach
    
    if( afterMaximum ) peakelIndices += prevMinIdx -> (smoothedIntensities.length-1)
    
    peakelIndices
  }

}