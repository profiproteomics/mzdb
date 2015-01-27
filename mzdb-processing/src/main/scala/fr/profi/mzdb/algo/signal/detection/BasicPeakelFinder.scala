package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.IPeakelData
import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
object BasicPeakelFinder extends IPeakelFinder {
  
  var consNbTimesThresh = 2
  
  def findPeakelsIndices(peaks: Seq[Peak] ): Array[Tuple2[Int,Int]] = {
    findPeakelsIndices( peaks.map( _.getIntensity.toDouble ).toArray)
  }
  
  def findPeakelsIndices(peakel: IPeakelData): Array[Tuple2[Int,Int]] = {
    findPeakelsIndices( peakel.getIntensityValues.map(_.toDouble).toArray)
  }
  
  def findPeakelsIndices(values: Array[Double]): Array[Tuple2[Int,Int]] = {
    
    val peakelIndexes = new ArrayBuffer[Tuple2[Int,Int]]
    if( values.length < 5 ) 
      return peakelIndexes.toArray
    
    var peakDetectionBegin = false
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
              peakelIndexes += Tuple2(prevMinIdx,peakIdx)
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
      peakelIndexes += Tuple2(prevMinIdx,values.length-1) //|| peaks.length == 0 
    
    peakelIndexes.toArray
  }

}