package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
object BasicPeakelFinder {

  def findPeakelsIndexes(peaks: Seq[Peak] ): Array[Tuple2[Int,Int]] = {
    findPeakelsIndexes( peaks.map( _.getIntensity.toDouble ).toArray, 2 )
  }
  
  def findPeakelsIndexes(values: Array[Double], consNbTimesThresh: Int ): Array[Tuple2[Int,Int]] = {
    
    var peakelIndexes = new ArrayBuffer[Tuple2[Int,Int]]
    if( values.length < 5 ) return peakelIndexes.toArray
    
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
            else if( prevSlope == -1 && afterMaximum && prevValue < prevMaxValue/2 ) {
              peakelIndexes += Tuple2(prevMinIdx,peakIdx)
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
    
    if( afterMaximum ) peakelIndexes += Tuple2(prevMinIdx,values.length-1) //|| peaks.length == 0 
    
    peakelIndexes.toArray
  }

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