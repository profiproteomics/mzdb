package fr.profi.mzdb.util.math

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LongMap
import fr.profi.util.collection._

object DerivativeAnalysis {
  
  def calcTernarySlopes( values: Array[Double], derivativeLevel: Int ): Array[Double] = {

    val signums = values.sliding(2).map { buffer =>
      val diff = (buffer(1) - buffer(0))
      if( diff == 0 ) 0.0 else diff.signum.toDouble
    } toArray
    
    if( derivativeLevel == 1 ) signums
    else calcTernarySlopes(signums, derivativeLevel - 1)
  }
  
  trait ILocalDerivativeChange {
    def value: Double
    def index: Int
    def isMinimum: Boolean
    def isMaximum: Boolean
  }
  case class LocalMinimum( value: Double, index: Int ) extends ILocalDerivativeChange {
    def isMinimum = true
    def isMaximum = false
  }
  case class LocalMaximum( value: Double, index: Int ) extends ILocalDerivativeChange {
    def isMinimum = false
    def isMaximum = true
  }
  
  def findMiniMaxi(
    values: Array[Double]
  ): Array[ILocalDerivativeChange] = {
    
    val valuesCount = values.length
    var prevIdx = 0
    var prevSlope = 0
    var prevMaxValue = 0.0
    var hasSeenAscendingSlope = false
    var afterMinimum = true
    var afterMaximum = false
    
    val changes = new ListBuffer[ILocalDerivativeChange]()
    
    val maxPrevIdx = valuesCount - 2
    while( prevIdx <= maxPrevIdx ) {
      val prevValue = values(prevIdx)
      val curValue = values(prevIdx + 1)
      val curDiff = (curValue - prevValue)
      val curSlope = if( curDiff == 0 ) 0 else curDiff.signum
      
      // Small hack to start maximum detection when signal is increasing
      if( hasSeenAscendingSlope == false && curSlope == 1 ) {
        hasSeenAscendingSlope = true
      }
      
      if( hasSeenAscendingSlope ) {
        
        if( afterMaximum && curValue > prevMaxValue ) {
          prevMaxValue = curValue
        }
        
        if( curSlope != prevSlope ) {
          if( prevSlope == 1 && afterMinimum ) {
            
            changes += LocalMaximum( values(prevIdx), prevIdx )
            prevMaxValue = prevValue
            afterMaximum = true
            afterMinimum = false
          }
          else if( prevSlope == -1 && afterMaximum ) {
            changes += LocalMinimum( values(prevIdx), prevIdx )
            afterMaximum = false
            afterMinimum = true
          }
        }
      }
      
      prevSlope = curSlope
      prevIdx += 1
      
    } // ends while
    
    if( changes.isEmpty )
      return Array()
      
    val firstChange = changes(0)
    
    // If needed, add missing initial minimum
    if( firstChange.isMaximum ) {
      val firstChangeIdx = firstChange.index
      
      // If maximum is the first value
      if( firstChangeIdx == 0 ) {
        // We remove it from the array
        changes.remove(0)
      } else {
        
        // Search for previous minimum value
        var prevMinValue = Double.MaxValue
        var prevMinIndex = -1
        var idx = 0
        while (idx <= firstChangeIdx) {
          val value = values(idx)
          if (value < prevMinValue) {
            prevMinValue = value
            prevMinIndex = idx
          }
          idx += 1
        }
        
        // Handle the case where the minimum value equals the maximum value
        if( prevMinValue != firstChange.value ) {
          changes.prepend( LocalMinimum( prevMinValue, prevMinIndex ) )
        } else {
          changes.prepend( LocalMinimum( values(0), 0 ) )
        }
      }
    }
    
    val lastChange = changes.last
    
    // If needed, add missing final minimum
    if( lastChange.isMaximum ) {
      val lastChangeIndex = lastChange.index
      val lastValueIndex = valuesCount - 1
      
      // If maximum is the last value
      if( lastChangeIndex == lastValueIndex ) {
        // We remove it from the array
        changes.remove(lastChangeIndex)
      } else {
        //val nextMinWithIndex = indexedValues.slice(lastChange.index, indexedValues.length).minBy(_._1)
        
        // Search for next minimum value
        var nextMinValue = Double.MaxValue
        var nextMinIndex = -1
        var idx = lastChange.index
        while (idx <= lastValueIndex) {
          val value = values(idx)
          if (value < nextMinValue) {
            nextMinValue = value
            nextMinIndex = idx
          }
          idx += 1
        }
        
        // Handle the case where the minimum value equals the maximum value
        if( nextMinValue != lastChange.value ) {
          changes.append( LocalMinimum( nextMinValue, nextMinIndex ) )
        } else {
          changes.append( LocalMinimum( values(lastValueIndex), lastValueIndex ) )
        }
      }
    }
    
    // Check we have an alternation of minima and maxima
    // TODO: remove me if exception not thrown for a while
    /*require(
      changes.tail.grouped(2).forall( buffer => buffer(0).isMaximum && buffer(1).isMaximum == false ),
      "invalid alternation of minima and maxima"
    )*/
    
    changes.toArray
  }
  
  def findSignificantMiniMaxi(
    values: Array[Double],
    miniMaxiDistanceThresh: Int,
    maxIntensityRelThresh: Float
  ): Array[ILocalDerivativeChange] = {
    
    val miniMaxi = findMiniMaxi(values)
    val miniMaxiCount = miniMaxi.length
    
    // Return miniMaxi if we have not at least two maxima
    if( miniMaxiCount <= 3 ) {
      return miniMaxi
    }
    
    // Split maxima and minima
    val indexedMiniMaxi = miniMaxi.toList.zipWithIndex
    val (indexedMaxima, indexedMinima) = indexedMiniMaxi.partition(_._1.isMaximum)
    
    // Map indexed map to know if they have been processed or not    
    val validatedMinimaIndexMap = new LongMap[Boolean](indexedMinima.length)
    for ( (k,v) <- indexedMinima ) validatedMinimaIndexMap.put(v, false)
    val validatedMaximaIndexMap = new LongMap[Boolean](indexedMaxima.length)
    for ( (k,v) <- indexedMaxima ) validatedMaximaIndexMap.put(v, true)
    
    // Loop over sorted maxima to filter changes
    val sortedIndexedMaxima = indexedMaxima.sortBy(-_._1.value)
    for(
      indexedMaximum <- sortedIndexedMaxima;
      if validatedMaximaIndexMap(indexedMaximum._2)
    ) {
      //println("maximum:"+ indexedMaximum._1)
      
      val( maximum, maxIdx ) = indexedMaximum
      
      def lookForSignificantMinimum( direction: Int ) {
        //println("direction: "+ direction)
        var minimumAlreadyValidated = false
        var isMinimumOK = false
        var minIdx = maxIdx + direction
        
        while( isMinimumOK == false && minimumAlreadyValidated == false ) {
          val indexedMinimum = indexedMiniMaxi(minIdx)
          val(minimum, minValueIdx) = indexedMinimum
          val isBoundary = minValueIdx == 0 || minValueIdx == miniMaxiCount - 1
          
          val isMinSignificant = if( isBoundary ) true
          else {
            // Retrieve next valid maximum (left or right)
            var nextValidMax: ILocalDerivativeChange = null
            var maxIdx = minIdx + direction
            while( nextValidMax == null && maxIdx > 0 && maxIdx < miniMaxiCount ) {
              
              if( validatedMaximaIndexMap(maxIdx) == true ) {
                
                val nextMax = miniMaxi(maxIdx)
                
                // Check this max is really valid
                val isUnderThresholdFromCurrentMax = (minimum.value / maximum.value) <= maxIntensityRelThresh
                val isUnderThresholdFromOtherMax = (minimum.value / nextMax.value) <= maxIntensityRelThresh
                
                val distFromCurrentMax = (minimum.index - maximum.index).abs
                val distFromOtherMax = (minimum.index - nextMax.index).abs
                
                // Combine the checks
                if( isUnderThresholdFromCurrentMax && isUnderThresholdFromOtherMax &&
                   distFromCurrentMax >= miniMaxiDistanceThresh && distFromOtherMax >= miniMaxiDistanceThresh
                 ) {
                  nextValidMax = nextMax
                } //{
                  // Invalidate this maximum
                  //validatedMaximaIndexMap(maxIdx) = false
                //}
              }
              
              maxIdx += 2 * direction
            }
            //println("nextValidMax:"+ nextValidMax)
            
            if( nextValidMax == null ) false else true
          }
          
          // If minimum value is significant => keep this minimum
          if( isMinSignificant ) {
            //println(s"significative minimum (dir=$direction):"+ minimum)
            isMinimumOK = true
            
            // Invalidate current minimum
            validatedMinimaIndexMap(minIdx) = true
            
          // Else if this minimum has already been validated
          } else if (validatedMinimaIndexMap(minIdx)) {
            //println(s"non significative minimum already validated (dir=$direction):"+ minimum)
            // Invalidate this minimum and current maximum
            validatedMinimaIndexMap(minIdx) = false
            validatedMaximaIndexMap(maxIdx) = false
            minimumAlreadyValidated = true
          }
          else {
            //println(s"non significative minimum (dir=$direction):"+ minimum)
            // Invalidate other maximum
            validatedMaximaIndexMap(minIdx + direction) = false
            
            minIdx += 2 * direction
          }
        }
        
      }
      
      // Find first right significant minimum
      lookForSignificantMinimum( +1 )
      
      // Find first left significant minimum
      lookForSignificantMinimum( -1 ) 
    }

    // Add validated minima and maxima to filteredIndexedChanges
    val filteredIndexedChanges = new ArrayBuffer[(ILocalDerivativeChange,Int)]()
    for( (minIdx,isValidated) <- validatedMinimaIndexMap ++ validatedMaximaIndexMap; if isValidated ) {
      filteredIndexedChanges += indexedMiniMaxi(minIdx.toInt)
    }

    val filteredAndSortedChanges = filteredIndexedChanges.distinct.sortBy(_._2)
    val significantChanges = new ArrayBuffer[ILocalDerivativeChange]
    //significantChanges ++= filteredAndSortedChanges
    
    // Look for consecutive minima to keep only the lowest one
    var prevMin: (ILocalDerivativeChange,Int) = null
    var prevMax: (ILocalDerivativeChange,Int) = null
    var sigChangeIdx = 0
    for( indexedDerivativeChange <- filteredAndSortedChanges ) {
      val (derivativeChange,derivativeChangeIdx) = indexedDerivativeChange
      
      // Add maximum
      if( derivativeChange.isMaximum ) {
        
        if( prevMax != null ) {
          // Search minimum between two consecutive maxima
          val firstIdx = prevMax._2
          val lastIdx = derivativeChangeIdx
          significantChanges += indexedMiniMaxi.slice(firstIdx, lastIdx + 1).minBy(_._1.value)._1
        }
        
        significantChanges += derivativeChange
        prevMin = null
        prevMax = indexedDerivativeChange
        sigChangeIdx += 1
      }
      // Add first encountered minimum
      else {
        
        if( prevMin == null ) {
          significantChanges += derivativeChange
          sigChangeIdx += 1
        }
        // Replace previous minimum if current one is lower
        else if (derivativeChange.value < prevMin._1.value) {
          significantChanges(sigChangeIdx - 1) = derivativeChange
        }
        
        prevMin = indexedDerivativeChange
        prevMax = null
      }
    }
    //println( significantChanges.map(c => c.index +":"+c.value +s"(${c.isMaximum})").mkString(" "))
    
    // Check we have an alternation of minima and maxima
    // TODO: remove me if exception not thrown for a while
    /*require(
      significantChanges.tail.grouped(2).forall( buffer => buffer(0).isMaximum && buffer(1).isMaximum == false ),
      "invalid alternation of minima and maxima"
    )*/
    
    significantChanges.toArray
  }

}