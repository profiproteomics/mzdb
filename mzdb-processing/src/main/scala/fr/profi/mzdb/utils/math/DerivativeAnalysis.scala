package fr.profi.mzdb.utils.math

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

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
    def isMaximum: Boolean
  }
  case class LocalMinimum( value: Double, index: Int ) extends ILocalDerivativeChange {
    def isMaximum = false
  }
  case class LocalMaximum( value: Double, index: Int ) extends ILocalDerivativeChange {
    def isMaximum = true
  }
  
  def findMiniMaxi(
    values: Array[Double]
  ): Array[ILocalDerivativeChange] = {
    
    var prevIdx = 0
    var prevSlope = 0
    var prevMaxValue = 0.0
    var hasSeenAscendingSlope = false
    var afterMinimum = true
    var afterMaximum = false
    
    val changes = new ArrayBuffer[ILocalDerivativeChange]()
    
    values.sliding(2).foreach { buffer =>
      val prevValue = buffer(0)
      val curValue = buffer(1)
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
    } //end sliding foreach
    
    if( changes.isEmpty )
      return Array()
      
    val indexedValues = values.zipWithIndex
    
    // If needed, add missing initial minimum
    if( changes.head.isMaximum ) {
      // If maximum is the first value
      if( changes.head.index == 0 ) {
        // We remove it from the array
        changes.remove(changes.head.index)
      } else {
        val prevMinWithIndex = indexedValues.slice(0,changes.head.index + 1).minBy(_._1)
        
        // Handle the case where the minimum value equals the maximum value
        if( prevMinWithIndex._1 != changes.head.value ) {
          changes.prepend( LocalMinimum( prevMinWithIndex._1, prevMinWithIndex._2 ) )
        } else {
          val firstIndexedValue = indexedValues.head
          changes.prepend( LocalMinimum( firstIndexedValue._1, firstIndexedValue._2 ) )
        }
      }
    }
    
    // If needed, add missing final minimum
    if( changes.last.isMaximum ) {
      val lastValueIndex = values.length -1
      
      // If maximum is the last value
      if( changes.last.index == lastValueIndex ) {
        // We remove it from the array
        changes.remove(lastValueIndex)
      } else {
        val nextMinWithIndex = indexedValues.slice(changes.last.index, indexedValues.length).minBy(_._1)
        
        // Handle the case where the minimum value equals the maximum value
        if( nextMinWithIndex._1 != changes.last.value ) {
          changes.append( LocalMinimum( nextMinWithIndex._1, nextMinWithIndex._2 ) )
        } else {
          val lastIndexedValue = indexedValues.last
          changes.append( LocalMinimum( lastIndexedValue._1, lastIndexedValue._2 ) )
        }
      }
    }
    
    // Check we have an alternation of minima and maxima
    // TODO: remove me if exception not thrown for a while
    require(
      changes.tail.grouped(2).forall( buffer => buffer(0).isMaximum && buffer(1).isMaximum == false ),
      "invalid alternation of minima and maxima"
    )
    
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
    val indexedMiniMaxi = miniMaxi.zipWithIndex
    val (indexedMaxima, indexedMinima) = indexedMiniMaxi.partition(_._1.isMaximum)
   
    // Map indexed map to know if they have been processed or not    
    val validatedMinimaIndexMap = new HashMap() ++ indexedMinima.map( _._2 -> false )
    val validatedMaximaIndexMap = new HashMap() ++ indexedMaxima.map( _._2 -> true )
    
    // Loop over sorted maxima to filter changes
    for(
      indexedMaximum <- indexedMaxima.sortBy(-_._1.value);
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
      filteredIndexedChanges += indexedMiniMaxi(minIdx)
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
    require(
      significantChanges.tail.grouped(2).forall( buffer => buffer(0).isMaximum && buffer(1).isMaximum == false ),
      "invalid alternation of minima and maxima"
    )
    
    significantChanges.toArray
  }

}