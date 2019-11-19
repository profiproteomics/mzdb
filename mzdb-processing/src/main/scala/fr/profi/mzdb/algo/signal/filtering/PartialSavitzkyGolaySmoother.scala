package fr.profi.mzdb.algo.signal.filtering

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.util.math.DerivativeAnalysis

/**
 * @author David Bouyssie
 *
 */
class PartialSavitzkyGolaySmoother( smoothingConfig: SavitzkyGolaySmoothingConfig ) extends ISignalSmoother {
  
  private val sgSmoother = new SavitzkyGolaySmoother(smoothingConfig)
  private val paddingOffset = 2
  
  def smoothTimeIntensityPairs(rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)] = {
    
    // Retrieve some values
    val intensities = rtIntPairs.map(_._2 )
    val peakCount = rtIntPairs.length
    
    // Calc second derivative as ternary slopes (-1,0,+1)
    val slopes = DerivativeAnalysis.calcTernarySlopes( intensities, derivativeLevel = 2)
    
    // Detect noisy parts (where smoothing is needed)
    var i = 2 // have a shift of 2 indices because of second derivative computation
    val nonZeroSlopesIndices = new ArrayBuffer[Int](peakCount)
    for( slope <- slopes ) {
      if( slope != 0 ) nonZeroSlopesIndices += i
      i += 1
    }
    
    // Check if we have detected noisy parts
    val rtIntPairsResult = if( nonZeroSlopesIndices.length < 2 ) rtIntPairs
    else {
      
      val noisyPartBoundaryPairs = new ArrayBuffer[(Int,Int)]()
      var noisyPartStart = nonZeroSlopesIndices.head
      
      // Iterate over pairs of indices
      for( buffer <- nonZeroSlopesIndices.sliding(2) ) {
        val prevIndex = buffer(0)
        val nextIndex = buffer(1)
        
        // Check if we are on a gap
        if( nextIndex - prevIndex > 1) {
          // Decreate start to include previous point
          noisyPartBoundaryPairs += (noisyPartStart - 1) -> prevIndex
            
          noisyPartStart = nextIndex
        }
      }
      
      // Add ending noise part (decreate start to include previous point)
      noisyPartBoundaryPairs += ( (noisyPartStart-1) -> nonZeroSlopesIndices.last)
      
      val smoothedRtIntPairs = rtIntPairs.clone()
      
      // Iterate over detected noisy parts to smooth them
      for(
        noisyPartBoundaryPair <- noisyPartBoundaryPairs;
        if (noisyPartBoundaryPair._2 - noisyPartBoundaryPair._1) > 1
      ) {
        
        // Extend noise parts with previous and next values
        val extendedBoundaries = {
          val(start,end) = noisyPartBoundaryPair
          val extendedStart = math.max( start - paddingOffset, 0)
          val extendedEnd = math.min( end + paddingOffset, peakCount - 1)
          extendedStart -> extendedEnd
        }
        
        // Smooth extended noisy part
        val extendedNoisyPart = rtIntPairs.slice(extendedBoundaries._1, extendedBoundaries._2 + 1)
        val smoothedExtendedNoisyPart = sgSmoother.smoothTimeIntensityPairs(extendedNoisyPart)
        
        // Retrieve only the part corresponding to non-extended boundaries
        val startIdx = noisyPartBoundaryPair._1 - extendedBoundaries._1
        val endIdx = startIdx + (noisyPartBoundaryPair._2 - noisyPartBoundaryPair._1)
        val smoothedNoisyPart = smoothedExtendedNoisyPart.slice(startIdx,endIdx+1)
        
        // Merge noisy part into smoothedRtIntPairs
        val noisyPartIndices = (noisyPartBoundaryPair._1 to noisyPartBoundaryPair._2).toArray 
        require( noisyPartIndices.length == smoothedNoisyPart.length, "invalid noise boundaries")
        
        for( (rtIntPair,i) <- smoothedNoisyPart.zip(noisyPartIndices) ) {
          smoothedRtIntPairs(i) = rtIntPair
        }
        
      }
      
      smoothedRtIntPairs
    }
    
    rtIntPairsResult
  }

}
