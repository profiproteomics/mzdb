package fr.profi.mzdb.algo

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.utils.math.VectorSimilarity

object FeatureScorer {
  
  // TODO: ms1_count
  // Compute the distribution of log(ms1_count)
  // Then compute the bounds using the box plot method
  // First results min = 5 ; max = 110

  def calcMeanPeakelCorrelation( peakels: Seq[Peakel] ): Double = {
    
    val peakelsCount = peakels.length 
    if( peakelsCount < 2 ) return 0.0
    
    var peakelCorrSum = 0.0
    peakels.sliding(2).foreach { case Seq(p1, p2) => 
      peakelCorrSum += calcPeakelCorrelation(p1, p2)
    }
    
    peakelCorrSum / (peakelsCount-1)
  }
  
  def calcPeakelCorrelation( firstPeakel: Peakel, secondPeakel: Peakel ): Double = {
    
    val secondPeakelPeakByTime = secondPeakel.getPeakByElutionTime
    
    val firstPeakelIntensities = new ArrayBuffer[Float]()
    val secondPeakelIntensities = new ArrayBuffer[Float]()
    
    // Compute the peak intersection of the two peakels
    var peakIndex = 0
    for( peakelPeak <- firstPeakel.definedPeaks ) {
      
      //System.out.println( "time=" + peakelPeak.elutionTime  );
      val secondPeakelPeak = secondPeakelPeakByTime.get( peakelPeak.getLcContext.getElutionTime )
      
      if( secondPeakelPeak != None ) {
        firstPeakelIntensities += peakelPeak.getIntensity() 
        secondPeakelIntensities += secondPeakelPeak.get.getIntensity()
        //System.out.println( "time=" + peakelPeak.elutionTime + " ; ft peak int=" + otherPeakelPeak.intensity + " ; olp ft peak int=" + peakelPeak.intensity );
      }
      
      peakIndex += 1
    }

    return VectorSimilarity.pearsonCorrelation( firstPeakelIntensities.toArray, secondPeakelIntensities.toArray );
  }
  


  
}