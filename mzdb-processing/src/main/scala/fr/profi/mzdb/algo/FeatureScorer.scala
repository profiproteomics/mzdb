package fr.profi.mzdb.algo

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.model.Feature
import scala.beans.BeanProperty
import fr.profi.mzdb.model.AveragineComputer
//import fr.profi.mzdb.model.MercuryLauncher
import util.control.Breaks._


object FeatureScorer {
  
  // TODO: ms1_count
  // Compute the distribution of log(ms1_count)
  // Then compute the bounds using the box plot method
  // First results min = 5 ; max = 110
	
  /*****************************************************************
   *  Estimation of the correlation factor
   ****************************************************************/
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
  
  
  /*****************************************************************
   *  Estimation of the overlapping factor
   ****************************************************************/
  def calcMeanOverlappingFactor( peakels: Seq[Peakel] ) : Double = {
    if (peakels.isEmpty)
      return 0d
    var m = 0d
    peakels.foreach(p => m += calcOverlappingFactor(p))
    m / peakels.length
  }
  
  def calcOverlappingFactor ( peakel : Peakel, baseline : Float = 0): Double = {
    var of = 0d
    val (mz, intens) = (peakel.getApex.getMz(), peakel.getApex.getIntensity())
    //the intersection point must at the end of the first peakel and at the beginning of the second peakel
    //this point is supposed to be shared by the two overlapping peakels. A baseline removal should be useful
    //at this point
    val lastIntens = peakel.definedPeaks.last.getIntensity()
    val firstIntens = peakel.definedPeaks.head.getIntensity()
    if (math.abs( (lastIntens / firstIntens) - 1) > 0.25f) {
      // do know which side is overlapping, real overlapping ?
      //or that means we do not see overlapping of low intensity
      //peakel
      return 0d
    }
    if ( lastIntens > firstIntens) {
      of += intens / lastIntens
    } else {
      of += intens / firstIntens
    }
    of
  }
  
  /*****************************************************************
   *  Estimation of the quality of the Isotopic Pattern
   ****************************************************************/
  // TODO: find another way than Mercury
  /*def calcIsotopicDistance(f : Feature, customAveragine: Float = 0f) :Double =  {
	  val mf = AveragineComputer.computeAveragine(f)
	  val (mz, intens) = mf.computeIsotopicDistribution(mf, f.getCharge)
	  //several possibilities calculate the istopicdistance at the apex
	  //or everywhere
	  val peaks = f.getIsotopicPatternAtApex.getPeaks
	  var (mzObs, intensObs) =  (peaks.map( x=> if (x != null) x.getMz() else 0), peaks.map( x=> if (x != null) x.getIntensity() else 0))
	  val maxIntensity = intensObs.max
	  intensObs = intensObs.map(x => x * 100 / maxIntensity)
	  var rmsd = 0d
	  breakable {
		  for (i <- 0 until mzObs.length) {
		    if (i  >= mz.length)
		      break
		    if (intensObs(i) != 0f) {
		    	rmsd += math.pow( intensObs(i) - intens(i), 2)
		    }
		  }
	  }
	  rmsd
  }   */
}