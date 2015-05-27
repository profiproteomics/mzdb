package fr.profi.mzdb.algo

import fr.profi.mzdb.model.ScanData
import scala.collection.immutable.TreeMap
import fr.profi.ms.model.TheoreticalIsotopePattern
import scala.collection.immutable.SortedMap
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.ms.algo.IsotopePatternEstimator
import scala.collection.mutable.ArrayBuffer

/**
 * This object defines two methods : the first one scores an Isotopic Pattern hypothesis while the second one 
 * search for the best isotopic pattern (but returns all hypothesis ordered by score)
 * 
 * @author CB205360
 *
 */
object IsotopicPatternScorer {
  
 /**
  * Tries to explain a peak at the specified mz value by testing different isotopic pattern explanations. 
  * 
  * 
 * @param scan : the MS data (mz, intensities) signal around the peak to explain
 * @param mz : the mz of the peak that must be explained 
 * @param ppmTol
 * @return : a list of isotopic patterns tested, ordered by score (better = higher score first).
 */
def calclIsotopicPatternHypotheses(scan : ScanData, mz: Double, ppmTol : Double): Array[(Double, TheoreticalIsotopePattern)] = {
     var result = ArrayBuffer[(Double, TheoreticalIsotopePattern)]()
     //val matchingPeakIdx = scan.getNearestPeakIndex(mz)
     for (charge <- 1 to 5) {
         val (score, theoreticalIP) = getIPHypothesis(scan, mz, charge, ppmTol)
         result += (score -> theoreticalIP)
         for (j <- 1 to theoreticalIP.theoreticalMaxPeakelIndex+1) {
            val alternativeMoz = mz - j * IsotopePatternEstimator.avgIsoMassDiff / charge
            val (score, theoreticalIP) = getIPHypothesis(scan, alternativeMoz, charge, ppmTol)
            result += (score -> theoreticalIP)
         }
      }
     result = result.sortWith{ (p1, p2) => 
       ((p1._1 == p2._1) && (p1._2.charge < p2._2.charge)) || 
       ((p1._1 != p2._1) && (p1._1 < p2._1))
     }
     result.toArray
   }
   

 /**
  * Scores a single isotopic pattern hypothesis. The score is based on the relative difference between predicted and 
  * observed intensities. In case a theoretical peak is not found in the MS data a penality is applied to the difference.
  * The penality is less important as the rank of the isotopic peak is high.   
  * 
 * @param scan : the MS data (mz, intensities) signal around the peak to explain
 * @param mz : the mz of the peak that must be explained 
 * @param charge : the isotopic pattern charge state
 * @param ppmTol
 * @return : the score and the theoretical isotopic pattern.
 */
def getIPHypothesis(scan : ScanData, mz : Double, charge : Int, ppmTol : Double) : (Double, TheoreticalIsotopePattern) = {
	  var score = 0.0
      val pattern = IsotopePatternEstimator.getTheoreticalPattern(mz, charge)
      var ipMoz = mz
      val isotopeAbundance = scan.getIntensityList()(scan.getNearestPeakIndex(ipMoz))
      val normAbundance = pattern.mzAbundancePairs(0)._2
      var rank = 0
      var matches = 0
      for (rank <- 0 until pattern.mzAbundancePairs.length) {
         ipMoz = if (rank == 0) ipMoz else ipMoz + IsotopePatternEstimator.avgIsoMassDiff/charge
         val ipAbundance = (pattern.mzAbundancePairs(rank)._2) * isotopeAbundance / normAbundance
         var nearestPeakIdx =  scan.getNearestPeakIndex(ipMoz)
         val penality = Math.min(100.0, 0.0001 * Math.pow(10, rank*2));
         val abundance = { 
           if ((1e6 * Math.abs(scan.getMzList()(nearestPeakIdx) - ipMoz) / ipMoz) < ppmTol) {
               ipMoz = scan.getMzList()(nearestPeakIdx)
               matches += 1
        	   scan.getIntensityList()(nearestPeakIdx) 
           } else {
             ipAbundance/100.0f 
           }
         }
         val d = ((ipAbundance - abundance) / math.min(abundance, ipAbundance)) * 1.0/penality
         score += d * d
      }
	  
      score = Math.log10(score) - matches
      (score, pattern)
  } 

// /**
//  * Scores a single isotopic pattern hypothesis. The score is based on the relative difference between predicted and 
//  * observed intensities. In case a theoretical peak is not found in the MS data a penality is applied to the difference.
//  * The penality is less important as the rank of the isotopic peak is high.   
//  * 
// * @param scan : the MS data (mz, intensities) signal around the peak to explain
// * @param mz : the mz of the peak that must be explained 
// * @param charge : the isotopic pattern charge state
// * @param matchingPeakIdx : the index of the peak to explain in the MS data array
// * @param matchingIsotopeIdx : the index of the isotope that must match this peak 
// * @param ppmTol
// * @return : the score and the theoretical isotopic pattern.
// */
//def getIPHypothesis(scan : ScanData, mz : Double, charge : Int, matchingPeakIdx : Int, matchingIsotopeIdx : Int, ppmTol : Double) : (Double, TheoreticalIsotopePattern) = {
//	  var score = 0.0
//      val pattern = IsotopePatternEstimator.getTheoreticalPattern(mz, charge)
//      val isotopeAbundance = scan.getIntensityList()(matchingPeakIdx)
//      val normAbundance = pattern.mzAbundancePairs(matchingIsotopeIdx)._2
//      var ipMoz = pattern.monoMz
//      var rank = 0
//      var matches = 0
//      for (rank <- 0 until pattern.mzAbundancePairs.length) {
//         ipMoz = if (rank == 0) ipMoz else ipMoz + IsotopePatternEstimator.avgIsoMassDiff/charge
//         val ipAbundance = (pattern.mzAbundancePairs(rank)._2) * isotopeAbundance / normAbundance
//         var nearestPeakIdx =  scan.getNearestPeakIndex(ipMoz)
//         val penality = Math.min(100.0, 0.0001 * Math.pow(10, rank*2));
//         val abundance = { 
//           if ((1e6 * Math.abs(scan.getMzList()(nearestPeakIdx) - ipMoz) / ipMoz) < ppmTol) {
//               ipMoz = scan.getMzList()(nearestPeakIdx)
//               matches += 1
//        	   scan.getIntensityList()(nearestPeakIdx) 
//           } else {
//             penality 
//           }
//         }
//         val d = ((ipAbundance - abundance) / math.min(abundance, ipAbundance)) * 1.0/penality
//         score += d * d
//      }
//	  
//      score = Math.log10(score) - matches
//      (score, pattern)
//  } 

}