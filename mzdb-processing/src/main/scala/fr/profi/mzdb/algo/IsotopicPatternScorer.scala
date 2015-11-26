package fr.profi.mzdb.algo

import fr.profi.mzdb.model.SpectrumData
import scala.collection.immutable.TreeMap
import fr.profi.ms.model.TheoreticalIsotopePattern
import scala.collection.immutable.SortedMap
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.ms.algo.IsotopePatternEstimator
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.LazyLogging

/**
 * This object defines two methods : the first one scores an Isotopic Pattern hypothesis while the second one 
 * search for the best isotopic pattern (but returns all hypothesis ordered by score)
 * 
 * @author CB205360
 *
 */
object IsotopicPatternScorer extends LazyLogging {
  
 /**
  * Tries to explain a peak at the specified mz value by testing different isotopic pattern explanations. 
  * 
  * 
 * @param spectrum : the MS data (mz, intensities) signal around the peak to explain
 * @param mz : the mz of the peak that must be explained 
 * @param ppmTol
 * @return : a list of isotopic patterns tested, ordered by score (better = higher score first).
 */
def calclIsotopicPatternHypotheses(spectrum : SpectrumData, mz: Double, ppmTol : Double): Array[(Double, TheoreticalIsotopePattern)] = {
     var result = ArrayBuffer[(Double, TheoreticalIsotopePattern)]()
     for (charge <- 1 to 5) {
//         val (score, theoreticalIP) = getIPHypothesis(spectrum, mz, charge, ppmTol)
         val (score, theoreticalIP) = getIPHypothesis(spectrum, mz, 0, charge, ppmTol)
         result += (score -> theoreticalIP)
         for (j <- 1 to theoreticalIP.theoreticalMaxPeakelIndex+1) {
//            val alternativeMoz = mz - j * IsotopePatternEstimator.avgIsoMassDiff / charge
//            val (score, theoreticalIP) = getIPHypothesis(spectrum, alternativeMoz, charge, ppmTol)
            val (score, theoreticalIP) = getIPHypothesis(spectrum, mz, j, charge, ppmTol)
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
 * @param spectrum : the MS data (mz, intensities) signal around the peak to explain
 * @param mz : the mz of the peak that must be explained 
 * @param charge : the isotopic pattern charge state
 * @param ppmTol
 * @return : the score and the theoretical isotopic pattern.
 */
def getIPHypothesis(spectrum : SpectrumData, mz : Double, charge : Int, ppmTol : Double) : (Double, TheoreticalIsotopePattern) = {
	  var score = 0.0
      val pattern = IsotopePatternEstimator.getTheoreticalPattern(mz, charge)
      var ipMoz = mz
      val isotopeAbundance = spectrum.getIntensityList()(spectrum.getNearestPeakIndex(ipMoz))
      val normAbundance = pattern.mzAbundancePairs(0)._2
      var rank = 0
      var matches = 0
      for (rank <- 0 until pattern.mzAbundancePairs.length) {
         ipMoz = if (rank == 0) ipMoz else ipMoz + IsotopePatternEstimator.avgIsoMassDiff/charge
         val ipAbundance = (pattern.mzAbundancePairs(rank)._2) * isotopeAbundance / normAbundance
         var nearestPeakIdx =  spectrum.getNearestPeakIndex(ipMoz)
         val penality = Math.max(0.01, Math.pow(10, -(2*rank - 4))) // Math.min(100.0, 0.0001 * Math.pow(10, rank*2));
         val abundance = { 
           if ((1e6 * Math.abs(spectrum.getMzList()(nearestPeakIdx) - ipMoz) / ipMoz) < ppmTol) {
               ipMoz = spectrum.getMzList()(nearestPeakIdx)
               matches += 1
        	   spectrum.getIntensityList()(nearestPeakIdx) 
           } else {
             ipAbundance/1000.0f 
           }
         }
         val d = ((ipAbundance - abundance) / math.min(abundance, ipAbundance)) * penality
         score += d * d
      }
	  
      score = Math.log10(score) - matches
      (score, pattern)
  }

  /**
   * Scores a single isotopic pattern hypothesis. The score is based on the relative difference between predicted and
   * observed intensities. In case a theoretical peak is not found in the MS data a penality is applied to the difference.
   * The penality is less important as the rank of the isotopic peak is high.
   *
   * @param spectrum : the MS data (mz, intensities) signal around the peak to explain
   * @param mz : the mz of the peak that must be explained
   * @param charge : the isotopic pattern charge state
   * @param matchingPeakIdx : the index of the peak to explain in the MS data array
   * @param matchingIsotopeIdx : the index of the isotope that must match this peak
   * @param ppmTol
   * @return : the score and the theoretical isotopic pattern.
   */
  def getIPHypothesis(spectrum: SpectrumData, initialMz: Double, isotopicShift: Int, charge: Int, ppmTol: Double): (Double, TheoreticalIsotopePattern) = {
    var score = 0.0
    val alternativeMoz = initialMz - isotopicShift * IsotopePatternEstimator.avgIsoMassDiff / charge
    //logger.info(s"Testing hypotheses monoMz = $alternativeMoz, charge = $charge")
    val pattern = IsotopePatternEstimator.getTheoreticalPattern(alternativeMoz, charge)
    val isotopeAbundance = spectrum.getIntensityList()(spectrum.getNearestPeakIndex(alternativeMoz))
    val normAbundance = pattern.mzAbundancePairs(0)._2
    var ipMoz = alternativeMoz
    var rank = 0
    var matches = 0
    for (rank <- 0 until pattern.mzAbundancePairs.length) {
      ipMoz = if (rank == 0) ipMoz else ipMoz + IsotopePatternEstimator.avgIsoMassDiff / charge
      val ipAbundance = (pattern.mzAbundancePairs(rank)._2) * isotopeAbundance / normAbundance
      var nearestPeakIdx = spectrum.getNearestPeakIndex(ipMoz)
      val penality = if (rank == isotopicShift) 1e2 else Math.max(0.01, Math.pow(10, -(2 * rank - 4)))
      val abundance = {
        if ((1e6 * Math.abs(spectrum.getMzList()(nearestPeakIdx) - ipMoz) / ipMoz) < ppmTol) {
          ipMoz = spectrum.getMzList()(nearestPeakIdx)
          matches += 1
          spectrum.getIntensityList()(nearestPeakIdx)
        } else {
          ipAbundance / 1000.0f
        }
      }
      val d = ((ipAbundance - abundance) / math.min(abundance, ipAbundance)) * penality
      //logger.info(s"mz = $ipMoz, rank = $rank, penality = $penality, d = $d")
      score += d * d
    }

    score = Math.log10(score) - matches
    (score, pattern)
  }

}