package fr.profi.mzdb.algo

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import com.typesafe.scalalogging.LazyLogging

import fr.profi.ms.algo.IsotopePatternEstimator
import fr.profi.ms.model.TheoreticalIsotopePattern

import fr.profi.mzdb.model.SpectrumData

/**
 * This object defines two methods: the first one scores an Isotopic Pattern hypothesis while the second one
 * search for the best isotopic pattern (but returns all hypothesis ordered by score)
 *
 * @author CB205360
 *
 */

// TODO: optimize this code

object IsotopicPatternScorer extends LazyLogging {
  
  private val MAX_CHARGE = 5
		
 /**
  * Tries to explain a peak at the specified mz value by testing different isotopic pattern explanations. 
  * 
  * 
 * @param spectrum the MS data (mz, intensities) signal around the peak to explain
 * @param mz the mz of the peak that must be explained 
 * @param ppmTol
 * @return a list of isotopic patterns tested, ordered by score (better = higher score first).
 */
def calcIsotopicPatternHypotheses(spectrum : SpectrumData, mz: Double, ppmTol : Double): Array[(Double, TheoreticalIsotopePattern)] = {
     var result = ArrayBuffer[(Double, TheoreticalIsotopePattern)]()
     for (charge <- 1 to MAX_CHARGE) {
         val (score, theoreticalIP) = getIPHypothesis(spectrum, mz, 0, charge, ppmTol)
         result += (score -> theoreticalIP)
         for (j <- 1 to theoreticalIP.theoreticalMaxPeakelIndex+1) {
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
   * @param spectrum the MS data (mz, intensities) signal around the peak to explain
   * @param mz the mz of the peak that must be explained
   * @param charge the isotopic pattern charge state
   * @param matchingPeakIdx the index of the peak to explain in the MS data array
   * @param matchingIsotopeIdx the index of the isotope that must match this peak
   * @param ppmTol
   * @return the score and the theoretical isotopic pattern.
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
      val rankPenality = if (rank == isotopicShift) 1e2 else Math.max(0.01, Math.pow(10, -(2 * rank - 4)))
      val abundance = {
        if ((1e6 * Math.abs(spectrum.getMzList()(nearestPeakIdx) - ipMoz) / ipMoz) < ppmTol) {
          ipMoz = spectrum.getMzList()(nearestPeakIdx)
          matches += 1
          spectrum.getIntensityList()(nearestPeakIdx)
        } else {
          // No peak found at the expected mz value : creates an artificial distance from expected abundance value 
          ipAbundance / 1000.0f
        }
      }
      val d = ((ipAbundance - abundance) / math.min(abundance, ipAbundance)) * rankPenality
      score += d * d
    }

    score = Math.log10(score) - matches
    (score, pattern)
  }
  
  // --- THE END OF THE FILE CORRESPOND TO A TENTATIVE BY DBO TO PROVIDE A NEW API (this code is buggy and must be fixed, before being used) --- //
  
  /**
   * Tries to explain a peak at the specified mz value by testing different isotopic pattern explanations.
   *
   * @param spectrum The MS data (mz, intensities) signal around the peak to explain.
   * @param mz The mz of the peak that must be explained.
   * @param ppmTol
   * @return A list of isotope patterns tested, ordered by descending score (better = higher score first).
   */
  // FIXME: fix the computation and set as public method
  private def calcIsotopicPatternHypothesesV2(spectrum: SpectrumData, mz: Double, ppmTol: Double): Array[(Double, TheoreticalIsotopePattern)] = {
    
    // Create a HashMap that will be used to cache already retrieved [m/z,intensity] pairs
    val mzIntPairCache = new HashMap[Double,(Double,Float)]()
    
    val getNearestMzIntPair = (mz: Double) => {
      mzIntPairCache.getOrElseUpdate(mz, {
        val peakIdx = spectrum.getNearestPeakIndex(mz)
        (spectrum.getMzList()(peakIdx) ,spectrum.getIntensityList()(peakIdx))
      })
    }
    
    this.calcIsotopicPatternHypotheses(
      getNearestMzIntPair,
      mz,
      ppmTol
    )
  }

  /**
   * Tries to explain a peak at the specified mz value by testing different isotopic pattern explanations.
   *
   * @param getNearestMzIntPair A function that is able to provide an [m/z,intensity] pair for a given m/z value.
   * @param mz The mz of the peak that must be explained.
   * @param ppmTol
   * @return A list of isotope patterns tested, ordered by descending score (better = higher score first).
   */
  // FIXME: fix the computation and set as public method
  private def calcIsotopicPatternHypotheses(
    getNearestMzIntPair: Double => (Double,Float),
    mz: Double,
    ppmTol: Double
  ): Array[(Double, TheoreticalIsotopePattern)] = {
    
    // We guess that we won't have more than 4 possible m/z values per charge state
    val maxExpectedResultsCount = MAX_CHARGE * 4
    val results = new ArrayBuffer[(Double, TheoreticalIsotopePattern)](maxExpectedResultsCount)
    
    for (charge <- 1 to MAX_CHARGE) {
      
      // Determine the theoretical IP for the initial m/z value
      val (score, theoreticalIP) = calcIsotopicPatternHypothesis(getNearestMzIntPair, mz, 0, charge, ppmTol)
      results += (score -> theoreticalIP)
      
      // Determine other theoretical IPs for alternative m/z values (interference expected to be before the initial m/z)
      for (j <- 1 to theoreticalIP.theoreticalMaxPeakelIndex + 1) {
        results += calcIsotopicPatternHypothesis(getNearestMzIntPair, mz, j, charge, ppmTol)
      }
    }

    results
      .sortWith { case ( (s1: Double, p1: TheoreticalIsotopePattern), (s2: Double, p2: TheoreticalIsotopePattern) ) =>
        ( (s1 == s2) && (p1.charge < p2.charge) ) || (s1 < s2)
      }
      .toArray
  }
  
  /**
   * Scores a single isotopic pattern hypothesis. The score is based on the relative difference between predicted and
   * observed intensities. In case a theoretical peak is not found in the MS data a penality is applied to the difference.
   * The penality is less important as the rank of the isotope peak is high.
   *
   * @param spectrum The MS data (mz, intensities) signal around the peak to explain.
   * @param mz The mz of the peak that must be explained.
   * @param charge The isotope pattern charge state.
   * @param matchingPeakIdx The index of the peak to explain in the MS data array.
   * @param matchingIsotopeIdx The index of the isotope that must match this peak.
   * @param ppmTol
   * @return The score and the theoretical isotope pattern.
   */
  private def calcIsotopicPatternHypothesis(
    getNearestMzIntPair: Double => (Double,Float),
    initialMz: Double,
    isotopicShift: Int,
    charge: Int,
    ppmTol: Double
  ): (Double, TheoreticalIsotopePattern) = {
    
    val alternativeMz = initialMz - isotopicShift * IsotopePatternEstimator.avgIsoMassDiff / charge
    //logger.info(s"Testing hypotheses monoMz = $alternativeMoz, charge = $charge")
    
    val pattern = IsotopePatternEstimator.getTheoreticalPattern(alternativeMz, charge)
    
    val mzIntPair = getNearestMzIntPair(alternativeMz)
    val monoIsoAb = mzIntPair._2//spectrum.getIntensityList()(spectrum.getNearestPeakIndex(mz))
    val monoIsoTheoAb = pattern.mzAbundancePairs(0)._2
    
    var score = 0.0
    var ipMz = alternativeMz
    var rank = 0
    var matches = 0
    
    for (rank <- 0 until pattern.mzAbundancePairs.length) {
      ipMz = if (rank == 0) ipMz else ipMz + IsotopePatternEstimator.avgIsoMassDiff / charge
      
      // TODO: compute an array of coeffs instead of duplicating the computation of the rankPenality
      val rankPenality = if (rank == isotopicShift) 1e2 else Math.max(0.01, Math.pow(10, -(2 * rank - 4)))
      
      // Compute scaling factor between current theoretical isotope and theoretical monoisotopic isotope
      val scalingToMonoFactor = (pattern.mzAbundancePairs(rank)._2) / monoIsoTheoAb
      
      // Normalize the observed monoisotopic abundance using the scaling factor
      val expectedAbundance = monoIsoAb * scalingToMonoFactor
      //val nearestPeakIdx = spectrum.getNearestPeakIndex(ipMz)
      
      val nearestMzIntPair = getNearestMzIntPair(alternativeMz)
      
      val foundAbundance = {
        val mzValue = nearestMzIntPair._1//spectrum.getMzList()(nearestPeakIdx)
        
        if ((1e6 * Math.abs(mzValue - ipMz) / ipMz) < ppmTol) {
          // Update ipMz for the next lookup
          ipMz = mzValue
          matches += 1
          nearestMzIntPair._2
        } else {
          // No peak found at the expected m/z value: creates an artificial distance from expected abundance value 
          expectedAbundance / 1000.0f
        }
      }
      
      // TODO: DBO => explain why computing math.min(foundAbundance, expectedAbundance), why not always use the expectedAbundance ?
      val relativeDiff = ((expectedAbundance - foundAbundance) / math.min(foundAbundance, expectedAbundance)) * rankPenality
      //logger.info(s"mz = $ipMoz, rank = $rank, penality = $penality, d = $d")
      
      score += relativeDiff * relativeDiff
    }

    // TODO: DBO => explain why use Math.log10 instead of Math.sqrt( score / matches )
    score = Math.log10(score) - matches
    
    (score, pattern)
  }

}