package fr.profi.mzdb.algo

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.LazyLogging
import fr.profi.ms.algo.IsotopePatternEstimator
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.model.SpectrumData

trait IIsotopicPatternScorer extends LazyLogging {

  private val MAX_CHARGE = 8

  /**
   * Tries to explain a peak at the specified mz value by testing different isotopic pattern explanations.
   *
   * @param spectrum the MS data (mz, intensities) signal around the peak to explain
   * @param mz       the mz of the peak that must be explained
   * @param ppmTol
   * @return a list of isotopic patterns tested, ordered by score (better = higher score first).
   */
  def calcIsotopicPatternHypotheses(spectrum: SpectrumData, mz: Double, ppmTol: Double): Array[(Double, TheoreticalIsotopePattern)] = {

    var result = ArrayBuffer[(Double, TheoreticalIsotopePattern)]()
    for (charge <- 1 to MAX_CHARGE) {
      val (score, theoreticalIP) = getIPHypothesis(spectrum, mz, 0, charge, ppmTol)
      result += (score -> theoreticalIP)

      var j = 1
      var backwardMz = mz - j * IsotopePatternEstimator.avgIsoMassDiff / charge
      var nearestPeakIdx = spectrum.getNearestPeakIndex(backwardMz)
      var existBackward = (1e6 * Math.abs(spectrum.getMzList()(nearestPeakIdx) - backwardMz) / backwardMz) < ppmTol

      while (existBackward && j <= theoreticalIP.theoreticalMaxPeakelIndex + 1) {
        val (score, theoreticalIP) = getIPHypothesis(spectrum, mz, j, charge, ppmTol)
        result += (score -> theoreticalIP)
        j = j + 1
        backwardMz = mz - j * IsotopePatternEstimator.avgIsoMassDiff / charge
        nearestPeakIdx = spectrum.getNearestPeakIndex(backwardMz)
        existBackward = (1e6 * Math.abs(spectrum.getMzList()(nearestPeakIdx) - backwardMz) / backwardMz) < ppmTol
      }
    }

    result = result.sortWith { (p1, p2) =>
      ((p1._1 == p2._1) && (p1._2.charge < p2._2.charge)) ||
        ((p1._1 != p2._1) && (p1._1 < p2._1))
    }
    result.toArray
  }

  def getIPHypothesis(spectrum: SpectrumData, initialMz: Double, isotopicShift: Int, charge: Int, ppmTol: Double): (Double, TheoreticalIsotopePattern)

  def selectBestPatternHypothese(putativePatterns: Array[(Double, TheoreticalIsotopePattern)], deltaScore: Double = 0.1): (Double, TheoreticalIsotopePattern)

}

/**
 * This object defines two methods: the first one scores an Isotopic Pattern hypothesis while the second one
 * search for the best isotopic pattern (but returns all hypothesis ordered by score)
 *
 * @author CB205360
 *
 */
object LegacyIsotopicPatternScorer extends IIsotopicPatternScorer {


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


  def selectBestPatternHypothese(putativePatterns: Array[(Double, TheoreticalIsotopePattern)], deltaScore: Double = 0.1): (Double, TheoreticalIsotopePattern) = {
    putativePatterns.head
  }

}

object DotProductPatternScorer extends IIsotopicPatternScorer {

    def getIPHypothesis(spectrum: SpectrumData, initialMz: Double, isotopicShift: Int, charge: Int, ppmTol: Double): (Double, TheoreticalIsotopePattern) = {
      var score = 0.0
      val mz = initialMz - isotopicShift * IsotopePatternEstimator.avgIsoMassDiff / charge
      val pattern = IsotopePatternEstimator.getTheoreticalPattern(mz, charge)

      val scale = spectrum.getIntensityList()(spectrum.getNearestPeakIndex(initialMz)).toDouble / pattern.mzAbundancePairs(isotopicShift)._2

      var ipMoz = mz
      val observed = new Array[Double](pattern.mzAbundancePairs.length)
      val expected = new Array[Double](pattern.mzAbundancePairs.length)
      for (rank <- 0 until pattern.mzAbundancePairs.length) {
        ipMoz = if (rank == 0) ipMoz
        else ipMoz + IsotopePatternEstimator.avgIsoMassDiff / charge
        val nearestPeakIdx = spectrum.getNearestPeakIndex(ipMoz)
        if ((1e6 * Math.abs(spectrum.getMzList()(nearestPeakIdx) - ipMoz) / ipMoz) < ppmTol) {
          observed(rank) = spectrum.getIntensityList()(nearestPeakIdx)
        }
        else { //  minus expected abundance to penalise signal absence
          observed(rank) = -pattern.mzAbundancePairs(rank)._2 * scale
        }
        expected(rank) = pattern.mzAbundancePairs(rank)._2
      }

      score = dotProduct(observed, expected)
      score = 1.0 - score
      (score, pattern)
    }

    def dotProduct(observed: Array[Double], expected: Array[Double]): Double = {
      var sumObserved = 0.0
      var sumExpected = 0.0
      var product = 0.0
      var k = 0

      var weights = expected.take(6)
      weights = weights.map( _ / weights.sum)

      while ( (k < observed.length) ) {
        val weight = if (k < weights.size) { weights(k) } else { 0.0 }
        product += weight * observed(k) * expected(k)
        sumExpected += weight * expected(k) * expected(k)
        sumObserved += weight * observed(k) * observed(k)

        k += 1
      }
      if ((sumExpected == 0) || (sumObserved == 0)) 0.0
      else product / (Math.sqrt(sumExpected) * Math.sqrt(sumObserved))
    }

  def selectBestPatternHypothese(putativePatterns: Array[(Double, TheoreticalIsotopePattern)], deltaScore: Double = 0.1): (Double, TheoreticalIsotopePattern) = {
    val refScore = putativePatterns.head._1
    val patterns = putativePatterns.filter(p => math.abs(p._1 - refScore) < deltaScore)
    patterns.maxBy(p => p._2.charge)
  }

}