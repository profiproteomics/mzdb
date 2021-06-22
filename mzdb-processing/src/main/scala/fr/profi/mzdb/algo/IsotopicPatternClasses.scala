package fr.profi.mzdb.algo

import scala.collection.mutable.ArrayBuffer
import com.almworks.sqlite4java.SQLiteConnection
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.SpectrumData
import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.util.ms.MsUtils

import java.util

trait IIsotopicPatternPredictor extends LazyLogging {

  val isotopicPatternScorer = LegacyIsotopicPatternScorer

  def isMatchReliable(
    spectrumData: SpectrumData,
    ppm: Double,
    moz: Double,
    charge: Int,
    mozTolInDa: Double
  ): Boolean = {

    val putativePatterns = isotopicPatternScorer.calcIsotopicPatternHypotheses(spectrumData, moz, ppm)
    val bestPattern = isotopicPatternScorer.selectBestPatternHypothese(putativePatterns)
    
    (bestPattern._2.charge == charge) && (math.abs(bestPattern._2.monoMz - moz) <= mozTolInDa)
  }


}

object MzDbPatternPredictor extends IIsotopicPatternPredictor {

  def getBestExplanation(
    reader: MzDbReader,
    sqliteConn: SQLiteConnection,
    peakel: Peakel,
    mozTolInDa: Double
  ): (Double, TheoreticalIsotopePattern) = {

    val apexMz = peakel.getApexMz
    val apexRt = peakel.getApexElutionTime

    val slices = reader.getMsSpectrumSlices(
      apexMz - 5,
      apexMz + 5,
      apexRt - 0.1f,
      apexRt + 0.1f
    )

    val sliceOpt = slices.find(_.getHeader.getSpectrumId == peakel.getApexSpectrumId)

    val ppmTol = if (peakel.getLeftHwhmMean == 0) (1e6 * mozTolInDa / peakel.getApexMz()).toFloat
    else (1e6 * peakel.getLeftHwhmMean / apexMz).toFloat
    
    val putativePatterns = isotopicPatternScorer.calcIsotopicPatternHypotheses(
      sliceOpt.get.getData(),
      peakel.getApexMz(),
      ppmTol
    )
    isotopicPatternScorer.selectBestPatternHypothese(putativePatterns)
  }

  def assessReliability(
    reader: MzDbReader,
    sqliteConn: SQLiteConnection,
    matchingPeakels: Array[Peakel],
    charge: Int,
    mozTolInDa: Double
  ): ArrayBuffer[(Peakel, Boolean)] = {

    val filteredPeakels = new ArrayBuffer[(Peakel, Boolean)](matchingPeakels.length)

    for (matchingPeakel <- matchingPeakels) {

      val apexMz = matchingPeakel.getApexMz
      val apexRt = matchingPeakel.getApexElutionTime

      val slices = reader.getMsSpectrumSlices(
        apexMz - 5,
        apexMz + 5,
        apexRt - 0.1f,
        apexRt + 0.1f
      )

      val sliceOpt = slices.find(_.getHeader.getSpectrumId == matchingPeakel.getApexSpectrumId)

      if (sliceOpt.isDefined) {
        val ppm = if (matchingPeakel.getLeftHwhmMean == 0) (1e6 * mozTolInDa / matchingPeakel.getApexMz()).toFloat else (1e6 * matchingPeakel.getLeftHwhmMean / apexMz).toFloat
        val isReliable = isMatchReliable(sliceOpt.get.getData(), ppm, matchingPeakel.getApexMz(), charge, mozTolInDa)
        filteredPeakels += Tuple2(matchingPeakel, isReliable)
      }
    }

    filteredPeakels
  }
}

object PeakelsPatternPredictor extends IIsotopicPatternPredictor with LazyLogging {

  def getBestExplanation(
    mozTolPPM: Float,
    coelutingPeakels: Seq[Peakel],
    peakel: Peakel
  ): (Double, TheoreticalIsotopePattern) = {

    val spectrumData: SpectrumData = buildSpectrumFromPeakels(coelutingPeakels, peakel)
    val putativePatterns = isotopicPatternScorer.calcIsotopicPatternHypotheses(spectrumData, peakel.getApexMz(), mozTolPPM)
    isotopicPatternScorer.selectBestPatternHypothese(putativePatterns)
  }

  def assessReliability(
    mozTolPPM: Float,
    coelutingPeakels: Seq[Peakel],
    matchingPeakels: Seq[Peakel],
    charge: Int,
    mozTolInDa: Double
  ): ArrayBuffer[(Peakel, Boolean)] = {

    val filteredPeakels = new ArrayBuffer[(Peakel, Boolean)](matchingPeakels.length)

    for (matchingPeakel <- matchingPeakels) {
      val spectrumData: SpectrumData = buildSpectrumFromPeakels(coelutingPeakels, matchingPeakel)
      val isReliable = isMatchReliable(spectrumData, mozTolPPM, matchingPeakel.getApexMz(), charge, mozTolInDa)
      filteredPeakels += Tuple2(matchingPeakel, isReliable)
    }

    filteredPeakels
  }

  def buildSpectrumFromPeakels(coelutingPeakels: Seq[Peakel], peakel: Peakel): SpectrumData = {
    val (mzList, intensityList) = this.slicePeakels(coelutingPeakels, peakel.getApexSpectrumId())
    val spectrumData = new SpectrumData(mzList.toArray, intensityList.toArray)
    spectrumData
  }


  /**
   * Slice the list of peakels at a specified Spectrum Id to build a virtual spectrum
   *
   * @param coelutingPeakels
   * @param matchingSpectrumId
   * @return
   */
  def slicePeakels(coelutingPeakels: Seq[Peakel], matchingSpectrumId: Long): (ArrayBuffer[Double], ArrayBuffer[Float]) = {
    val coelutingPeakelsCount = coelutingPeakels.length
    
    val mzList = new ArrayBuffer[Double](coelutingPeakelsCount)
    val intensityList = new ArrayBuffer[Float](coelutingPeakelsCount)

    val SPAN = 1

    coelutingPeakels.sortBy(_.getApexMz()).map { peakel =>

          var index = util.Arrays.binarySearch(peakel.getSpectrumIds, matchingSpectrumId)
          var foundPeak = (index >= 0) && (index < peakel.peaksCount)
          if (!foundPeak) {
            index = ~index
            foundPeak = index > 0 && index < peakel.peaksCount
          }
          var intensitySum = 0.0f
          var mzSum = 0.0
          var count = 0
          if (foundPeak) {
            val minBound = Math.max(0, index - SPAN)
            val maxBound = Math.min(index + SPAN, peakel.getPeaksCount - 1)
            for (i <- minBound to maxBound) {
              intensitySum = intensitySum + peakel.intensityValues(i)
              mzSum = mzSum + peakel.mzValues(i);
              count = count + 1
            }
            mzList += (mzSum/count) //peakel.mzValues(index)
            intensityList += (intensitySum / count)
          }
      }

    
    (mzList, intensityList)
  }
  
}
