package fr.profi.mzdb.algo

import scala.collection.mutable.ArrayBuffer

import com.almworks.sqlite4java.SQLiteConnection

import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.SpectrumData
import com.typesafe.scalalogging.LazyLogging

trait IIsotopicPatternPredictor extends LazyLogging {

  def isMatchReliable(
    spectrumData: SpectrumData,
    ppm: Float,
    moz: Double,
    charge: Int,
    mozTolInDa: Double
  ): Boolean = {

    val putativePatterns = IsotopicPatternScorer.calcIsotopicPatternHypotheses(spectrumData, moz, ppm)
    val bestPattern = putativePatterns.head
    
    (bestPattern._2.charge == charge) && (math.abs(bestPattern._2.monoMz - moz) <= mozTolInDa)
  }

}

object MzDbPatternPredictor extends IIsotopicPatternPredictor {

  def getBestExplanation(
    reader: MzDbReader,
    sqliteConn: SQLiteConnection,
    peakel: Peakel,
    charge: Int,
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
    
    val putativePatterns = IsotopicPatternScorer.calcIsotopicPatternHypotheses(
      sliceOpt.get.getData(),
      peakel.getApexMz(),
      ppmTol
    )
    
    putativePatterns.head
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
    peakel: Peakel,
    charge: Int,
    mozTolInDa: Double
  ): (Double, TheoreticalIsotopePattern) = {

    val matchingSpectrumId = peakel.getApexSpectrumId()
    val coelutingPeakelsCount = coelutingPeakels.length
    //logger.debug(s"Found $coelutingPeakelsCount co-eluting peakels")

    val mzList = new ArrayBuffer[Double](coelutingPeakelsCount)
    val intensityList = new ArrayBuffer[Float](coelutingPeakelsCount)

    // Slice the obtained peakels to create a virtual spectrum
    coelutingPeakels.map { peakel =>
      val peakelCursor = peakel.getNewCursor()
      var foundPeak = false

      // TODO: optimize this search (start from the apex or implement binary search)
      while (peakelCursor.next() && foundPeak == false) {
        if (peakelCursor.getSpectrumId() == matchingSpectrumId) {
          mzList += peakelCursor.getMz()
          intensityList += peakelCursor.getIntensity()
          foundPeak = true
        }
      }
    }

    val spectrumData = new SpectrumData(mzList.toArray, intensityList.toArray)
    val putativePatterns = IsotopicPatternScorer.calcIsotopicPatternHypotheses(spectrumData, peakel.getApexMz(), mozTolPPM)
    putativePatterns.head
  }

  def assessReliability(
    mozTolPPM: Float,
    coelutingPeakels: Seq[Peakel],
    matchingPeakels: Seq[Peakel],
    charge: Int,
    mozTolInDa: Double
  ): ArrayBuffer[(Peakel, Boolean)] = {

    val filteredPeakels = new ArrayBuffer[(Peakel, Boolean)](matchingPeakels.length)

    // DBO: the if statement was previously used to decrease the impact of the mzDB lookup
    // Now the lookup is performed using the in-memory R*Tree
    //if (matchingPeakels.length == 1) {
    for (matchingPeakel <- matchingPeakels) {

      val (mzList, intensityList) = slicePeakels(coelutingPeakels, matchingPeakel)

      val spectrumData = new SpectrumData(mzList.toArray, intensityList.toArray)
      val isReliable = isMatchReliable(spectrumData, mozTolPPM, matchingPeakel.getApexMz(), charge, mozTolInDa)

//      logger.info("assess reliability for mz={}, spectrumId={} against {} data leads to {}", matchingPeakel.getApexMz(), matchingPeakel.getApexSpectrumId(), mzList.size, isReliable) 
      
      filteredPeakels += Tuple2(matchingPeakel, isReliable)

    }

    filteredPeakels
  }

  def slicePeakels(coelutingPeakels: Seq[Peakel],matchingPeakel: Peakel): (ArrayBuffer[Double], ArrayBuffer[Float]) = {
    // Slice the obtained peakels to create a virtual spectrum
    val matchingSpectrumId = matchingPeakel.getApexSpectrumId()
    val mzList = new ArrayBuffer[Double](coelutingPeakels.length)
    val intensityList = new ArrayBuffer[Float](coelutingPeakels.length)
      
    coelutingPeakels.sortBy(_.getApexMz()).map { peakel =>
      val peakelCursor = peakel.getNewCursor()
      var foundPeak = false

      // TODO: optimize this search (start from the apex or implement binary search)
      while (peakelCursor.next() && foundPeak == false) {
        if (peakelCursor.getSpectrumId() == matchingSpectrumId) {
          mzList += peakelCursor.getMz()
          intensityList += peakelCursor.getIntensity()
          foundPeak = true
        }
      }
    }
    
    (mzList, intensityList)
  }
  
}
