package fr.profi.mzdb.io.writer.mgf

import com.almworks.sqlite4java.SQLiteException
import com.typesafe.scalalogging.LazyLogging
import fr.profi.ms.algo.IsotopePatternEstimator
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.algo.DotProductPatternScorer
import fr.profi.mzdb.db.model.params.param.CVEntry
import fr.profi.mzdb.model.{IonMobilityType, Peak, SpectrumHeader, SpectrumSlice}
import fr.profi.mzdb.{MzDbReader, Settings}
import fr.profi.util.metrics.Metric

import scala.util.control.Breaks.{break, breakable}

/**
 * @author CB205360
 */

object MgfBoostPrecursorExtractor {

  def readIonMobilityCV(spectrumHeader: SpectrumHeader): Option[String] = {
    val cvParam = spectrumHeader.getCVParam(CVEntry.FAIMS_COMPENSATION_VOLTAGE)
    if (cvParam != null) {
      Some(cvParam.getValue)
    } else {
      None
    }
  }

}


case class SpectrumDataSource(var spectrumHeader : SpectrumHeader,
                              var isolationWindowBounds : Array[Float],
                              var nearestSlice : Option[SpectrumSlice] = None) extends LazyLogging {


  def getNearestSpectrumSlice() : Option[SpectrumSlice] = {
    nearestSlice
  }

  def getNearestPeak(targetPrecMz: Double, mzTolPPM: Float) : Option[Peak] = {
    if (nearestSlice.isDefined) {
      val p = nearestSlice.get.getNearestPeak(targetPrecMz, mzTolPPM)
      Option(p)
    } else {
      None
    }
  }


  def isInIsolationWindow(mz: Double): Boolean = {
    mz >= isolationWindowBounds(0) && mz <= isolationWindowBounds(1)
  }

  def getCandidatePeaksFromIsolationWindow(swIntensityThreshold: Float): Array[Peak] = {
    var peaks = Array.empty[Peak]
    val allPeaks = getAllPeaksFromIsolationWindow()
    if (!allPeaks.isEmpty) {
      val maxPeak = allPeaks.maxBy(_.getIntensity)
      peaks = peaks ++ allPeaks.filter(_.getIntensity > swIntensityThreshold*maxPeak.getIntensity).sortBy(_.getIntensity).reverse
    }
    peaks
  }

  def getAllPeaksFromIsolationWindow(): Array[Peak] = {
    if (!nearestSlice.isDefined) {
      logger.error("No nearest slice for scan #{}.", spectrumHeader.getSpectrumId)
      return Array.empty[Peak]
    }
    nearestSlice.get.toPeaks.filter(p => (p.getMz >= isolationWindowBounds(0)) && (p.getMz <=  isolationWindowBounds(1)))
  }

}

case class IsotopicPatternMatch(var score: Double, var theoreticalPattern: TheoreticalIsotopePattern, matchingPeaks: List[Option[Peak]])

object IsotopicPatternMatch {

  def apply(score: Double, theoreticalPattern: TheoreticalIsotopePattern) = new IsotopicPatternMatch(score, theoreticalPattern, List.fill(theoreticalPattern.isotopeCount)(None))

  def apply(score: Double, theoreticalPattern: TheoreticalIsotopePattern, spectrumSlice: SpectrumSlice, mzTolPPM: Float) = {

    val peaks = for (p <- theoreticalPattern.mzAbundancePairs) yield {
      val peak = spectrumSlice.getNearestPeak(p._1, mzTolPPM)
      Option(peak)
    }
    new IsotopicPatternMatch(score, theoreticalPattern, peaks.toList)
  }

}

object ScanSelectorModes extends Enumeration {
  type ScanSelectorMode = Value
  val MASTER_SCAN, SAME_CYCLE, NEAREST, ALL = Value
}

class MgfBoostPrecursorExtractor(mzTolPPM: Float,
                                 useHeader: Boolean = true,
                                 useSW: Boolean = true,
                                 swMaxPrecursorsCount: Int = 1,
                                 swIntensityThreshold: Float = 0.2f,
                                 scanSelector : ScanSelectorModes.Value = ScanSelectorModes.SAME_CYCLE,
                                 pifThreshold: Double = 0.125,
                                 rankThreshold : Int = 2) extends DefaultPrecursorComputer(mzTolPPM) {

  private val metric = new Metric("MgfBoost")

  @throws[SQLiteException]
  override def getMgfPrecursors(mzDbReader: MzDbReader, spectrumHeader: SpectrumHeader): Array[MgfPrecursor] = {

    var precursors = Seq[MgfPrecursor]()
    var headerPrecursor : MgfPrecursor = null

    val spectrumData = _buildSpectrumDataSource(mzDbReader, spectrumHeader, scanSelector)
    val altPrecursorPeaks = spectrumData.getCandidatePeaksFromIsolationWindow(swIntensityThreshold)

    // try to predict the precursor mz from the mz value found in the MS2 header

    if (useHeader) {
      val headerPrecMz = _getHeaderPrecursorMz(spectrumHeader)
      val refinedHeaderMz = spectrumData.getNearestPeak(headerPrecMz, mzTolPPM).map(_.getMz).getOrElse(headerPrecMz)
      val (predictionOpt, predictionNote, patternMatchOpt) = _predictPrecursorFromTarget(spectrumData, refinedHeaderMz, spectrumHeader.getPrecursorCharge)
      val (refinedHeaderPrecMz, charge) = predictionOpt.get //predictionOpt.getOrElse((refinedHeaderMz, spectrumHeader.getPrecursorCharge.intValue())) // (headerPrecMz, spectrumHeader.getPrecursorCharge)

      val precursorOpt = _buildMgfPrecursor(spectrumHeader.getElutionTime, refinedHeaderPrecMz, charge)
      if (precursorOpt.isDefined) {
        val precursor = precursorOpt.get

        if (Math.abs(1e6 * (headerPrecMz - refinedHeaderPrecMz) / headerPrecMz) > mzTolPPM) {
          precursor.addAnnotation("source", "isotope.header")
        } else {
          precursor.addAnnotation("source", "monoisotope.header")
        }

        if (spectrumData.nearestSlice.isDefined)
          precursor.addAnnotation("ms1.scan.number", spectrumData.nearestSlice.get.getHeader.getSpectrumId )
        precursor.addAnnotation("in.sw", spectrumData.isInIsolationWindow(refinedHeaderPrecMz))
        precursor.addAnnotation("scan.number", spectrumHeader.getSpectrumId)
        precursor.addAnnotation("prediction", predictionNote)
        if (patternMatchOpt.isDefined) precursor.addAnnotation("prediction.pattern.score", patternMatchOpt.get.score)
        precursor.addAnnotation("filtered.peaks.count.sw", altPrecursorPeaks.size)
        val (totalRatio, maxRatio) = _computePrecursorSignalRatio(patternMatchOpt, spectrumData)
        precursor.addAnnotation("precursor.signal.total.sw", totalRatio)
        precursor.addAnnotation("precursor.signal.max.sw", maxRatio)
        val (precRank, maxPrecPeakInSW) = _computePrecursorSignalRank(patternMatchOpt, spectrumData)
        precursor.addAnnotation("precursor.rank.sw", precRank)
        if (maxPrecPeakInSW.isDefined)
          precursor.addAnnotation("precursor.intensity.sw", maxPrecPeakInSW.get.getIntensity)
        headerPrecursor = precursor
        precursors = precursors :+ precursor
      }

      if (charge != spectrumHeader.getPrecursorCharge) metric.incr("header_charge_changed")
      if (Math.abs(1e6 * (headerPrecMz - refinedHeaderPrecMz) / headerPrecMz) > mzTolPPM) metric.incr("header_mz_changed")
    }

  if (useSW) {

    // Try to predict alternative precursors from the Selection Window (SW)

    val scanSelectors = if (scanSelector.equals(ScanSelectorModes.ALL)) {
      Seq(ScanSelectorModes.SAME_CYCLE, ScanSelectorModes.MASTER_SCAN, ScanSelectorModes.NEAREST)
    } else {
      Seq(scanSelector)
    }

    var previousSpectrumDataIndexes : Seq[Long] = Seq()

    for (selector <- scanSelectors) {

      val spectrumData = _buildSpectrumDataSource(mzDbReader, spectrumHeader, selector)

      if (spectrumData.nearestSlice.isDefined && (!previousSpectrumDataIndexes.contains(spectrumData.nearestSlice.get.getHeader.getSpectrumId))) {

        previousSpectrumDataIndexes = previousSpectrumDataIndexes :+ spectrumData.nearestSlice.get.getHeader.getSpectrumId
        val altPrecursorPeaks = spectrumData.getCandidatePeaksFromIsolationWindow(swIntensityThreshold)
        var swPrecursors = Seq() ++ precursors

        val maxPrec = swPrecursors.length + swMaxPrecursorsCount
        var rank = 0

        for (
          altPrecursorPeak <- altPrecursorPeaks
          if (swPrecursors.length < maxPrec)
        ) {

          val (altPredictionOpt, altPredictionNote, altPatternMatchOpt) = this._predictPrecursorFromTarget(spectrumData, altPrecursorPeak.getMz, 0)
          if (altPredictionOpt.isDefined) {
            val (refinedAltPrecMz, charge) = altPredictionOpt.get
            if ((charge > 1) && (charge <= Settings.maxIsotopicChargeState)) {

              if (!swPrecursors.exists(p => Math.abs(1e6 * (p.getPrecMz - refinedAltPrecMz) / p.getPrecMz) < mzTolPPM && p.getCharge == charge)) {
                val altPrecursorOpt = _buildMgfPrecursor(spectrumHeader.getElutionTime, refinedAltPrecMz, charge)
                if (altPrecursorOpt.isDefined) {
                  val altPrecursor = altPrecursorOpt.get
                  altPrecursor.addAnnotation("source", "sw")
                  altPrecursor.addAnnotation("ms1.scan.number", spectrumData.nearestSlice.get.getHeader.getSpectrumId)
                  altPrecursor.addAnnotation("in.sw", spectrumData.isInIsolationWindow(refinedAltPrecMz))
                  altPrecursor.addAnnotation("scan.number", spectrumHeader.getSpectrumId)
                  altPrecursor.addAnnotation("rank", rank)
                  altPrecursor.addAnnotation("prediction", altPredictionNote)
                  if (altPatternMatchOpt.isDefined) altPrecursor.addAnnotation("prediction.pattern.score", altPatternMatchOpt.get.score)

                  altPrecursor.addAnnotation("filtered.peaks.count.sw", altPrecursorPeaks.size)
                  val (totalRatio, maxRatio) = _computePrecursorSignalRatio(altPatternMatchOpt, spectrumData)
                  altPrecursor.addAnnotation("precursor.signal.total.sw", totalRatio)
                  altPrecursor.addAnnotation("precursor.signal.max.sw", maxRatio)
                  val (precRank, maxPrecPeakInSW) = _computePrecursorSignalRank(altPatternMatchOpt, spectrumData)
                  altPrecursor.addAnnotation("precursor.rank.sw", precRank)
                  if (maxPrecPeakInSW.isDefined)
                    altPrecursor.addAnnotation("precursor.intensity.sw", maxPrecPeakInSW.get.getIntensity)
                  altPrecursor.addAnnotation("scan.selector", selector.toString)

                  val l = altPrecursor.getAnnotation("rank")

                  swPrecursors = swPrecursors :+ altPrecursor
                }
              } else {
                if ((headerPrecursor != null) && (!headerPrecursor.getAnnotation("source").toString.contains("sw")) &&
                  (Math.abs(1e6 * (headerPrecursor.getPrecMz - refinedAltPrecMz) / headerPrecursor.getPrecMz) < mzTolPPM && headerPrecursor.getCharge == charge)) {
                  headerPrecursor.addAnnotation("source", headerPrecursor.getAnnotation("source") + " & sw")
                  headerPrecursor.addAnnotation("rank", rank)
                }
              }
            }
          }
          rank = rank + 1
        }
        // combine swPrecursors with precursors
        for (swPrecursor <- swPrecursors) {
          if (!precursors.exists(p => Math.abs(1e6 * (p.getPrecMz - swPrecursor.getPrecMz) / p.getPrecMz) < mzTolPPM && p.getCharge == swPrecursor.getCharge)) {
            precursors = precursors :+ swPrecursor
          }
        }
      }
    }
  }

    // filter putative precursors
    precursors = precursors.filter( p => {
      val rank = p.getAnnotation("rank")
      val totalRatio = p.getAnnotation("precursor.signal.total.sw").asInstanceOf[Float]
      ((rank == null) || (rank.asInstanceOf[Int] <= rankThreshold)) && ((totalRatio < 0) || (totalRatio >= pifThreshold))
    })

    precursors.zipWithIndex.foreach{ case (p, idx)  =>  p.addAnnotation("mgf.id", s"${spectrumHeader.getSpectrumId}.${idx+1}")}

    precursors.toArray
  }

  private def _computePrecursorSignalRank(patternMatchOpt: Option[IsotopicPatternMatch], spectrumData: SpectrumDataSource): (Int, Option[Peak]) = {
    if (patternMatchOpt.isDefined) {
      val peaks = spectrumData.getAllPeaksFromIsolationWindow().sortBy(_.getIntensity).reverse
      val patternPeaksInSW = patternMatchOpt.get.matchingPeaks.filter(po => po.isDefined && spectrumData.isInIsolationWindow(po.get.getMz)).map(_.get)
      if (patternPeaksInSW.isEmpty) {
        (-2, None)
      } else {
        val patternMaxPeakInSW = patternPeaksInSW.maxBy(_.getIntensity)
        var rank = 0
        breakable {
          for (p <- peaks) {
            if (p.compareTo(patternMaxPeakInSW) == 0) break
            rank = rank + 1
          }
        }
        (rank, Some(patternMaxPeakInSW))
      }
    } else {
      (-1, None)
    }
  }


  private def _computePrecursorSignalRatio(patternMatchOpt: Option[IsotopicPatternMatch], spectrumData: SpectrumDataSource): (Float, Float) = {
    if (patternMatchOpt.isDefined) {
      val peaks = spectrumData.getAllPeaksFromIsolationWindow()
      val patternPeaksInSW = patternMatchOpt.get.matchingPeaks.filter(po => po.isDefined && spectrumData.isInIsolationWindow(po.get.getMz)).map(_.get)
      val precIntensity = patternPeaksInSW.foldLeft(0.0f)((s, p) => s + p.getIntensity)
      val totalIntensity = peaks.foldLeft(0.0f)((s, p) => s + p.getIntensity)
      val maxPatternRatio = if (!patternPeaksInSW.isEmpty && !peaks.isEmpty) { patternPeaksInSW.map(_.getIntensity).max/peaks.map(_.getIntensity).max } else 0.0f
      (precIntensity/totalIntensity, maxPatternRatio)
    } else {
      (-1.0f, -1.0f)
    }
  }

  private def _buildMgfPrecursor(time: Float, refinedAltPrecMz: Double, charge: Int) : Option[MgfPrecursor] = {
    if (charge != 0) {
      Some(new MgfPrecursor(refinedAltPrecMz, charge, time))
    } else {
      None
    }
  }

  private def _getHeaderPrecursorMz(spectrumHeader: SpectrumHeader): Double = {
//    if (spectrumHeader.getPrecursor != null) {
//      spectrumHeader.getPrecursor.parseFirstSelectedIonMz()
//    } else {
      spectrumHeader.getPrecursorMz()
//    }
  }

  private def _buildSpectrumDataSource(reader: MzDbReader, spectrumHeader: SpectrumHeader, scanSelector : ScanSelectorModes.ScanSelectorMode): SpectrumDataSource = {

    val time = spectrumHeader.getElutionTime
    val precursor = spectrumHeader.getPrecursor()

    val iw = precursor.getIsolationWindow
    if (iw == null) return null

    val cvEntries = Array[CVEntry](CVEntry.ISOLATION_WINDOW_LOWER_OFFSET, CVEntry.ISOLATION_WINDOW_TARGET_MZ, CVEntry.ISOLATION_WINDOW_UPPER_OFFSET)
    val cvParams = iw.getCVParams(cvEntries)

    val lowerMzOffset = cvParams(0).getValue.toFloat //1.1f;
    val targetMz = cvParams(1).getValue.toFloat
    val upperMzOffset = cvParams(2).getValue.toFloat
    val minmz = targetMz - lowerMzOffset
    val maxmz = targetMz + upperMzOffset
    val minrt = time - 5
    val maxrt = time + 5

    //
    // depending on the scanSelectorMode and FAIMS mode, determine the MS1 scan considered as the nearest Spectrum
    //

    var nearestSlice : Option[SpectrumSlice] = None

    if (scanSelector.equals(ScanSelectorModes.MASTER_SCAN)) {
        val masterScanUP = spectrumHeader.getScanList.getScans.get(0).getUserParam("[Thermo Trailer Extra]Master Scan Number:")
        if (masterScanUP != null) {
          val masterScanIndex = masterScanUP.getValue.toInt
          if (masterScanIndex >= 0) {
            val masterScan = reader.getSpectrum(masterScanIndex)
            nearestSlice = Some(new SpectrumSlice(masterScan.getHeader, masterScan.getData.mzRangeFilter(minmz - 5, maxmz + 5)))
          }
        }
      }

    val spectrumSlices = reader.getMsSpectrumSlices(minmz - 5, maxmz + 5, minrt, maxrt)
    val ionMobilityMode = reader.getIonMobilityMode
    val hasIonMobility = (ionMobilityMode != null) && ionMobilityMode.getIonMobilityType.equals(IonMobilityType.FAIMS)
    val cvOpt = { if (hasIonMobility) MgfBoostPrecursorExtractor.readIonMobilityCV(spectrumHeader) else None }

    if (!spectrumSlices.isEmpty) {

      if (scanSelector.equals(ScanSelectorModes.SAME_CYCLE) || (scanSelector.equals(ScanSelectorModes.MASTER_SCAN) && !nearestSlice.isDefined)) {
        if (spectrumSlices.nonEmpty) {
          val sliceOpt = spectrumSlices.find(x => (x.getHeader.getCycle == spectrumHeader.getCycle) && (!hasIonMobility || cvOpt.get == MgfBoostPrecursorExtractor.readIonMobilityCV(x.getHeader).get))
          if (sliceOpt.isDefined) {
            nearestSlice = sliceOpt
          }
        }
      }

      if (scanSelector.equals(ScanSelectorModes.NEAREST) || !nearestSlice.isDefined) {
        val time = spectrumHeader.getElutionTime
        val filteredSlices = spectrumSlices.filter(x => !hasIonMobility || cvOpt.get == MgfBoostPrecursorExtractor.readIonMobilityCV(x.getHeader).get)
        val slice = filteredSlices.minBy { x => Math.abs(x.getHeader.getElutionTime - time) }
        nearestSlice = Some(slice)
      }

    }

    SpectrumDataSource(spectrumHeader,
                       Array(minmz,maxmz),
                       nearestSlice)
  }

  def _predictPrecursorFromTarget(spectrumData: SpectrumDataSource, precMz: Double, targetZ: Int): (Option[(Double, Int)], String, Option[IsotopicPatternMatch]) = {

    val spectrumSlice = spectrumData.getNearestSpectrumSlice()
    if (spectrumSlice.isDefined) {

      val nearestPeak = spectrumSlice.get.getNearestPeak(precMz, mzTolPPM)
      if (nearestPeak == null) {
        metric.incr("no_peak_in_MS_survey")
        if (targetZ > 0) return (Some(precMz, targetZ), "no peak in the MS survey", None) else return (None, "no peak in the MS survey", None)
      }

      val bestPatternMatch = _findIsotopicPatternMatch(spectrumSlice.get, precMz)

        val pattern = bestPatternMatch.theoreticalPattern
//        val p0 = spectrumSlice.get.getNearestPeak(pattern.monoMz, mzTolPPM)
//        val p1 = spectrumSlice.get.getNearestPeak(pattern.mzAbundancePairs(1)._1, mzTolPPM)

        if (!bestPatternMatch.matchingPeaks(0).isDefined) metric.incr("prediction_without_isotope0")
        if (!bestPatternMatch.matchingPeaks(1).isDefined) metric.incr("prediction_without_isotope1")

        if (!bestPatternMatch.matchingPeaks(1).isDefined) {
          if (pattern.charge <= 1)  metric.incr("ignored.prediction_without isotope1.predicted_1+")
          if (targetZ > 0) return (Some(precMz, targetZ), "no isotope 2 in prediction", None) else return (None, "no isotope 2 in prediction", None)
        }

        if (pattern.charge > 1) {
          val predictedMz = { if (bestPatternMatch.matchingPeaks(0).isDefined) bestPatternMatch.matchingPeaks(0).get.getMz else pattern.monoMz }
          (Some(predictedMz, pattern.charge), "pattern prediction", Some(bestPatternMatch))
        } else {
          if (targetZ <= 0) {
            val predictedMz = { if (bestPatternMatch.matchingPeaks(0).isDefined) bestPatternMatch.matchingPeaks(0).get.getMz else pattern.monoMz }
            (Some(predictedMz, pattern.charge), "pattern prediction 1+", Some(bestPatternMatch))
          } else {
            metric.incr("ignored.predicted_1+")
            (Some(precMz, targetZ), "ignored 1+ prediction", None)
          }

        }
    } else {
      metric.incr("no_spectrum_slice_found")
      if (targetZ > 0) (Some(precMz, targetZ), "no spectrum slice found", None) else (None, "no spectrum slice found", None)
    }
  }


  private def _findIsotopicPatternMatch(spectrumSlice: SpectrumSlice, precMz: Double): IsotopicPatternMatch = {

    var putativePatterns = DotProductPatternScorer.calcIsotopicPatternHypotheses(spectrumSlice.getData, precMz, mzTolPPM)
    var bestPattern = DotProductPatternScorer.selectBestPatternHypothese(putativePatterns)
    var targetMz = precMz
    var previousMz = 0.0
    while ( Math.abs(1e6 * (targetMz - previousMz) / targetMz) > mzTolPPM && Math.abs(1e6 * (targetMz - bestPattern._2.monoMz) / targetMz) > mzTolPPM) {
      previousMz = targetMz
      targetMz = bestPattern._2.monoMz
      putativePatterns = DotProductPatternScorer.calcIsotopicPatternHypothesesFromCharge(spectrumSlice.getData, targetMz, bestPattern._2.charge, mzTolPPM)
      bestPattern = DotProductPatternScorer.selectBestPatternHypothese(putativePatterns)
    }

    IsotopicPatternMatch(bestPattern._1, bestPattern._2, spectrumSlice, mzTolPPM)
  }


  override def  getMethodName(): String =  {
    "Proline/MGFBoost refined precursor mz"
  }

  override def  getMethodVersion(): String =  {
    "3.6.3"
  }

  def dumpMetrics(): Unit = {
    logger.info(metric.toString());
  }

  def extractSWStats(mzDbReader: MzDbReader, spectrumHeader: SpectrumHeader, mzTolPPM: Float): Map[String, Any] = {

    var result = Map.empty[String, Any]
    val precursor = spectrumHeader.getPrecursor()
    val time = spectrumHeader.getElutionTime()

    val spectrumSlices = this._getSpectrumSlicesInIsolationWindow(mzDbReader, precursor, time, 5)
    if (spectrumSlices == null || spectrumSlices.isEmpty) {
      result += ("cause" -> "no spectrum slice")
      return result
    }

    result += ("header.moz" -> _getHeaderPrecursorMz(spectrumHeader))
    result += ("header.charge" -> spectrumHeader.getPrecursorCharge)

    val iw = precursor.getIsolationWindow
    val sw_center = iw.getCVParam(CVEntry.ISOLATION_WINDOW_TARGET_MZ).getValue.toFloat

    result += ("sw_center.moz" -> sw_center)
    result += ("sw_center.lower_offset" -> iw.getCVParam(CVEntry.ISOLATION_WINDOW_LOWER_OFFSET).getValue.toFloat)
    result += ("sw_center.upper_offset" -> iw.getCVParam(CVEntry.ISOLATION_WINDOW_UPPER_OFFSET).getValue.toFloat)

    if (!spectrumSlices.isEmpty) {
      val closestSlice = spectrumSlices.filter(_.getHeader.getCycle == spectrumHeader.getCycle)
      val allPeaks = closestSlice.flatMap(_.toPeaks)

      if (!allPeaks.isEmpty) {
        val maxPeak = allPeaks.maxBy(_.getIntensity)
        result += ("sw_content.max_intensity" -> maxPeak.getIntensity)
        for (i <- 0 to 10) {
          result += (s"sw_content_${i}.count" -> allPeaks.filter(_.getIntensity >= i/10.0 * maxPeak.getIntensity).size)
        }

        val precMzArray = allPeaks.sortBy(_.getIntensity).reverse
        val swPrecMz = precMzArray.zipWithIndex.minBy{ case(p, index) => math.abs(p.getMz - sw_center) }

        result += ("sw_center.peak.moz" -> swPrecMz._1.getMz)
        result += ("sw_center.peak.intensity" -> swPrecMz._1.getIntensity)
        result += ("sw_center.peak.rank" -> swPrecMz._2)


      } else {
        result += ("cause" -> "empty_sw")
      }
    }

    result
  }

    def extractPrecursorStats(mzDbReader: MzDbReader, spectrumHeader: SpectrumHeader, precMz: Double, mzTolPPM: Float): Map[String, Any] = {
    var result = Map.empty[String, Any]
    result += ("found" -> "false")

    // similar to _extractPeaksFromIsolationWindow

    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()

    // Do a XIC in the isolation window and around the provided time

    val spectrumData = this._buildSpectrumDataSource(mzDbReader, spectrumHeader, ScanSelectorModes.SAME_CYCLE)
    if (spectrumData.nearestSlice == null)  {
      result += ("cause" -> "no spectrum slice")
      return result
    }

    val headerMoz = _getHeaderPrecursorMz(spectrumHeader)
    result += ("header.moz" -> headerMoz)
    result += ("header.charge" -> spectrumHeader.getPrecursorCharge)
    result += ("header.found" -> "false") // default value that will be updated later

    val headerPrecMz = _getHeaderPrecursorMz(spectrumHeader)
    val refinedHeaderPrecMz = spectrumData.getNearestPeak(headerPrecMz, mzTolPPM).map(_.getMz).getOrElse(headerPrecMz)
    val (predictionOpt, predictionNote, patternMatchOpt) = _predictPrecursorFromTarget(spectrumData, refinedHeaderPrecMz, spectrumHeader.getPrecursorCharge)
    result += ("header.prediction.note" -> predictionNote)
    if (predictionOpt.isDefined) {
      result += ("header.prediction.moz" -> predictionOpt.get._1)
      result += ("header.prediction.charge" -> predictionOpt.get._2)
    }

    val iw = precursor.getIsolationWindow
    val sw_center = iw.getCVParam(CVEntry.ISOLATION_WINDOW_TARGET_MZ).getValue.toFloat
    result += ("sw_center.moz" -> sw_center)


    if (!spectrumData.nearestSlice.isEmpty) {

      val allPeaks = spectrumData.getCandidatePeaksFromIsolationWindow(swIntensityThreshold)

      if (!allPeaks.isEmpty) {


        val maxPeak = allPeaks.maxBy(_.getIntensity)
        val swPrecMzArray = allPeaks.sortBy(_.getIntensity).reverse

        var rank = 0
        breakable {
          for (swPrecMz <- swPrecMzArray) {

            // prediction from the rank 0 peak
            if (rank == 0) {
              result += ("rank0.initial.intensity" -> swPrecMz.getIntensity)
              result += ("rank0.initial.moz" -> swPrecMz.getMz)

              val (altPredictionOpt, altPredictionNote, altPatternMatchOpt) = this._predictPrecursorFromTarget(spectrumData, swPrecMz.getMz, 0)
              result += ("rank0.prediction.note" -> altPredictionNote)
              if (altPredictionOpt.isDefined) {
                val altprecMz = altPredictionOpt.get._1
                result += ("rank0.prediction.moz" -> altprecMz)
                result += ("rank0.prediction.charge" -> altPredictionOpt.get._2)
                val altprecursors = allPeaks.sortBy(_.getIntensity).reverse.zipWithIndex.filter{ case (peak, index) => math.abs(1e6 * (peak.getMz - altprecMz) / altprecMz) <= mzTolPPM }
                if (!altprecursors.isEmpty) {
                  val altprecursor = altprecursors.minBy{case(peak, index) => math.abs(peak.getMz - altprecMz) }
                  result += ("rank0.prediction.intensity" -> altprecursor._1.getIntensity)
                  result += ("rank0.prediction.rank" -> altprecursor._2)
                }
              }
            }


            // prediction from the peak matching the ident.moz value (the precMz argument)
            if (Math.abs(1e6 * (precMz - swPrecMz.getMz) / precMz) < mzTolPPM) {
              result += ("found" -> "true")
              result += ("ident.initial.moz" -> precMz)
              result += ("ident.initial.rank" -> rank)
              result += ("ident.initial.intensity" -> swPrecMz.getIntensity)

              val (altPredictionOpt, altPredictionNote, altPatternMatchOpt) = this._predictPrecursorFromTarget(spectrumData, swPrecMz.getMz, 0)
              result += ("ident.prediction.note" -> altPredictionNote)
              if (altPredictionOpt.isDefined) {
                val altprecMz = altPredictionOpt.get._1
                result += ("ident.prediction.moz" -> altprecMz)
                result += ("ident.prediction.charge" -> altPredictionOpt.get._2)
                val altprecursors = allPeaks.sortBy(_.getIntensity).reverse.zipWithIndex.filter{ case (peak, index) => math.abs(1e6 * (peak.getMz - altprecMz) / altprecMz) <= mzTolPPM }
                if (!altprecursors.isEmpty) {
                  val altprecursor = altprecursors.minBy{case(peak, index) => math.abs(peak.getMz - altprecMz) }
                  result += ("ident.prediction.intensity" -> altprecursor._1.getIntensity)
                  result += ("ident.prediction.rank" -> altprecursor._2)
                }
              }
              break
            }
            rank = rank + 1
          }
        }

        //prediction from the threshold% top peak  closest to the selection window center
        val precMzArray = allPeaks.filter(_.getIntensity > swIntensityThreshold * maxPeak.getIntensity).sortBy(_.getIntensity).reverse
        val swPrecMz = precMzArray.zipWithIndex.minBy{ case(p, index) => math.abs(p.getMz - sw_center) }

        val (altPredictionOpt, altPredictionNote, altPatternMatchOpt) = this._predictPrecursorFromTarget(spectrumData, swPrecMz._1.getMz, 0)
        result += ("swcenter.prediction.note" -> altPredictionNote)
        result += ("swcenter.initial.moz" -> swPrecMz._1.getMz)
        result += ("swcenter.initial.intensity" -> swPrecMz._1.getIntensity)
        result += ("swcenter.initial.rank" -> swPrecMz._2)
        if (altPredictionOpt.isDefined) {
          val altprecMz = altPredictionOpt.get._1
          result += ("swcenter.prediction.moz" -> altprecMz)
          result += ("swcenter.prediction.charge" -> altPredictionOpt.get._2)
          val altprecursors = allPeaks.sortBy(_.getIntensity).reverse.zipWithIndex.filter{ case (peak, index) => math.abs(1e6 * (peak.getMz - altprecMz) / altprecMz) <= mzTolPPM }
          if (!altprecursors.isEmpty) {
            val altprecursor = altprecursors.minBy{case(peak, index) => math.abs(peak.getMz - altprecMz) }
            result += ("swcenter.prediction.intensity" -> altprecursor._1.getIntensity)
            result += ("swcenter.prediction.rank" -> altprecursor._2)
          }
        }

        // + rechercher les found = false pour voir si on les trouve dans le scan MS1 !!
        var spectrumSlice = spectrumData.getNearestSpectrumSlice()
        if (spectrumSlice.isDefined) {

          if(result("found") != "true") {
              val nearestPeak = spectrumSlice.get.getNearestPeak(precMz, mzTolPPM)
              if (nearestPeak != null) {
                result += ("found" -> "outside")
              }
          }

          spectrumSlice = spectrumData.getNearestSpectrumSlice()
          val nearestPeak = spectrumSlice.get.getNearestPeak(headerMoz, mzTolPPM)
          if (nearestPeak != null) {
            result += ("header.found" -> "true")
          }

        }



      } else {
        result += ("cause" -> "no peaks in MS1")
      }
    } else {
      result += ("cause" -> "no spectrum slice")
    }
    result
  }

  /**
   *
   * Extract distance between isotopic peaks of the specified precursor mass & charge. The rank of the peak matching the precursor
   * mass is also extracted.
   *
   * @param mzDbReader
   * @param spectrumHeader
   * @param precMz
   * @param precZ
   * @param mzTolPPM
   * @return
   */
  def extractPrecursorIsotopesStats(mzDbReader: MzDbReader, spectrumHeader: SpectrumHeader, precMz: Double, precZ: Int, mzTolPPM: Float) : Map[String, Any] = {

    var result = Map.empty[String, Any]
    result += ("found" -> "false")

    // similar to _extractPeaksFromIsolationWindow

    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()

    // Do a XIC in the isolation window and around the provided time
    val spectrumData = this._buildSpectrumDataSource(mzDbReader, spectrumHeader, ScanSelectorModes.SAME_CYCLE)
    if (spectrumData.nearestSlice == null)  {
      result += ("cause" -> "no spectrum slice")
      return result
    }

    if (!spectrumData.nearestSlice.isEmpty) {
      val allPeaks = spectrumData.getCandidatePeaksFromIsolationWindow(swIntensityThreshold)

      if (!allPeaks.isEmpty) {

        val maxPeak = allPeaks.maxBy(_.getIntensity)
        val swPrecMzArray = allPeaks.sortBy(_.getIntensity).reverse
        val ms1Slice = spectrumData.getNearestSpectrumSlice()

        var rank = 0
        breakable {
          for (swPrecMz <- swPrecMzArray) {

            // prediction from the peak matching the ident.moz value (the precMz argument)
            if (Math.abs(1e6 * (precMz - swPrecMz.getMz) / precMz) < mzTolPPM) {
              result += ("found" -> "true")
              result += ("ident.initial.moz" -> precMz)
              result += ("ident.initial.rank" -> rank)
              result += ("ident.initial.intensity" -> swPrecMz.getIntensity)
              var previousMz = precMz
//              val avgIsotopicShift = Array(0.0, 1.002961, 1.00235, 1.0022, 1.0022)
              for (k <- 1 to 4) {
//              val isotopeMz = previousMz + avgIsotopicShift(k)/precZ
                val isotopeMz = previousMz + IsotopePatternEstimator.avgIsoMassDiff/precZ
                val isotopicPeak = ms1Slice.get.getNearestPeak(isotopeMz, mzTolPPM)
                if(isotopicPeak != null) {
                  val shift = precZ*(isotopicPeak.getMz - previousMz)
                  result += ("ident.isotope.shift."+k -> shift)
                  result += ("ident.isotope.shift.ppm."+k -> 1e6*math.abs((isotopicPeak.getMz - isotopeMz)/isotopicPeak.getMz))
                  result += ("ident.isotope.shift.abs."+k -> (isotopicPeak.getMz - precMz))
                  previousMz = isotopicPeak.getMz
//                  if (math.abs(1e6 * (isotopicPeak.getMz - isotopeMz) / isotopicPeak.getMz) > mzTolPPM) {
//                    logger.info(" !!!! Error closest peak is not inside tolerance bounds")
//                  }
//                  if (shift >= 1.02) {
//                    logger.info("this one");
//                  }
                } else {
                  previousMz = isotopeMz
                }
              }
              break
            }
            rank = rank + 1
          }
        }

      } else {
        result += ("cause" -> "no peaks in MS1")
      }
    } else {
      result += ("cause" -> "no spectrum slice")
    }
    result

  }


}