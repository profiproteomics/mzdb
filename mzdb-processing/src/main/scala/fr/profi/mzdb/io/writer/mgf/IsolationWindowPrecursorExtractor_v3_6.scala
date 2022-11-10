package fr.profi.mzdb.io.writer.mgf

import com.almworks.sqlite4java.SQLiteException
import fr.profi.ms.algo.IsotopePatternEstimator
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.algo.DotProductPatternScorer
import fr.profi.mzdb.db.model.params.param.CVEntry
import fr.profi.mzdb.model.{Peak, SpectrumHeader, SpectrumSlice}
import fr.profi.util.metrics.Metric

import com.almworks.sqlite4java.SQLiteException
import fr.profi.ms.algo.IsotopePatternEstimator
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.algo.DotProductPatternScorer
import fr.profi.mzdb.db.model.params.param.CVEntry
import fr.profi.mzdb.model.{Peak, SpectrumHeader, SpectrumSlice}
import fr.profi.util.metrics.Metric

import scala.util.control.Breaks.{break, breakable}

/**
 * @author CB205360
 */

class IsolationWindowPrecursorExtractor_v3_6(mzTolPPM: Float) extends DefaultPrecursorComputer(mzTolPPM) {


  private var lastPrediction: (SpectrumHeader, (Double, Int)) = _
  private var metric = new Metric("MGF")

  @throws[SQLiteException]
  override def getMgfPrecursors(mzDbReader: MzDbReader, spectrumHeader: SpectrumHeader): Array[MgfPrecursor] = {
    val time = spectrumHeader.getElutionTime
    var precursors = Seq[MgfPrecursor]()

    // try to predict the precursor mz from the mz value found in the MS2 header
    lastPrediction = null

    val headerPrecMz = _getHeaderPrecursorMz(spectrumHeader)
    val (predictionOpt, predictionNote) = _getPrecursorMz(mzDbReader, spectrumHeader, headerPrecMz, spectrumHeader.getPrecursorCharge)
    val (refinedHeaderPrecMz, charge) =  predictionOpt.getOrElse((headerPrecMz, spectrumHeader.getPrecursorCharge.intValue())) // (headerPrecMz, spectrumHeader.getPrecursorCharge)

    val precursor = buildMgfPrecursor(time, refinedHeaderPrecMz, charge)
    precursor.addAnnotation("source", "header")
    precursor.addAnnotation("scan.number", spectrumHeader.getSpectrumId)
    precursor.addAnnotation("prediction", predictionNote)

    precursors = precursors :+ precursor

    if (charge != spectrumHeader.getPrecursorCharge) metric.incr("header charge changed")
    if (Math.abs(1e6 * (headerPrecMz - refinedHeaderPrecMz) / headerPrecMz) > mzTolPPM) metric.incr("header mz changed")

    // extract an alternative mz from the isolation window and make a prediction if this value is
    // different from the MS2 header value and from the already predicted precursor
    val altPrecMzArray = _extractPrecMzFromIsolationWindow(mzDbReader, spectrumHeader, 5)
    val maxPrec = precursors.length + 1
    var rank = 0
    for (
      altPrecMz <- altPrecMzArray
      if (precursors.length < maxPrec)
    ) {

      val maxPeak = altPrecMzArray.maxBy(_.getIntensity)
      val (altPredictionOpt, altPredictionNote) = this._getPrecursorMz(mzDbReader, spectrumHeader, altPrecMz.getMz, 0)
      if (altPredictionOpt.isDefined) {
        val (refinedAltPrecMz, charge) = altPredictionOpt.get
        if ( (charge > 1) && (charge < 6) &&
             !precursors.find( p => Math.abs(1e6 * (p.getPrecMz - refinedAltPrecMz) / p.getPrecMz) < mzTolPPM && p.getCharge == charge).isDefined ) {
            val altPrecursor = buildMgfPrecursor(time, refinedAltPrecMz, charge)
            altPrecursor.addAnnotation("source", "sw")
            altPrecursor.addAnnotation("scan.number", spectrumHeader.getSpectrumId)
            altPrecursor.addAnnotation("initialPeak", altPrecMz)
            altPrecursor.addAnnotation("maxPeak", maxPeak)
            altPrecursor.addAnnotation("rank", rank)
            altPrecursor.addAnnotation("prediction", altPredictionNote)

            precursors = precursors :+ altPrecursor
        }
      }
      rank = rank + 1
    }

//    val headerPrecMz = _getHeaderPrecursorMz(spectrumHeader)
//    val predictionOpt = _getPrecursorMz(mzDbReader, spectrumHeader, headerPrecMz, spectrumHeader.getPrecursorCharge)
//    val (refinedHeaderPrecMz, charge, predictionNote) =  (headerPrecMz, spectrumHeader.getPrecursorCharge, "ms2 head") // predictionOpt.getOrElse((headerPrecMz, spectrumHeader.getPrecursorCharge, "no prediction")) // (headerPrecMz, spectrumHeader.getPrecursorCharge)
//
//    if (!precursors.find( p => Math.abs(1e6 * (p.getPrecMz - refinedHeaderPrecMz) / p.getPrecMz) < mzTolPPM && p.getCharge == charge).isDefined ) {
//
//      val precursor = buildMgfPrecursor(time, refinedHeaderPrecMz, charge)
//      precursor.addAnnotation("source", "header")
//      precursor.addAnnotation("scan.number", spectrumHeader.getSpectrumId)
//      precursor.addAnnotation("prediction", predictionNote)
//      precursors = precursors :+ precursor
//      metric.incr("predicted from header")
//    }


    precursors.toArray
  }

  private def buildMgfPrecursor(time: Float, refinedAltPrecMz: Double, charge: Int) : AnnotatedMgfPrecursor = {
    if (charge != 0) {
      new AnnotatedMgfPrecursor(refinedAltPrecMz, charge, time)
    } else {
      new AnnotatedMgfPrecursor(refinedAltPrecMz, time)
    }
  }

  private def _getHeaderPrecursorMz(spectrumHeader: SpectrumHeader): Double = {
    spectrumHeader.getPrecursor.parseFirstSelectedIonMz()
  }



  def _getPrecursorMz(reader: MzDbReader, spectrumHeader: SpectrumHeader, targetPrecMz: Double, targetZ: Int): (Option[(Double, Int)], String) = {

    var precMz = targetPrecMz
    val time = spectrumHeader.getElutionTime()

    val refinedPrecMz = _refinePrecMz(reader, spectrumHeader, precMz, mzTolPPM);

    if (refinedPrecMz != null) {
      precMz = refinedPrecMz
    } else {
      metric.incr("no peak around precursorMz")
      if (targetZ > 0) return (Some(precMz, targetZ), "no peak around precursorMz") else return (None, "no peak around precursorMz")
    }

    val spectrumSlice = getMS1SpectrumSlice(reader, spectrumHeader, precMz, time)

    if (spectrumSlice.isDefined) {

      val nearestPeak = spectrumSlice.get.getNearestPeak(precMz, mzTolPPM)
      if (nearestPeak == null) {
        metric.incr("no peak in the MS survey")
        if (targetZ > 0) return (Some(precMz, targetZ), "no peak in the MS survey") else return (None, "no peak in the MS survey")
      }

      val bestPattern = getBestIsotopicPatternMatch(spectrumSlice.get, precMz)

      if (bestPattern.isDefined) {

        val pattern = bestPattern.get._2
        val p0 = spectrumSlice.get.getNearestPeak(pattern.monoMz, mzTolPPM)
        val p1 = spectrumSlice.get.getNearestPeak(pattern.mzAbundancePairs(1)._1, mzTolPPM)

        if (p0 == null) metric.incr("Prediction without p0")
        if (p1 == null) metric.incr("Prediction without p1")

        if (p1 == null) {
          if (pattern.charge <= 1)  metric.incr("ignored prediction without p1.1+ prediction")
          if (targetZ > 0) return (Some(precMz, targetZ), "no isotope 2 in prediction") else return (None, "no isotope 2 in prediction")
        }

        if ((pattern.charge > 1)) {

          (Some(pattern.monoMz, pattern.charge), "pattern prediction")

        } else {

          if (targetZ <= 0) {
            (Some(pattern.monoMz, pattern.charge), "pattern prediction 1+")
          } else {
            metric.incr("ignored 1+ prediction")
            (Some(precMz, targetZ), "ignored 1+ prediction")
          }

        }
      } else {
        if (targetZ > 0) (Some(precMz, targetZ), "no best pattern prediction") else (None, "no best pattern prediction")
      }
    } else {
      metric.incr("no_spectrum_slice_found")
      if (targetZ > 0) (Some(precMz, targetZ), "no spectrum slice found") else (None, "no spectrum slice found")
    }
  }

  override def getPrecursorMz(reader: MzDbReader, spectrumHeader: SpectrumHeader): Double = {
    val precMz = _getHeaderPrecursorMz(spectrumHeader)
    val altPrecMzArray = _extractPrecMzFromIsolationWindow(reader, spectrumHeader, 5);

    lastPrediction = null
    if (!altPrecMzArray.isEmpty) {
      val (prediction, note) = _getPrecursorMz(reader, spectrumHeader, altPrecMzArray(0).getMz, 0)
      if (prediction.isDefined) {
        lastPrediction = (spectrumHeader, prediction.get)
        return prediction.get._1
      }
    }

    val (prediction, note) = _getPrecursorMz(reader, spectrumHeader, precMz, spectrumHeader.getPrecursorCharge)
    if (prediction.isDefined) {
      lastPrediction = (spectrumHeader, prediction.get)
      prediction.get._1
    } else {
      precMz
    }
  }

  private def getMS1SpectrumSlice(reader: MzDbReader, spectrumHeader: SpectrumHeader, precMz: Double, time: Float): Option[SpectrumSlice] = {
    val slices = reader.getMsSpectrumSlices(precMz - 5, precMz + 5, time-5f, time+5f)
    if (!slices.isEmpty) {
      val sliceOpt = slices.find(x => x.getHeader.getCycle == spectrumHeader.getCycle)
      if (!sliceOpt.isDefined) {
        Some(slices.minBy { x => Math.abs(x.getHeader.getElutionTime - time) })
      } else {
        sliceOpt
      }
    } else None
  }

  private def getBestIsotopicPatternMatch(spectrumSlice: SpectrumSlice, precMz: Double): Option[(Double, TheoreticalIsotopePattern)] = {

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

//    val pattern = bestPattern._2
//    val p0 = spectrumSlice.getNearestPeak(pattern.monoMz, mzTolPPM)
//    val p1 = spectrumSlice.getNearestPeak(pattern.mzAbundancePairs(1)._1, mzTolPPM)


    Some(bestPattern)
  }

  override def getPrecursorCharge(reader: MzDbReader, spectrumHeader: SpectrumHeader): Int = {
    val charge = spectrumHeader.getPrecursorCharge
      if ((lastPrediction != null) && (spectrumHeader == lastPrediction._1)) {
        lastPrediction._2._2
      } else {
        charge
      }
  }
  
  override def  getParamName(): String =  {
    "Proline refined precursor mz"
  }

  def dumpMetrics(): Unit = {
    logger.info(metric.toString());
  }


  def _refinePrecMz(reader: MzDbReader, spectrumHeader: SpectrumHeader, targetPrecMz: Double, mzTolPPM: Float) : Double = {
    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()

    val spectrumSlices = this._getSpectrumSlicesInIsolationWindow(reader, precursor, time, 5)
    if (spectrumSlices == null) return targetPrecMz

    val closestSlice = spectrumSlices.filter(_.getHeader.getCycle == spectrumHeader.getCycle)

    if (!closestSlice.isEmpty) {
      val p = closestSlice(0).getNearestPeak(targetPrecMz, mzTolPPM)
      if (p == null) targetPrecMz else p.getMz
    } else {
      targetPrecMz
    }
  }


  /**
   * Refines the provided target m/z value by looking at the nearest value in the survey.
   *
   * @param precMz
   * the precursor m/z value to refine
   * @return the refined precursor m/z value
   * @throws SQLiteException
   * @throws StreamCorruptedException
   */
  private def _extractPrecMzFromIsolationWindow(mzDbReader: MzDbReader, spectrumHeader: SpectrumHeader, timeTol: Float): Array[Peak] = {

    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()
    var altMz = Array.empty[Peak]

    // Do a XIC in the isolation window and around the provided time
    val spectrumSlices = this._getSpectrumSlicesInIsolationWindow(mzDbReader, precursor, time, timeTol)
    if (spectrumSlices == null) return Array.empty[Peak]

    val closestSlice = spectrumSlices.filter(_.getHeader.getCycle == spectrumHeader.getCycle)

    val allPeaks = closestSlice.flatMap(_.toPeaks)

    if (!allPeaks.isEmpty) {

      val maxPeak = allPeaks.maxBy(_.getIntensity)
      altMz = altMz ++ allPeaks.filter(_.getIntensity > 0.2*maxPeak.getIntensity).sortBy(_.getIntensity).reverse
    }

    altMz
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

        val precMzArray = allPeaks.sortBy(_.getIntensity).reverse //allPeaks.filter(_.getIntensity > 0.2 * maxPeak.getIntensity).sortBy(_.getIntensity).reverse
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

    // similar to _extractPrecMzFromIsolationWindow

    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()

    // Do a XIC in the isolation window and around the provided time
    val spectrumSlices = this._getSpectrumSlicesInIsolationWindow(mzDbReader, precursor, time, 5)
    if (spectrumSlices == null)  {
      result += ("cause" -> "no spectrum slice")
      return result
    }

    val headerMoz = _getHeaderPrecursorMz(spectrumHeader)
    result += ("header.moz" -> headerMoz)
    result += ("header.charge" -> spectrumHeader.getPrecursorCharge)
    result += ("header.found" -> "false") // default value that will be updated later

    val headerPrecMz = _getHeaderPrecursorMz(spectrumHeader)
    val (predictionOpt, predictionNote) = _getPrecursorMz(mzDbReader, spectrumHeader, headerPrecMz, spectrumHeader.getPrecursorCharge)
    result += ("header.prediction.note" -> predictionNote)
    if (predictionOpt.isDefined) {
      result += ("header.prediction.moz" -> predictionOpt.get._1)
      result += ("header.prediction.charge" -> predictionOpt.get._2)
    }

    val iw = precursor.getIsolationWindow
    val sw_center = iw.getCVParam(CVEntry.ISOLATION_WINDOW_TARGET_MZ).getValue.toFloat
    result += ("sw_center.moz" -> sw_center)


    if (!spectrumSlices.isEmpty) {
      val closestSlice = spectrumSlices.filter(_.getHeader.getCycle == spectrumHeader.getCycle)
      val allPeaks = closestSlice.flatMap(_.toPeaks)

      if (!allPeaks.isEmpty) {


        val maxPeak = allPeaks.maxBy(_.getIntensity)
        val swPrecMzArray = allPeaks.sortBy(_.getIntensity).reverse // allPeaks.filter(_.getIntensity > 0.2 * maxPeak.getIntensity).sortBy(_.getIntensity).reverse

        var rank = 0
        breakable {
          for (swPrecMz <- swPrecMzArray) {

            // prediction from the rank 0 peak
            if (rank == 0) {
              result += ("rank0.initial.intensity" -> swPrecMz.getIntensity)
              result += ("rank0.initial.moz" -> swPrecMz.getMz)

              val (altPredictionOpt, altPredictionNote) = this._getPrecursorMz(mzDbReader, spectrumHeader, swPrecMz.getMz, 0)
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

              val (altPredictionOpt, altPredictionNote) = this._getPrecursorMz(mzDbReader, spectrumHeader, swPrecMz.getMz, 0)
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

        //prediction from the 20% top peak  closest to the selection window center
        val precMzArray = allPeaks.filter(_.getIntensity > 0.2 * maxPeak.getIntensity).sortBy(_.getIntensity).reverse
        val swPrecMz = precMzArray.zipWithIndex.minBy{ case(p, index) => math.abs(p.getMz - sw_center) }

        val (altPredictionOpt, altPredictionNote) = this._getPrecursorMz(mzDbReader, spectrumHeader, swPrecMz._1.getMz, 0)
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
        var spectrumSlice = getMS1SpectrumSlice(mzDbReader, spectrumHeader, precMz, time)
        if (spectrumSlice.isDefined) {

          if(result("found") != "true") {
              val nearestPeak = spectrumSlice.get.getNearestPeak(precMz, mzTolPPM)
              if (nearestPeak != null) {
                result += ("found" -> "outside")
              }
          }

          spectrumSlice = getMS1SpectrumSlice(mzDbReader, spectrumHeader, headerMoz, time)
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

    // similar to _extractPrecMzFromIsolationWindow

    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()

    // Do a XIC in the isolation window and around the provided time
    val spectrumSlices = this._getSpectrumSlicesInIsolationWindow(mzDbReader, precursor, time, 5)
    if (spectrumSlices == null)  {
      result += ("cause" -> "no spectrum slice")
      return result
    }

    if (!spectrumSlices.isEmpty) {
      val closestSlice = spectrumSlices.filter(_.getHeader.getCycle == spectrumHeader.getCycle)
      val allPeaks = closestSlice.flatMap(_.toPeaks)

      if (!allPeaks.isEmpty) {

        val maxPeak = allPeaks.maxBy(_.getIntensity)
        val swPrecMzArray = allPeaks.sortBy(_.getIntensity).reverse // allPeaks.filter(_.getIntensity > 0.2 * maxPeak.getIntensity).sortBy(_.getIntensity).reverse
        val ms1Slice = getMS1SpectrumSlice(mzDbReader, spectrumHeader, precMz, time)

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