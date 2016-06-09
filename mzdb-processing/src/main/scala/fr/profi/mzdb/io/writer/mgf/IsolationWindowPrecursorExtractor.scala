package fr.profi.mzdb.io.writer.mgf

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.SpectrumHeader
import fr.profi.mzdb.algo.IsotopicPatternScorer
import fr.profi.ms.model.TheoreticalIsotopePattern
import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.db.model.params.Precursor


/**
 * @author CB205360
 */

class IsolationWindowPrecursorExtractor(mzTolPPM: Float)  extends DefaultPrecursorComputer(mzTolPPM) { 
  
  private var lastPrediction: (SpectrumHeader, TheoreticalIsotopePattern) = _
  
  override def getPrecursorMz(reader: MzDbReader, spectrumHeader: SpectrumHeader): Double = {
    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()
    var precMz = precursor.parseFirstSelectedIonMz()
    val altPrecMz = this.refinePrecMz(reader, precursor, precMz, mzTolPPM, time, 5);
    if (altPrecMz != null) { precMz = altPrecMz }
    val bestPattern = getBestIsotopicPatternMatch(reader, spectrumHeader, precMz, time)
    if (bestPattern.isDefined) {
      lastPrediction = (spectrumHeader, bestPattern.get)
      if (math.abs(precMz - bestPattern.get.monoMz) > 1e-3)
        logger.info(s"scan ${spectrumHeader.getInitialId} : change predicted precursorMz from $precMz to ${bestPattern.get.monoMz}")
      bestPattern.get.monoMz
    } else {
      logger.info("no prediction : returns precursorMz")
      precMz
    }
  }

  private def getBestIsotopicPatternMatch(reader: MzDbReader, spectrumHeader: SpectrumHeader, precMz: Double, time: Float): Option[TheoreticalIsotopePattern] = {
    val slices = reader.getMsSpectrumSlices(precMz - 5, precMz + 5, time-5f, time+5f)
    if (!slices.isEmpty) {
      val slice = slices.minBy { x => Math.abs(x.getHeader.getElutionTime-time) }
      val putativePatterns = IsotopicPatternScorer.calcIsotopicPatternHypotheses(slice.getData(), precMz, mzTolPPM)
      Some(putativePatterns.head._2)
    } else None
  }

  override def getPrecursorCharge(reader: MzDbReader, spectrumHeader: SpectrumHeader): Int = {
    val charge = spectrumHeader.getPrecursorCharge
    if (charge >  0) {
      charge
    } else {
    if ( (lastPrediction != null) && (spectrumHeader == lastPrediction._1)) {
       if (charge !=  lastPrediction._2.charge)
        logger.info(s"scan ${spectrumHeader.getInitialId} : change predicted charge from $charge to ${lastPrediction._2.charge}")
      lastPrediction._2.charge
    } else {
      val time = spectrumHeader.getElutionTime()
      var precMz = spectrumHeader.getPrecursorMz()
      val bestPattern = getBestIsotopicPatternMatch(reader, spectrumHeader, precMz, time)
      if (bestPattern.isDefined) {
       if (charge !=  bestPattern.get.charge)
        logger.info(s"change predicted charge from $charge to ${bestPattern.get.charge}")
        bestPattern.get.charge
      } else {
        charge
      }
    }
    }
  }
  
  override def  getParamName(): String =  {
    "Proline refined precursor mz"
  }
}