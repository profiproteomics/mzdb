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

class ProlinePrecursorMzComputer(reader: MzDbReader, mzTolPPM: Float)  extends DefaultPrecursorComputer(reader, mzTolPPM) { 
  
  private var lastPrediction: (SpectrumHeader, TheoreticalIsotopePattern) = _
  
  override def getPrecursorMz(spectrumHeader: SpectrumHeader): Double = {
    val time = spectrumHeader.getElutionTime()
    val precursor = spectrumHeader.getPrecursor()
    var precMz = precursor.parseFirstSelectedIonMz()
    precMz = this.refinePrecMz(reader, precursor, precMz, mzTolPPM, time, 5);
    
    val bestPattern = getBestIsotopicPatternMatch(spectrumHeader, precMz, time)
    if (bestPattern.isDefined) {
      lastPrediction = (spectrumHeader, bestPattern.get)
      if (math.abs(precMz - bestPattern.get.monoMz) > 1e-3)
        logger.info(s"change predicted precursorMz from $precMz to ${bestPattern.get.monoMz}")
      bestPattern.get.monoMz
    } else {
      logger.info("no prediction : returns precursorMz")
      precMz
    }
  }

  private def getBestIsotopicPatternMatch(spectrumHeader: SpectrumHeader, precMz: Double, time: Float): Option[TheoreticalIsotopePattern] = {
    val slices = reader.getMsSpectrumSlices(precMz - 5, precMz + 5, time-5f, time+5f)
    if (!slices.isEmpty) {
      val slice = slices.minBy { x => Math.abs(x.getHeader.getElutionTime-time) }
      val putativePatterns = IsotopicPatternScorer.calclIsotopicPatternHypotheses(slice.getData(), precMz, mzTolPPM)
      Some(putativePatterns.head._2)
    } else None
  }

  override def getPrecursorCharge(spectrumHeader: SpectrumHeader): Int = {
    val charge = spectrumHeader.getPrecursorCharge
    if ((charge <= 0) && (lastPrediction != null) && (spectrumHeader == lastPrediction._1)) {
       if (charge !=  lastPrediction._2.charge)
        logger.info(s"change predicted charge from $charge to ${lastPrediction._2.charge}")
      lastPrediction._2.charge
    } else {
      val time = spectrumHeader.getElutionTime()
      var precMz = spectrumHeader.getPrecursorMz()
      val bestPattern = getBestIsotopicPatternMatch(spectrumHeader, precMz, time)
      if (bestPattern.isDefined) {
       if (charge !=  bestPattern.get.charge)
        logger.info(s"Change predicted charge from $charge to ${bestPattern.get.charge}")
        bestPattern.get.charge
      } else {
        charge
      }
    }
  }
  
  override def  getParamName(): String =  {
    "Proline refined precursor mz"
  }
}