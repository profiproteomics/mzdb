package fr.profi.mzdb.io.writer.mgf

import Preprocessing.JSpectrum.IsobaricTag
import Preprocessing.{Config, JPeak, JSpectrum}
import fr.profi.mzdb.model.SpectrumData
import fr.profi.util.ms.mozToMass

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.ArrayBuffer

object PCleanProcessor {

  def apply(methodName: String): PCleanProcessor = {
    val p = new PCleanProcessor
    p.methodName = if (methodName.isEmpty) { None } else { Some(methodName) }
    p
  }

}

class PCleanProcessor extends ISpectrumProcessor {

  var methodName: Option[String] = None

  JSpectrum.setImmoniumIons()

  override def processSpectrum(mgfPrecursor: MgfPrecursor, spectrumData: SpectrumData): SpectrumData = {

    val jSpectrum = new JSpectrum

    Config.ms2tol = 0.05
    jSpectrum.setParentMass(mozToMass(mgfPrecursor.getPrecMz, mgfPrecursor.getCharge))
    jSpectrum.setParentMassToCharge(mgfPrecursor.getPrecMz)
    jSpectrum.setCharge(mgfPrecursor.getCharge)
    //    jSpectrum.setSpectrumTitle(spectrum.getSpectrumTitle)
    jSpectrum.setIntensity(0.0)
    //    jSpectrum.setRt(spectrum.getPrecursor.getRt)

    val mzList = spectrumData.getMzList
    val intensList = spectrumData.getIntensityList

    for (i <- 0 until spectrumData.getPeaksCount) {
      val jPeak = new JPeak(mzList(i), intensList(i))
      jSpectrum.addRawPeak(jPeak)
    }
    jSpectrum.resetPeaks()
    jSpectrum.removeImmoniumIons

    /* pClean module1 */
    if (methodName.isDefined) {
      jSpectrum.sortPeaksByMZ()
      jSpectrum.module1(methodName.get, true, true, false, false)
    }
    /* pClean module2 */
    jSpectrum.sortPeaksByMZ()
    jSpectrum.module2(true, true, false, true)

    val newMzList = new ArrayBuffer[Double](jSpectrum.getPeaks.size())
    val newIntensityList = new ArrayBuffer[Float](jSpectrum.getPeaks.size())

    jSpectrum.getPeaks.foreach { p =>
      newMzList += p.getMz
      newIntensityList += p.getIntensity.toFloat
    }

    val newSpectrumData = new SpectrumData(newMzList.toArray, newIntensityList.toArray)

    newSpectrumData
  }
}
