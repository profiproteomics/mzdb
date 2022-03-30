package fr.profi.mzdb.io.writer.mgf

import Preprocessing.{Config, JPeak, JSpectrum}
import fr.profi.mzdb.model.SpectrumData
import fr.profi.util.ms.mozToMass

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable.ArrayBuffer

class PCleanProcessor extends ISpectrumProcessor {


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

    for (i <- 0 until spectrumData.getPeaksCount)  {
      val jPeak = new JPeak(mzList(i), intensList(i))
      jSpectrum.addRawPeak(jPeak)
    }
    jSpectrum.resetPeaks()
    jSpectrum.removeImmoniumIons

    /*moudle2 treatment*/
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
