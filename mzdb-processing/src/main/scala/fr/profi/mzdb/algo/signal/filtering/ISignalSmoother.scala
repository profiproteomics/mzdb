package fr.profi.mzdb.algo.signal.filtering

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.algo.signal.generation.ChromatogramBuilder
import fr.profi.mzdb.model.PeakelBuilder

trait ISmoothingConfig

trait ISignalSmoother {
  
  def smoothTimeIntensityPairs(rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)]
  
  def smoothPeakel(peakel: Peakel): Peakel = {
    
    val chromBuilder = new ChromatogramBuilder()
    chromBuilder.addPeakel(peakel)
    val rtIntPairs = chromBuilder.result()
    val smoothedRtIntPairs = smoothTimeIntensityPairs(rtIntPairs)
    require(
      smoothedRtIntPairs.length == peakel.scanInitialIds.length,
      "can't use the smoothPeakel method with a smoother algo returning different number of values"
    )
    
    val tmpPeakel = new PeakelBuilder(
      scanInitialIds = new ArrayBuffer() ++ peakel.scanInitialIds,
      elutionTimes = new ArrayBuffer() ++ peakel.elutionTimes,
      mzValues = new ArrayBuffer() ++ peakel.mzValues,
      intensityValues = new ArrayBuffer() ++ smoothedRtIntPairs.map(_._2.toFloat)
    ).result(peakel.id)
    
    tmpPeakel.copy(
      leftHwhmMean = peakel.leftHwhmMean,
      leftHwhmCv = peakel.leftHwhmCv,
       rightHwhmMean= peakel.rightHwhmMean,
      rightHwhmCv= peakel.rightHwhmCv
    )
  }
  
}