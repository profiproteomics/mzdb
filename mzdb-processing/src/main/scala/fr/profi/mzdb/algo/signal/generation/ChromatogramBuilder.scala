package fr.profi.mzdb.algo.signal.generation

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LongMap

import fr.profi.mzdb.algo.signal.distortion.IErrorGenerator
import fr.profi.mzdb.algo.signal.distortion.IntensityVsTimeNoiseAdder
import fr.profi.mzdb.model.ILcContext
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.PeakelBuilder
import fr.profi.mzdb.model.FullLcContext
import fr.profi.mzdb.model.PeakelCursor
import fr.profi.mzdb.model.Peak

class ChromatogramBuilder() {
  
  val peakelBuffer = new ArrayBuffer[Peakel]()
  
  def addPeakel( peakel: Peakel ): Unit = {
    peakelBuffer += peakel
  }
  
  def addPeakels( peakels: Seq[Peakel] ): Unit = {
    peakelBuffer ++= peakels
  }
  
  def result(): Array[(Float,Double)] = {
    
    val lcContextBySpectrumId = new LongMap[ILcContext]
    for( peakel <- peakelBuffer ) {
      val peakelCursor = peakel.getNewCursor()
      while( peakelCursor.next() ) {
        val spectrumId = peakelCursor.getSpectrumId()
        val time = peakelCursor.getElutionTime()
        lcContextBySpectrumId += spectrumId -> FullLcContext(spectrumId,time)
      }
    }
    
    val allPeaks = for( peakel <- peakelBuffer; peak <- peakel.toPeaks(lcContextBySpectrumId) ) yield peak
    val peaksBySpectrumId = allPeaks.groupBy(_.getLcContext().getSpectrumId() )
    
    val mergedPeaks = for( (spectrumId, peaks) <- peaksBySpectrumId ) yield {
      val peaksCount = peaks.length
      if( peaksCount == 1 ) peaks.head
      else {        
        val peakelBuilder = new PeakelBuilder(peaks.toArray)
        val mzValues = peakelBuilder.getMzValues()
        val intensityValues = peakelBuilder.getIntensityValues()
        val leftHwhms = peakelBuilder.getLeftHwhms()
        val rightHwhms = peakelBuilder.getRightHwhms()
        
        new Peak(
          mzValues.sum / peaksCount,
          intensityValues.sum,
          leftHwhms.sum / peaksCount,
          rightHwhms.sum / peaksCount,
          peaks.head.getLcContext()
        )

      }
    }
    
    mergedPeaks.toArray.map { peak =>
      peak.getLcContext().getElutionTime() -> peak.getIntensity().toDouble
    } sortBy(_._1)
  }

}