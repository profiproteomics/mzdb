package fr.profi.mzdb.algo.signal.generation

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
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
    
    val lcContextByScanIdBuilder = Map.newBuilder[Long,ILcContext]
    for( peakel <- peakelBuffer ) {
      val peakelCursor = peakel.getNewCursor()
      while( peakelCursor.next() ) {
        val scanId = peakelCursor.getScanId()
        val time = peakelCursor.getElutionTime()
        lcContextByScanIdBuilder += scanId -> FullLcContext(scanId,time)
      }
    }
    
    val lcContextByScanId = lcContextByScanIdBuilder.result()
    val allPeaks = for( peakel <- peakelBuffer; peak <- peakel.toPeaks(lcContextByScanId) ) yield peak
    val peaksByScanId = allPeaks.groupBy(_.getLcContext().getScanId() )
    
    val mergedPeaks = for( (scanId, peaks) <- peaksByScanId ) yield {
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