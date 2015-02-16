package fr.profi.mzdb.algo.signal.generation

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model._
import fr.profi.mzdb.utils.math.pdf.GaussianModel

object PeakelGenerator {

  def generate( mz: Double, time: Float, intensity: Float, duration: Float, samplingSize: Int, mzPeakHwhm: Float ): Peakel = {
    require( samplingSize > 0, "samplingSize must be strictly positive" )
    require( duration > 0, "duration must be strictly positive" )
    
    val halfDuration = duration / 2
    val gaussModel = new GaussianModel( time, intensity.toDouble, halfDuration, 0.5 )
    
    val peaks = new ArrayBuffer[Peak]
    for( x <- ((time - halfDuration)) to ((time + halfDuration)) by samplingSize ) {
      val y = gaussModel.getYValue(x)
      peaks += new Peak( mz, y.toFloat, mzPeakHwhm, mzPeakHwhm, new FullLcContext(x.round,x) )
    }
    
    new PeakelBuilder( peaks.toArray ).result()
  }
  
}