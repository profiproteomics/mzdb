package fr.profi.mzdb.algo.signal.generation

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.utils.math.pdf.GaussianModel
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.ElutionTimeContext

object PeakelGenerator {

  def generate( mz: Double, time: Float, intensity: Float, duration: Float, samplingSize: Float, peakHwhm: Float ): Peakel = {
    require( samplingSize > 0 )
    require( duration > 0 )
    
    val halfDuration = duration / 2
    val gaussModel = new GaussianModel( time, intensity.toDouble, halfDuration, 0.5 )
    
    val peaks = new ArrayBuffer[Peak]
    for( x <- ((time - halfDuration)) to ((time + halfDuration)) by samplingSize ) {
      val y = gaussModel.getYValue(x)
      //println( ""+x +"\t"+y )
      peaks +=  new Peak( mz, y.toFloat, peakHwhm, peakHwhm, new ElutionTimeContext(x) ) 
    }
    
    new Peakel( peaks.toArray )
  }
  
}