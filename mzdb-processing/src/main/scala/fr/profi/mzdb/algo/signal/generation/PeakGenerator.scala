package fr.profi.mzdb.algo.signal.generation

import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
class PeakGenerator {

  def generate( mz: Double, intensity: Float, hwhm: Float ): Peak = {
    require( mz > 0 )
    require( intensity > 0 )
    require( hwhm > 0 )
    
    
    null
    
  }
  
}