package fr.profi.mzdb.model

import scala.collection.mutable.ArrayBuffer

case class PeakListGroup( private val peakLists: Iterable[PeakList] ) {
  
  // TODO: speed optimization => index peaklists by mzRange + binary search dans Array[MzRange] ???
  
  def getNearestPeak( mzToExtract: Double, mzTolDa: Double ): Option[Peak] = {
    
    // Initialize the minimum m/z difference with the provided m/z tolerance
    var minMzDiff = mzTolDa    
    var nearestPeak = Option.empty[Peak]
    
    for( pkl <- peakLists ) {
      val peakOpt = pkl.getNearestPeak( mzToExtract, mzTolDa )
      
      if( peakOpt != None ) {
        val peak = peakOpt.get
        val mzDiff = Math.abs( peak.mz - mzToExtract )
        
        if ( mzDiff < minMzDiff ) { 
          minMzDiff = mzDiff
          nearestPeak = Some(peak)
        }
      }
    }
    
    nearestPeak
  }
  
  def getPeaksInRange( minMz: Double, maxMz: Double ): Array[Peak] = {
    
    val peaks = new ArrayBuffer[Peak]()
    
    for( pkl <- peakLists ) {
      val peaksOpt = pkl.getPeaksInRange( minMz, maxMz )
      if( peaksOpt != None ) peaks ++= peaksOpt.get
    }
    
    peaks.toArray
  }
  
}