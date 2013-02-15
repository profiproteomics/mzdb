package fr.profi.mzdb.model

import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

case class MzRange( minMz: Double, maxMz: Double )

object PeakList {
  
  def calcMzRange( peaks: Array[Peak] ): MzRange = {
    var( minMz, maxMz ) = (0.0,0.0)
    
    if (peaks.length > 0) { //assume this peaks are sorted
      minMz = peaks.first.getMz();
      maxMz = peaks.last.getMz();
    }
    
    MzRange( minMz, maxMz )
  }
  
}

case class PeakList protected( @BeanProperty indexPrecision: Double, @BeanProperty val mzRange: MzRange ) {
  
  private val indexedPeaks = new HashMap[Int,ArrayBuffer[Peak]]
  
  def this( peaks: Array[Peak], indexPrecision: Double = 0.1 ) = {
    this( indexPrecision, PeakList.calcMzRange( peaks ) )
    
    // Index the peaks
    for ( peak <- peaks ) {
      val mzIndex = this.calcMzIndex( peak.mz )
      indexedPeaks.getOrElseUpdate( mzIndex, new ArrayBuffer[Peak] ) += peak
    }
    
  }
  
  protected def calcMzIndex( mz: Double ): Int = (mz/indexPrecision).toInt
  
  def getMinMz = mzRange.minMz
  def getMaxMz = mzRange.maxMz
  
  /** Gets the nearest peak.
   *
   * @param mzToExtract interest mz value
   * @param mzTolDa tolerance in mz dimension in Dalton
   * @return nearest Peak or null
   */
  def getNearestPeak( mzToExtract: Double, mzTolDa: Double ): Option[Peak] = {
    
    val peaksInRange = getPeaksInRange( mzToExtract - mzTolDa, mzToExtract + mzTolDa )
    if( peaksInRange == None || peaksInRange.get.length == 0 ) return Option.empty[Peak]
    
    Some( peaksInRange.get.sortBy { p => Math.abs(p.getMz() - mzToExtract) } first )
  }
  
  /** Gets the peaks in range.
   * 
   * @author Marc Dubois
   * @param searchedMinMz the searched min mz
   * @param searchedMaxMz the searched max mz
   * @return the peaks in range
   */
  def getPeaksInRange( searchedMinMz: Double, searchedMaxMz: Double ): Option[Array[Peak]] = {
    
    // Check if the m/z to extract is in the peaklist m/z range
    if( searchedMinMz > this.getMaxMz || searchedMaxMz < this.getMinMz ) {
      return Option.empty[Array[Peak]]
    }

    val minMzIndex = this.calcMzIndex( searchedMinMz ) - 1
    val maxMzIndex = this.calcMzIndex( searchedMaxMz ) + 1

    // Initialize the minimum m/z difference with the provided m/z tolerance
    val peaksInRange = new ArrayBuffer[Peak];

    for( idx <- minMzIndex to maxMzIndex ) {
      val peaks = indexedPeaks.get(idx) //peaks are sorted
      
      if(peaks != None ) {
        this._getPeaksInRange( searchedMinMz, searchedMaxMz, peaks.get, peaksInRange )
      }
    }
    
    Some(peaksInRange.toArray)
  }
  
  /** Gets the peaks in range.
   * 
   * @author David Bouyssie
   * @param minMz the min mz
   * @param maxMz the max mz
   * @param peaks the peaks
   * @param the peaks buffer
   */
  private def _getPeaksInRange( minMz: Double, maxMz: Double, peaks: Seq[Peak], peaksInRange: ArrayBuffer[Peak] ) {
    for (p <- peaks) {
      if( p.getMz() >= minMz ) {
        if( p.getMz() <= maxMz ) peaksInRange += p
        else return ()
      }
    }  
  }
  
}