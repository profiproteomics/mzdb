package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LongMap

case class MzRange( minMz: Double, maxMz: Double )

object PeakList {
  
  final val DEFAULT_INDEX_PRECISION = 0.1
  
  def calcMzRange( peaks: Array[Peak] ): MzRange = {
    var( minMz, maxMz ) = (0.0,0.0)
    
    if (peaks.length > 0) { //assume this peaks are sorted
      minMz = peaks.head.getMz()
      maxMz = peaks.last.getMz()
    }
    
    MzRange( minMz, maxMz )
  }
  
}

case class PeakList protected( @BeanProperty indexPrecision: Double, @BeanProperty val mzRange: MzRange ) {
  
  private val indexedPeaks = new LongMap[ArrayBuffer[Peak]]
  
  def this( peaks: Array[Peak], indexPrecision: Double = PeakList.DEFAULT_INDEX_PRECISION ) = {
    this( indexPrecision, PeakList.calcMzRange( peaks ) )
    
    // Index the peaks
    for ( peak <- peaks ) {
      val mzIndex = this.calcMzIndex( peak.mz )
      indexedPeaks.getOrElseUpdate( mzIndex, new ArrayBuffer[Peak] ) += peak
    }
    
  }
  
  protected def calcMzIndex( mz: Double ): Long = (mz/indexPrecision).toLong
  
  def getMinMz() = mzRange.minMz
  def getMaxMz() = mzRange.maxMz
  
  /**
   * Returns all peaks contained in the PeakList.
   * @return an array containing all the indexed peaks sorted by m/z.
   */
  def getAllPeaks(): Array[Peak] = {
    val sortedMzIndexes = this.indexedPeaks.keys.toArray.sorted
    sortedMzIndexes.flatMap( this.indexedPeaks(_) )
  }
  
  /** 
   * Gets the nearest peak.
   * 
   * @param mzToExtract interest mz value
   * @param mzTolDa tolerance in mz dimension in Dalton
   * @return nearest Peak or null
   */
  def getNearestPeak( mzToExtract: Double, mzTolDa: Double ): Peak = {
    
    val peaksInRangeOpt = getPeaksInRange( mzToExtract - mzTolDa, mzToExtract + mzTolDa )
    if( peaksInRangeOpt.isEmpty || peaksInRangeOpt.get.length == 0 ) return null
    
    peaksInRangeOpt.get.minBy { p => math.abs(p.getMz() - mzToExtract) }
  }
  
  /** Gets the peaks in range.
   * 
   * @author Marc Dubois
   * @param searchedMinMz the searched min mz
   * @param searchedMaxMz the searched max mz
   * @return the peaks in range
   */
  def getPeaksInRange( searchedMinMz: Double, searchedMaxMz: Double ): Option[ArrayBuffer[Peak]] = {
    
    // Check if the m/z to extract is in the peaklist m/z range
    if( searchedMinMz > this.getMaxMz || searchedMaxMz < this.getMinMz ) {
      return Option.empty[ArrayBuffer[Peak]]
    }

    // Compute index range
    val minMzIndex = this.calcMzIndex( searchedMinMz )
    val maxMzIndex = this.calcMzIndex( searchedMaxMz )

    // Initialize the minimum m/z difference with the provided m/z tolerance
    val peaksInRangeBuffer = new ArrayBuffer[Peak]
    
    for( idx <- minMzIndex to maxMzIndex ) {
      val peaks = indexedPeaks.get(idx) //peaks are sorted
      if(peaks != None ) {
        this._getPeaksInRange( searchedMinMz, searchedMaxMz, peaks.get, peaksInRangeBuffer )
      }
    }
    
    Some(peaksInRangeBuffer)
  }
  
  /** Gets the peaks in range.
   * 
   * @author David Bouyssie
   * @param minMz the min mz
   * @param maxMz the max mz
   * @param peaks the peaks
   * @param the peaks buffer
   */
  private def _getPeaksInRange(
    minMz: Double,
    maxMz: Double,
    peaks: Seq[Peak],
    peaksInRange: ArrayBuffer[Peak]
  ) {
    for (p <- peaks) {
      val mz = p.getMz() 
      if( mz >= minMz ) {
        if( mz <= maxMz ) peaksInRange += p
        else return ()
      }
    }  
  }
  
}