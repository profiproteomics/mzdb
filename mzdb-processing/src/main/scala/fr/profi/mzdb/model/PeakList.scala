package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LongMap

object PeakList {
  
  def sortPeaksByDescIntensity(peaks: Array[Peak]) {
    
    // Sort peaks by descending intensity values
    val descIntPeaksOrdering = new Ordering[Peak] {
      def compare(a: Peak, b: Peak) = b.getIntensity.compare(a.getIntensity)
    }
    
    scala.util.Sorting.quickSort(peaks)(descIntPeaksOrdering)
  }
  
  def sortPeaksByAscMz(peaks: Array[Peak]) {
    
    // Sort peaks by ascending mz values
    val ascMzPeaksOrdering = new Ordering[Peak] {
      def compare(a: Peak, b: Peak) = a.getMz.compare(b.getMz)
    }
    
    scala.util.Sorting.quickSort(peaks)(ascMzPeaksOrdering)
  }
  
  /*
  def sortPeakIdsByDescIntensity(peakIds: Array[Long], intensityList: Array[Float]): Array[Long] = {
    
    // Sort peaks indices by descending intensity values
    val descIntPeaksOrdering = new Ordering[java.lang.Integer] {
      def compare(a: java.lang.Integer, b: java.lang.Integer) = {
        intensityList(b).compare(intensityList(a))
      }
    }
    
    sortPeakIds(peakIds, descIntPeaksOrdering)
  }
  
  def sortPeaksIdsByAscMz(peakIds: Array[Long], mzList: Array[Double]): Array[Long] = {
    
    // Sort peaks indices by ascending mz values
    val ascMzPeaksOrdering = new Ordering[java.lang.Integer] {
      def compare(a: java.lang.Integer, b: java.lang.Integer) = {
        mzList(a).compare(mzList(b))
      }
    }
    
    sortPeakIds(peakIds, ascMzPeaksOrdering)
  }
  
  protected def sortPeakIds(peakIds: Array[Long], peaksOrdering: Ordering[java.lang.Integer]): Array[Long] = {
    
    val peaksCount = peakIds.length
    
    // Create an array containing peaks indices
    val indices = new Array[java.lang.Integer](peaksCount)
    var idx = 0
    while (idx < peaksCount ) {
      indices(idx) = idx
      idx += 1
    }
    
    // Sort peakIds indices according to provided ordering
    scala.util.Sorting.quickSort(indices)(peaksOrdering)
    
    // Sort peakIds according to computed indices order
    val sortedIds = new Array[Long](peaksCount)
    
    idx = 0
    while (idx < peaksCount ) {
      sortedIds(idx) = peakIds(indices(idx))
      idx += 1
    }
    
    sortedIds
  }*/
  
  def sortPeaksCoordsByDescIntensity(peaksCoords: Array[Array[Int]], intensityList: Array[Float]): Array[Array[Int]] = {
    
    // Sort peaks indices by descending intensity values
    val descIntPeaksOrdering = new Ordering[java.lang.Integer] {
      def compare(a: java.lang.Integer, b: java.lang.Integer) = intensityList(b).compare(intensityList(a))
    }
    
    sortPeaksCoords(peaksCoords, descIntPeaksOrdering)
  }
  
  protected def sortPeaksCoords(peaksCoords: Array[Array[Int]], peaksOrdering: Ordering[java.lang.Integer]): Array[Array[Int]] = {
    
    val peaksCount = peaksCoords.length
    
    // Create an array containing peaks indices
    val indices = new Array[java.lang.Integer](peaksCount)
    var idx = 0
    while (idx < peaksCount ) {
      indices(idx) = idx
      idx += 1
    }
    
    // Sort peakIds indices according to provided ordering
    scala.util.Sorting.quickSort(indices)(peaksOrdering)
    //val sortedIndices = indices.sorted(peaksOrdering)
    
    // Sort peakIds according to computed indices order
    val sortedIds = new Array[Array[Int]](peaksCount)
    
    idx = 0
    while (idx < peaksCount ) {
      sortedIds(idx) = peaksCoords(indices(idx))
      idx += 1
    }
    
    sortedIds
  }
  
}

case class PeakList protected(
  @BeanProperty val spectrum: Spectrum,
  @BeanProperty val peaksCount: Int,
  @BeanProperty val minMz: Double,
  @BeanProperty val maxMz: Double
) {
  
  private val MZ_INDEX_PRECISION = 0.1
  protected def calcMzIndex( mz: Double ): Long = (mz/MZ_INDEX_PRECISION).toLong
  
  def this( spectrum: Spectrum ) = {
    this( spectrum, spectrum.getData.getPeaksCount, spectrum.getData.getMinMz, spectrum.getData.getMaxMz )
  }
  
  private val mzWindow = maxMz - minMz
  
  private val spectrumHeader = spectrum.getHeader
  private val spectrumData = spectrum.getData
  private val mzList = spectrumData.getMzList
  private val intensityList = spectrumData.getIntensityList
  private val leftHwhmList = spectrumData.getLeftHwhmList
  private val rightHwhmList = spectrumData.getRightHwhmList
  
  def getSpectrumHeader(): SpectrumHeader = spectrumHeader
  def getSpectrumData(): SpectrumData = spectrumData
  def getMzList(): Array[Double] = mzList
  def getIntensityList(): Array[Float] = intensityList
  def getLeftHwhmList(): Array[Float] = leftHwhmList
  def getRightHwhmList(): Array[Float] = rightHwhmList
  
  def buildPeak( peakIdx: Int ): Peak = {
    if (peakIdx == -1) return null
    
    new Peak(
      mzList(peakIdx),
      intensityList(peakIdx),
      if (leftHwhmList == null) 0f else leftHwhmList(peakIdx),
      if (rightHwhmList == null) 0f else rightHwhmList(peakIdx),
      spectrumHeader
    )
  }
  
  def getPeakAt( peakIdx: Int ): Peak = {
    buildPeak(peakIdx)
  }
  
  /**
   * Gets the nearest peak.
   * 
   * @param mzToExtract interest mz value
   * @param mzTolDa tolerance in mz dimension in Dalton
   * @return nearest Peak or null
   */
  def getNearestPeak( mzToExtract: Double, mzTolDa: Double ): Peak = {
    val nearestPeakIdx = this.getNearestPeakIdx(mzToExtract, mzTolDa)
    if (nearestPeakIdx == -1) return null
    
    buildPeak(nearestPeakIdx)
  }
  
  /**
   * Gets the nearest peak index.
   * 
   * @param mzToExtract interest mz value
   * @param mzTolDa tolerance in mz dimension in Dalton
   * @return nearest index or -1
   */
  def getNearestPeakIdx( mzToExtract: Double, mzTolDa: Double ): Int = {
    if (peaksCount == 0) return -1
    
    val searchedMinMz = mzToExtract - mzTolDa
    val searchedMaxMz = mzToExtract + mzTolDa
    
    // Check if the m/z to extract is in the peaklist m/z range
    if( searchedMinMz > maxMz || searchedMaxMz < minMz ) {
      return -1
    }
    
    /*println("mzToExtract: "+ mzToExtract)
    println("minMz: "+ minMz)
    println("maxMz: "+ maxMz)*/

    // Try to estimate the starting point for a search of mzToExtract
    val nPeaks = this.peaksCount
    
    val expectedNearestIdx = if( mzToExtract < minMz ) 0
    else if (mzToExtract > maxMz) nPeaks - 1
    else ( (nPeaks-1) * (mzToExtract - minMz) / mzWindow).toInt
    assert( expectedNearestIdx >= 0 && expectedNearestIdx < nPeaks, s"invalid expectedNearestIdx=$expectedNearestIdx while peaks count is $nPeaks" )
    
    // Check if we are above, below or in the valid m/z range
    val mzAtExpectedNearestIdx = mzList(expectedNearestIdx)
    val idxOffset = if (mzAtExpectedNearestIdx < mzToExtract) 1 else -1
    
    // Initialize the minimum m/z difference with the provided m/z tolerance
    var lowestDeltaMz = mzTolDa
    var prevDeltaMz = mzWindow
    var nearestMzIdx = -1
    
    // Perform the search of mzToExtract from the estimated starting point
    var mzIdx = expectedNearestIdx
    var outOfRange = false
    while (mzIdx >= 0 && mzIdx < nPeaks && !outOfRange) {
      
      val mz = mzList(mzIdx)
      //println("mz: "+ mz)
      val deltaMz = mz - mzToExtract
      val absDeltaMz = if (deltaMz <= 0) 0 - deltaMz else deltaMz
      //println("deltaMz"+ absDeltaMz)
      
      // Check if we found a better peak candidate 
      if( absDeltaMz < lowestDeltaMz ) {
        lowestDeltaMz = absDeltaMz
        nearestMzIdx = mzIdx
      // Check if we previously found a peak and we are now out of the valid m/z range
      //} else if (absDeltaMz != lowestDeltaMz && nearestMzIdx != -1 ) {
      } else if (absDeltaMz > prevDeltaMz) {
        // Stop to search because it won't find a better candidate
        outOfRange = true
      }
      
      mzIdx += idxOffset
      prevDeltaMz = absDeltaMz
    }
    
    /*if (nearestMzIdx != -1 ) {
      println("delta idx :" + (nearestMzIdx - expectedNearestIdx) + " nPeaks = " + nPeaks )
    }*/
    
    nearestMzIdx
  }
  
}
