package fr.profi.mzdb.model

// TODO: rename this file into PeakListContainers

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

trait IPeakListContainer {
  
  val peakLists: Array[PeakList]
  
  val peakListsCount = peakLists.length
  
  val peaksCount = peakLists.foldLeft(0) { case (s,pkl) =>
    if (pkl == null) s else s + pkl.getPeaksCount()
  }
  
  def getPeakAt( peaklistIdx: Int, peakIdx: Int ): Peak = {
    val peaklist = peakLists(peaklistIdx)
    if (peaklist == null) null
    else peaklist.getPeakAt(peakIdx)
  }
  
  /**
   * Returns all peaks contained in the PeakListGroup.
   * @return an array containing all peakLists peaks (assumed to be sorted by m/z).
   */
  /*def getAllPeaks(): Array[Peak] = {
    peakLists.toArray.flatMap( _.getAllPeaks() )
  }*/
  
  /** 
   * Gets the nearest peak in the peaklist group.
   * 
   * @param mzToExtract interest mz value
   * @param mzTolDa tolerance in mz dimension in Dalton
   * @return nearest Peak or null
   */
  def getNearestPeak( mzToExtract: Double, mzTolDa: Double ): Peak = {
    
    // Initialize the minimum m/z difference with the provided m/z tolerance
    var minMzDiff = mzTolDa
    var nearestPeak: Peak = null
    
    for( pkl <- peakLists ) {
      val peak = pkl.getNearestPeak( mzToExtract, mzTolDa )
      
      if( peak != null ) {
        val mzDiff = Math.abs( peak.mz - mzToExtract )
        
        if ( mzDiff < minMzDiff ) { 
          minMzDiff = mzDiff
          nearestPeak = peak
        }
      }
    }
    
    nearestPeak
  }
  
  /*def getPeaksInRange( minMz: Double, maxMz: Double ): Array[Peak] = {
    
    val peaks = new ArrayBuffer[Peak]()
    
    for( pkl <- peakLists ) {
      val peaksOpt = pkl.getPeaksInRange( minMz, maxMz )
      if( peaksOpt != None ) peaks ++= peaksOpt.get
    }
    
    peaks.toArray
  }*/
}

/** Generic implementation of the PeakList container mainly used to represent a given RunSlice **/
case class PeakListCollection( val peakLists: Array[PeakList] ) extends IPeakListContainer {
  
  def getPeaksCoordsSortedByDescIntensity(): Array[Array[Int]] = {
    
    val curRsPeaksCoords = new Array[Array[Int]](peaksCount)
    val curRsIntensityList = new Array[Float](peaksCount)

    // Merge peaklists to build curRsPeakCoords
    var curRsPeakIdx = 0
    var peaklistIdx = 0
    while( peaklistIdx < peakListsCount ) {
      val peaklist = peakLists(peaklistIdx)
      if (peaklist != null) {
        val peaksCount = peaklist.getPeaksCount()
        val intensityList = peaklist.getIntensityList()
        
        var peakIdx = 0
        while (peakIdx < peaksCount) {
          curRsPeaksCoords(curRsPeakIdx) = Array(peaklistIdx, peakIdx)
          curRsIntensityList(curRsPeakIdx) = intensityList(peakIdx)
          peakIdx += 1
          curRsPeakIdx += 1
        }
      }
    
      peaklistIdx += 1
    }
    assert( curRsPeakIdx == peaksCount, "some peaks are missing in curRsPeakIds")
    
    PeakList.sortPeaksCoordsByDescIntensity(curRsPeaksCoords, curRsIntensityList)
  }
  
  /*def getPeaksSortedByDescIntensity(): Array[Peak] = {
    
    // Merge peaklists to build curRsPeaks
    val curRsPeaks = new Array[Peak](peaksCount)
    
    var curRsPeakIdx = 0
    for (peaklist <- peakLists) {
      val peaksCount = peaklist.getPeaksCount()
      val peaks = peaklist.getPeaks()
      
      var peakIdx = 0
      while (peakIdx < peaksCount) {
        curRsPeaks(curRsPeakIdx) = peaks(peakIdx)
        peakIdx += 1
        curRsPeakIdx += 1
      }
    }
    assert( curRsPeakIdx == peaksCount, "some peaks are missing in curRsPeakIds")
    
    PeakList.sortPeaksByDescIntensity(curRsPeaks)
    
    curRsPeaks
  }*/
}

/** PeakList container implementation used by the PeakListTree **/
case class PeakListTriplet( val peakLists: Array[PeakList] ) extends IPeakListContainer {
  
  require( peakLists.length <= 3, s"invalid number of peaklists for a triplet: " + peakLists.length)
    
  // Then here begin the search in the central peaklist
  // Search before if peakIdx == 0 or search after if peakIdx == lastIdx
  def addNearestPeakToPeakelCoordinates(
    mzToExtract: Double,
    mzTolDa: Double,
    peakelCoords: PeakelCoordinates,
    peakelCoordsIdx: Int,
    intensityThreshold: Float = 0f
  ): Int = {
    
    // Initialize the minimum m/z difference with the provided m/z tolerance
    var minMzDiff = mzTolDa
    var nearestPeakIdx: Int = -1
    var nearestPeaklist: PeakList = null
    
    def getNearestPeakIdx(pkl: PeakList) {
      val peakIdx = pkl.getNearestPeakIdx( mzToExtract, mzTolDa )
      
      if( peakIdx != -1 ) {
        val mz = pkl.getMzList()(peakIdx)
        val mzDiff = Math.abs( mz - mzToExtract )
        
        if ( mzDiff < minMzDiff ) {
          val intensity = pkl.getIntensityList()(peakIdx)
          if (intensity > intensityThreshold) {
            minMzDiff = mzDiff
            nearestPeakIdx = peakIdx
            nearestPeaklist = pkl
          }
        }
      }
    }
    
    // Try to optimize getNearestPeakIdx when PeakListTriplet is composed by 3 PeakLists
    // TODO: create a fixed array of length == 3 with null values instead of dynamic array length ?
    val peakListIdxOrder = if (peakListsCount == 3) {
      getNearestPeakIdx( peakLists(1) )
      
      if (nearestPeakIdx == -1) {
        getNearestPeakIdx( peakLists(0) )
        getNearestPeakIdx( peakLists(2) )
      } else if (nearestPeakIdx == 0) {
        getNearestPeakIdx( peakLists(0) )
      } else if (nearestPeakIdx == (peakLists(1).peaksCount - 1) ) {
        getNearestPeakIdx( peakLists(2) )
      }
    } else {
      var peakListIdx = 0
      while(peakListIdx < peakListsCount) {
        getNearestPeakIdx( peakLists(peakListIdx) )
        peakListIdx += 1
      }
    }
    
    peakelCoords.peakLists(peakelCoordsIdx) = nearestPeaklist
    peakelCoords.peakIndices(peakelCoordsIdx) = nearestPeakIdx
    
    nearestPeakIdx
  }
  
}