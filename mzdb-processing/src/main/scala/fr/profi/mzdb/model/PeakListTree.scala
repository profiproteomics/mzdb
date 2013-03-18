package fr.profi.mzdb.model

import collection.mutable.MapBuilder
import collection.mutable.ArrayBuffer
import util.control.Breaks._
import fr.profi.mzdb.utils.ms.MsUtils

object PeakListTree {
  
  def groupPeaklists( peakListsByScanId: Map[Int, Seq[PeakList]] ): Map[Int,PeakListGroup] = {    
    Map() ++ peakListsByScanId.map { kv => kv._1 -> new PeakListGroup( kv._2 ) }
  }
  
}

case class PeakListTree( private var pklGroupByScanId: Map[Int,PeakListGroup] ) {

  /*def this( peakListsByScanId: Map[Int, Seq[PeakList]] ) = {
    this( PeakListTree.groupPeaklists(peakListsByScanId) )
  }*/
  
  def getNearestPeak( scanId: Int, mzToExtract: Double, mzTolDa: Double ): Option[Peak] = {
    pklGroupByScanId(scanId).getNearestPeak( mzToExtract, mzTolDa )    
  }
  
  def getPeaksInRange( scanId: Int, minMz: Double, maxMz: Double ): Array[Peak] = {
    pklGroupByScanId(scanId).getPeaksInRange( minMz, maxMz )    
  }
  
  def scansIDs() : Array[Int] = { pklGroupByScanId.keys.toArray.sortWith(_ < _) }
  
  def extractIsotopicPattern( scanHeader: ScanHeader, mz: Double, mzTolPPM: Float,
                              charge: Int, maxNbPeaks: Int ): Option[IsotopicPattern] = {
    
    val scanId = scanHeader.id
    val pklGroupAsOpt = pklGroupByScanId.get(scanId)    
    if( charge < 1 || pklGroupAsOpt == None ) return Option.empty[IsotopicPattern]
    
    val pklGroup = pklGroupAsOpt.get
    val peaks = new ArrayBuffer[Peak]( maxNbPeaks )
    
    breakable {
      for( peakPos <- 0 until maxNbPeaks ) {
        
        // Compute some vars
        // TODO: check this the best way to compute isotope masses
        // TODO: use compute averagine to infer the delta mass
        val mzToExtract =  mz + (peakPos * 1.002 / charge)
        val mzTolDa = MsUtils.ppmToDa( mzToExtract, mzTolPPM )
        
        // Try to retrieve the nearest peak
        val nearestPeak = pklGroup.getNearestPeak( mzToExtract, mzTolDa)
        
        // If nearest peak is found, add it it to the list of peaks
        if( nearestPeak != None ) peaks += nearestPeak.get
        else 
          break
        
      }
    }
    
    if( peaks.length == 0 ) 
      return Option.empty[IsotopicPattern]
    
    // Compute IP intensity using the 2 first peaks
    // TODO: use parameter which specifies the number of peaks to use
    val ipPeaks = peaks.toArray.map( Option(_) )
    val ipIntensity = IsotopicPattern.sumPeakIntensities(ipPeaks, 2);
    
    Some( new IsotopicPattern( ipPeaks(0).get.mz, ipIntensity, charge, ipPeaks, scanHeader, null, 0f ) )
    
  }
  
}