package fr.profi.mzdb.model

import collection.mutable.MapBuilder
import collection.mutable.ArrayBuffer
import util.control.Breaks._
import fr.profi.mzdb.utils.ms.MsUtils

object PeakListTree {
  
  val mzDiff = 1.002
  
  def groupPeaklists( peakListsByScanId: Map[Int, Seq[PeakList]] ): Map[Int,PeakListGroup] = {    
    Map() ++ peakListsByScanId.map { kv => kv._1 -> new PeakListGroup( kv._2 ) }
  }
  
  def extractIsotopicPattern(pklGroup: PeakListGroup, mz: Double, mzTolPPM: Float, charge: Int, maxNbPeaks: Int ) : Array[Option[Peak]] = {
    val peaks = new ArrayBuffer[Peak]( maxNbPeaks )
    breakable {
      for( peakPos <- 0 until maxNbPeaks ) {
        
        // Compute some vars
        // TODO: check this the best way to compute isotope masses
        // TODO: use compute averagine to infer the delta mass
        val mzToExtract =  mz + (peakPos * PeakListTree.mzDiff / charge)
        val mzTolDa = MsUtils.ppmToDa( mzToExtract, mzTolPPM )
        
        // Try to retrieve the nearest peak
        val nearestPeak = pklGroup.getNearestPeak( mzToExtract, mzTolDa)
        
        // If nearest peak is found, add it it to the list of peaks
        if( nearestPeak != None ) peaks += nearestPeak.get
        else 
          break
      }
    }
    peaks.toArray.map( Option(_) )
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
  
  protected def extractIsotopicPattern(
    scanHeader: ScanHeader, 
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    nbPeaksToSum: Int,
    overlapShiftOpt: Option[Int] // overlap if defined
  ): Option[IsotopicPatternLike] = {
    
    val mz = theoreticalIP.mz
    val charge = theoreticalIP.charge
    val maxNbPeaks = theoreticalIP.isotopesCount
    val scanId = scanHeader.id
    val pklGroupAsOpt = pklGroupByScanId.get(scanId)    
    if( charge < 1 || pklGroupAsOpt == None ) 
      return Option.empty[IsotopicPatternLike]
    
    val pklGroup = pklGroupAsOpt.get
    val ipPeaks = PeakListTree.extractIsotopicPattern(pklGroup, mz, mzTolPPM, charge, maxNbPeaks)
    if (ipPeaks.isEmpty)
      return Option.empty[IsotopicPatternLike]
    
    val ipIntensity = IsotopicPatternLike.sumPeakIntensities(ipPeaks, nbPeaksToSum)
    
    if( overlapShiftOpt.isEmpty )
      Some( new IsotopicPattern( ipPeaks(0).get.mz, ipIntensity, charge, ipPeaks, scanHeader, null, 0f ) )
    else
      Some( new OverlappingIsotopicPattern( ipPeaks(0).get.mz, ipIntensity, charge, ipPeaks, overlapShiftOpt.get ) )
  }
  
  def extractIsotopicPattern(
    scanHeader: ScanHeader,
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    nbPeaksToSum: Int
  ): Option[IsotopicPattern] = {
    this.extractIsotopicPattern(
      scanHeader,
      theoreticalIP,
      mzTolPPM,
      nbPeaksToSum,
      None
    ).asInstanceOf[Option[IsotopicPattern]]
  }
  
  def extractOverlappingIsotopicPattern(
    scanHeader: ScanHeader, 
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    nbPeaksToSum: Int,
    overlapShift: Int
  ): Option[OverlappingIsotopicPattern] = {
    this.extractIsotopicPattern(
      scanHeader,
      theoreticalIP,
      mzTolPPM,
      nbPeaksToSum,
      Some(overlapShift)
    ).asInstanceOf[Option[OverlappingIsotopicPattern]]
  }
  
}