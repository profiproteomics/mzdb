package fr.profi.mzdb.model

import collection.mutable.MapBuilder
import collection.mutable.ArrayBuffer
import util.control.Breaks._
import fr.profi.mzdb.utils.ms.MsUtils

object PeakListTree {
  
  val mzDiff = 1.0027
  
  def groupPeaklists( peakListsByScanId: Map[Int, Seq[PeakList]] ): Map[Int,PeakListGroup] = {    
    Map() ++ peakListsByScanId.map { kv => kv._1 -> new PeakListGroup( kv._2 ) }
  }
  
  def extractIsotopicPattern(pklGroup: PeakListGroup, mz: Double, mzTolPPM: Float, 
		  						           charge: Int, maxNbPeaks: Int, 
		  						           maxTheoreticalPeakelIndex:Int= 0) : Array[Peak] = {
    
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
        if (nearestPeak == null && peakPos >= maxTheoreticalPeakelIndex) {
            break
        } //else
        peaks += nearestPeak
      }
    }
    peaks.toArray
  }  
}

case class PeakListTree( private var pklGroupByScanId: Map[Int,PeakListGroup] ) {

  /*def this( peakListsByScanId: Map[Int, Seq[PeakList]] ) = {
    this( PeakListTree.groupPeaklists(peakListsByScanId) )
  }*/
  
  def getNearestPeak( scanId: Int, mzToExtract: Double, mzTolDa: Double ): Peak = {
    pklGroupByScanId(scanId).getNearestPeak( mzToExtract, mzTolDa )    
  }
  
  def getPeaksInRange( scanId: Int, minMz: Double, maxMz: Double ): Array[Peak] = {
    pklGroupByScanId(scanId).getPeaksInRange( minMz, maxMz )    
  }
  
  def scansIDs() : Array[Int] = { pklGroupByScanId.keys.toArray.sortWith(_ < _) }
  
  lazy val scansIds = this.scansIDs()
 
  def getXic(mz:Double, mzTolPPM: Double) : Array[Peak] = {
    this.scansIds.map( pklGroupByScanId(_).getNearestPeak(mz, mz * mzTolPPM / 1e6))
  }
  
  
  protected def extractIsotopicPattern(
    scanHeader: ScanHeader, 
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    maxNbPeaksInIP: Option[Int],
    overlapShiftOpt: Option[Int], // overlap if defined return an overlapping isotopicPatternObject
    maxTheoreticalPeakelIndex:Int
  ): Option[IsotopicPatternLike] = {
    
    val mz = theoreticalIP.mz
    val charge = theoreticalIP.charge
    val maxNbPeaks = if (maxNbPeaksInIP != null) maxNbPeaksInIP.get else theoreticalIP.getRelativeAbundances.filter(_ >= 5).length
    val scanId = scanHeader.id
    val pklGroupAsOpt = pklGroupByScanId.get(scanId)
    
    if( charge < 1 || pklGroupAsOpt == None ) 
      return Option.empty[IsotopicPatternLike]
    
    val pklGroup = pklGroupAsOpt.get
    val ipPeaks = PeakListTree.extractIsotopicPattern(pklGroup, mz, mzTolPPM, charge, maxNbPeaks, maxTheoreticalPeakelIndex)
    
    // No peaks found, there is a gap
    if (ipPeaks.isEmpty || ipPeaks.count(_ != null) == 0 )
      return Option.empty[IsotopicPatternLike]
    
    // sum by default 2
    val ipIntensity = IsotopicPatternLike.sumPeakIntensities(ipPeaks, 2)
    
    val ipMz = if (ipPeaks.head != null) ipPeaks.head.mz else mz
    if( overlapShiftOpt.isEmpty )
      Some( new IsotopicPattern( ipMz, ipIntensity, charge, ipPeaks, scanHeader, null, 0f ) )
    else
      Some( new OverlappingIsotopicPattern( ipMz, ipIntensity, charge, ipPeaks, overlapShiftOpt.get ) )
  }
  
  def extractIsotopicPattern(
    scanHeader: ScanHeader,
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    maxNbPeaksInIP: Option[Int] = null,
    maxTheoreticalPeakelIndex: Int= 0
  ): Option[IsotopicPattern] = {
    this.extractIsotopicPattern(
      scanHeader,
      theoreticalIP,
      mzTolPPM,
      maxNbPeaksInIP,
      None,
      maxTheoreticalPeakelIndex
    ).asInstanceOf[Option[IsotopicPattern]]
  }
  
  def extractOverlappingIsotopicPattern(
    scanHeader: ScanHeader, 
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    nbPeaksToSum: Option[Int] = null,
    overlapShift: Int,
    maxTheoreticalPeakelIndex:Int = 0
  ): Option[OverlappingIsotopicPattern] = {
    this.extractIsotopicPattern(
      scanHeader,
      theoreticalIP,
      mzTolPPM,
      nbPeaksToSum,
      Some(overlapShift),
      maxTheoreticalPeakelIndex
    ).asInstanceOf[Option[OverlappingIsotopicPattern]]
  }
  
}