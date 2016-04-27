package fr.profi.mzdb.model

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap
import scala.util.control.Breaks._

import fr.profi.chemistry.model.MolecularConstants
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.utils.ms.MsUtils
import fr.profi.util.collection._

object PeakListTree {
  
  val avgIsotopeMassDiff = MolecularConstants.AVERAGE_PEPTIDE_ISOTOPE_MASS_DIFF
  
  def groupPeaklists( peakListsBySpectrumId: LongMap[Seq[PeakList]] ): LongMap[PeakListGroup] = {    
    peakListsBySpectrumId.map { kv => kv._1 -> new PeakListGroup( kv._2 ) }
  }

  def extractIsotopicPattern(
    pklGroup: PeakListGroup,
    mz: Double,
    mzTolPPM: Float,
    charge: Int,
    maxNbPeaks: Int,
    maxTheoreticalPeakelIndex: Int = 0
  ): Array[Peak] = {
    
    val peaks = new ArrayBuffer[Peak]( maxNbPeaks )
    breakable {
      for( peakPos <- 0 until maxNbPeaks ) {
        
        // Compute some vars
        // TODO: check this the best way to compute isotope masses
        // TODO: use compute averagine to infer the delta mass
        val mzToExtract =  mz + (peakPos * PeakListTree.avgIsotopeMassDiff / charge)
        val mzTolDa = MsUtils.ppmToDa( mzToExtract, mzTolPPM )
        
        // Try to retrieve the nearest peak
        val nearestPeak = pklGroup.getNearestPeak( mzToExtract, mzTolDa)
        
        // If nearest peak is found, add it it to the list of peaks
        if (nearestPeak == null && peakPos > maxTheoreticalPeakelIndex) {
            break
        } //else
        peaks += nearestPeak
      }
    }
    peaks.toArray
  }  
}

case class PeakListTree( pklGroupBySpectrumId: LongMap[PeakListGroup], spectrumHeaderById: LongMap[SpectrumHeader] ) {

  lazy val spectrumIds: Array[Long] = pklGroupBySpectrumId.keys.toArray.sorted
  
  class SpectrumHeaderMap() {
    
    private val shCount = spectrumIds.length
      
    private val shMapBuilder = collection.immutable.Map.newBuilder[SpectrumHeader,Int]
    private val pklTreeSpectrumHeaders = new Array[SpectrumHeader](shCount)
    
    spectrumIds.zipWithIndex.foreach { case (sId,idx) =>
      val sh = spectrumHeaderById(sId)
      shMapBuilder += (sh -> idx)
      pklTreeSpectrumHeaders(idx) = sh
    }
    
    private val pklTreeShMap = shMapBuilder.result()
    
    def getSpectrumHeader( shIdx: Int ): Option[SpectrumHeader] = {
      if( shIdx < 0 || shIdx >= shCount ) None
      else Some( pklTreeSpectrumHeaders(shIdx) )
    }
    
    def getSpectrumHeaderIndex( spectrumHeader: SpectrumHeader ): Int = pklTreeShMap(spectrumHeader)
  }
  
  lazy val spectrumHeaderMap = new SpectrumHeaderMap()

    // TODO: end of move to peaklist tree or create a dedicated class ???
  
  /*def this( peakListsBySpectrumId: Map[Int, Seq[PeakList]] ) = {
    this( PeakListTree.groupPeaklists(peakListsBySpectrumId) )
  }*/
  
  /**
   * Returns all peaks contained in the PeakListTree.
   * @return an array containing all peakLists peaks (assumed to be sorted by spectrum then m/z).
   */
  def getAllPeaks(): Array[Peak] = {
    this.spectrumIds.flatMap( pklGroupBySpectrumId(_).getAllPeaks() )
  }
  
  def getNearestPeak( spectrumId: Long, mzToExtract: Double, mzTolDa: Double ): Peak = {
    pklGroupBySpectrumId(spectrumId).getNearestPeak( mzToExtract, mzTolDa )
  }
  
  def getPeaksInRange( spectrumId: Long, minMz: Double, maxMz: Double ): Array[Peak] = {
    pklGroupBySpectrumId(spectrumId).getPeaksInRange( minMz, maxMz )
  }
 
  def getXic(mz:Double, mzTolPPM: Double) : Array[Peak] = {
    this.spectrumIds.map( pklGroupBySpectrumId(_).getNearestPeak(mz, mz * mzTolPPM / 1e6))
  }
  
  protected def extractIsotopicPattern(
    spectrumHeader: SpectrumHeader, 
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    maxNbPeaksInIP: Option[Int],
    overlapShiftOpt: Option[Int], // overlap if defined return an overlapping isotopicPatternObject
    maxTheoreticalPeakelIndex:Int
  ): Option[IsotopicPatternLike] = {
    
    val mz = theoreticalIP.monoMz
    val charge = theoreticalIP.charge
    val maxNbPeaks = if (maxNbPeaksInIP.isDefined) maxNbPeaksInIP.get else theoreticalIP.abundances.filter(_ >= 5).length
    val spectrumId = spectrumHeader.id
    val pklGroupAsOpt = pklGroupBySpectrumId.get(spectrumId)
    
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
      Some( new IsotopicPattern( ipMz, ipIntensity, charge, ipPeaks, spectrumHeader, null, 0f ) )
    else
      Some( new OverlappingIsotopicPattern( ipMz, ipIntensity, charge, ipPeaks, overlapShiftOpt.get ) )
  }
  
  def extractIsotopicPattern(
    spectrumHeader: SpectrumHeader,
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    maxNbPeaksInIP: Option[Int] = null,
    maxTheoreticalPeakelIndex: Int= 0
  ): Option[IsotopicPattern] = {
    this.extractIsotopicPattern(
      spectrumHeader,
      theoreticalIP,
      mzTolPPM,
      maxNbPeaksInIP,
      None,
      maxTheoreticalPeakelIndex
    ).asInstanceOf[Option[IsotopicPattern]]
  }
  
  def extractOverlappingIsotopicPattern(
    spectrumHeader: SpectrumHeader, 
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    nbPeaksToSum: Option[Int] = null,
    overlapShift: Int,
    maxTheoreticalPeakelIndex:Int = 0
  ): Option[OverlappingIsotopicPattern] = {
    this.extractIsotopicPattern(
      spectrumHeader,
      theoreticalIP,
      mzTolPPM,
      nbPeaksToSum,
      Some(overlapShift),
      maxTheoreticalPeakelIndex
    ).asInstanceOf[Option[OverlappingIsotopicPattern]]
  }
  
}