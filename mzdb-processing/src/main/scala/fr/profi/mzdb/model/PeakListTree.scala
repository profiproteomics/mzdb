package fr.profi.mzdb.model

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap
import scala.util.control.Breaks._

import fr.profi.chemistry.model.MolecularConstants
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.util.ms.MsUtils
import fr.profi.util.collection._

object PeakListTree {
  
  val avgIsotopeMassDiff = MolecularConstants.AVERAGE_PEPTIDE_ISOTOPE_MASS_DIFF
  
  def buildPeakListTriplets( peakListsBySpectrumId: LongMap[Array[PeakList]] ): LongMap[PeakListTriplet] = {    
    peakListsBySpectrumId.map { kv => kv._1 -> new PeakListTriplet( kv._2 ) }
  }

  def extractIsotopicPattern(
    pklTriplet: PeakListTriplet,
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
        val nearestPeak = pklTriplet.getNearestPeak( mzToExtract, mzTolDa)
        
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

case class PeakListTree(
  pklTripletBySpectrumId: LongMap[PeakListTriplet],
  spectrumHeaderById: LongMap[SpectrumHeader]
) {
  
  val peaksCount = pklTripletBySpectrumId.values.foldLeft(0){ case (s,pklTriplet) => s + pklTriplet.peaksCount }

  lazy val spectrumIds: Array[Long] = pklTripletBySpectrumId.keys.toArray.sorted
  
  class SpectrumHeaderMap() {
    
    private val shCount = spectrumIds.length
      
    private val shMap = new LongMap[Int](shCount)
    private val pklTreeSpectrumHeaders = new Array[SpectrumHeader](shCount)
    
    spectrumIds.zipWithIndex.foreach { case (sId,idx) =>
      val sh = spectrumHeaderById(sId)
      shMap.put(sh.getId, idx)
      pklTreeSpectrumHeaders(idx) = sh
    }
    
    def getSpectrumHeaders(): Array[SpectrumHeader] = pklTreeSpectrumHeaders
    
    def getSpectrumHeader( shIdx: Int ): Option[SpectrumHeader] = {
      if( shIdx < 0 || shIdx >= shCount ) None
      else Some( pklTreeSpectrumHeaders(shIdx) )
    }
    
    def getSpectrumHeaderIndex( spectrumHeaderId: Long ): Int = shMap(spectrumHeaderId)
  }
  
  lazy val spectrumHeaderMap = new SpectrumHeaderMap()
      
  /*def getIndexedPeak( peakId: Long ): IndexedPeak = {
    val peaklist = peaklistByPeakId(peakId)
    peaklist.getIndexedPeak(peakId)
  }*/
  
  def getPeakAt( spectrumIdx: Int, peaklistIdx: Int, peakIdx: Int ): Peak = {
    val spectrumId = spectrumIds(spectrumIdx)
    val pklTriplet = pklTripletBySpectrumId(spectrumId)
    val peaklist = pklTriplet.peakLists(peaklistIdx)
    peaklist.getPeakAt(peakIdx)
  }
  
  /**
   * Returns all peaks contained in the PeakListTree.
   * @return an array containing all peakLists peaks (assumed to be sorted by spectrum then m/z).
   */
  /*def getAllPeaks(): Array[Peak] = {
    this.spectrumIds.flatMap( pklTripletBySpectrumId(_).getAllPeaks() )
  }*/
  
  def getNearestPeak( spectrumId: Long, mzToExtract: Double, mzTolDa: Double ): Peak = {
    pklTripletBySpectrumId(spectrumId).getNearestPeak( mzToExtract, mzTolDa )
  }
  
  /*def getPeaksInRange( spectrumId: Long, minMz: Double, maxMz: Double ): Array[Peak] = {
    pklTripletBySpectrumId(spectrumId).getPeaksInRange( minMz, maxMz )
  }*/
 
  def getXic(mz:Double, mzTolPPM: Double): Array[Peak] = {
    this.spectrumIds.map( pklTripletBySpectrumId(_).getNearestPeak(mz, mz * mzTolPPM / 1e6))
  }
  
  protected def extractIsotopicPattern(
    spectrumHeader: SpectrumHeader, 
    theoreticalIP: TheoreticalIsotopePattern,
    mzTolPPM: Float,
    maxNbPeaksInIP: Option[Int],
    overlapShiftOpt: Option[Int], // overlap if defined return an overlapping isotopicPatternObject
    maxTheoreticalPeakelIndex: Int
  ): Option[IsotopicPatternLike] = {
    
    val mz = theoreticalIP.monoMz
    val charge = theoreticalIP.charge
    val maxNbPeaks = if (maxNbPeaksInIP.isDefined) maxNbPeaksInIP.get else theoreticalIP.abundances.filter(_ >= 5).length
    val spectrumId = spectrumHeader.getId
    val pklTriplet = pklTripletBySpectrumId.getOrNull(spectrumId)
    
    if( charge < 1 || pklTriplet == null )
      return Option.empty[IsotopicPatternLike]
    
    val ipPeaks = PeakListTree.extractIsotopicPattern(pklTriplet, mz, mzTolPPM, charge, maxNbPeaks, maxTheoreticalPeakelIndex)
    
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