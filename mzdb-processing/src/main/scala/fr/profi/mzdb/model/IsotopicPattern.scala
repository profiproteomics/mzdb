package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.LongMap

object IsotopicPatternLike {
  
  /**
   * Sum peak intensities.
   *
   * @param peaks the peaks
   * @param maxNbPeaks the max nb peaks
   * @return the double
   */
  def sumPeakIntensities( peaks: Array[Peak], maxNbPeaks: Int ): Float = {    
    val filteredPeaks = if( maxNbPeaks > 0 ) peaks.take( maxNbPeaks ) else peaks
    
    peaks.foldLeft(0f) { (intSum,peak) =>
      if (peak != null) intSum + peak.getIntensity() else intSum
    }
  }
}

trait IsotopicPatternLike {
  val mz: Double
  var intensity: Float
  val charge: Int
  val peaks: Array[Peak]
  
  def computeIntensity( maxNbPeaks: Int ) = {    
    this.intensity = IsotopicPatternLike.sumPeakIntensities( this.peaks, maxNbPeaks )
  }
}

case class OverlappingIsotopicPattern(
  @BeanProperty val mz: Double,
  @BeanProperty var intensity: Float,
  @BeanProperty val charge: Int,
  @BeanProperty val peaks: Array[Peak],
  @BeanProperty val overlapShift: Int
) extends IsotopicPatternLike

/** The Class IsotopicPattern.
 * @author David Bouyssie
 *
 */
case class IsotopicPattern(
  @BeanProperty mz: Double,
  @BeanProperty var intensity: Float,
  @BeanProperty charge: Int,
  @BeanProperty peaks: Array[Peak],
  @transient @BeanProperty var spectrumHeader: SpectrumHeader,
  @BeanProperty var overlappingIps: Array[OverlappingIsotopicPattern] = null,
  @BeanProperty var qualityScore: Float = 0f
) extends IsotopicPatternLike {
  
  require(peaks.count(_ != null) > 0, "no defined peak provided")
  
  lazy val spectrumInitialId = spectrumHeader.getInitialId
  
  // Update the LC context of corresponding peaks
  for( p <- peaks  if p != null ) { 
    p.setLcContext(spectrumHeader) 
  }
  
  def normalizeIntensities( nfBySpectrumId: LongMap[Float] ) {
    val nf = nfBySpectrumId(this.spectrumHeader.getId)
    
    for( p <- peaks if p != null ) {
      if( p.isNormalized == false)
        p.normalizeIntensity(nf)
    }
  }
}
