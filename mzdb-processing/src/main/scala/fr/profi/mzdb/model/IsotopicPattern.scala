package fr.profi.mzdb.model

import scala.beans.BeanProperty
//import com.codahale.jerkson.JsonSnakeCase
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonInclude.Include

object IsotopicPatternLike {
  
  /**
   * Sum peak intensities.
   *
   * @param peaks the peaks
   * @param maxNbPeaks the max nb peaks
   * @return the double
   */
  def sumPeakIntensities( peaks: Array[Option[Peak]], maxNbPeaks: Int ): Float = {    
    val filteredPeaks = if( maxNbPeaks > 0 ) peaks.take( maxNbPeaks ) else peaks 
    var sum = 0f
    for( pOpt <- peaks; p <- pOpt ) 
      sum += p.getIntensity()
    
    sum
  }
}

trait IsotopicPatternLike {
  val mz: Double
  var intensity: Float
  val charge: Int
  val peaks: Array[Option[Peak]]
  
  def computeIntensity( maxNbPeaks: Int ) = {    
    this.intensity = IsotopicPatternLike.sumPeakIntensities( this.peaks, maxNbPeaks )
  }
}


@JsonInclude( Include.NON_NULL )
case class OverlappingIsotopicPattern(
  @BeanProperty val mz: Double,
  @BeanProperty var intensity: Float,
  @BeanProperty val charge: Int,
  @BeanProperty val peaks: Array[Option[Peak]],
  @BeanProperty val overlapShift: Int
) extends IsotopicPatternLike

/** The Class IsotopicPattern.
 * @author David Bouyssie
 *
 */
@JsonInclude( Include.NON_NULL )
case class IsotopicPattern (
  @BeanProperty mz: Double,
  @BeanProperty var intensity: Float,
  @BeanProperty charge: Int,
  @BeanProperty peaks: Array[Option[Peak]],
  @transient @BeanProperty var scanHeader: ScanHeader,
  @BeanProperty var overlappingIps: Array[OverlappingIsotopicPattern] = null,
  @BeanProperty var qualityScore: Float = 0f
) extends IsotopicPatternLike {
  
  lazy val scanInitialId = scanHeader.getInitialId
  
  // Update the LC context of corresponding peaks
  for( pOpt <- peaks; p <- pOpt if pOpt != None ) { 
    p.setLcContext(scanHeader) 
    }
  
  def normalizeIntensities( nfByScanId: Map[Int,Float] ) {
    val nf = nfByScanId(this.scanHeader.id)
    
    for( pOpt <- peaks; p <- pOpt if pOpt != None ) {
      if( p.isNormalized == false)
        p.normalizeIntensity(nf)
    }
  }
}
