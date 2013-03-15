package fr.profi.mzdb.model

import scala.reflect.BeanProperty
//import com.codahale.jerkson.JsonSnakeCase
import com.fasterxml.jackson.annotation.JsonInclude
import com.fasterxml.jackson.annotation.JsonInclude.Include

object IsotopicPattern {
  
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

/** The Class IsotopicPattern.
 * @author David Bouyssie
 *
 */
//@JsonSnakeCase
@JsonInclude( Include.NON_NULL )
case class IsotopicPattern (
  @BeanProperty mz: Double,
  @BeanProperty var intensity: Float,
  @BeanProperty charge: Int,
  @BeanProperty peaks: Array[Option[Peak]],
  @transient @BeanProperty var scanHeader: ScanHeader,
  @BeanProperty var overlappingIps: Array[IsotopicPattern] = null,
  @BeanProperty var qualityScore: Float = 0f
) {
  
  lazy val scanInitialId = scanHeader.getInitialId
  
  // Update the LC context of corresponding peaks
  for( pOpt <- peaks; p <- pOpt if pOpt != None ) { p.setLcContext(scanHeader) }
  
  /**
   * Compute intensity.
   *
   * @param maxNbPeaks the max nb peaks
   */
  def computeIntensity( maxNbPeaks: Int ) = {    
    this.intensity = IsotopicPattern.sumPeakIntensities( this.peaks, maxNbPeaks )
  }
  
  def normalizeIntensities( nfByScanId: Map[Int,Float] ) {
    val nf = nfByScanId(this.scanHeader.id)
    
    for( pOpt <- peaks; p <- pOpt if pOpt != None ) {
      if( p.isNormalized == false)
        p.normalizeIntensity(nf)
    }
  }

}
