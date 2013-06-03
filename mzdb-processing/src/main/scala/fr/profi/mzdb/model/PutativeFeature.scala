package fr.profi.mzdb.model

import scala.reflect.BeanProperty
import fr.profi.mzdb.utils.misc.InMemoryIdGen
import fr.profi.mzdb.utils.misc.IsotopicPatternLookup

object PutativeFeature extends InMemoryIdGen

/**
 * @author David Bouyssie
 *
 */
case class PutativeFeature(
  @BeanProperty id: Int,
  @BeanProperty mz: Double,
  @BeanProperty charge: Int
) {
  
  @BeanProperty lazy val theoreticalIP: TheoreticalIsotopePattern = {
    IsotopicPatternLookup.getTheoreticalPattern(mz, charge)
  }

  @BeanProperty var scanId: Int = 0
  @BeanProperty var elutionTime: Float = 0
  @BeanProperty var firstScanId: Int = 0
  @BeanProperty var lastScanId: Int = 0
  @BeanProperty var evidenceMsLevel: Int = 0 // TODO: comment this attribute
  @BeanProperty var isPredicted: Boolean = false
  @BeanProperty var peakelsCount: Int = 0
  
  def this( id: Int, mz: Double, charge: Int, firstScanId: Int, lastScanId: Int, evidenceMsLevel: Int ) = {
    this( id, mz, charge )
    this.firstScanId = firstScanId
    this.lastScanId = lastScanId
    this.evidenceMsLevel = evidenceMsLevel
  }
  
  def this( id: Int, mz: Double, charge: Int, scanId: Int, evidenceMsLevel: Int ) = {
    this( id, mz, charge )
    this.scanId = scanId
    this.evidenceMsLevel = evidenceMsLevel
  }
  
  def this( id: Int, mz: Double, charge: Int, elutionTime: Float, evidenceMsLevel: Int ) = {
    this( id, mz, charge )
    this.elutionTime = elutionTime
    this.evidenceMsLevel = evidenceMsLevel
  }
  
}