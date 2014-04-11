package fr.profi.mzdb.model

import scala.beans.BeanProperty

/**
 * @author David Bouyssie
 *
 */
case class TheoreticalIsotopePattern(
  @BeanProperty mz: Double,
  @BeanProperty charge: Int,
  @BeanProperty relativeAbundances: Array[Float]
) {
  
  lazy val isotopesCount = relativeAbundances.length
  
  /** Gets the index of the max theoretical elution peak */
  def getTheoriticalMaxPeakelIndex(): Int = {
    
    val relativeAbundances = this.getRelativeAbundances
    val maxValue = relativeAbundances.maxBy(x => x)
    
    relativeAbundances.indexOf(maxValue)
  }
  
}