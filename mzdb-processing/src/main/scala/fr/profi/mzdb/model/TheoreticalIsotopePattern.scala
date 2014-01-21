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
  
}