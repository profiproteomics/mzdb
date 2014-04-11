package fr.profi.ms.model

/**
 * @author David Bouyssie
 *
 */
case class TheoreticalIsotopePattern(
  mzIntensityPairs: Array[(Double,Float)],
  charge: Int  
) {
  
  lazy val monoMz = mzIntensityPairs(0)._1
  lazy val isotopeCount = mzIntensityPairs.length
  
  // TODO: compute this value
  //lazy val relativeAbundances = null
  
  /** Gets the index of the max theoretical elution peak */
  def getTheoriticalMaxPeakelIndex(): Int = {
    
    val pairWithMaxInt = mzIntensityPairs.maxBy(mi => mi._2)
    
    mzIntensityPairs.indexOf(pairWithMaxInt)
  }
  
}