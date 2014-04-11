package fr.profi.ms.model

/**
 * @author David Bouyssie
 *
 */
case class TheoreticalIsotopePattern(
  mzAbundancePairs: Array[(Double,Float)],
  charge: Int  
) {
  
  lazy val monoMz = mzAbundancePairs(0)._1
  lazy val isotopeCount = mzAbundancePairs.length  
  lazy val abundances = mzAbundancePairs.map(_._2)
  
  /** Gets the index of the max theoretical elution peak */
  lazy val theoreticalMaxPeakelIndex: Int = {
    
    val pairWithMaxInt = mzAbundancePairs.maxBy(mi => mi._2)
    
    mzAbundancePairs.indexOf(pairWithMaxInt)
  }
  
}