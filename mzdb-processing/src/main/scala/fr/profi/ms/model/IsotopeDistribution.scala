package fr.profi.ms.model

import fr.profi.chemistry.model.MolecularConstants

/**
 * @author David Bouyssie
 *
 */
case class IsotopeDistribution( isotopeCombinations: Array[IsotopeCombination], charge: Int ) {
  
  val isotopicVariantsByNucleonCount = isotopeCombinations.groupBy( _.nucleonCount )
  
  lazy val theoIsotopePattern = {
    
    val mzAbundancePairs = isotopicVariantsByNucleonCount.toArray.sortBy(_._1).map { case(nucleonCount,isotopeCombinations)  =>
      val massSum = isotopeCombinations.foldLeft(0.0) { (m,c) => m + c.monoMass * c.probability }
      val coeffSum = isotopeCombinations.foldLeft(0.0) { (m,c) => m + c.probability }
      val weightedMass = (massSum / coeffSum)
      (weightedMass - charge * MolecularConstants.ELECTRON_MASS) / charge -> coeffSum.toFloat
    }
    
    TheoreticalIsotopePattern(mzAbundancePairs, charge)
  }

}