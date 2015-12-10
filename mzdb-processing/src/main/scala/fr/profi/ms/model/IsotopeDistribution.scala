package fr.profi.ms.model

import fr.profi.chemistry.model.MolecularConstants

/**
 * @author David Bouyssie
 *
 */
case class IsotopeDistribution( isotopeCombinations: Array[IsotopeCombination], charge: Int ) {
  require( isotopeCombinations != null, "isotopeCombinations is null" )
  require( isotopeCombinations.isEmpty == false, "isotopeCombinations is empty" )
  
  val isotopicVariantsByNucleonCount = isotopeCombinations.groupBy( _.nucleonCount )
  
  lazy val theoIsotopePattern = {
    
    var maxAbundance = 0.0
    
    val mzAbundancePairs = isotopicVariantsByNucleonCount.toArray.sortBy(_._1).map { case(nucleonCount,isotopeCombinations)  =>
      val massSum = isotopeCombinations.foldLeft(0.0) { (m,c) => m + c.monoMass * c.probability }
      val coeffSum = isotopeCombinations.foldLeft(0.0) { (m,c) => m + c.probability }
      if( coeffSum > maxAbundance ) maxAbundance = coeffSum
      
      val weightedMass = (massSum / coeffSum)
      (weightedMass - charge * MolecularConstants.ELECTRON_MASS) / charge -> coeffSum
      
    // Normalize abundances
    }.map { case (mz,ab) =>
      val normalizedAb = 100 * (ab / maxAbundance )
      mz -> normalizedAb.toFloat
    }
    
    TheoreticalIsotopePattern(mzAbundancePairs, charge)
  }

}