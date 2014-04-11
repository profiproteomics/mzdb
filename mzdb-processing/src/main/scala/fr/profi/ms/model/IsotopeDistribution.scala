package fr.profi.ms.model

import fr.profi.chemistry.model.MolecularConstants

/**
 * @author David Bouyssie
 *
 */
case class IsotopeDistribution( isotopeCombinations: Array[IsotopeCombination] ) {
  
  val isotopicVariantsByMassNumber: Int = {
    
    /*for( isotopeCombination <- isotopeCombinations.sortBy(_.monoMass) ) {
      val mass = isotopeCombination.monoMass - MolecularConstants.ELECTRON_MASS
      println( isotopeCombination.formula + " "+isotopeCombination.nucleonNumber +" "+mass+" "+ (isotopeCombination.probability *100) )
    }*/
    
    /*isotopeCombinations.groupBy( _.nucleonNumber ).toList.sortBy(_._1).map { kv =>
      val combi = kv._2
      val massSum = combi.foldLeft(0.0) { (m,c) => m + c.monoMass * c.probability }
      val coeffSum = combi.foldLeft(0.0) { (m,c) => m + c.probability }
      val weightedMass = (massSum / coeffSum) - MolecularConstants.ELECTRON_MASS
      println( weightedMass+" "+ (coeffSum*100) )
    }*/
    

    
    0
  }

}