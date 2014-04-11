package fr.profi.ms.model

import fr.profi.chemistry.model.AtomIsotopeComposition
import scala.collection.mutable.HashMap
import fr.profi.chemistry.model.AtomIsotopicVariant
import fr.profi.chemistry.model.AbundanceMapOps

case class IsotopeCombination(
  abundanceMap: Map[AtomIsotopicVariant,Float],
  probability: Double
) {
  require( abundanceMap != null, "composition is null" )
  
  private val isotopeComposition = new AtomIsotopeComposition( new HashMap[AtomIsotopicVariant,Float]() ++ abundanceMap )
  
  def getCloneOfMutableAbundanceMap() = isotopeComposition.abundanceMap.clone()
  
  lazy val atomCount = isotopeComposition.getSumOfAbundances()
  lazy val formula = isotopeComposition.toFormula()
  lazy val monoMass = isotopeComposition.getMonoMass()
  lazy val nucleonCount = abundanceMap.foldLeft(0f)( (s,kv) => s + (kv._1.isotope.nucleonNumber * kv._2) )
  
}