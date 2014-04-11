package fr.profi.ms.algo

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import fr.profi.chemistry.model._
import fr.profi.ms.model._
import fr.proline.util.MathUtils


/**
 * @author David Bouyssie
 *
 */
object IsotopeDistributionComputer {
  
  final val TARGETED_ATOM_COUNT_INCREMENT = 1024
  
  def computeIsotopeDistribution(
    compoundAtomComposition: AtomComposition,
    isotopeCombinationsByAtom: Map[Atom,Array[IsotopeCombination]],
    minProba: Float = 0f
  ): IsotopeDistribution = {
    
    val compoundAtomCountByAtom = compoundAtomComposition.abundanceMap
    val atoms = compoundAtomCountByAtom.keys.toArray.sortBy(_.symbol)
    
    val emptyCombination = IsotopeCombination( Map.empty[AtomIsotopicVariant,Float], 1f )
    val computedCombinations = new ArrayBuffer[IsotopeCombination]()
    
    this._combineIsotopeCombinations(
      computedCombinations,
      emptyCombination,
      compoundAtomCountByAtom,
      isotopeCombinationsByAtom,
      atoms,
      minProba
    )
    
    IsotopeDistribution(computedCombinations.toArray)    
  }
  
  private def _combineIsotopeCombinations(
    computedCombinations: ArrayBuffer[IsotopeCombination],
    lastCombination: IsotopeCombination,
    compoundAtomCountByAtom: HashMap[Atom,Float],
    isotopeCombinationsByAtom: Map[Atom,Array[IsotopeCombination]],
    atoms: Array[Atom],
    minProba: Float = 0f
  ) {
    if( atoms.isEmpty ) {
      this.synchronized {
        computedCombinations += lastCombination
      }
      return
    }
    
    // Extract first atom from the atoms array
    val curAtom = atoms.head
    val remainingAtoms = atoms.tail
    
    // Retrieve matching isotope combinations for the current atom abundance
    val isotopeCombinations = isotopeCombinationsByAtom(curAtom)
    val matchingIsotopeCombinations = isotopeCombinations.filter { isotopeCombination =>
      if( curAtom == BiomoleculeAtomTable.getAtom("S") ) {
        //println( isotopeCombination.toFormula() )
        //println( compoundAtomCountByAtom(curAtom) )
        //println( isotopeCombination.getAtomCount == compoundAtomCountByAtom(curAtom) )
      }
      isotopeCombination.atomCount == compoundAtomCountByAtom(curAtom)
      //nearlyEqual( isotopeCombination.getAtomCount, compoundAtomCountByAtom(curAtom) )
    }
    
    for( isotopeCombination <- matchingIsotopeCombinations ) {
      //println( isotopeCombination.toFormula() )
      
      val newProbability = lastCombination.probability * isotopeCombination.probability
      
      if( newProbability >= minProba ) {
        
        // Merge last composition with the one of the current isotope combination
        val newComposition = lastCombination.getIsotopeCompositionClone
        newComposition += isotopeCombination.getIsotopeCompositionClone
        
        // Instantiate a new isotope combination corresponding to the merged composition
        val newCombination = IsotopeCombination( newComposition.abundanceMap.toMap, newProbability )
        //lastCombination.copy()//IsotopeCombination( lastCombination, newProbability)
        
        this._combineIsotopeCombinations(
          computedCombinations,
          newCombination,
          compoundAtomCountByAtom,
          isotopeCombinationsByAtom,
          remainingAtoms,
          minProba
        )
      }
    }
    
    ()
  }
  
  
  def computeIsotopicVariantCombinations(
    maxAtomCountByAtom: Map[Atom,Int],
    minProba: Float = 0f
  ): Map[Atom,Array[IsotopeCombination]] = {    
    maxAtomCountByAtom.map { case (atom,maxAtomCount) =>
      atom -> this.computeAtomIsotopicVariantCombinations(atom,maxAtomCount,minProba)
    }
  }
  
  def computeAtomIsotopicVariantCombinations(
    atom: Atom,
    maxAtomCount: Int,
    minProba: Float = 0f
  ): Array[IsotopeCombination] = {
    
    require( maxAtomCount >= 1 ,"maxAtomCount must be >= 1" )
    require( minProba >= 0 && minProba <= 1,"minProba must be a number between 0 and 1" )
    
    val atomIsotopes = atom.isotopicVariants
    
    // Initialize the pool of all computed combinations
    val computedCombinationByFormula = new HashMap[String,IsotopeCombination]()
    computedCombinationByFormula += "" -> IsotopeCombination( Map.empty[AtomIsotopicVariant,Float], 1f )
    
    this._computeIsotopicVariantCombinations(computedCombinationByFormula,atomIsotopes,0,maxAtomCount,minProba)
    
    computedCombinationByFormula.values.toArray
  }
  
  private def _computeIsotopicVariantCombinations(
    computedCombinationByFormula: HashMap[String,IsotopeCombination],
    atomIsotopes: Array[AtomIsotopicVariant],
    lastTargetedAtomCount: Int,
    maxAtomCount: Int,
    minProba: Float
  ) {
    if( lastTargetedAtomCount == maxAtomCount ) return
    
    // Set the current targeted atom count
    val curTargetedAtomCount = math.min(lastTargetedAtomCount + TARGETED_ATOM_COUNT_INCREMENT,maxAtomCount)
    
    //val lastCombination = IsotopeCombination( new HashMap[AtomIsotopicVariant,Float], 1f )
    computedCombinationByFormula.par.foreach { computedCombination =>
      
      val (formula,combination) = computedCombination
      
      // Skip formula that didn't reach the targeted atom count
      if( combination.atomCount == lastTargetedAtomCount ) {
        
        this._createNewIsotopeCombinations(
          computedCombinationByFormula,
          combination,
          atomIsotopes,
          curTargetedAtomCount,
          minProba
        )        
      }
    }
    
    this._computeIsotopicVariantCombinations(computedCombinationByFormula,atomIsotopes,curTargetedAtomCount,maxAtomCount,minProba)
  }
  
  private def _createNewIsotopeCombinations(
    computedCombinationByFormula: HashMap[String,IsotopeCombination],
    lastCombination: IsotopeCombination,
    atomIsotopes: Array[AtomIsotopicVariant],
    targetedAtomCount: Int,
    minProba: Float
  ) {
    val atomCount = lastCombination.atomCount
    if( atomCount >= targetedAtomCount ) return
    
    //println(s"last combination formula: ${lastCombination.toFormula()}")
    
    // Retrieve previous abundance map
    val lastAbundanceMap = lastCombination.abundanceMap
    
    for( atomIsotope <- atomIsotopes ) {
      
      // Clone the abundance map and initialize the current variant counter if needed
      val newAbundanceMap = new HashMap[AtomIsotopicVariant,Float]()
      newAbundanceMap ++= lastAbundanceMap
      newAbundanceMap.getOrElseUpdate(atomIsotope, 0)
      
      // Increment the abundance of the current isotopic variant
      newAbundanceMap(atomIsotope) += 1
      
      val formula = new AtomIsotopeComposition(newAbundanceMap).toFormula()
      if( computedCombinationByFormula.contains(formula) ) {
        //println(s"combination ${formula} already computed => skipped")
      } else {
        
        // Get the current count for this variant and for the total number of atoms
        val newIsotopeCount = newAbundanceMap(atomIsotope)
        val newAtomCount = atomCount + 1
        
        // Compute the new probability for this combination
        val newProba = lastCombination.probability * atomIsotope.isotope.abundance * newAtomCount / newIsotopeCount
        
        // Return if probability is too low
        if( newProba < minProba ) {
          //println( s"too low probability for combination ${formula}: " + newProba )
        } else {
          //println( Seq(lastCombination.probability, isotopicVariant.isotope.abundance, newCount, otherVariantsCombinationCount).mkString(", ") )
          
          val newCombination = IsotopeCombination( newAbundanceMap.toMap, newProba )
          //println( s"last combination probability = " + lastCombination.probability )
          //println( s"combination ${formula} has a probability of " + newProba )
          
          this.synchronized {
            computedCombinationByFormula += formula -> newCombination
          }
          
          this._createNewIsotopeCombinations(
            computedCombinationByFormula,
            newCombination,
            atomIsotopes,
            targetedAtomCount,
            minProba
          )
        }
      }
    }
    
    ()
  }
  
  def nearlyEqual(a: Float, b: Float): Boolean = {
    nearlyEqual(a,b,MathUtils.EPSILON_FLOAT)  
  }
  
  /**
    * Compare two floats with a given tolerance (epsilon).
    * Source : http://floating-point-gui.de/errors/comparison/
    *
    * TODO: put Java version in Math Utils
    */
  def nearlyEqual(a: Float, b: Float, epsilon: Float): Boolean = {

    if (a == b) { // shortcut, handles infinities
      true
    } else {
      val absA = math.abs(a)
      val absB = math.abs(b)
      val diff = math.abs(a - b)
      
      if (a == 0 || b == 0 || diff < java.lang.Float.MIN_NORMAL) {
        
        // a or b is zero or both are extremely close to it
        // relative error is less meaningful here
        diff < (epsilon * java.lang.Float.MIN_NORMAL)
        
      } else { // use relative error
        (diff / (absA + absB) ) < epsilon
      }
    }

  }

}