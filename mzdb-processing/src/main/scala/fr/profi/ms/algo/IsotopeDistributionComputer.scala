package fr.profi.ms.algo

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import fr.profi.chemistry.model._
import fr.profi.ms.model._
import fr.profi.util.MathUtils

/**
 * @author David Bouyssie
 *
 */
object IsotopeDistributionComputer {
  
  final val TARGETED_ATOM_COUNT_INCREMENT = 1024
  
  def computeIsotopeDistribution(
    compoundAtomComposition: AtomComposition,
    charge: Int,
    isotopeCombinationMap: Map[(Atom,Float),Array[IsotopeCombination]],
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
      isotopeCombinationMap,
      atoms,
      minProba
    )
    
    IsotopeDistribution(computedCombinations.toArray,charge)
  }
  
  private def _combineIsotopeCombinations(
    computedCombinations: ArrayBuffer[IsotopeCombination],
    lastCombination: IsotopeCombination,
    compoundAtomCountByAtom: HashMap[Atom,Float],
    isotopeCombinationMap: Map[(Atom,Float),Array[IsotopeCombination]],
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
    val curAtomCount = compoundAtomCountByAtom(curAtom)
    val remainingAtoms = atoms.tail
    
    // Retrieve matching isotope combinations for the current atom abundance
    val matchingIsotopeCombinationsOpt = isotopeCombinationMap.get( (curAtom,curAtomCount) )
    require(
      matchingIsotopeCombinationsOpt.isDefined,
      s"provided computedCombinations has no entry for atom ${curAtom.symbol} with abundance ${curAtomCount}"
    )
    
    for( isotopeCombination <- matchingIsotopeCombinationsOpt.get ) {
      
      val newProbability = lastCombination.probability * isotopeCombination.probability
      
      if( newProbability >= minProba ) {
        
        // Merge last composition with the one of the current isotope combination
        val newAbundanceMap = lastCombination.getCloneOfMutableAbundanceMap()
        AbundanceMapOps.addAbundanceMap(newAbundanceMap, isotopeCombination.abundanceMap)
        
        // Instantiate a new isotope combination corresponding to the merged composition
        val newCombination = IsotopeCombination( newAbundanceMap.toMap, newProbability )
        
        this._combineIsotopeCombinations(
          computedCombinations,
          newCombination,
          compoundAtomCountByAtom,
          isotopeCombinationMap,
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
  ): Map[(Atom,Float),Array[IsotopeCombination]] = {
    
    val mapBuilder = Map.newBuilder[(Atom,Float),Array[IsotopeCombination]]
    
    for( (atom,maxAtomCount) <- maxAtomCountByAtom ) {
      
      val isotopeCombinations = this.computeAtomIsotopicVariantCombinations(atom,maxAtomCount,minProba)
      
      for( (atomCount, combinations) <- isotopeCombinations.groupBy(_.atomCount) ) {
        mapBuilder += (atom,atomCount) -> combinations
      }
    }
    
    mapBuilder.result()
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
  
  @tailrec
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
          Seq(combination),
          atomIsotopes,
          curTargetedAtomCount,
          minProba
        )
      }
    }
    
    this._computeIsotopicVariantCombinations(computedCombinationByFormula,atomIsotopes,curTargetedAtomCount,maxAtomCount,minProba)
  }
  
  @tailrec
  private def _createNewIsotopeCombinations(
    computedCombinationByFormula: HashMap[String,IsotopeCombination],
    lastCombinations: Seq[IsotopeCombination],
    atomIsotopes: Array[AtomIsotopicVariant],
    targetedAtomCount: Int,
    minProba: Float
  ) {
    if( lastCombinations.isEmpty ) return

    val newCombinationsBuffers = new ArrayBuffer[IsotopeCombination](atomIsotopes.length)

    for (lastCombination <- lastCombinations) {
      val atomCount = lastCombination.atomCount
      if (atomCount < targetedAtomCount) {
  
        // Retrieve previous abundance map
        val lastAbundanceMap = lastCombination.abundanceMap
  
        for (atomIsotope <- atomIsotopes) {
  
          // Clone the abundance map and initialize the current variant counter if needed
          val newAbundanceMap = new HashMap[AtomIsotopicVariant, Float]()
          newAbundanceMap ++= lastAbundanceMap
          newAbundanceMap.getOrElseUpdate(atomIsotope, 0)
  
          // Increment the abundance of the current isotopic variant
          newAbundanceMap(atomIsotope) += 1
  
          val formula = new AtomIsotopeComposition(newAbundanceMap).toFormula()
          if (computedCombinationByFormula.contains(formula) == false) {
  
            // Get the current count for this variant and for the total number of atoms
            val newIsotopeCount = newAbundanceMap(atomIsotope)
            val newAtomCount = atomCount + 1
  
            // Compute the new probability for this combination
            val newProba = lastCombination.probability * atomIsotope.isotope.abundance * newAtomCount / newIsotopeCount
  
            // Return if probability is too low
            if (newProba >= minProba) {
  
              val newCombination = IsotopeCombination(newAbundanceMap.toMap, newProba)
  
              this.synchronized {
                computedCombinationByFormula += formula -> newCombination
              }
  
              newCombinationsBuffers += newCombination
            }
          }
        }
      }
    }

    this._createNewIsotopeCombinations(
      computedCombinationByFormula,
      newCombinationsBuffers,
      atomIsotopes,
      targetedAtomCount,
      minProba
    )
  }
  
  
  /*def nearlyEqual(a: Float, b: Float): Boolean = {
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

  }*/

}