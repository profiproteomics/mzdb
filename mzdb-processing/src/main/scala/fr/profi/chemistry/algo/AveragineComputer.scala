package fr.profi.chemistry.algo

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

import fr.profi.chemistry.model._
import fr.profi.ms.model.IsotopeCombination
import fr.proline.util.math.median

/**
 * @author David Bouyssie
 *
 */
class AveragineComputer( val aaTable: AminoAcidTableLike, val atomTable: AtomTableLike ) {
  
  private val C_ATOM = atomTable.getAtom("C")
  private val H_ATOM = atomTable.getAtom("H")
  private val O_ATOM = atomTable.getAtom("O")
  private val N_ATOM = atomTable.getAtom("N")
  private val S_ATOM = atomTable.getAtom("S")
  private val MIN_C_S_RATIO = 3 + 2 // Carbons from Cys + Gly
  private val MIN_H_S_RATIO = 5 + 3 // Hydrogens from Cys + Gly
  private val H_C_RATIO_RANGE = (10f/11, 2) // Try, Arg
  private val N_O_RATIO_RANGE = (1f/3, 4) // (Asp || Glu, Arg)
  private val C_N_RATIO_RANGE = (1.5f, 9) // (Arg, Phe || Tyr)
  
  private val abundanceRangeByAtom = aaTable.getAbundanceRangeByAtom(atomTable)
  
  private val compositionAdjustments = {
    
    // Compute the combination of composition adjustments
    val computedCompositionByFormula = new HashMap[String,AtomComposition]()
    this._computeCompositionAdjustments(
      computedCompositionByFormula,
      new AtomComposition( new HashMap[Atom,Float] ),
      Array(C_ATOM,O_ATOM,N_ATOM,S_ATOM), // TODO: alternative => remove H_ATOM from atoms array
      Map(
        C_ATOM -> Range.inclusive(-30,30),
        O_ATOM -> Range.inclusive(-6,6),
        N_ATOM -> Range.inclusive(-10,10),
        S_ATOM -> Range.inclusive(0,2)
        /*C_ATOM -> Range.inclusive(-54,54),
        O_ATOM -> Range.inclusive(-12,12),
        N_ATOM -> Range.inclusive(-18,18),
        S_ATOM -> Range.inclusive(0,6)*/
      )
    )
    
    // Adjust the number of hydrogens
    for( atomComposition <- computedCompositionByFormula.values ) {
      
      val nbH = -1 * (atomComposition.getMonoMass / H_ATOM.monoMass).toInt
      
      // Add hydrogens to the composition
      atomComposition.abundanceMap += H_ATOM -> nbH
      
      // Minimize the mass deviation
      if( atomComposition.getMonoMass > (0.5 * H_ATOM.monoMass) ) {
        atomComposition.abundanceMap(H_ATOM) -= 1
      } else if( atomComposition.getMonoMass < (- 0.5 * H_ATOM.monoMass) ) {
        atomComposition.abundanceMap(H_ATOM) += 1
      }

    }
    //println("compositionAdjustments count="+ computedCompositionByFormula.size)
    
    computedCompositionByFormula.values.map( c => (c,c.getMonoMass) ).toArray
  }
  
  val averageAAMass = aaTable.averageAAMass
  val averageAtomComposition = aaTable.getAverageAtomComposition(atomTable)
  
  def computeAveragine(compoundMass: Double, adjustAbundances: Boolean = true, adjMassTol: Float = 0.01f ): (AtomComposition,Int) = {
    
    val avgAtomComp = averageAtomComposition.clone()    
    avgAtomComp.fitToMonoMass(compoundMass)
    
    var matchingAdjustmentsCount = 0
    
    if( adjustAbundances ) {
      avgAtomComp.roundAbundances()
      //println(avgAtomComp.toFormula())
      
      val deltaMass = avgAtomComp.getMonoMass - compoundMass
      
      // Check if need to perform the adjustment
      // TODO: keep this ??? it may provided unexpected result
      if( math.abs(deltaMass) > adjMassTol ) {
        
        val avgAtomCount = this.calcAverageAACount(compoundMass)
        val expectedAbRangeByAtom = abundanceRangeByAtom.map { case(atom,abRange) =>
          val newAbRange = abRange.copy( _1 = abRange._1 * avgAtomCount, _2 = abRange._2 * avgAtomCount)
          //println(atom.symbol + " "+ newAbRange)
          (atom,newAbRange)
        }
        
        val matchingAdjustments = compositionAdjustments
          .filter { case (adj,monoMass) =>
            if( math.abs(deltaMass + monoMass) > adjMassTol ) false
            else {
              val tmpComp = (avgAtomComp.clone() += adj)
              val tmpAbundanceMap = tmpComp.abundanceMap
              
              var hasExpectedAbundances = true
              for( (atom,ab) <- tmpAbundanceMap; if hasExpectedAbundances ) {
                val expectedAbRange = expectedAbRangeByAtom(atom)
                if( ab < expectedAbRange._1 || ab > expectedAbRange._2 ) hasExpectedAbundances = false
              }
              
              if( hasExpectedAbundances ) {
                val carbonAb = tmpAbundanceMap.getOrElse(C_ATOM, 0f)
                val hydrogenAb = tmpAbundanceMap.getOrElse(H_ATOM, 0f)
                val oxygenAb = tmpAbundanceMap.getOrElse(O_ATOM, 0f)
                val nitrogenAb = tmpAbundanceMap.getOrElse(O_ATOM, 0f)
                
                val hcRatio = hydrogenAb/carbonAb
                val noRatio = hydrogenAb/carbonAb
                val cnRatio = hydrogenAb/carbonAb
                
                if( hcRatio < H_C_RATIO_RANGE._1 || hcRatio > H_C_RATIO_RANGE._2 ) hasExpectedAbundances = false
                else if ( noRatio < N_O_RATIO_RANGE._1 || noRatio > N_O_RATIO_RANGE._2 ) hasExpectedAbundances = false
                else if ( cnRatio < C_N_RATIO_RANGE._1 || cnRatio > C_N_RATIO_RANGE._2 ) hasExpectedAbundances = false
                
                if( hasExpectedAbundances ) {
                  val sulfurAb = tmpAbundanceMap.getOrElse(S_ATOM, 0f)
                  if( sulfurAb > 0 ) {
                    val minCarbonAb = MIN_C_S_RATIO * sulfurAb
                    if( carbonAb < minCarbonAb ) hasExpectedAbundances = false
                    val minHydrogenAb = MIN_H_S_RATIO * sulfurAb
                    if( hydrogenAb < minHydrogenAb ) hasExpectedAbundances = false
                  }
                }
              }
              
              hasExpectedAbundances
            }
          }
          .sortBy { case (adj,monoMass) => math.abs(deltaMass + monoMass) }
          
        matchingAdjustmentsCount = matchingAdjustments.length
        
        if( matchingAdjustments.isEmpty == false )
          avgAtomComp += matchingAdjustments(0)._1
      }
      
    }
    
    (avgAtomComp,matchingAdjustmentsCount)
  }
  
  private def _computeCompositionAdjustments(
    computedCompositionByFormula: HashMap[String,AtomComposition],
    lastCombination: AtomComposition,
    atoms: Array[Atom],
    adjCountRangeByAtom: Map[Atom,Range]
  ) {
    if( atoms.isEmpty ) return
    
    // Retrieve previous abundance map
    val lastAbundanceMap = lastCombination.abundanceMap
    val curAtom = atoms.head
    val remainingAtoms = atoms.tail
    val adjCountRange = adjCountRangeByAtom(curAtom)
    
    for( atomCount <- adjCountRange ) {
      
      // Clone the abundance map and initialize the current atom counter if needed
      val newAbundanceMap = lastAbundanceMap.clone()
      
      // Increment the abundance of the current atom
      newAbundanceMap += curAtom -> atomCount
      
      val newComposition = new AtomComposition(newAbundanceMap)
      val formula = newComposition.toFormula()
      
      if( computedCompositionByFormula.contains(formula) == false ) {

        this.synchronized {
          computedCompositionByFormula += formula -> newComposition
        }
        
      }
        
      this._computeCompositionAdjustments(
        computedCompositionByFormula,
        newComposition,
        remainingAtoms,
        adjCountRangeByAtom
      )
    }
    
  }
  
  def calcAverageAACount( mass: Double ): Int = {
    math.round( (mass / averageAAMass).toFloat )
  }
  
}