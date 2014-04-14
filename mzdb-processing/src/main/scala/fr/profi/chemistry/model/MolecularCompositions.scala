package fr.profi.chemistry.model

import scala.collection.mutable.HashMap
import implicits._
import MolecularConstants._
import scala.collection.MapLike

// TODO: rename this file to MolecularCompositions

object AbundanceMapOps {
  
  def sumAbundances[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float] ): Float = {
    abundanceMap.foldLeft(0f)( (sum,ab) => sum + ab._2 )
  }
  
  def sumAbsoluteAbundances[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float] ): Float = {    
    abundanceMap.foldLeft(0f)( (sum,ab) => sum + math.abs(ab._2) )
  }
  
  protected def calcMass[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float], massExtractor: IMolecularEntity => Double ): Double = {
    
    var mass = 0.0    
    for( (entity,entityAb) <- abundanceMap ) {
      val entityMass = massExtractor(entity)
      mass += entityAb * entityMass
    }
    
    mass
  }

  def calcMonoMass[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float] ): Double = {
    this.calcMass( abundanceMap, entity => entity.monoMass )
  }
  
  def calcAverageMass[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float] ): Double = {
    this.calcMass( abundanceMap, entity => entity.averageMass )
  }

  def multiplyBy[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float], number: Float ): Unit = synchronized {
    
    for( (key,value) <- abundanceMap )
      abundanceMap(key) *= number

    abundanceMap
  }
  
  def divideBy[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float], number: Float ): Unit = synchronized {    
    for( (key,value) <- abundanceMap )
      abundanceMap(key) /= number
  }

  def addAbundanceMap[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float], otherAbundanceMap: scala.collection.Map[M,Float] ): Unit = synchronized {
    for( (key,value) <- otherAbundanceMap ) {
      abundanceMap.getOrElseUpdate(key, 0f)
      abundanceMap(key) += value
    }
  }
  
  /*def averageWithAbundanceMap[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float], otherAbundanceMap: HashMap[M,Float] ) {
    addAbundanceMap(abundanceMap, otherAbundanceMap)
    divideBy(abundanceMap, 2)
  }

  def normalize[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float] ) {
    val sumOfAbundances = sumAbundances(abundanceMap)
    divideBy(abundanceMap, sumOfAbundances)    
  }*/
  
  def stringifyToFormula[M <: IMolecularEntity]( abundanceMap: HashMap[M,Float] ): String = {

    val sortedAtoms = abundanceMap.keys.toList.sortBy(_.symbol)
     
    val strings = for(
      entity <- sortedAtoms;
      abundance <- abundanceMap.get(entity).map(_.toInt);
      if abundance != 0
    ) yield {
      val sb = new StringBuilder()
      
      if( abundance != 0 ) {
        sb.append(entity.symbol)
        
        if( abundance != 1 ) {
          sb.append("(")
            .append(abundance)
            .append(")")
        }
      }

      sb.toString
    }
    
    strings.mkString(" ")
  }
  
}

/**
 * @author David Bouyssie
 *
 */
abstract class AbstractMolecularEntityComposition[M <: IMolecularEntity] {
 
  // K=symbol ; V=abundance
  def abundanceMap: HashMap[M,Float]
  
  def getSumOfAbundances(): Float = AbundanceMapOps.sumAbundances(abundanceMap)   

  def getSumOfAbsoluteAbundances(): Float = AbundanceMapOps.sumAbsoluteAbundances(abundanceMap)
  
  def getMonoMass(): Double = AbundanceMapOps.calcMonoMass(abundanceMap)
  
  def getAverageMass(): Double = AbundanceMapOps.calcAverageMass(abundanceMap)

  def *=( number: Float ) = {
    AbundanceMapOps.multiplyBy(abundanceMap, number)
    this
  }
  
  def /=( number: Float ) = {
    AbundanceMapOps.divideBy(abundanceMap, number)
    this
  }

  def +=( otherComposition: AbstractMolecularEntityComposition[M] ) = synchronized {    
    AbundanceMapOps.addAbundanceMap(abundanceMap, otherComposition.abundanceMap)    
    this
  }
  
  def average( otherComposition: AbstractMolecularEntityComposition[M] ) = {
    (this += otherComposition) /= 2
  }
  
  def fitToMonoMass( monoMass: Double): Double = {
    
    this *= ( monoMass / this.getMonoMass() ).toFloat
    
    this.getMonoMass() - monoMass
  }

  def normalize() = { this /= this.getSumOfAbundances }
  
  def toFormula(): String = {

    val sortedAtoms = this.abundanceMap.keys.toList.sortBy(_.symbol)
     
    val strings = for(
      entity <- sortedAtoms;
      abundance <- this.abundanceMap.get(entity).map(_.toInt);
      if abundance != 0
    ) yield {
      val sb = new StringBuilder()
      
      if( abundance != 0 ) {
        sb.append(entity.symbol)
        
        if( abundance != 1 ) {
          sb.append("(")
            .append(abundance)
            .append(")")
        }
      }

      sb.toString
    }
    
    strings.mkString(" ")
  }

}

class AminoAcidComposition(
  val abundanceMap: HashMap[AminoAcidResidue,Float]
) extends AbstractMolecularEntityComposition[AminoAcidResidue] {
  require(abundanceMap != null, "abundanceMap is null" )
  
  // Define a secondary constructor using an amino acid sequence as input
  def this( sequence: String, aaTable: AminoAcidTableLike ) = {
    this( {
      val tmpAbundanceMap = new HashMap[AminoAcidResidue,Float]()
    
      val seqWithoutSpace = sequence.replaceAll("\\s+", "")
    
      // Update abundanceMap
      val aaChars = sequence.toCharArray()
      for( aaChar <- aaChars  ) {
        
        val aaSymbol = aaChar.toString()
        val aaOpt = aaTable.getAminoAcidOpt(aaSymbol)
        require( aaOpt.isDefined, s"amino acid ${aaSymbol} is missing in provided aaTable" )
        
        val aa = aaOpt.get
        val aaCount = tmpAbundanceMap.getOrElseUpdate(aa, 0)
        tmpAbundanceMap(aa) = aaCount + 1
      }

      tmpAbundanceMap
      
    })
  }
  
  override def clone() = new AminoAcidComposition( abundanceMap.clone() )
  
  override def getMonoMass(): Double = {
    super.getMonoMass() + WATER_MONO_MASS
  }
  
  override def getAverageMass(): Double = {
    super.getAverageMass() + WATER_AVERAGE_MASS
  }
  
  def getAtomBasedMonoMass( atomTable: AtomTableLike ): Double = {
    this.getAtomBasedMass( atomTable, atomComp => atomComp.getMonoMass() )
  }
  
  def getAtomBasedAverageMass( atomTable: AtomTableLike ): Double = {
    this.getAtomBasedMass( atomTable, atomComp => atomComp.getAverageMass() )
  }
  
  def getAtomBasedMass( atomTable: AtomTableLike, massExtractor: AtomComposition => Double ): Double = {    
    val molAtomComposition = this.getAtomComposition( atomTable )
    massExtractor(molAtomComposition)
  }
  

  def getAtomComposition( atomTable: AtomTableLike ): AtomComposition = {
    require( atomTable != null, "atomTable is null" )
    
    // Initiate abundanceMap with water atoms
    val atomAbundanceMap = new HashMap[Atom,Float]
    atomAbundanceMap += atomTable.getAtom("H") -> 2f
    atomAbundanceMap += atomTable.getAtom("O") -> 1f
    
    val molAtomComposition = new AtomComposition( atomAbundanceMap ) //, atomTable)
    
    // Add aa sequence atoms
    for( (aminoAcid,aaAbundance) <- abundanceMap ) {
    
      val aaAtomComposition = new AtomComposition( aminoAcid.formula, atomTable )
      aaAtomComposition *= aaAbundance
      molAtomComposition += aaAtomComposition
    }
    
    molAtomComposition
  }

}

class AtomComposition( // or ElementalComposition
  val abundanceMap: HashMap[Atom,Float]
) extends AbstractMolecularEntityComposition[Atom] {
  require(abundanceMap != null, "abundanceMap is null" )
  
  // Define a secondary constructor using a formula as input
  def this( formula: String, atomTable: AtomTableLike ) = {
    this( {
      val tmpAbundanceMap = new HashMap[Atom,Float]()
    
      val formulaElements = formula.split(" ")    
  
      for( formulaElement <- formulaElements ) {
        
        val (atomSymbol, abundance) = if( formulaElement.contains("(") == false ) formulaElement -> 1
        else {          
          val elemAbParts: Array[String] = formulaElement.split("""\(""")
          val elemSymbol = elemAbParts.head
          val elemQuant = elemAbParts.last.replace(")", "").toInt
          
          (elemSymbol,elemQuant)
        }
        
        val atomOpt = atomTable.getAtomOpt(atomSymbol)
        require( atomOpt.isDefined, s"atom symbol ${atomSymbol} is missing in provided atomTable" )
        
        val atom = atomOpt.get
        tmpAbundanceMap += atom -> abundance        
      }

      tmpAbundanceMap
      
    }) //, atomTable )
  }
  
  override def clone() = new AtomComposition( abundanceMap.clone() )

  def roundAbundances() = synchronized {

    // TODO : implements constraints (atom default abundances)
    //val newAbundanceMap = new HashMap[String,Int]()
    /*for( (atomSymbol,abundance) <- this.abundanceMap; if abundance < 1 ) {
      newAbundanceMap(atomSymbol) = 0f
    }*/
    
    val massBeforeRounding = this.getMonoMass()
    
    // Round abundances
    val deltaAbMap = new HashMap[Atom,Float]()
    for( (atom,abundance) <- this.abundanceMap ) {
      val roundedVal = if( abundance < 1 ) 0 else math.round(abundance)
      deltaAbMap += atom -> math.abs(abundance - roundedVal)
      this.abundanceMap( atom ) = roundedVal
    }
    
    this._adjustNumOfAtoms(massBeforeRounding,this.getMonoMass(),deltaAbMap)
  }
  
  private def _adjustNumOfAtoms(refMass: Double, curMass: Double, deltaAbMap: HashMap[Atom,Float] ): Unit = synchronized {
    
    // Find the best atom to optimize
    // The bestAtom is the one with the biggest abundance delta and with a given mass that doesn't exceed the current delta mass
    var bestAtom: Atom = null
    var maxDeltaAb = 0f
    val deltaMass = refMass - curMass
    val absDeltaMass = math.abs(deltaMass)
    
    // TODO: review this algorithm
    for( (atom,deltaAb) <- deltaAbMap ) {
      if( deltaAb > maxDeltaAb ) {
        
        val atomMass = atom.monoMass
        
        var correctedDeltaMass = 0.0
        // Substract atom mass if delta mass is positive
        if( deltaMass > 0 ) { correctedDeltaMass = deltaMass - atomMass }
        // Add atom mass if delta mass is negative
        else if( deltaMass < 0 ) { correctedDeltaMass = deltaMass + atomMass }
        
        if( math.abs(correctedDeltaMass) < absDeltaMass ) {
          bestAtom = atom
          maxDeltaAb = math.abs(deltaAb)
        }
      }
    }
    
    if( bestAtom != null ) {
      
      val abundance = this.abundanceMap(bestAtom)
      
      if( deltaMass > 0 ) {
        this.abundanceMap(bestAtom) = abundance + 1
      } else if( deltaMass < 0 ) {
        this.abundanceMap(bestAtom) = abundance - 1
      }
      
      val newCurMass = this.getMonoMass()
      
      if( math.abs(refMass - newCurMass) > 0.504 ) // 1/2 mass H
        this._adjustNumOfAtoms( refMass, newCurMass, deltaAbMap )
    }
    
    ()
  }

}

class AtomIsotopeComposition(
  val abundanceMap: HashMap[AtomIsotopicVariant,Float]
) extends AbstractMolecularEntityComposition[AtomIsotopicVariant] {
  
  override def clone() = new AtomIsotopeComposition( abundanceMap.clone() )
  
}

