package fr.profi.chemistry.model

import fr.profi.util.StringUtils

/**
 * @author David Bouyssie
 * 
 * According to the IUPAC Gold Book[1] a molecular entity is "any constitutionally or isotopically distinct
 * atom, molecule, ion, ion pair, radical, radical ion, complex, conformer, etc.,
 * identifiable as a separately distinguishable entity".
 */
trait IMolecularEntity {
  
  def symbol: String // MUST be unique for a collection of molecular entities
  def name: String
  def monoMass: Double // The monoisotopic mass
  def averageMass: Double
  
  require( StringUtils.isNotEmpty(symbol), "symbol is empty" )
  require( name != null, "name is null" )
  require( monoMass > 0, "monoMass must be a strictly positive number" )
  require( averageMass > 0, "monoMass must be a strictly positive number" )
  
}

trait IMolecule extends IMolecularEntity {
  
  val formula: String
  
  // TODO: require it is matching the regex /^(\w{1}(\(-*\d+\))*\s*)+$/

  def getAtomComposition( atomTable: AtomTableLike ): AtomComposition = {    
    new AtomComposition( this.formula, atomTable )
  }

}

trait IPolymer extends IMolecularEntity {
  def sequence: String
}

case class AminoAcidResidue(
  code1: Char,
  code3: String,
  name: String,
  formula: String,
  monoMass: Double,
  averageMass: Double,
  occurrence: Float, // occurrence in human proteins
  pKa1: Float = 0, // C-term pKa
  pKa2: Float = 0, // N-term pKa
  pKa3: Float = 0, // side chain pKa
  pI: Float = 0,
  codons: Array[String] = Array()
) extends IMolecule {
  require( code3.length == 3, "code3 must contain three characters" )
  
  for( codon <- codons ) {
    require( codon.length == 3, "a codon must contain three characters" )
    require( codon matches """[ACUG]+""", "a codon must only contain ACUG letters" )
  } 
  
  def symbol: String = code1.toString
  
}

case class AminoAcidMod(
  name: String,
  formula: String,
  monoMass: Double,
  averageMass: Double,
  positionConstraint: String, // any N-term, any C-term, protein N-term, protein C-term
  residueConstraint: String
) extends IMolecule {
  // TODO: require positionConstraint type
  
  def symbol: String = name
  
}

// The protonNumber uniquely identify an element
case class Atom( symbol: String, atomicNumber: Short, isotopes: Array[Isotope], name: String = "" ) extends IMolecularEntity {  
  require( isotopes != null, "isotopes is null" )
  require( isotopes.isEmpty == false, "isotopes is empty" )
  
  def protonNumber = atomicNumber
  
  def monoMass: Double = isotopes.head.mass
  
  lazy val averageMass: Double = {
    
    var weightedMassSum = 0.0
    var weightSum = 0.0
    for( iso <- isotopes ) {
      weightedMassSum += (iso.mass * iso.abundance)
      weightSum += iso.abundance
    }

    weightedMassSum / weightSum
  }
  
  lazy val isotopicVariants = {
    for( isotope <- isotopes ) yield AtomIsotopicVariant(this,isotope)
  }
  
  def getNeutronNumber( isotopeIdx: Int ) = {
    isotopes(isotopeIdx).getNeutronNumber( protonNumber )
  }
  
}

case class AtomIsotopicVariant( atom: Atom, isotope: Isotope ) extends IMolecularEntity {
  def symbol = isotope.massNumber + atom.symbol
  def name = symbol
  def monoMass = isotope.mass
  def averageMass = isotope.mass
  def mass = isotope.mass
}

case class Isotope( massNumber: Short, mass: Double, abundance: Float ) {
  require( massNumber > 0, "massNumber must be a strictly positive number")
  require( mass > 0, "mass must be a strictly positive number")
  require( abundance >= 0, "abundance must be a positive number")

  def nucleonNumber = massNumber
  
  def getNeutronNumber( protonNumber: Int ) = {
    massNumber - protonNumber
  }
  
}

case class Peptide(
  sequence: String,
  monoMass: Double,
  averageMass: Double,
  pK: Float = 0, //has 'pK' => ( is => 'rw', isa => Dict[ 'N-term' => Num, 'C-term' => Num, internal => Num ] );
  pI: Float = 0,
  mods: Array[AminoAcidMod] = Array()
) extends IMolecularEntity with IPolymer {
  
  def name = sequence
  def symbol = sequence

}
