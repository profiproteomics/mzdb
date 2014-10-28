package fr.profi.chemistry.model

object Polarity extends Enumeration {
  val POSITIVE = Value("+")
  val NEGATIVE = Value("-")
}

object MolecularConstants {  
  final val ELECTRON_MASS = 0.00054857990946 // Source: NIST 2010 CODATA
  final val PROTON_MASS = 1.007276466812 // Source: NIST 2010 CODATA   
  final val WATER_MONO_MASS = 18.010565
  final val WATER_AVERAGE_MASS = 18.01525697318
  final val AVERAGE_AA_MASS = 111.1254f // TODO: marco => why difference with 111.10523866044295 by computation
  // TODO: refine this value and put a source reference here (publication ?)
  final val AVERAGE_PEPTIDE_ISOTOPE_MASS_DIFF = 1.0027
}

trait Ionizable extends IMolecularEntity {
  
  var charge: Int = 0
  var polarity = Polarity.POSITIVE
  
  def ionize( charge: Int, polarity: Polarity.Value = Polarity.POSITIVE ) = {
    require( charge > 0, "charge must be a strictly positive integer")
    
    this.charge = charge
    this.polarity = polarity

    this
  }
  
  def getIonMonoMass(): Double = {
    if( charge == 0 ) this.monoMass
    else _neutralMassToIonMass(this.monoMass)
  }

  def getIonAverageMass(): Double = {
    if( charge == 0 ) this.averageMass
    else _neutralMassToIonMass(this.averageMass)
  }

  private def _neutralMassToIonMass( neutralMass: Double ): Double = {  
  
    val deltaMass = charge * MolecularConstants.PROTON_MASS;
    
    polarity match {
      case Polarity.POSITIVE => neutralMass + deltaMass
      case Polarity.NEGATIVE => neutralMass - deltaMass
    }
  }
  
  def getMoz(): Option[Double] = if( charge > 0 ) Some(getIonMonoMass / charge) else None
  
}