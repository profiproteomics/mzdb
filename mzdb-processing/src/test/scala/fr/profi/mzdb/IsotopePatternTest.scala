package fr.profi.mzdb

import org.junit.Before
import org.junit.FixMethodOrder
import org.junit.Test
import org.junit.runners.MethodSorters

import com.typesafe.scalalogging.StrictLogging

import fr.profi.chemistry.model.BiomoleculeAtomTable
import fr.profi.chemistry.model.HumanAminoAcidTable
import fr.profi.ms.algo._

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class IsotopePatternTest extends StrictLogging  {

  @Before
  @throws(classOf[Exception])
  def setUp() = {
    
  }
  
  @Test
  def a_patternComputer() = {
    
    val aaTable = HumanAminoAcidTable
    val atomTable = BiomoleculeAtomTable
    
    // Compute the averagine atom composition
    val averagineComputer = new fr.profi.chemistry.algo.AveragineComputer(aaTable, atomTable)
    val averagine = averagineComputer.computeAveragine(2600.0, adjustAbundances = true)._1
    
    // Create a map defining the maximum number of atoms
    val maxAtomCountByAtom = Map(
      atomTable.getAtom("C") -> 1000,
      atomTable.getAtom("H") -> 2000,
      atomTable.getAtom("O") -> 1000,
      atomTable.getAtom("N") -> 1000,
      atomTable.getAtom("P") -> 10,
      atomTable.getAtom("S") -> 3,
      atomTable.getAtom("Se") -> 1
    )
    
    // Compute the isotopic variant combinations required for isotope distribution computation
    val combinations = IsotopeDistributionComputer.computeIsotopicVariantCombinations(maxAtomCountByAtom, 0.00001f)
    val computer = IsotopeDistributionComputer
    
    val t0 = System.currentTimeMillis()
    var i = 0
    while( i < 100 ) {
      
      // Compute the isotope distribution
      val theoDistrib = computer.computeIsotopeDistribution(averagine, 1, combinations, 0.01f )
      val theoPattern = theoDistrib.theoIsotopePattern
      
      if( i == 0 ) {
        theoPattern.mzAbundancePairs.foreach { p =>
          logger.info(p._1+" , "+p._2)
        }
      }
      
      i += 1
    }
    
    logger.info( "Isotope pattern computation took: " + (System.currentTimeMillis() - t0) / 1000f )  
  }
  
  @Test
  def b_patternInterpolator() = {
    
    val interpolator = IsotopePatternInterpolator
    
    val t0 = System.currentTimeMillis()    
    var i = 0
    while( i < 1000 ) {
      
      val theoPattern = interpolator.getTheoreticalPattern(2600.0, 1)
      
      if( i == 0 ) {
        theoPattern.mzAbundancePairs.foreach { p =>
          logger.info(p._1+" , "+p._2)
        }
      }
      
      i += 1
    }
    
    logger.info( "Isotope pattern interpolation took: " + (System.currentTimeMillis() - t0) / 1000f )  
  }

  @Test
  def c_patternEstimator() = {
    
    val estimator = IsotopePatternEstimator
    
    val t0 = System.currentTimeMillis()    
    var i = 0
    while( i < 1000 ) {
      
      val theoPattern = estimator.getTheoreticalPattern(2600.0, 1)
      
      if( i == 0 ) {
        theoPattern.mzAbundancePairs.foreach { p =>
          logger.info(p._1+" , "+p._2)
        }
      }
      
      i += 1
    }
    
    logger.info( "Isotope pattern estimation took: " + (System.currentTimeMillis() - t0) / 1000f )  
  }

}