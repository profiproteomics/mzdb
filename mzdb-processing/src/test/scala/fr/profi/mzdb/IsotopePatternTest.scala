package fr.profi.mzdb

import com.typesafe.scalalogging.slf4j.Logging
import org.junit.AfterClass
import org.junit.Assert
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Before
import fr.profi.ms.algo.IsotopePatternInterpolator
import fr.profi.ms.algo.IsotopePatternEstimator
import org.junit.FixMethodOrder
import org.junit.runners.MethodSorters
import fr.profi.chemistry.model.HumanAminoAcidTable
import fr.profi.chemistry.model.BiomoleculeAtomTable
import fr.profi.ms.algo.IsotopeDistributionComputer

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class IsotopePatternTest extends Logging  {

  @Before
  @throws(classOf[Exception])
  def setUp() = {
    
  }
  
  @Test
  def a_patternInterpolator() = {
    val theoPattern = IsotopePatternInterpolator.getTheoreticalPattern(2600.0, 1)
    theoPattern.mzAbundancePairs.foreach { p =>
    	logger.info(p._1+" , "+p._2)
    }
  }

  @Test
  def b_patternEstimator() = {
    val theoPattern = IsotopePatternEstimator.getTheoreticalPattern(2600.0, 1)
    theoPattern.mzAbundancePairs.foreach { p =>
    	logger.info(p._1+" , "+p._2)
    }
  }

// TODO : a quoi correspond le parametre isotopeCombinationMap de IsotopeDistributionComputer.computeIsotopeDistribution ??
//   @Test
//  def c_patternComputer() = {
//     val computer = new fr.profi.chemistry.algo.AveragineComputer(HumanAminoAcidTable, BiomoleculeAtomTable)
//	 val composition = computer.computeAveragine(2600.0, adjustAbundances = false)._1
//    val theoPattern = IsotopeDistributionComputer.computeIsotopeDistribution(composition, 1, )
//    theoPattern.mzAbundancePairs.foreach { p =>
//    	logger.info(p._1+" , "+p._2)
//    }
//  }

}