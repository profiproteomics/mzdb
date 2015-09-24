package fr.profi.ms.algo

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.util.control.Breaks._
import com.typesafe.scalalogging.LazyLogging
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.util.math.calcLineParams
import fr.profi.util.ms.mozToMass
import fr.profi.chemistry.model.MolecularConstants

/**
 * Isotopic Pattern (mz, intensities) estimator based on a probabilistic distribution of atom isotopes 
 * and average atomic composition of a peptide at a specified mz.
 * 
 * @author CB205360
 *
 */
object IsotopePatternEstimator extends LazyLogging {

  final val avgIsoMassDiff = MolecularConstants.AVERAGE_PEPTIDE_ISOTOPE_MASS_DIFF

  final val coeffs = Array(
    Array(1.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00),
    Array(0.00000E+00, 5.55674E-04, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00),
    Array(0.00000E+00, 4.94405E-05, 1.54387E-07, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00),
    Array(0.00000E+00, 0.00000E+00, 2.74728E-08, 2.85962E-11, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00),
    Array(0.00000E+00, 0.00000E+00, 1.22218E-09, 7.63295E-12, 3.97254E-15, 0.00000E+00, 0.00000E+00, 0.00000E+00),
    Array(0.00000E+00, 0.00000E+00, 0.00000E+00, 6.79135E-13, 1.41381E-15, 4.41487E-19, 0.00000E+00, 0.00000E+00),
    Array(0.00000E+00, 0.00000E+00, 0.00000E+00, 2.01418E-14, 1.88689E-16, 1.96404E-19, 4.08871E-23, 0.00000E+00),
    Array(0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 1.11923E-17, 3.49498E-20, 2.18273E-23, 3.24570E-27))

  // A mass (not m/z) must be provided
  def getTheoreticalPattern(mz: Double, charge: Int): TheoreticalIsotopePattern = {
    require(charge > 0, "charge must be greater than zero")

    // Convert m/z into mass
    var mass = mozToMass(mz, charge)
    var m = new Array[Double](coeffs(0).length)
    m(0) = 1.0
    m(1) = mass
    
    for(i <- 2 until m.length) {
      m(i) = m(i-1)*mass
    }
    
    val mzIntPairs = new ArrayBuffer[(Double, Float)](coeffs.length)
    val r = mult(coeffs, m)
    val max = r.reduceLeft(_ max _)
    for (i <- 0 until r.length) {
    	val isoMz = mz + (i * avgIsoMassDiff / charge)
        mzIntPairs += (isoMz -> (r(i)*100.0/max).toFloat )      
    }

    TheoreticalIsotopePattern(mzIntPairs.toArray, charge)
  }

  def mult(a: Array[Array[Double]], b: Array[Double]) : Array[Double]= {
    for (row <- a)
      yield row zip b map Function.tupled(_ * _) sum
    
  }
}