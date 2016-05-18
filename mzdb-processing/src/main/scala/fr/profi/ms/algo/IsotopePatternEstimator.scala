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
    Array(0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 1.11923E-17, 3.49498E-20, 2.18273E-23, 3.24570E-27)
  )
  final private val coeffsMatrixLen = coeffs.length
  final private val coeffsRowLen = coeffs(0).length

  // A mass (not m/z) must be provided
  def getTheoreticalPattern(mz: Double, charge: Int): TheoreticalIsotopePattern = {
    require(charge > 0, "charge must be greater than zero")

    // Convert m/z into mass
    val mass = mozToMass(mz, charge)
    val m = new Array[Double](coeffsRowLen)
    m(0) = 1.0
    m(1) = mass
    
    var prevMass = mass
    var i = 2
    while (i < coeffsRowLen) {
      val curMass = prevMass * mass
      m(i) = curMass
      prevMass = curMass
      
      i += 1
    }
    
    val (r, max) = mult(coeffs, m)
    val relativeFactor = 100 / max
    
    val mzIntPairs = new Array[(Double, Float)](coeffsMatrixLen)
    i = 0
    while (i < coeffsMatrixLen) {
      val isoMz = mz + (i * avgIsoMassDiff / charge)
      val isoRelInt = r(i) * relativeFactor
      mzIntPairs(i) = Tuple2(isoMz, isoRelInt.toFloat )
      i += 1
    }

    TheoreticalIsotopePattern(mzIntPairs, charge)
  }

  /*def mult(m: Array[Array[Double]], b: Array[Double]) : Array[Double]= {
    for (row <- a)
      yield row zip b map Function.tupled(_ * _) sum
    
  }*/

  /**
   * @author David Bouyssie
   * @return the array of obtained values after dot product, and the maximum value in the array
   */
  private def mult(aMatrix: Array[Array[Double]], b: Array[Double]): (Array[Double],Double) = {
    
    val len = aMatrix.length
    val result = new Array[Double](len)
    
    var max = 0.0
    var i = 0
    while (i < len) {
      val value = dotProduct(aMatrix(i), b)
      result(i) = value
      if (value > max) max = value
      i += 1
    }
    
    (result,max)
  }
  
  private def dotProduct(a: Array[Double], b: Array[Double]): Double = {
    var value = 0.0
    var sum = 0.0

    val len = a.length
    var i = 0
    while (i < len) {
      value = a(i) * b(i)
      sum = sum + value
      i += 1
    }

    sum
  }
}