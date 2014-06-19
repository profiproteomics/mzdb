package fr.profi.ms.algo

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.util.control.Breaks._
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.util.math.calcLineParams
import fr.profi.util.ms.mozToMass

/**
 * @author Marco
 *
 */
object IsotopePatternInterpolator extends Logging {
  
  final val LOOKUP_TABLE_URL = "/lookup_table.txt"
  final val avgIsoMassDiff = 1.0027
    
  lazy val lookupTable: TreeMap[Int, Array[Int]] = {
    
    // Parse the file into an array buffer
    val rows = new ArrayBuffer[Array[Int]]
    Source.fromURL(getClass().getResource(LOOKUP_TABLE_URL)).getLines.foreach { line => 
      rows += line.split("\t").map(_.toInt)
    }
    
    // Calculate the number of isotopes
    val maxIsotopesCount = rows.last.length - 1
    
    // Create the lookup table
    val tableBuilder = TreeMap.newBuilder[Int, Array[Int]]
    rows.foreach { row =>
      val pattern = Array.fill(maxIsotopesCount)(0)
      Array.copy(row,1,pattern,0,row.size - 1)
      tableBuilder += (row.head -> pattern)
    }
    
    tableBuilder.result
  }
  
  // A mass (not m/z) must be provided
  def getTheoreticalPattern(mz: Double, charge: Int): TheoreticalIsotopePattern = {
    val keys = lookupTable.keys.toBuffer
    //require(mass >= keys.head && mass <= keys.last, "provided m/z is out of lookup table bounds: " + mass)
    
    // Convert m/z into mass    
    var mass = mozToMass(mz,charge)
    if (mass > keys.last ) 
      mass= keys.last
     
    val idx = keys.indexWhere(_ >= mass)
    val (x1, x2) = (keys(idx - 1), keys(idx))
    val (minArray, maxArray) = (lookupTable(x1), lookupTable(x2) )
    
    val mzIntPairs = new ArrayBuffer[(Double,Float)](minArray.length)
    
    breakable {
      for( i <- 0 until minArray.length) {
        val (y1,y2) = (minArray(i),maxArray(i))
        if (y1 > 0 || y2 > 0) {
          val (slope, intercept) = calcLineParams(x1, y1, x2, y2)
          val abundance = ( (slope * mass) + intercept ).toFloat
          val isoMz = mz + (i * avgIsoMassDiff / charge)
          mzIntPairs += (isoMz -> abundance)
        } else 
          break
      }
    }
    
    TheoreticalIsotopePattern(mzIntPairs.toArray,charge)
  }
  
  def calcAbundancesRmsd(theoInt: Array[Float], obsInt: Array[Float]): Double = {
    val maxObsInt = obsInt.max
    val scaledInt = obsInt.map(_ * 100 / maxObsInt)
    val s = theoInt.zip(scaledInt).foldLeft(0d)( (s, ab) => s + math.pow((ab._1 - ab._2), 2) )
    math.sqrt(s)
  }
  
}