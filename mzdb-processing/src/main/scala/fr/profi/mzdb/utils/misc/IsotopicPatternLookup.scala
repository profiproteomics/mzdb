package fr.profi.mzdb.utils.misc

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.util.control.Breaks._
import com.weiglewilczek.slf4s.Logging

import fr.profi.mzdb.model.TheoreticalIsotopePattern
import fr.proline.util.math.calcLineParams
import fr.proline.util.ms.mozToMass

/**
 * @author Marco
 *
 */
object IsotopicPatternLookup extends Logging {
  
  final val LOOKUP_TABLE_URL = "/lookup_table.txt"
    
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
    
    // Convert m/z into mass
    val mass = mozToMass(mz,charge)
    
    val keys = lookupTable.keys.toBuffer
    require(mass >= keys.first && mass <= keys.last, "provided m/z is out of lookup table bounds: " + mass)
      
    val idx = keys.indexWhere(_ >= mass)
    val (x1, x2) = (keys(idx - 1), keys(idx))
    val (minArray, maxArray) = (lookupTable(x1), lookupTable(x2) )
    
    val pat = new ArrayBuffer[Float](minArray.length)
    
    breakable {
      for( i <- 0 until minArray.length) {
        val (y1,y2) = (minArray(i),maxArray(i))
        if (y1 > 0 || y2 > 0) {
          val (slope, intercept) = calcLineParams(x1, y1, x2, y2)
          pat += ( (slope * mass) + intercept ).toFloat
        } else break
      }
    }
    
    new TheoreticalIsotopePattern(mz,charge,pat.toArray)
  }
  
}