/**
 *
 */
package fr.profi.mzdb.utils.misc

import io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap

/**
 * @author Marco
 *
 */
object IsotopicPatternLookup {
  val url = "/lookup_table.txt"
  lazy val lookupTable = loadData()
  
  def loadData() : TreeMap[Double, ArrayBuffer[Double]] = {
    var table = new TreeMap[Double, ArrayBuffer[Double]]
    val source = Source.fromURL(getClass().getResource(url))
    source.getLines.foreach {line => 
      println("hola")
      val splittedLine = new ArrayBuffer[Double]() ++line.split("\t").map(_.toDouble)
      table += (splittedLine(0) -> splittedLine.slice(1, splittedLine.length))         
    }
    table
  }
  
  def getIsotopicPatternForMz(mz:Double) : Array[Double] = {
    val keys = lookupTable.keys.toBuffer
    if (mz < keys.first || mz > keys.last) {
      println("[getIsotopicPatternForMz] : mz requested out of lookup table bound:" + mz)
      return Array[Double]()
    }
      
    val idx = keys.indexWhere(_ >= mz)
    val (x1, x2) = (keys(idx - 1), keys(idx))
    val (minArray, maxArray) = (lookupTable(x1), lookupTable(x2) )
    //minArray always shorter
    while (minArray.length < maxArray.length) {
      minArray += 0d
    }
    val r = new ArrayBuffer[Double]
    minArray.zip(maxArray).map{ case (y1, y2) => 
      val (slope, intercept) = _getSlopeAndIntercept(x1, y1, x2, y2)
      r += (slope * mz) + intercept
    }
    r.toArray
  }
  
  private def _getSlopeAndIntercept(x1: Double, y1: Double, x2 : Double, y2: Double) : Pair[Double, Double] = {
    val slope = (y2 - y1) / (x2 - x1)
    val intercept = y2 - (slope * x2)
    (slope, intercept)
  }
  
}