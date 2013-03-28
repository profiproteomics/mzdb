package fr.profi.mzdb.utils.math.wavelet

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
/**
 * class for modelize Ridge
 * internal map storing scale for key  and maxIdx of cwt coefficient
 * at the consdidered scale
 */
object Ridge {
  val maxGap = 4
}

case class Ridge(var gap: Int = 0) {
  var maximaIndexPerScale = HashMap[Int, Option[Int]]()

  def isEnded() : Boolean = { gap == Ridge.maxGap }

  /**
   * in theory the peak centroid
   */
  lazy val maxIndex: Pair[Int, Int] = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._2.get)
    (v._1, v._2.get)
  }

  /**
   * in theory the peak apex
   */
  lazy val maxIdxAtFirstScale: Pair[Int, Int] = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).minBy(x => x._1)
    (v._1, v._2.get)
  }
  
  
  lazy val maxIdxAtLastScale: Pair[Int, Int] = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._1)
    (v._1, v._2.get)
  }

  def add(scaleIdx: Int, maximaIdx: Option[Int]) { maximaIndexPerScale(scaleIdx) = maximaIdx}
  def hasScale(scaleIdx: Int): Boolean = { maximaIndexPerScale.get(scaleIdx) == None }
  def get(scaleIdx: Int): Option[Int] = {maximaIndexPerScale.get(scaleIdx).get}
  def incGap() = {gap += 1}
  def initGap() = {gap = 0}
  def length(): Int = {maximaIndexPerScale.size}

  def startLevel(): Int = {
    var v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._1)
    v._1
  }

  lazy val totalGaps: Int = {
    var c = 0
    maximaIndexPerScale.values.foreach(x => if (x == None) c += 1)
    c
  }

}


trait RidgesFinder {
  
  /**
   * FindRidges : find Ridge among cwt coefficients start from biggest scale
   * find local maxima then go up among scale finding local maximum in a given
   * window
   *
   * winLength: minWindow window is proportionnal to the scale: scale * 2 + 1
   */
  
  def findRidges(maximaIndexesPerScale: Array[Array[Int]], winLength: Int = 5): Array[Ridge] = {

    var lastMaximaRow = maximaIndexesPerScale.last
    var ridges = new ArrayBuffer[Ridge]
    
    for (m <- lastMaximaRow) { 
      var r = Ridge()
      r.add(maximaIndexesPerScale.length - 1, Some(m))
      ridges += r
    }
    
    for (i <- maximaIndexesPerScale.length - 2 to 0 by -1) {
      var currentRow = maximaIndexesPerScale(i)
      var winSize: Int = if ((i * 2 + 1).toInt > winLength) (i * 2 + 1).toInt else winLength // winLength is th minimal window
      
      var treatedIdx = new ArrayBuffer[Int]()

      for (ridge <- ridges if !ridge.isEnded()) {
        var prevMaxIndex = ridge.maximaIndexPerScale(i + 1)
        //the prev max could be None because of gap
        //looking for a valid prevMax
        if (prevMaxIndex == None) {
          var k = i + 2
          while (ridge.maximaIndexPerScale(k) == None) {
            k += 1
          }
          prevMaxIndex = ridge.maximaIndexPerScale(k)
        }

        var closestMax = currentRow.minBy(x => math.abs(prevMaxIndex.get - x)) //sortBy(x => math.abs(prevMaxIndex.get - x))

        if (math.abs(prevMaxIndex.get - closestMax) < winSize) { // /2
          ridge.add(i, Some(closestMax))
          ridge.initGap()
          treatedIdx += closestMax

        } else {
          ridge.incGap()
          ridge.add(i, None)
        }

      }
      //start a new ridge for all max not assigned to a ridge
      for (maxIdx <- currentRow if !treatedIdx.contains(maxIdx)) {
        var r_ = Ridge()
        r_.add(i, Some(maxIdx))
        ridges += r_
      }
    }
    //remove no good ridgeLines
    //filer ridge line with gap >= maxAllowedGap and ridge starting at level <= 2
    ridges = ridges.filter(x => !x.isEnded && x.startLevel > 2) 

    //remove different ridge lines with the same apex, take the longest
    var ridgesPerMaxIndexAtFirstScale = HashMap[Int, ArrayBuffer[Ridge]]()
    ridges.foreach { r => ridgesPerMaxIndexAtFirstScale.getOrElseUpdate(r.maxIdxAtFirstScale._2, new ArrayBuffer[Ridge]()) += r }
    var lastOfLastRidges = new ArrayBuffer[Ridge]()
    ridgesPerMaxIndexAtFirstScale.foreach { case (u, v) => lastOfLastRidges += v.maxBy { x => x.length() } }
    lastOfLastRidges.toArray[Ridge]
  }
}