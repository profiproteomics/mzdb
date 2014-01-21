package fr.profi.mzdb.utils.math.wavelet

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.beans.BeanProperty

/**
 * class for modelize Ridge
 * internal map storing scale for key  and maxIdx of cwt coefficient
 * at the consdidered scale
 */

case class Ridge(var gap: Int = 0) {
  /** internal map stocking the scale as key the maxIdx and the max value as Option may not exist (gap) */
  val maximaIndexPerScale = HashMap[Int, Option[Pair[Int, Double]]]()
  var totalGaps = 0
  var SNR : Float = 0
  var id:Int = -1
  var associatedRidge = Option.empty[Ridge] //used in to remove conflict
  
  def isEnded(maxGap: Int = 4) : Boolean = { gap == maxGap }

  /**in theory the peak centroid
   * @return a Tuple3 containing scale, maxIdx, value
   * */
  lazy val maxCoeffPos: Tuple3[Int, Int, Double] = {
    val v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._2.get._2) //order by value of the coefficient
    (v._1, v._2.get._1, v._2.get._2)
  }

  /** in theory the peak apex
   *  @return a Tuple3 containing scale, maxIdx, value
   *  */
  lazy val firstScaleMaxCoeffPos: Tuple3[Int, Int, Double] = {
    val v = maximaIndexPerScale.filter(x => x._2 != None).minBy(x => x._1) //order by scale
    (v._1, v._2.get._1, v._2.get._2)
  }
  
  
  lazy val lastScaleMaxCoeffPos: Tuple3[Int, Int, Double] = {
    val v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._1)
    (v._1, v._2.get._1, v._2.get._2)
  }

  def add(scaleIdx: Int, maxima: Option[Pair[Int, Double]]) { maximaIndexPerScale(scaleIdx) = maxima }
  def hasScale(scaleIdx: Int): Boolean = { maximaIndexPerScale.get(scaleIdx) == None }
  def get(scaleIdx: Int): Option[Pair[Int, Double]] = { maximaIndexPerScale.get(scaleIdx).get }
  def incGap() = {gap += 1; totalGaps +=1 }
  def initGap() = {gap = 0}
  def length(): Int = { maximaIndexPerScale.size }//with gap since we stop it 

  def startingScale(): Int = {
    val v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._1)
    v._1
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
  
  def findRidges(maximaIndexesPerScale: Array[Array[Int]], coeffs: Array[Array[Double]], winLength: Int = 5, maxGap:Int = 4): Pair[Array[Ridge], Array[Ridge]] = {
     
    if (maximaIndexesPerScale.isEmpty)
      return new Pair[Array[Ridge], Array[Ridge]](Array[Ridge](), Array[Ridge]())
      
    val lastMaximaRow = maximaIndexesPerScale.last
    val ridges = new ArrayBuffer[Ridge]
    val orphanRidges = new ArrayBuffer[Ridge]
    
    for (m <- lastMaximaRow) { 
      var r = Ridge()
      r.add(maximaIndexesPerScale.length - 1, Some(Pair(m, coeffs.last(m))))
      ridges += r
    }
    
    for (i <- maximaIndexesPerScale.length - 2 to 0 by -1) {
      val currentRow = maximaIndexesPerScale(i)
      val winSize: Int = if ((i / 2 + 1).toInt > winLength) (i / 2 + 1).toInt else winLength // winLength is th minimal window
      
      var treatedIdx = Set[Int]()

      for (ridge <- ridges if !ridge.isEnded(maxGap) ) {
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

        val closestMax = currentRow.minBy(x => math.abs(prevMaxIndex.get._1 - x)) //sortBy(x => math.abs(prevMaxIndex.get - x))

        if (math.abs(prevMaxIndex.get._1 - closestMax) < winSize / 2) { // /2
          ridge.add(i, Some(Pair(closestMax, coeffs(i)(closestMax))))
          ridge.initGap()
          treatedIdx += closestMax

        } else {
          ridge.incGap()
          ridge.add(i, None)
        }

      }
      //start a new ridge for all max not assigned to a ridge
      for (maxIdx <- currentRow if !treatedIdx.contains(maxIdx)) {
        val r_ = Ridge()
        r_.add(i, Some(Pair(maxIdx, coeffs(i)(maxIdx))))
        ridges += r_
      }
    }
    
    //remove different ridge lines with the same apex, take the longest
    val ridgesPerMaxIndexAtFirstScale = HashMap[Int, ArrayBuffer[Ridge]]()
    ridges.foreach { r => ridgesPerMaxIndexAtFirstScale.getOrElseUpdate(r.firstScaleMaxCoeffPos._2, new ArrayBuffer[Ridge]()) += r }
    val lastOfLastRidges = new ArrayBuffer[Ridge]()
    ridgesPerMaxIndexAtFirstScale.foreach { case (u, v) => lastOfLastRidges += v.maxBy { x => x.length() } }
    (lastOfLastRidges.toArray, orphanRidges.toArray)
  }
}