package fr.profi.mzdb.utils.math.wavelet

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.reflect.BeanProperty

/**
 * class for modelize Ridge
 * internal map storing scale for key  and maxIdx of cwt coefficient
 * at the consdidered scale
 */

case class Ridge(var gap: Int = 0) {
  /** internal map stocking the scale as key the maxIdx and the max value as Option may not exist (gap) */
  val maximaIndexPerScale = HashMap[Float, Option[Pair[Int, Double]]]()
  var totalGaps = 0
  var SNR : Float = 0
  var id:Int = -1
  var associatedRidge = Option.empty[Ridge] //used in to remove conflict
  
  def isEnded(maxGap: Int = 4) : Boolean = { gap == maxGap }

  /**in theory the peak centroid
   * @return a Tuple3 containing scale, maxIdx, value
   * */
  lazy val maxCoeffPos: Tuple3[Float, Int, Double] = {
    val v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._2.get._2) //order by value of the coefficient
    (v._1, v._2.get._1, v._2.get._2)
  }

  /** in theory the peak apex
   *  @return a Tuple3 containing scale, maxIdx, value
   *  */
  lazy val firstScaleMaxCoeffPos: Tuple3[Float, Int, Double] = {
    val v = maximaIndexPerScale.filter(x => x._2 != None).minBy(x => x._1) //order by scale
    (v._1, v._2.get._1, v._2.get._2)
  }
  
  
  lazy val lastScaleMaxCoeffPos: Tuple3[Float, Int, Double] = {
    val v = maximaIndexPerScale.filter(x => x._2 != None).maxBy(x => x._1)
    (v._1, v._2.get._1, v._2.get._2)
  }

  def add(scale: Float, maxima: Option[Pair[Int, Double]]) { maximaIndexPerScale(scale) = maxima }
  def hasScale(scale: Float): Boolean = { maximaIndexPerScale.get(scale) == None }
  def get(scale: Float): Option[Pair[Int, Double]] = { maximaIndexPerScale(scale) }
  def incGap() = {gap += 1; totalGaps +=1 }
  def initGap() = {gap = 0}
  def length(): Int = { maximaIndexPerScale.size }//with gap since we stop it 

  def startingScale(): Float = {
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
  var coeffs: HashMap[Float, Array[Float]]
  
  def findRidges(maximaIndexesPerScale: HashMap[Float, Array[Int]],  winLength: Int = 5, maxGap:Int = 4): Pair[Array[Ridge], Array[Ridge]] = {
    //check emptyness
    if (maximaIndexesPerScale.isEmpty)
      return new Pair(Array[Ridge](), Array[Ridge]())
   
      
    val sortedScales = maximaIndexesPerScale.keys.toBuffer.sorted
    val lastMaximaRow = maximaIndexesPerScale(sortedScales.last)
    val ridges = new ArrayBuffer[Ridge]
    val orphanRidges = new ArrayBuffer[Ridge]
    
    //init a ridge for each max
    for (maxVal <- lastMaximaRow) { 
      val ridge = Ridge()
      ridge.add(sortedScales.last, Some(Pair(maxVal, coeffs(sortedScales.last)(maxVal))))
      ridges += ridge
    }
    
    //if only one scale
    if (sortedScales.length == 1) {
      return Pair(ridges.toArray, orphanRidges.toArray)
    }
    
    //else
    for (i <- sortedScales.length - 2 to 0 by -1)  {
      
      val currentScale = sortedScales(i)
      val currentRow = maximaIndexesPerScale(currentScale)
      
      if (! currentRow.isEmpty) {
        
        val winSize: Int = if ((currentScale / 2 + 1).toInt > winLength) (currentScale / 2 + 1).toInt else winLength // winLength is th minimal window
        
        var treatedIdx = Set[Int]()
  
        for (ridge <- ridges if !ridge.isEnded(maxGap) ) {
          var k = i + 1
          var prevMaxIndex = ridge.maximaIndexPerScale.getOrElse(sortedScales(k), None)
         
          //find a valid max
          while (prevMaxIndex == None) {
            k += 1
            prevMaxIndex = ridge.maximaIndexPerScale(sortedScales(k))
          }
          
          //check buggy stuff
          if (! prevMaxIndex.isDefined)
            throw new Exception("prevMaxIndex must be defined")
          
          val closestMax = currentRow.minBy(x => math.abs(prevMaxIndex.get._1 - x)) 
  
          if (math.abs(prevMaxIndex.get._1 - closestMax) < winSize / 2) { // /2
            ridge.add(currentScale, Some(Pair(closestMax, coeffs(currentScale)(closestMax))))
            ridge.initGap()
            treatedIdx += closestMax
  
          } else {
            ridge.incGap()
            ridge.add(currentScale, None)
          }
        }
        //start a new ridge for all max not assigned to a ridge
        for (maxIdx <- currentRow if !treatedIdx.contains(maxIdx)) {
          val ridge = Ridge()
          ridge.add(currentScale, Some(Pair(maxIdx, coeffs(currentScale)(maxIdx))))
          ridges += ridge
        }
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