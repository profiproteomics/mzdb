package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer

import org.apache.commons.math.stat.StatUtils

import fr.profi.mzdb.utils.misc.InMemoryIdGen
import fr.profi.util.math.linearInterpolation


/*object Peakel {
  
  def calcPeaksIndexRange( peaks: Array[Peak] ): Pair[Int,Int] = {
    
    var firstLcContext: ILcContext = null
    var lastLcContext: ILcContext = null
    
    var firstIdx = -1
    var lastIdx = -1
    
    var idx = 0
    for( p <- peaks ) {
      if( firstIdx == -1 && p != null ) firstIdx = idx
      if( firstIdx >= 0 && lastIdx == -1 && p == null ) lastIdx = idx - 1
      idx += 1
    }
    
    (firstIdx, lastIdx)
  }
  
  def calcLcContextRange( definedPeaks: Array[Peak] ): Pair[ILcContext,ILcContext] = {
    
    var firstLcContext: ILcContext = null
    var lastLcContext: ILcContext = null
    
    val peaksCount = definedPeaks.length
    if( peaksCount > 0 ) {    
      firstLcContext = definedPeaks.head.lcContext
      lastLcContext = definedPeaks.last.lcContext
    }
    
    (firstLcContext, lastLcContext)
  }
  
}
*/

trait IPeakelData {
  
  def getScanIds(): Seq[Int]
  def getElutionTimes(): Seq[Float]
  def getMzValues(): Seq[Double]
  def getIntensityValues(): Seq[Float]
  
  def getElutionTimeIntensityPairs() = getElutionTimes.zip(getIntensityValues)
  
  def getNewCursor(): IPeakelDataCursor = {
    this match {
      case peakel: Peakel => new PeakelCursor(peakel)
      case peakelBuilder: PeakelBuilder => new PeakelBuilderCursor(peakelBuilder)
      case dataMessage: PeakelDataMessage => new PeakelDataMessageCursor(dataMessage)
    }
  }
  
  def integratePeakel(): (Float,Float,Float) = {
    
    val apexIntensity = this.getIntensityValues.max  
    val halfApexIntensity = apexIntensity / 2
    val lastTime = getElutionTimes.last
    
    //val intensitiesAboveHM = intensityList.zipWithIndex.filter(_._1 >= halfApexIntensity )
    //print(apex.getLcContext().getElutionTime()+"\t")
    
    // --- Interpolate the time of peaks at the half maximum peakel intensity ---
    val intensityElutionTimePairs = getIntensityValues.zip(getElutionTimes)
    val leftTimeAtHalfApex = _interpolateFirstElutionTimeAtHalfMaximum(intensityElutionTimePairs, halfApexIntensity)
    val rightTimeAtHalfApex = _interpolateFirstElutionTimeAtHalfMaximum(intensityElutionTimePairs.reverse, halfApexIntensity)
    //print( mz + "\t"+ leftTimeAtHalfApex + "\t" + rightTimeAtHalfApex + "\t" + defPeaksAboveHM.length)
    
    // Search for the apex and integrate IPs
    var computedSum = 0f
    var computedArea = 0f
    //var computedAAHM = 0f
    var prevPeakTime = 0f
    var prevPeakIntensity = 0f
    //var prevPeakIntensityAboveHM = halfApexIntensity
    //var prevPeakTimeAboveHM = leftTimeAtHalfApex
    
    val peakelCursor = getNewCursor()
    
    while( peakelCursor.next() ) {
      
      // Compute intensity sum
      val intensity = peakelCursor.getIntensity()
      val curPeakTime = peakelCursor.getElutionTime()

      computedSum += intensity
      
      // Compute intensity area
      if( peakelCursor.peakIndex > 0 ) {
        val deltaTime = curPeakTime - prevPeakTime
        computedArea += (intensity + prevPeakIntensity ) * deltaTime / 2
      }
      // Compute intensity uahm
      /*if (curPeakTime >= leftTimeAtHalfApex && prevPeakTimeAboveHM < lastTime ) {
        
        if( curPeakTime <= rightTimeAtHalfApex)  {
          val deltaTime = curPeakTime - prevPeakTimeAboveHM
          computedAAHM += (peak.intensity + prevPeakIntensityAboveHM - apexIntensity) * deltaTime / 2
          prevPeakTimeAboveHM = curPeakTime
          prevPeakIntensityAboveHM = peak.intensity
        } else {
          val deltaTime = curPeakTime - prevPeakTimeAboveHM
          computedAAHM += (prevPeakIntensityAboveHM - halfApexIntensity) * deltaTime / 2
          prevPeakIntensityAboveHM = halfApexIntensity
          prevPeakTimeAboveHM = lastTime
        }
      }*/
      prevPeakTime = curPeakTime
      prevPeakIntensity = intensity
    }
    
    //println("\t" + computedArea + "\t" +computedAAHM )
    
    if( computedArea == 0 ) {
      computedArea = computedSum
      //computedAAHM = computedSum
    }
    
    ( computedSum, computedArea, rightTimeAtHalfApex - leftTimeAtHalfApex )
  }
  
  private def _interpolateFirstElutionTimeAtHalfMaximum( intensityTimePairs: Seq[(Float, Float)], halfApexIntensity: Float): Float = {
    
    val firstPeakIndex2 = intensityTimePairs.indexWhere(_._1 >= halfApexIntensity)
    val firstPeakIndex1 = if (firstPeakIndex2 > 0) firstPeakIndex2 - 1 else 0
    
    // If we don't have two distinct peaks, return time value of the first peakel peak
    val firstPeakTime = if (firstPeakIndex2 <= firstPeakIndex1) getElutionTimes.head
    else {
      
      // Linear interpolation
      val interpolatedTime = linearInterpolation(
        halfApexIntensity,
        Seq( intensityTimePairs(firstPeakIndex1), intensityTimePairs(firstPeakIndex2) ),
        fixOutOfRange = false
      )
      
      interpolatedTime
    }
    
    firstPeakTime
  }
  
  /** Just check elution peak in terms of duration in nb scans */
  def hasEnoughPeaks(minPeaksCount:Int): Boolean = {
    
    if (getScanIds.length < minPeaksCount)
      return false
    
    true
  }

  def toPeaks( lcContextByScanId: Map[Int,ILcContext] ): Array[Peak] = {
    
    val peakelCursor = this match {
      case peakel: Peakel => new PeakelCursor(peakel)
      case peakelBuilder: PeakelBuilder => new PeakelBuilderCursor(peakelBuilder)
    }
    
    val peaks = new Array[Peak](this.getScanIds.length)
    while( peakelCursor.next() ) {
      peaks(peakelCursor.peakIndex) = peakelCursor.toPeak( lcContextByScanId )
    }
    
    peaks
  }
  
  /*def toLcMsPeaks(): Array[LcMsPeak] = {
    val cursor = new PeakelCursor(this)
    
    val peaks = new Array[LcMsPeak](this.scanIds.length)
    while( cursor.next() ) {
      peaks(cursor.peakIndex) = cursor.toLcMsPeak()
    }
    
    peaks
  }*/
  
  
  
}

object Peakel extends InMemoryIdGen

/** The Class Peakel.
 * @author David Bouyssie
 *
 */
case class Peakel(
  @BeanProperty var id: Int = Peakel.generateNewId(),
  scanIds: Array[Int],
  elutionTimes: Array[Float],
  mzValues: Array[Double],
  intensityValues: Array[Float],
  @BeanProperty intensitySum: Float,
  @BeanProperty area: Float,
  @BeanProperty leftHwhmMean: Float = 0f,
  @BeanProperty leftHwhmCv: Float = 0f,
  @BeanProperty rightHwhmMean: Float = 0f,
  @BeanProperty rightHwhmCv: Float = 0f
) extends IPeakelData with ILcContext {
  
  def getScanIds(): Seq[Int] = scanIds
  def getElutionTimes(): Seq[Float] = elutionTimes
  def getMzValues(): Seq[Double] = mzValues
  def getIntensityValues(): Seq[Float] = intensityValues
  
  // Make some requirements
  require( scanIds != null && scanIds.length > 0, "some scanIds must be provided" )
  require( mzValues != null && mzValues.length > 0, "some peaks must be provided" )
  require( scanIds.length == mzValues.length, "scanIds and mzValues must have the same size" )
  require( mzValues.length == intensityValues.length, "mzList and intensityList must have the same size" )
  
  @BeanProperty val apexIndex = intensityValues.zipWithIndex.maxBy(_._1)._2
  
  def this(
    id: Int,
    dataMessage: PeakelDataMessage,
    intensitySum: Float,
    area: Float,
    leftHwhmMean: Float,
    leftHwhmCv: Float,
    rightHwhmMean: Float,
    rightHwhmCv: Float
  ) {
    this(
      id,
      dataMessage.scanIds,
      dataMessage.elutionTimes,
      dataMessage.mzValues,
      dataMessage.intensityValues,
      intensitySum,
      area,
      leftHwhmMean,
      leftHwhmCv,
      rightHwhmMean,
      rightHwhmCv
    )
  }
  
  //def getApexLcContext() = lcContexts(apexIndex)
  //def getFirstLcContext() = lcContexts.head
  //def getLastLcContext() = lcContexts.last
  def getApexScanId() = scanIds(apexIndex)
  def getFirstScanId() = scanIds.head
  def getLastScanId() = scanIds.last
  
  def getApexElutionTime() = elutionTimes(apexIndex)
  def getFirstElutionTime() = elutionTimes.head
  def getLastElutionTime() = elutionTimes.last
  
  def getApexMz() = mzValues(apexIndex)
  def getApexIntensity() = intensityValues(apexIndex)
  def getMz() = mzValues(apexIndex) // TODO: lazy val ???
  override def getElutionTimeIntensityPairs() = elutionTimes.zip(intensityValues)
  
  def getCursorAtApex(): PeakelCursor = PeakelCursor(this, apexIndex)
  
  // ILcContext java interface implementation 
  def getScanId() : Int = getApexScanId()
  def getElutionTime(): Float = getApexElutionTime()
  
  // Access to data of a peak at a given index
  def getPeakElutionTime(pIdx: Int): Float = elutionTimes(pIdx)
  def getPeakMz(pIdx: Int): Double = mzValues(pIdx)
  def getPeakIntensity(pIdx: Int): Float = intensityValues(pIdx)
  
  def calcDuration(): Float = getLastElutionTime - getFirstElutionTime
  
  def calcWeightedAverageTime(): Float = {
    var intensitySum = 0f
    
    val weightedTimeSum = intensityValues.zipWithIndex.foldLeft(0f) { (timeSum,intensityWithIdx) =>
      val(intensity,index) = intensityWithIdx
      intensitySum += intensity
      timeSum + (elutionTimes(index) * intensity)
    }
    
    weightedTimeSum / intensitySum
  }
  
  /*def calcPeakelsIntersection( otherPeakel: Peakel ): Pair[Array[Peak],Array[Peak]] = {
    val thisPeakByTime = this.peakByElutionTime
    val otherPeakByTime = otherPeakel.peakByElutionTime
    val intersectingTimes = (thisPeakByTime.keys ++ otherPeakByTime.keys)
      .groupBy(e=>e)
      .withFilter(_._2.size == 2)
      .map(_._1)
      .toArray.sorted
    
    Pair( intersectingTimes.map( thisPeakBytTime(_) ), intersectingTimes.map( otherPeakByTime(_) ) )    
  }*/
  
  def toPeakelDataMessage(): PeakelDataMessage = {
    new PeakelDataMessage( scanIds, elutionTimes, mzValues, intensityValues )
  }
  
}

class PeakelBuilder(
  val scanIds: ArrayBuffer[Int] = new ArrayBuffer[Int](),
  val elutionTimes: ArrayBuffer[Float] = new ArrayBuffer[Float](),
  val mzValues: ArrayBuffer[Double] = new ArrayBuffer[Double](),
  val intensityValues: ArrayBuffer[Float] = new ArrayBuffer[Float](),
  @BeanProperty val leftHwhms: ArrayBuffer[Float] = new ArrayBuffer[Float](),
  @BeanProperty val rightHwhms: ArrayBuffer[Float] = new ArrayBuffer[Float]()
) extends IPeakelData {
  
  def getScanIds(): Seq[Int] = scanIds
  def getElutionTimes(): Seq[Float] = elutionTimes
  def getMzValues(): Seq[Double] = mzValues
  def getIntensityValues(): Seq[Float] = intensityValues
  
  def this( peaks: Array[Peak] ) = {
    this()
    
    this += peaks
  }
  
  def this( peakelDataMessage: PeakelDataMessage ) = {
    this()
    
    this.scanIds ++= peakelDataMessage.scanIds
    this.elutionTimes ++= peakelDataMessage.elutionTimes
    this.mzValues ++= peakelDataMessage.mzValues
    this.intensityValues ++= peakelDataMessage.intensityValues
  }
  
  def +=( peak: Peak ): PeakelBuilder = {
    require( peak != null, "peak is null")
    
    val lcContext = peak.getLcContext()
    
    this.add(lcContext.getScanId, lcContext.getElutionTime, peak.mz, peak.intensity, peak.leftHwhm, peak.rightHwhm)
    
    this
  }
  
  def +=( peaks: Array[Peak] ): PeakelBuilder = {
    require( peaks != null, "peaks is null")
    
    for( peak <- peaks ) this += peak    
    this
  }
  
  /*
  def add( scanId: Int, elutionTime: Float, mz: Double, intensity: Float ): PeakelBuilder = {
    scanIdBuffer += scanId
    elutionTimeBuffer += elutionTime
    mzBuffer += mz
    intensityBuffer += intensity
    
    this
  }
  */
  
  def add( scanId: Int, elutionTime: Float, mz: Double, intensity: Float, leftHwhm: Float, rightHwhm: Float ): PeakelBuilder = {
    scanIds += scanId
    elutionTimes += elutionTime
    mzValues += mz
    intensityValues += intensity
    leftHwhms += leftHwhm
    rightHwhms += rightHwhm
    
    this
  }
  
  def result( id: Int = Peakel.generateNewId() ): Peakel = {
    //val leftHwhmValues = if( leftHwhmBuffer.count(_ > 0) == 0 ) null else leftHwhmBuffer.toArray
    //val rightHwhmValues = if( rightHwhmBuffer.count(_ > 0) == 0 ) null else rightHwhmBuffer.toArray
    
    val(intensity, area, fwhm) = this.integratePeakel()
    
    val(leftHwhmMean,leftHwhmSd) = _calcMeanAndSd(leftHwhms)
    val leftHwhmCv = if( leftHwhmSd > 0 ) 100 * leftHwhmMean / leftHwhmSd else 0f
    val(rightHwhmMean,rightHwhmSd) = _calcMeanAndSd(rightHwhms)
    val rightHwhmCv = if( rightHwhmSd > 0 ) 100 * rightHwhmMean / rightHwhmSd else 0f
    
    Peakel(
      id = id,
      scanIds.toArray,
      elutionTimes.toArray,
      mzValues.toArray,
      intensityValues.toArray,
      intensity,
      area,
      leftHwhmMean,
      leftHwhmCv,
      rightHwhmMean,
      rightHwhmCv
    )
  }
  
  private def _calcMeanAndSd( values: ArrayBuffer[Float] ): (Float,Float) = {
    if( values == null ) return (0f,0f)
    
    val valuesAsDoubles = values.map(_.toDouble).toArray
    val mean = StatUtils.mean(valuesAsDoubles)
    val variance = StatUtils.variance(valuesAsDoubles, mean)
   
    (mean.toFloat, math.sqrt(variance).toFloat )
  }
  
  def calcFwhms(): Array[Float] = {
    val( lh, rh ) = ( this.leftHwhms, this.rightHwhms )
    if( lh == null || rh == null ) return null

    this.scanIds.indices.toArray.map(i => lh(i) + rh(i) )
  }
  
  /** Restrict to is inclusive here **/
  def restrictToScanIdRange( firstScanId: Int, lastScanId: Int ): Option[PeakelBuilder] = {
    
    val matchingScanIdsWithIdx = scanIds.zipWithIndex.filter { case (scanId,idx) =>
      scanId >= firstScanId && scanId <= lastScanId
    }
    if( matchingScanIdsWithIdx.length < 2 ) return None
    
    val firstIdx = matchingScanIdsWithIdx.head._2
    val lastBoundary = matchingScanIdsWithIdx.last._2 + 1
    
    val scanIdCount = scanIds.length
    val newPeakelBuilder = new PeakelBuilder(
      scanIds.slice(firstIdx, lastBoundary),
      elutionTimes.slice(firstIdx, lastBoundary),
      mzValues.slice(firstIdx, lastBoundary),
      intensityValues.slice(firstIdx, lastBoundary),
      if( leftHwhms.length == scanIdCount ) null else leftHwhms.slice(firstIdx, lastBoundary),
      if( rightHwhms.length == scanIdCount ) null else rightHwhms.slice(firstIdx, lastBoundary)
    )
    
    Some( newPeakelBuilder )
  }
  
}

trait IPeakelDataCursor {
  
  def peakelData: IPeakelData
  var peakIndex: Int
  
  require( peakIndex >= -1 && peakIndex <= peakelData.getScanIds.length, "peakeIndex is out of bounds")
    
  //def isOutOfBounds(): Boolean = peakIndex <= -1 || peakIndex >= peakel.lcContexts.length
  
  def next(): Boolean = {
    if( peakIndex >= peakelData.getScanIds.length - 1 ) false
    else {
      peakIndex += 1
      true
    }
  }
  def previous(): Boolean = {
    if( peakIndex <= 0 ) false
    else {
      peakIndex -= 1
      true
    }
  }
  
  //def getLcContext(): ILcContext = peakel.lcContexts(peakIndex)
  def getScanId(): Int
  def getElutionTime(): Float
  def getMz(): Double
  def getIntensity(): Float
  
  def toPeak( lcContextByScanId: Map[Int,ILcContext] ): Peak = {
    new Peak( getMz, getIntensity, 0f, 0f, lcContextByScanId(getScanId) )
  }
  
  /*def toLcMsPeak(): LcMsPeak = {
    new LcMsPeak( getScanId, getElutionTime, getMz, getIntensity, getLeftHwhm, getRightHwhm )
  }*/
}

case class PeakelCursor(
  peakelData: Peakel,
  var peakIndex: Int = - 1
) extends IPeakelDataCursor {  
  def getScanId(): Int = peakelData.scanIds(peakIndex)
  def getElutionTime(): Float = peakelData.elutionTimes(peakIndex)
  def getMz(): Double = peakelData.mzValues(peakIndex)
  def getIntensity(): Float = peakelData.intensityValues(peakIndex)
}

case class PeakelBuilderCursor(
  peakelData: PeakelBuilder,
  var peakIndex: Int = - 1
) extends IPeakelDataCursor {  
  def getScanId(): Int = peakelData.scanIds(peakIndex)
  def getElutionTime(): Float = peakelData.elutionTimes(peakIndex)
  def getMz(): Double = peakelData.mzValues(peakIndex)
  def getIntensity(): Float = peakelData.intensityValues(peakIndex)
}
case class PeakelDataMessageCursor(
  peakelData: PeakelDataMessage,
  var peakIndex: Int = - 1
) extends IPeakelDataCursor {  
  def getScanId(): Int = peakelData.scanIds(peakIndex)
  def getElutionTime(): Float = peakelData.elutionTimes(peakIndex)
  def getMz(): Double = peakelData.mzValues(peakIndex)
  def getIntensity(): Float = peakelData.intensityValues(peakIndex)
}

/** Class used for MessagePack serialization purpose **/
@org.msgpack.annotation.Message
case class PeakelDataMessage(
  // MessagePack requires mutable fields
  var scanIds: Array[Int],
  var elutionTimes: Array[Float],
  var mzValues: Array[Double],
  var intensityValues: Array[Float]
) extends IPeakelData {
  
  // Plain constructor needed for MessagePack
  def this() = this(Array(),Array(),Array(),Array())
  
  def getScanIds(): Seq[Int] = scanIds
  def getElutionTimes(): Seq[Float] = elutionTimes
  def getMzValues(): Seq[Double] = mzValues
  def getIntensityValues(): Seq[Float] = intensityValues
  
}