package fr.profi.mzdb.model

import beans.BeanProperty
import fr.profi.mzdb.algo.feature.scoring.FeatureScorer
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

/** The Class Peakel.
 * @author David Bouyssie
 *
 * @param index the index
 * @param peaks the peaks
 */
case class Peakel1(
  @BeanProperty index: Int,
  //@BeanProperty var mz: Double,
  @BeanProperty peaks: Array[Peak]
) extends ILcContext {
  
  // Make some requirements
  require( peaks != null && peaks.length > 0,"some peaks must be provided" )
  
  // Define other Peakel attributes
  @BeanProperty val definedPeaks = for( p<- peaks if p != null) yield p
  require( definedPeaks.length > 0,"some defined peaks must be provided" )
  
  @BeanProperty val definedPeaksIndexRange = Peakel.calcPeaksIndexRange( peaks )
  @BeanProperty val lcContextRange = Peakel.calcLcContextRange( definedPeaks )
  @BeanProperty val firstScanContext = lcContextRange._1
  @BeanProperty val lastScanContext = lcContextRange._2
  @BeanProperty val apexIndex = peaks.indices.filter( peaks(_) != null ).reduce { (a,b) =>
                                  if( peaks(a).intensity > peaks(b).intensity ) a else b
                                }
  
  @BeanProperty val apexScanContext = peaks(apexIndex).lcContext
  @BeanProperty val mz = this.getApex().mz
  @BeanProperty var intensity = 0f
  @BeanProperty var area = 0f
  @BeanProperty var fwhm = 0f
  
  // Define lazy attributes
  @BeanProperty lazy val peakByElutionTime = definedPeaks.map { p => p.getLcContext.getElutionTime -> p } toMap
  @BeanProperty lazy val duration = lastScanContext.getElutionTime - firstScanContext.getElutionTime
  @BeanProperty lazy val weightedAverageTime = {
    var intensitySum = 0f
    val weightedTimeSum = definedPeaks.foldLeft(0f) { (timeSum,peak) =>
      val intensity = peak.intensity
      intensitySum += peak.intensity
      timeSum + (peak.getLcContext.getElutionTime * intensity)
    }
    weightedTimeSum / intensitySum
  }
  @BeanProperty var localMaxima : Array[Int] = null
  
  // Update feature intensity and area
  this._integratePeakel()
  
  protected def getPeakTime(p: Peak): Float = p.getLcContext().getElutionTime()
  protected def getPeakIntensityTimePair(p: Peak): Pair[Float,Float] = (p.getIntensity(),p.getLcContext().getElutionTime())
    
  protected def _integratePeakel() {
    
    val apex = this.getApex()
    val apexIntensity = apex.getIntensity()
    val halfApexIntensity = apex.getIntensity() / 2
    val defPeaks = this.definedPeaks
    val lastTime = getPeakTime(defPeaks.last)
    
    val defPeaksAboveHM = defPeaks.filter(_.getIntensity() >= halfApexIntensity )
    //print(apex.getLcContext().getElutionTime()+"\t")
    
    // --- Interpolate the time of peaks at the half maximum peakel intensity ---
    val leftTimeAtHalfApex = _interpolateFirstPeakTimeAtHalfMaximum(defPeaks, halfApexIntensity)
    val rightTimeAtHalfApex = _interpolateFirstPeakTimeAtHalfMaximum(defPeaks.reverse, halfApexIntensity)
    //print( mz + "\t"+ leftTimeAtHalfApex + "\t" + rightTimeAtHalfApex + "\t" + defPeaksAboveHM.length)
    
    // Search for the apex and integrate IPs
    var computedSum = 0f
    var computedArea = 0f
    //var computedAAHM = 0f
    var prevPeakTime = 0f
    var prevPeakIntensity = 0f
    //var prevPeakIntensityAboveHM = halfApexIntensity
    //var prevPeakTimeAboveHM = leftTimeAtHalfApex
    
    for( peakIndex <- 0 until defPeaks.length ) {
      
      // Compute intensity sum
      val peak = defPeaks(peakIndex)
      val curPeakTime = peak.lcContext.getElutionTime

      computedSum += peak.intensity
      
      // Compute intensity area
      if( peakIndex > 0 ) {
        val deltaTime = curPeakTime - prevPeakTime
        computedArea += (peak.intensity + prevPeakIntensity ) * deltaTime / 2
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
      prevPeakIntensity = peak.intensity
    }
    
    //println("\t" + computedArea + "\t" +computedAAHM )
    
    if( computedArea == 0 ) {
      computedArea = computedSum
      //computedAAHM = computedSum
    }
    
    this.intensity = computedSum
    this.area = computedArea
    this.fwhm = rightTimeAtHalfApex - leftTimeAtHalfApex
  }
  
  private def _interpolateFirstPeakTimeAtHalfMaximum(peakArray: Array[Peak], halfApexIntensity: Float): Float = {
    
    val firstPeakIndex2 = peakArray.indexWhere(_.getIntensity() >= halfApexIntensity)
    val firstPeakIndex1 = if (firstPeakIndex2 > 0) firstPeakIndex2 - 1 else 0
    
    // If we don't have two distinct peaks, return time value of the first peakel peak
    val firstPeakTime = if (firstPeakIndex2 <= firstPeakIndex1) getPeakTime(definedPeaks(0))
    else{
      val (p1, p2) = (peakArray(firstPeakIndex1), peakArray(firstPeakIndex2))
      //print(getPeakTime(p1)+"\t"+getPeakTime(p2)+"\t")
      
      // Linear interpolation
      val interpolatedTime = linearInterpolation(halfApexIntensity, Seq( getPeakIntensityTimePair(p1), getPeakIntensityTimePair(p2) ), fixOutOfRange = false)
      interpolatedTime
    }
    firstPeakTime
  }
  
  def getApex(): Peak = peaks(apexIndex)
  
  def getIntensities(): Array[Float] = getDefinedPeaks.map { _.intensity }
  
  def calcPeakelsIntersection( otherPeakel: Peakel1 ): Pair[Array[Peak],Array[Peak]] = {
    val thisPeakBytTime = this.peakByElutionTime
    val otherPeakByTime = otherPeakel.peakByElutionTime
    val intersectingTimes = (thisPeakBytTime.keys ++ otherPeakByTime.keys)
      .groupBy(e=>e)
      .withFilter(_._2.size == 2)
      .map(_._1)
      .toArray.sorted
    Pair( intersectingTimes.map( thisPeakBytTime(_) ), intersectingTimes.map( otherPeakByTime(_) ) )    
  }
  
  /** Just check elution peak in terms of duration in nb scans */
  def isGoodForPeakDetection(minConsecutiveScans:Int): Boolean = {
    
    val peaks = this.peaks
    if (peaks.length < minConsecutiveScans)
      return false

    val definedPeaks = this.definedPeaks
    // Note: 3 definedPeaks are required for SG filter
    if (definedPeaks.length <= 3)
      return false
    
    true
  }
  
  // ILcContext java interface implementation 
  def getScanId() : Int = { getApexScanContext().getScanId() }
  def getElutionTime(): Float = { getApexScanContext().getElutionTime() }
  
}*/

/** The Class Peakel.
 * @author David Bouyssie
 *
 */
case class Peakel(
  @BeanProperty lcContexts: Array[ILcContext],
  @BeanProperty mzValues: Array[Double],
  @BeanProperty intensityValues: Array[Float],
  @BeanProperty leftHwhmValues: Array[Float] = null,
  @BeanProperty rightHwhmValues: Array[Float] = null
) extends ILcContext {
  
  def this( peaks: Array[Peak] ) {
    this(
      if( peaks == null ) throw new IllegalArgumentException("peaks is null") else peaks.map(_.lcContext),
      peaks.map(_.mz),
      peaks.map(_.intensity),
      if( peaks.count(_.leftHwhm > 0) == 0 ) null else peaks.map(_.leftHwhm),
      if( peaks.count(_.rightHwhm > 0) == 0 ) null else peaks.map(_.rightHwhm)
    )
  }
  
  // Make some requirements
  require( lcContexts != null && lcContexts.length > 0, "some LC contexts must be provided" )
  require( mzValues != null && mzValues.length > 0, "some peaks must be provided" )
  require( lcContexts.length == mzValues.length, "lcContexts and mzList must have the same size" )
  require( mzValues.length == intensityValues.length, "mzList and intensityList must have the same size" )
  
  // Define other Peakel attributes
  @BeanProperty val apexIndex = intensityValues.zipWithIndex.maxBy(_._1)._2
  @BeanProperty var intensity = 0f
  @BeanProperty var area = 0f
  @BeanProperty var fwhm = 0f
  //@BeanProperty var localMaxima: Array[Int] = null  
  
  // Update feature intensity, area and fwhm
  this._integratePeakel()
  
  def getApexLcContext() = lcContexts(apexIndex)
  def getFirstLcContext() = lcContexts.head
  def getLastLcContext() = lcContexts.last
  
  def getApexElutionTime() = lcContexts(apexIndex).getElutionTime
  def getApexMz() = mzValues(apexIndex)
  def getApexIntensity() = intensityValues(apexIndex)
  def getMz() = mzValues(apexIndex) // TODO: lazy val ???
  def getElutionTimes() = lcContexts.map(_.getElutionTime)
  def getLcContextIntensityPairs() = lcContexts.zip(intensityValues)
  
  def getCursorAtApex(): PeakelCursor = PeakelCursor(this, apexIndex)
  def getNewCursor(): PeakelCursor = PeakelCursor(this, 0)
  
  protected def getPeakTime(pIdx: Int): Float = lcContexts(pIdx).getElutionTime()
  /*protected def getIntensityElutionTimePair(pIdx: Int): Pair[Float,Float] = {
    ( intensityValues(pIdx), lcContexts(pIdx).getElutionTime )
  }*/
  
  protected def _integratePeakel() {
    
    val apexIntensity = getApexIntensity()
    val halfApexIntensity = apexIntensity / 2
    val lastTime = lcContexts.last.getElutionTime()
    
    //val intensitiesAboveHM = intensityList.zipWithIndex.filter(_._1 >= halfApexIntensity )
    //print(apex.getLcContext().getElutionTime()+"\t")
    
    // --- Interpolate the time of peaks at the half maximum peakel intensity ---
    val intensityElutionTimePairs = intensityValues.zip(getElutionTimes)
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
    
    val peakelCursor = new PeakelCursor( this )
    
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
    
    this.intensity = computedSum
    this.area = computedArea
    this.fwhm = rightTimeAtHalfApex - leftTimeAtHalfApex
  }
  
  private def _interpolateFirstElutionTimeAtHalfMaximum( intensityTimePairs: Array[(Float, Float)], halfApexIntensity: Float): Float = {
    
    val firstPeakIndex2 = intensityTimePairs.indexWhere(_._1 >= halfApexIntensity)
    val firstPeakIndex1 = if (firstPeakIndex2 > 0) firstPeakIndex2 - 1 else 0
    
    // If we don't have two distinct peaks, return time value of the first peakel peak
    val firstPeakTime = if (firstPeakIndex2 <= firstPeakIndex1) getPeakTime(0)
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
  
  def calcDuration(): Float = getLastLcContext.getElutionTime - getFirstLcContext.getElutionTime
  
  def calcFwhms(): Array[Float] = {
    val( lh, rh ) = ( this.leftHwhmValues, this.rightHwhmValues )
    if( lh == null || rh == null ) return null

    this.lcContexts.indices.toArray.map(i => lh(i) + rh(i) )
  }
  
  def calcWeightedAverageTime(): Float = {
    var intensitySum = 0f
    
    val weightedTimeSum = intensityValues.zipWithIndex.foldLeft(0f) { (timeSum,intensityWithIdx) =>
      val(intensity,index) = intensityWithIdx
      intensitySum += intensity
      timeSum + (lcContexts(index).getElutionTime * intensity)
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
  
  /** Just check elution peak in terms of duration in nb scans */
  def hasEnoughPeaks(minPeaksCount:Int): Boolean = {
    
    if (lcContexts.length < minPeaksCount)
      return false
    
    true
  }
  
  // ILcContext java interface implementation 
  def getScanId() : Int = { getApexLcContext().getScanId() }
  def getElutionTime(): Float = { getApexLcContext().getElutionTime() }
  
  /** Restrict to is inclusive here **/
  def restrictToLcContextRange( minLcContext: ILcContext, maxLcContext: ILcContext ): Option[Peakel] = {
    val matchingLcContexts = lcContexts.zipWithIndex.filter { case (lcCtx,idx) =>
      val scanId = lcCtx.getScanId
      scanId >= minLcContext.getScanId && scanId <= maxLcContext.getScanId
    }
    if( matchingLcContexts.length < 2 ) return None
    
    val firstIdx = matchingLcContexts.head._2
    val lastBoundary = matchingLcContexts.last._2 + 1
    
    val peakel = Peakel(
      lcContexts = lcContexts.slice(firstIdx, lastBoundary),
      mzValues = mzValues.slice(firstIdx, lastBoundary),
      intensityValues = intensityValues.slice(firstIdx, lastBoundary),
      leftHwhmValues = if( leftHwhmValues == null ) null else leftHwhmValues.slice(firstIdx, lastBoundary),
      rightHwhmValues = if( rightHwhmValues == null ) null else rightHwhmValues.slice(firstIdx, lastBoundary)
    )
    
    Some( peakel )
  }
  
  def toPeaks(): Array[Peak] = {
    val cursor = new PeakelCursor(this)
    
    val peaks = new Array[Peak](this.lcContexts.length)
    while( cursor.next() ) {
      peaks(cursor.peakIndex) = cursor.toPeak()
    }
    
    peaks
  }
  
}

case class PeakelCursor(
  peakel: Peakel,
  var peakIndex: Int = - 1
) {
  
  def next(): Boolean = {
    if( peakIndex == peakel.lcContexts.length - 1 ) false
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
  
  def getLcContext(): ILcContext = peakel.lcContexts(peakIndex)
  def getElutionTime(): Float = this.getLcContext().getElutionTime()
  def getMz(): Double = peakel.mzValues(peakIndex)
  def getIntensity(): Float = peakel.intensityValues(peakIndex)
  def getLeftHwhm(): Option[Float] = Option( peakel.leftHwhmValues ).map( _(peakIndex) )
  def getRightHwhm(): Option[Float] = Option( peakel.rightHwhmValues ).map( _(peakIndex) )
  
  def toPeak(): Peak = {
    new Peak( getMz, getIntensity, getLeftHwhm.getOrElse(0f), getRightHwhm.getOrElse(0f), getLcContext )
  }
}