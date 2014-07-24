package fr.profi.mzdb.model

import beans.BeanProperty
import fr.profi.mzdb.algo.feature.scoring.FeatureScorer
import fr.profi.util.math.linearInterpolation

object Peakel {
  
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
case class Peakel(
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
  
  def calcPeakelsIntersection( otherPeakel: Peakel ): Pair[Array[Peak],Array[Peak]] = {
    val thisPeakBytTime = this.peakByElutionTime
    val otherPeakByTime = otherPeakel.peakByElutionTime
    val intersectingTimes = (thisPeakBytTime.keys ++ otherPeakByTime.keys)
      .groupBy(e=>e)
      .withFilter(_._2.size == 2)
      .map(_._1)
      .toArray.sorted
    Pair( intersectingTimes.map( thisPeakBytTime(_) ), intersectingTimes.map( otherPeakByTime(_) ) )    
  }
  
  // TODO: remove from this class
  def computeCorrelationWith( otherPeakel: Peakel ): Double = {    
    FeatureScorer.calcPeakelCorrelation(this, otherPeakel)    
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
  
}