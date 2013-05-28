package fr.profi.mzdb.model

import reflect.BeanProperty
import fr.profi.mzdb.algo.feature.scoring.FeatureScorer

object Peakel {
  
  def calcPeaksIndexRange( peaks: Array[Option[Peak]] ): Pair[Int,Int] = {
    
    var firstLcContext: ILcContext = null
    var lastLcContext: ILcContext = null
    
    var firstIdx = -1
    var lastIdx = -1
    
    var idx = 0
    for( p <- peaks ) {
      if( firstIdx == -1 && p != None ) firstIdx = idx
      if( firstIdx >= 0 && lastIdx == -1 && p == None ) lastIdx = idx - 1
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
  @BeanProperty peaks: Array[Option[Peak]]
) {
  
  // Make some requirements
  require( peaks != null )
  
  // Define other Peakel attributes
  @BeanProperty val definedPeaks = for( p<- peaks if p != None) yield p.get
  @BeanProperty val definedPeaksIndexRange = Peakel.calcPeaksIndexRange( peaks )
  @BeanProperty val lcContextRange = Peakel.calcLcContextRange( definedPeaks )
  @BeanProperty val firstScanContext = lcContextRange._1
  @BeanProperty val lastScanContext = lcContextRange._2  
  @BeanProperty val apexIndex = peaks.indices.filter( peaks(_) != None ).reduce { (a,b) =>
                                  if( peaks(a).get.intensity > peaks(b).get.intensity ) a else b
                                }
  
  @BeanProperty val apexScanContext = peaks(apexIndex).get.lcContext
  @BeanProperty val mz = this.getApex().mz
  @BeanProperty var intensity = 0f
  @BeanProperty var area = 0f
  
  // Define lazy attributes
  @BeanProperty lazy val peakByElutionTime = definedPeaks.map { p => p.getLcContext.getElutionTime -> p } toMap
  @BeanProperty lazy val duration = lastScanContext.getElutionTime - firstScanContext.getElutionTime
  @BeanProperty var localMaxima : Array[Int] = null
  
  // Update feature intensity and area
  this._integratePeakel()
  
  
  
  protected def _integratePeakel() {
    
    // Search for the apex and integrate IPs
    var intensitySum = 0f
    var intensityArea = 0f
    var prevPeakTime = 0f
    var prevPeakIntensity = 0f
    
    val defPeaks = this.definedPeaks
    for( peakIndex <- 0 until defPeaks.length ) {

      val peak = defPeaks(peakIndex)
      intensitySum += peak.intensity
      
      val curPeakTime = peak.lcContext.getElutionTime
      
      if( peakIndex > 0 ) {
        val deltaTime = curPeakTime - prevPeakTime
        intensityArea += (peak.intensity + prevPeakIntensity ) * deltaTime / 2
      }
      
      prevPeakTime = curPeakTime
      prevPeakIntensity = peak.intensity
      
    }
    
    if( intensityArea == 0 ) intensityArea = intensitySum
    
    this.intensity = intensitySum
    this.area = intensityArea
    
  }  
  
  def getApex(): Peak = peaks(apexIndex).get
  
  def getIntensities(): Array[Float] = getDefinedPeaks.map { _.intensity }
  
  // TODO: remove from this class
  def computeCorrelationWith( otherPeakel: Peakel ): Double = {    
    FeatureScorer.calcPeakelCorrelation(this, otherPeakel)    
  }
  
}