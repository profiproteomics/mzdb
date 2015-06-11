package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import fr.profi.mzdb.utils.misc.InMemoryIdGen

object Feature extends InMemoryIdGen {

  var nbPeakelsToIntegrate = 3

  def getPeakelsScanIds(indexedPeakels: Seq[(Peakel,Int)]): Array[Int] = {
    indexedPeakels.toArray.flatMap(_._1.scanInitialIds).distinct.sorted
  }

  /*def calcScanHeaderRange(peakels: Seq[Peakel]): Pair[ScanHeader, ScanHeader] = {

    var firstScanHeader: ScanHeader = null
    var lastScanHeader: ScanHeader = null

    val nbPeakels = peakels.length
    for (peakel <- peakels) {
      if (firstScanHeader == null || peakel.getFirstLcContext().getScanId < firstScanHeader.getId) {
        firstScanHeader = peakel.getFirstLcContext().asInstanceOf[ScanHeader]
      }
      if (lastScanHeader == null || peakel.getLastLcContext().getScanId > lastScanHeader.getId) {
        lastScanHeader = peakel.getLastLcContext.asInstanceOf[ScanHeader]
      }
    }

    (firstScanHeader, lastScanHeader)
  }
  */
  
  /**
   * Take independant peakels and return an array of aligned peakel.
   */
  /*def alignPeakels( unalignedPeakels: Array[Peakel] ): Array[Peakel] = {
    
    val distinctLcContexts = unalignedPeakels
      .flatMap( _.definedPeaks.map( _.getLcContext ) )
      .distinct
      .sortBy( _.getScanId )
    
    for( peakel <- unalignedPeakels ) yield {
      val peakByLcCtx = peakel.definedPeaks.map( p => p.getLcContext -> p ).toMap

      val newPeaks = distinctLcContexts.map( lcCtx => peakByLcCtx.getOrElse(lcCtx, null) )
      
      peakel.copy( peaks = newPeaks )
    }
        
  }*/
  
  def ipsToIndexedPeakels(ips: Seq[IsotopicPatternLike]): Array[(Peakel,Int)] = {
    for( (peakelBuilder,idx) <- this.ipsToIndexedPeakelBuilders(ips) )
      yield peakelBuilder.result() -> idx
  }

  def ipsToIndexedPeakelBuilders(ips: Seq[IsotopicPatternLike]): Array[(PeakelBuilder,Int)] = {

    // Determine the maximum number of peaks
    val maxNbPeaks = ips.map(_.peaks.length).max

    val peakelBuilders = new ArrayBuffer[(PeakelBuilder,Int)]()
    breakable {
      for (peakelIdx <- 0 until maxNbPeaks) {
        val peakelBuilderOpt = this._ipsToIndexedPeakelBuilders(ips, peakelIdx)
        
        if (peakelBuilderOpt.isDefined)
          peakelBuilders += peakelBuilderOpt.get
        else
          break
      }
    }
    
    peakelBuilders.toArray
  }

  protected def _ipsToIndexedPeakelBuilders(ips: Seq[IsotopicPatternLike], peakelIdx: Int): Option[(PeakelBuilder,Int)] = {

    val peaks = new ArrayBuffer[Peak]()

    for (ip <- ips) {
      if (peakelIdx < ip.peaks.length) {
        
        val peak = ip.peaks(peakelIdx)
        
        if (peak != null)
          peaks += peak
      }
    }

    if (peaks.isEmpty == false)
      Some( new PeakelBuilder(peaks.toArray) -> peakelIdx )
    else
      Option.empty[(PeakelBuilder,Int)]
  }

  def calcPeakelsAreaRatios(indexedPeakels: Seq[(Peakel,Int)]): Option[Array[Float]] = {

    val peakelsCount = indexedPeakels.length
    if (peakelsCount < 2) return Option.empty[Array[Float]]

    val peakelAreaRatios = new Array[Float](peakelsCount - 1)

    for (peakelIdx <- 1 until peakelsCount) {
      val curPeakel = indexedPeakels(peakelIdx)._1
      val prevPeakel = indexedPeakels(peakelIdx - 1)._1
      peakelAreaRatios(peakelIdx - 1) = curPeakel.area / prevPeakel.area
    }

    Some(peakelAreaRatios)
  }

  def sumPeakelsArea(indexedPeakels: Seq[(Peakel,Int)], maxNbPeakels: Int): Float = {
    require(maxNbPeakels > 0)
    val nbPeakels = indexedPeakels.length
    val cappedMaxNbPeakels = if (maxNbPeakels >= nbPeakels) nbPeakels else maxNbPeakels

    indexedPeakels.take(cappedMaxNbPeakels).foldLeft(0f) { (tmpArea, p) => tmpArea + p._1.area }
  }

}

case class Feature(
  @BeanProperty id: Int,
  @BeanProperty var mz: Double,
  @BeanProperty charge: Int,
  @BeanProperty indexedPeakels: Array[(Peakel,Int)],
  @BeanProperty isPredicted: Boolean = false,
  @BeanProperty var ms2ScanIds: Array[Int] = Array.empty[Int]
) extends ILcContext {

  def this(id: Int, mz: Double, charge: Int, isotopicPatterns: Seq[IsotopicPatternLike], isPredicted: Boolean ) = {
    this(id, mz, charge, Feature.ipsToIndexedPeakels(isotopicPatterns), isPredicted)
  }

  def this(mz: Double, charge: Int, isotopicPatterns: Seq[IsotopicPatternLike], isPredicted: Boolean ) = {
    this(Feature.generateNewId(), mz, charge, isotopicPatterns, isPredicted)
  }
  
  def this(mz: Double, charge: Int, peakels: Buffer[Peakel], isPredicted: Boolean ) = {
    this(id = Feature.generateNewId(), mz = mz, charge = charge, indexedPeakels = peakels.zipWithIndex.toArray, isPredicted = isPredicted)
  }

  private var _ftIntensitySum = 0.0
  private var _basePeakelIndex = 0

  // Private block to compute some feature properties
  {
    var _peakelIdx = 0
    var _maxIntensity = 0.0
    
    for ( (peakel,idx) <- indexedPeakels ) {
  
      val peakelIntensity = peakel.intensitySum
      if (_peakelIdx < Feature.nbPeakelsToIntegrate) {
        _ftIntensitySum += peakelIntensity
      }
  
      // Check if it is the most intense peakel at apex
      if (peakel.getApexIntensity > _maxIntensity) {
        _maxIntensity = peakel.getApexIntensity
        _basePeakelIndex = _peakelIdx
      }
  
      _peakelIdx += 1
    }
  }

  // Define some immutable attributes

  // Define some lazy attributes
  @BeanProperty lazy val area = Feature.sumPeakelsArea(this.indexedPeakels, Feature.nbPeakelsToIntegrate)
  @BeanProperty lazy val weightedAverageTime = getBasePeakel.calcWeightedAverageTime

  // Define some mutable attributes
  @BeanProperty var qualityProperties: FeatureQualityProperties = null
  @BeanProperty var overlapProperties: FeatureOverlapProperties = null
  @BeanProperty var hasMonoPeakel = true
  
  def calcPeakelsAreaRatios() = Feature.calcPeakelsAreaRatios(this.indexedPeakels)
  def calcDuration(): Float = {
    val minTime = indexedPeakels.map(_._1.elutionTimes.head).min
    val maxTime = indexedPeakels.map(_._1.elutionTimes.last).max
    
    maxTime - minTime
  }
  
  def getApexIndex() = getBasePeakel.apexIndex
  def getApexScanId() = getBasePeakel.getApexScanId()
  def getBasePeakel() = getPeakel(_basePeakelIndex)
  def getBasePeakelIndex() = _basePeakelIndex
  def getFirstPeakel() = indexedPeakels.head._1
  // TODO: use base or first peakel ???
  def getElutionTime() = getBasePeakel.getApexElutionTime()
  def getIntensitySum() = _ftIntensitySum
  def getMs1Count() = getScanIds.length
  def getMs2Count(): Int = if (ms2ScanIds != null) ms2ScanIds.length else 0
  def getPeakel( index: Int ) = indexedPeakels(index)._1
  def getPeakels() = indexedPeakels.map(_._1)
  def getPeakelsCount() = indexedPeakels.length
  def getScanIds() = Feature.getPeakelsScanIds(indexedPeakels)
  
  // ILcContext java interface implementation 
  def getScanId() : Int = getApexScanId()

  /*def getIsotopicPattern(idx: Int): IsotopicPattern = {
    val ipPeaks = peakels.map { _.peaks(idx) }

    // FIX BUG: at predicted time ft extractor
    //if (ipPeaks.forall(_ == null))
    //  return null

    val mz = if (ipPeaks(0) != null) ipPeaks(0).mz else this.mz

    val intensity = IsotopicPatternLike.sumPeakIntensities(ipPeaks, Feature.nbPeakelsToIntegrate)

    new IsotopicPattern(
      mz = mz,
      intensity = intensity,
      charge = this.charge,
      peaks = ipPeaks,
      scanHeader = scanHeaders(idx)
    )
  }

  def getIsotopicPatternAtApex(): IsotopicPattern = {
    getIsotopicPattern(apexIndex)
  }

  def getIsotopicPatterns(): Array[IsotopicPattern] = {
    val nbPeaks = this.peakels(0).peaks.length

    (0 until nbPeaks).map { this.getIsotopicPattern(_) } toArray
  }*/

  def getXIC(peakelIdx: Int): Tuple2[Array[Float], Array[Float]] = {
    require( peakelIdx >= 0 && peakelIdx < indexedPeakels.length, "peakelIdx is out of range")

    val peakel = this.indexedPeakels(peakelIdx)._1
    
    (peakel.elutionTimes, peakel.intensityValues)
  }

  // TODO: ask for a given number of peakels ???
  def getSummedXIC(): Tuple2[Array[Float], Array[Float]] = {

    val intSumByTime= new HashMap[Float,Float]()
    intSumByTime.sizeHint(this.getMs1Count)

    // Sum all peakel intensities
    for ( (peakel,idx) <- this.indexedPeakels) {
      for( (time,intensity) <- peakel.getElutionTimeIntensityPairs ) {
        intSumByTime.getOrElseUpdate(time, 0f)
        intSumByTime(time) += intensity
      }
    }
    
    // Build vectors
    val xValues = Array.fill(intSumByTime.size)(0f)
    val yValues = Array.fill(intSumByTime.size)(0f)
    
    var i = -1
    for( elutionTime <- intSumByTime.keys.toList.sorted ) {
      i += 1
      
      xValues(i) = elutionTime
      yValues(i) = intSumByTime(elutionTime)
    }

    (xValues, yValues)
  }

  override def toString(): String = {
    "" + this.mz + "@" + this.getElutionTime
  }
 
  
  /**
   * Creates a feature object from ips and averagine
   * @param minLcContext minimum LC context used to restrict the length of peakels
   * @param maxLcContext maximum LC context used to restrict the length of peakels
   * @retrun a new Feature or null if there are no peaks in the provided index range
   */
  /*def restrictToScanIdRange( minScanId: Int, maxScanId: Int ): Option[Feature] = {
    require(minScanId != maxScanId, "different scan ids must be provided")
    
    val restrictedPeakels = new ArrayBuffer[Peakel]()
    
    breakable {
      for ( (peakel,idx) <- this.indexedPeakels) {
        
        val slicedPeakelOpt = peakel.restrictToScanIdRange(minScanId, maxScanId)
        
        if ( slicedPeakelOpt.isDefined )
          restrictedPeakels += slicedPeakelOpt.get
        else
          break
      }
    }

    if (restrictedPeakels.isEmpty) return None

    Some(
      Feature(
        this.id,
        this.mz,
        this.charge,
        restrictedPeakels.toArray.zipWithIndex,
        isPredicted = this.isPredicted,
        ms2ScanIds = this.ms2ScanIds
      )
    )
  }*/
  
}

case class FeatureQualityProperties(
  @BeanProperty var qualityScore: Float = 0f,
  @BeanProperty var isGoodQuality: Boolean = true,
  @BeanProperty var meanPeakelCorrelation: Float = 0f //(float) FeatureScorer.computeMeanPeakelCorrelation(peakels);
)

case class FeatureOverlapProperties(
  @BeanProperty var overlapPMCC: Float = 0f,
  @BeanProperty var overlapRelativeFactor: Float = 0f,
  @BeanProperty var overlappingFeatures: Array[OverlappingFeature] = null,
  @BeanProperty var bestOverlappingFeature: OverlappingFeature = null
)
