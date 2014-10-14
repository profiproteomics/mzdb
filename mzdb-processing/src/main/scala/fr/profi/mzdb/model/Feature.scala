package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import fr.profi.mzdb.utils.misc.InMemoryIdGen

object Feature extends InMemoryIdGen {

  var nbPeakelsToIntegrate = 3

  def getPeakelsScanHeaders(indexedPeakels: Seq[(Peakel,Int)]): Array[ScanHeader] = {
    indexedPeakels.toArray.flatMap(_._1.lcContexts.map(_.asInstanceOf[ScanHeader])).distinct.sortBy(_.getScanId)
  }

  def calcScanHeaderRange(peakels: Seq[Peakel]): Pair[ScanHeader, ScanHeader] = {

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

  def buildIndexedPeakels(ips: Seq[IsotopicPatternLike]): Array[(Peakel,Int)] = {

    // Determine the maximum number of peaks
    val maxNbPeaks = ips.map(_.peaks.length).max

    val peakels = new ArrayBuffer[(Peakel,Int)]()
    breakable {
      for (peakelIdx <- 0 until maxNbPeaks) {
        val peakelOpt = this._buildIndexedPeakel(ips, peakelIdx)
        
        if (peakelOpt.isDefined)
          peakels += peakelOpt.get
        else
          break
      }
    }
    
    peakels.toArray
  }

  protected def _buildIndexedPeakel(ips: Seq[IsotopicPatternLike], peakelIdx: Int): Option[(Peakel,Int)] = {

    val peaks = new ArrayBuffer[Peak]()

    for (ip <- ips) {
      if (peakelIdx < ip.peaks.length) {
        
        val peak = ip.peaks(peakelIdx)
        
        if (peak != null)
          peaks += peak
      }
    }

    if (peaks.isEmpty == false)    
      Some( new Peakel(peaks.toArray) -> peakelIdx )
    else
      Option.empty[(Peakel,Int)]
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
    this(id, mz, charge, Feature.buildIndexedPeakels(isotopicPatterns), isPredicted)
  }

  def this(mz: Double, charge: Int, isotopicPatterns: Seq[IsotopicPatternLike], isPredicted: Boolean ) = {
    this(Feature.generateNewId(), mz, charge, isotopicPatterns, isPredicted)
  }

  // Require that all peakels have the same length
  //val firstPeakelLength = peakels(0).lcContexts.length
  /*for (peakel <- peakels) {
    if (peakel.lcContexts.length != firstPeakelLength)
      throw new IllegalArgumentException("all peakels must have the same length")
  }*/

  // Compute some feature properties
  private var _ftIntensitySum = 0.0
  private var _basePeakelIndex = 0

  // Private block to compute some feature properties
  {
    var _peakelIdx = 0
    var _maxIntensity = 0.0
    
    for ( (peakel,idx) <- indexedPeakels ) {
  
      val peakelIntensity = peakel.intensity
      if (_peakelIdx < Feature.nbPeakelsToIntegrate) {
        _ftIntensitySum += peakelIntensity
      }
  
      // Check if it is the apex
      if (peakelIntensity > _maxIntensity) {
        _maxIntensity = peakelIntensity
        _basePeakelIndex = _peakelIdx
      }
  
      _peakelIdx += 1
    }
  }

  // Define some immutable attributes  
  @BeanProperty val elutionTime = getBasePeakel.getApexLcContext.getElutionTime()

  // Define some lazy attributes
  @BeanProperty lazy val peakelsAreaRatios = Feature.calcPeakelsAreaRatios(this.indexedPeakels)
  @BeanProperty lazy val area = Feature.sumPeakelsArea(this.indexedPeakels, Feature.nbPeakelsToIntegrate)
  @BeanProperty lazy val weightedAverageTime = getBasePeakel.calcWeightedAverageTime

  // Define some mutable attributes
  @BeanProperty var hasMonoPeakel = true
  
  // TODO: put all these params in a FeatureQuality case class
  @BeanProperty var qualityScore = 0f
  @BeanProperty var isGoodQuality = true  
  @BeanProperty var meanPeakelCorrelation = 0f //(float) FeatureScorer.computeMeanPeakelCorrelation(peakels);
  @BeanProperty var overlapPMCC = 0f
  @BeanProperty var overlapRelativeFactor = 0f
  @BeanProperty var overlappingFeatures: Array[OverlappingFeature] = null
  @BeanProperty var bestOverlappingFeature: OverlappingFeature = null
  //debug purposes
  //@BeanProperty var parentXIC: Peakel = null
  //@BeanProperty var filteredXIC: Chromatogram = null // x-axis = time ; y-axis = IP intensities
  
  def getApexIndex = getBasePeakel.apexIndex
  def getApexScanHeader() = getBasePeakel.getApexLcContext.asInstanceOf[ScanHeader]
  def getBasePeakel() = getPeakel(_basePeakelIndex)
  def getBasePeakelIndex() = _basePeakelIndex
  def getFirstPeakel() = indexedPeakels.head._1
  def getIntensitySum() = _ftIntensitySum
  def getMs1Count() = getBasePeakel.lcContexts.length
  def getMs2Count(): Int = if (ms2ScanIds != null) ms2ScanIds.length else 0
  def getPeakel( index: Int ) = indexedPeakels(index)._1
  def getPeakels() = indexedPeakels.map(_._1)
  def getPeakelsCount() = indexedPeakels.length
  def getScanHeaders() = Feature.getPeakelsScanHeaders(indexedPeakels)
  
  // ILcContext java interface implementation 
  def getScanId() : Int = { getApexScanHeader().getScanId() }

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
    
    (peakel.getElutionTimes, peakel.intensityValues)
  }

  // TODO: ask for a given number of peakels ???
  def getSummedXIC(): Tuple2[Array[Float], Array[Float]] = {

    val intSumByLcContext = new HashMap[ILcContext,Float]()
    intSumByLcContext.sizeHint(this.getMs1Count)

    // Sum all peakel intensities
    for ( (peakel,idx) <- this.indexedPeakels) {      
      for( (lcContext,intensity) <- peakel.getLcContextIntensityPairs ) {
        intSumByLcContext.getOrElseUpdate(lcContext, 0f)
        intSumByLcContext(lcContext) += intensity
      }
    }
    
    // Build vectors
    val xValues = Array.fill(intSumByLcContext.size)(0f)
    val yValues = Array.fill(intSumByLcContext.size)(0f)
    
    var i = -1
    for( lcContext <- intSumByLcContext.keys.toList.sortBy(_.getScanId) ) {
      i += 1
      
      xValues(i) = lcContext.getElutionTime
      yValues(i) = intSumByLcContext(lcContext)
    }

    (xValues, yValues)
  }

  override def toString(): String = {
    "" + this.mz + "/" + this.elutionTime
  }
  
  /**
   * Creates a feature object from ips and averagine
   * @param minLcContext minimum LC context used to restrict the length of peakels
   * @param maxLcContext maximum LC context used to restrict the length of peakels
   * @retrun a new Feature or null if there are no peaks in the provided index range
   */
  def restrictToLcContextRange( minLcContext: ILcContext, maxLcContext: ILcContext ): Option[Feature] = {
    require(minLcContext != maxLcContext, "different LC contexts must be provided")
    
    val restrictedPeakels = new ArrayBuffer[Peakel]()
    
    breakable {
      for ( (peakel,idx) <- this.indexedPeakels) {
        
        val slicedPeakelOpt = peakel.restrictToLcContextRange(minLcContext, maxLcContext)
        
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
  }
  
}