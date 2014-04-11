package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import fr.profi.mzdb.utils.misc.InMemoryIdGen
import fr.profi.mzdb.utils.ms.IsotopicPatternLookup

object Feature extends InMemoryIdGen {

  var nbPeakelsToIntegrate = 3

  def getPeakelsScanHeaders(peakels: Seq[Peakel]): Array[ScanHeader] = {

    val scanHeaders = new ArrayBuffer[ScanHeader]

    val nbScans = peakels(0).peaks.length

    for (scanIdx <- 0 until nbScans) {
      val firstDefinedPeak = peakels.find(_.peaks(scanIdx) != null)
      if (firstDefinedPeak.isDefined)
        scanHeaders += firstDefinedPeak.get.peaks(scanIdx).getLcContext.asInstanceOf[ScanHeader]
    }

    scanHeaders.toArray
  }

  def calcScanHeaderRange(peakels: Seq[Peakel]): Pair[ScanHeader, ScanHeader] = {

    var firstScanHeader: ScanHeader = null
    var lastScanHeader: ScanHeader = null

    val nbPeakels = peakels.length
    for (peakel <- peakels) {
      if (firstScanHeader == null || peakel.firstScanContext.getScanId < firstScanHeader.getId) {
        firstScanHeader = peakel.firstScanContext.asInstanceOf[ScanHeader]
      }
      if (lastScanHeader == null || peakel.lastScanContext.getScanId > lastScanHeader.getId) {
        lastScanHeader = peakel.firstScanContext.asInstanceOf[ScanHeader]
      }
    }

    (firstScanHeader, lastScanHeader)
  }

  def buildPeakels(ips: Seq[IsotopicPatternLike]): Array[Peakel] = {

    // Determine the maximum number of peaks
    val maxNbPeaks = ips.map(_.peaks.length).max

    val peakels = new ArrayBuffer[Peakel]()
    breakable {
      for (peakelIdx <- 0 until maxNbPeaks) {
        val peakelOpt = this._buildPeakel(ips, peakelIdx)
        
        if (peakelOpt.isDefined)
          peakels += peakelOpt.get
        else
          break
      }
    }
    peakels.toArray
  }

  protected def _buildPeakel(ips: Seq[IsotopicPatternLike], peakelIdx: Int): Option[Peakel] = {

    val peaks = new ArrayBuffer[Peak]()
    var definedPeaksCount = 0

    for (ip <- ips) {
      if (peakelIdx < ip.peaks.length) {
        
        val peak = ip.peaks(peakelIdx)
        peaks += peak
        
        if (peak != null) definedPeaksCount += 1
        
      } else peaks += null
    }

    if (definedPeaksCount > 0)
      Some(new Peakel(peakelIdx, peaks.toArray))
    else
      Option.empty[Peakel]
  }

  def calcPeakelsAreaRatios(peakels: Seq[Peakel]): Option[Array[Float]] = {

    val peakelsCount = peakels.length
    if (peakelsCount < 2) return Option.empty[Array[Float]]

    val peakelAreaRatios = new Array[Float](peakelsCount - 1)

    for (peakelIdx <- 1 until peakelsCount) {
      val curPeakel = peakels(peakelIdx)
      val prevPeakel = peakels(peakelIdx - 1)
      peakelAreaRatios(peakelIdx - 1) = curPeakel.area / prevPeakel.area
    }

    Some(peakelAreaRatios)
  }

  def sumPeakelsArea(peakels: Seq[Peakel], maxNbPeakels: Int): Float = {
    require(maxNbPeakels > 0)
    val nbPeakels = peakels.length
    val cappedMaxNbPeakels = if (maxNbPeakels >= nbPeakels) nbPeakels else maxNbPeakels

    peakels.take(cappedMaxNbPeakels).foldLeft(0f) { (tmpArea, p) => tmpArea + p.area }
  }

}

case class Feature(
  @BeanProperty id: Int,
  @BeanProperty var mz: Double,
  @BeanProperty charge: Int,
  @BeanProperty peakels: Array[Peakel]
) {

  def this(id: Int, mz: Double, charge: Int, isotopicPatterns: Seq[IsotopicPatternLike]) = {
    this(id, mz, charge, Feature.buildPeakels(isotopicPatterns))
  }

  def this(mz: Double, charge: Int, isotopicPatterns: Seq[IsotopicPatternLike]) = {
    this(Feature.generateNewId(), mz, charge, isotopicPatterns)
  }

  // Require that all peakels have the same length
  var firstPeakelLength = peakels(0).peaks.length
  for (peakel <- peakels) {
    if (peakel.peaks.length != firstPeakelLength)
      throw new IllegalArgumentException("all peakels must have the same length")
  }

  // Compute some feature properties
  private var _ftIntensitySum = 0.0
  private var _maxIntensity = 0.0
  private var _basePeakelIndex = 0

  private var _peakelIdx = 0
  for (peakel <- peakels) {

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

  // Define some immutable attributes
  @BeanProperty val scanHeaders = Feature.getPeakelsScanHeaders(peakels)
  @BeanProperty val peakelsCount = peakels.length
  @BeanProperty val basePeakelIndex = _basePeakelIndex
  @BeanProperty val basePeakel = peakels(_basePeakelIndex)
  @BeanProperty val apexIndex = basePeakel.apexIndex
  @BeanProperty val apexScanHeader = basePeakel.getApex.getLcContext.asInstanceOf[ScanHeader]
  @BeanProperty val ms1Count = basePeakel.peaks.length
  @BeanProperty var ms2ScanIds: Array[Int] = Array.empty[Int]
  @BeanProperty val intensitySum = _ftIntensitySum

  // Define some lazy attributes
  @BeanProperty lazy val peakelsAreaRatios = Feature.calcPeakelsAreaRatios(this.peakels)
  @BeanProperty lazy val area = Feature.sumPeakelsArea(this.peakels, Feature.nbPeakelsToIntegrate)
  @BeanProperty lazy val elutionTime = this.apexScanHeader.time

  // Define some mutable attributes
  @BeanProperty var qualityScore = 0f
  @BeanProperty var isGoodQuality = true
  @BeanProperty var hasMonoPeakel = true
  @BeanProperty var meanPeakelCorrelation = 0f //(float) FeatureScorer.computeMeanPeakelCorrelation(peakels);
  @BeanProperty var overlapPMCC = 0f
  @BeanProperty var overlapRelativeFactor = 0f
  @BeanProperty var overlappingFeatures: Array[OverlappingFeature] = null
  @BeanProperty var bestOverlappingFeature: OverlappingFeature = null
  //debug purposes
  @BeanProperty var parentXIC: Peakel = null

  //@BeanProperty var filteredXIC: Chromatogram = null // x-axis = time ; y-axis = IP intensities

  def getMs2Count(): Int = {
    if (ms2ScanIds != null) ms2ScanIds.length else 0
  }

  def getIsotopicPattern(idx: Int): IsotopicPattern = {
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
  }

  def getXIC(peakelIdx: Int): Tuple2[Array[Float], Array[Float]] = {

    val peaks = this.peakels(peakelIdx).definedPeaks
    val xValues = new Array[Float](peaks.length)
    val yValues = new Array[Float](peaks.length)

    for (idx <- 0 until peaks.length) {
      val peak = peaks(idx)
      xValues(idx) = peak.getLcContext.getElutionTime
      yValues(idx) = peak.getIntensity()
    }

    (xValues, yValues)
  }

  def getSummedXIC(nbPeaksToSum: Int = this.peakels(0).peaks.length): Tuple2[Array[Float], Array[Float]] = {

    //val nbPeaksToSum = this.peakels(0).peaks.length
    val xValues = Array.fill(nbPeaksToSum)(0f)
    val yValues = Array.fill(nbPeaksToSum)(0f)

    for (peakel <- this.peakels) {
      val peaks = peakel.peaks
      for (idx <- 0 until nbPeaksToSum) {
        val peak = peaks(idx)
        if (peak != null) {
          xValues(idx) = peak.getLcContext.getElutionTime
          yValues(idx) += peak.getIntensity()
        }
      }
    }

    (xValues, yValues)
  }

  override def toString(): String = {
    "" + this.mz + "/" + this.elutionTime
  }
  //def eachIsotopicPattern

  // TODO: use Savitzky-Golay filter to produce a smoothed XIC,
  // then try to deconvolute signal if overlapping features
  // Return a raw XIC if not enough peaks
  //def computeFilteredXIC() { throw new Exception("NYI") }
  
  /**
   * Creates a feature object from ips and averagine
   * @param peakelIdxRange a range of peakel indexes used to restrict the number of peakels
   * @retrun a new Feature or null if there are no peaks in the provided index range
   */
  def restrictToPeakelIdxRange(
    peakelIdxRange: Pair[Int, Int]
  ): Feature = {
    require(peakelIdxRange._1 != peakelIdxRange._2)
    
    val (minIdx, maxIdx) = (peakelIdxRange._1, peakelIdxRange._2)
    val peakels = new ArrayBuffer[Peakel]()
    
    breakable {
      for (peakel <- this.peakels) {
        
        val idx = peakel.index        
        val peaks = peakel.peaks.slice(minIdx, maxIdx + 1)
        
        if (peaks.count(_ != null) > 0)
          peakels += peakel.copy(peaks = peaks)
        else
          break
      }
    }

    if (peakels.isEmpty || peakels.forall(_ == null) == true)
      return null

    new Feature(this.id, this.mz, this.charge, peakels.toArray)
  }

}