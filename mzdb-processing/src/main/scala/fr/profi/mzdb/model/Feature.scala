package fr.profi.mzdb.model

import reflect.BeanProperty
import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import fr.profi.mzdb.utils.misc.InMemoryIdGen

object Feature extends InMemoryIdGen {
  
  var nbPeakelsToIntegrate = 2
  
  def getPeakelsScanHeaders( peakels: Seq[Peakel] ): Array[ScanHeader] = {
    
    var scanHeaders = new ArrayBuffer[ScanHeader]
    
    val nbScans = peakels(0).peaks.length
    for (scanIdx <- 0 until nbScans ) {
      val firstDefinedPeak = peakels.find( _.peaks(scanIdx) != None ).get.peaks(scanIdx).get
      scanHeaders += firstDefinedPeak.getLcContext.asInstanceOf[ScanHeader]
    }
    
    scanHeaders.toArray
  }
  
  def calcScanHeaderRange( peakels: Seq[Peakel] ): Pair[ScanHeader,ScanHeader] = {
    
    var firstScanHeader: ScanHeader = null
    var lastScanHeader: ScanHeader = null
    
    val nbPeakels = peakels.length
    for (peakel <- peakels) {
      if( firstScanHeader == null || peakel.firstScanContext.getScanId < firstScanHeader.getId ) {
        firstScanHeader = peakel.firstScanContext.asInstanceOf[ScanHeader]
      }
      if( lastScanHeader == null || peakel.lastScanContext.getScanId > lastScanHeader.getId ) {
        lastScanHeader = peakel.firstScanContext.asInstanceOf[ScanHeader]
      }
    }
    
    (firstScanHeader, lastScanHeader)
  }
  
  def buildPeakels( ips: Seq[IsotopicPatternLike] ): Array[Peakel] = {
    
    // Determine the maximum number of peaks
    var maxNbPeaks = 0
    for( ip <- ips ) {  
      val nbPeaks = ip.peaks.length
      if( nbPeaks > maxNbPeaks ) maxNbPeaks = nbPeaks
    }
  
    val peakels = new ArrayBuffer[Peakel](maxNbPeaks)
    
    for( peakelIdx <- 0 until maxNbPeaks ) {
      val peakelOpt = this._buildPeakel(ips, peakelIdx)
      if( peakelOpt.isDefined ) peakels += peakelOpt.get
    }

    peakels.toArray
  }

  protected def _buildPeakel( ips: Seq[IsotopicPatternLike], peakelIdx: Int ): Option[Peakel] = {
    
    val peaks = new ArrayBuffer[Option[Peak]]()
    var definedPeaksCount = 0
    
    for( ip <- ips ) {
      if( peakelIdx < ip.peaks.length ) {
        val peak = ip.peaks(peakelIdx)
        peaks += peak
        if( peak.isDefined ) definedPeaksCount += 1
      } else peaks += Option.empty[Peak]
    }
    
    if( definedPeaksCount > 0 ) Some( new Peakel( peakelIdx, peaks.toArray ) )
    else Option.empty[Peakel]
  }
  
  def calcPeakelsAreaRatios( peakels: Seq[Peakel] ): Option[Array[Float]] = {
    
    val peakelsCount = peakels.length
    if( peakelsCount < 2 ) return Option.empty[Array[Float]]

    val peakelAreaRatios = new Array[Float](peakelsCount-1)
    
    for( peakelIdx <- 1 until peakelsCount ) {
      val curPeakel = peakels(peakelIdx)
      val prevPeakel = peakels(peakelIdx-1)
      peakelAreaRatios(peakelIdx-1) = curPeakel.area/prevPeakel.area
    }

    Some( peakelAreaRatios )
  }
  
  def sumPeakelsArea( peakels: Seq[Peakel], maxNbPeakels: Int ): Float = {
    require( maxNbPeakels > 0 )
    val nbPeakels = peakels.length
    val cappedMaxNbPeakels = if( maxNbPeakels >= nbPeakels ) nbPeakels - 1 else maxNbPeakels
    
    peakels.take( cappedMaxNbPeakels )
           .foldLeft(0f) { (tmpArea,p) => tmpArea + p.area }
  }
  
}

case class Feature (
  @BeanProperty id: Int,
  @BeanProperty var mz: Double,
  @BeanProperty charge: Int,
  @BeanProperty peakels: Array[Peakel]
   ) {
  
  def this( id: Int, mz: Double, charge: Int, isotopicPatterns: Seq[IsotopicPatternLike] ) = {
    this( Feature.generateNewId(), mz, charge, Feature.buildPeakels(isotopicPatterns) )
  }
  
  def this( mz: Double, charge: Int, isotopicPatterns: Seq[IsotopicPatternLike] ) = {
    this( Feature.generateNewId(), mz, charge, isotopicPatterns )
  }
  
  // Require that all peakels have the same length
  var firstPeakelLength = peakels(0).peaks.length
  for ( peakel <- peakels ) {
    if( peakel.peaks.length != firstPeakelLength)
      throw new IllegalArgumentException("all peakels must have the same length")
  }
  
  // Compute some feature properties
  private var _ftIntensitySum = 0.0
  private var _maxIntensity = 0.0
  private var _basePeakelIndex = 0

  private var _peakelIdx = 0
  for( peakel <- peakels ) {
    
    val peakelIntensity = peakel.intensity
    if( _peakelIdx < Feature.nbPeakelsToIntegrate ) {
      _ftIntensitySum += peakelIntensity
    }

    // Check if it is the apex
    if( peakelIntensity > _maxIntensity ) {
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
  @BeanProperty var ms2ScanIds: Array[Int] = null
  @BeanProperty val intensitySum = _ftIntensitySum
  
  // Define some lazy attributes
  @BeanProperty lazy val peakelsAreaRatios = Feature.calcPeakelsAreaRatios( this.peakels )  
  @BeanProperty lazy val area = Feature.sumPeakelsArea( this.peakels, Feature.nbPeakelsToIntegrate )
  @BeanProperty lazy val elutionTime = this.apexScanHeader.time
  
  // Define some mutable attributes
  @BeanProperty var qualityScore = 0f
  @BeanProperty var meanPeakelCorrelation = 0f //(float) FeatureScorer.computeMeanPeakelCorrelation(peakels);
  @BeanProperty var overlapPMCC = 0f
  @BeanProperty var overlapRelativeFactor = 0f
  @BeanProperty var overlappingFeatures : Array[Feature] = null
  @BeanProperty var bestOverlappingFeature: Feature = null
  //@BeanProperty var filteredXIC: Chromatogram = null // x-axis = time ; y-axis = IP intensities
  

  def getMs2Count(): Int = {
    if( ms2ScanIds != null ) ms2ScanIds.length else 0
  }
  
  def getIsotopicPattern( idx: Int ): IsotopicPattern = {
    val ipPeaks = peakels.map { _.peaks(idx) }
    val mz = if( ipPeaks(0) != None ) ipPeaks(0).get.mz else this.mz
    val intensity = IsotopicPatternLike.sumPeakIntensities(ipPeaks, Feature.nbPeakelsToIntegrate)
    
    new IsotopicPattern(
          mz = mz,
          intensity = intensity,
          charge = this.charge,
          peaks = ipPeaks,
          scanHeader = scanHeaders(idx),
          null,
          0f
         )
  }
  
  def getIsotopicPatternAtApex(): IsotopicPattern = {
    getIsotopicPattern( apexIndex )
  }
  
  def getIsotopicPatterns(): Array[IsotopicPattern] = {
    val nbPeaks = this.peakels(0).peaks.length
    
    ( 0 until nbPeaks ).map { this.getIsotopicPattern(_) } toArray
  }
  
  def getXIC( peakelIdx: Int ): Tuple2[Array[Float], Array[Float]] = {
    
    val peaks = this.peakels(peakelIdx).definedPeaks
    val xValues = new Array[Float](peaks.length)
    val yValues = new Array[Float](peaks.length)
    
    for( idx <- 0 until peaks.length ) {
      val peak = peaks(idx)
      xValues(idx) = peak.getLcContext.getElutionTime
      yValues(idx) = peak.getIntensity()
    }
    
    (xValues,yValues)
  }
  
  def getSummedXIC(): Tuple2[Array[Float], Array[Float]] = {
    
    val nbPeaks = this.peakels(0).peaks.length
    val xValues = Array.fill(nbPeaks)(0f)
    val yValues = Array.fill(nbPeaks)(0f)
    
    for( peakel <- this.peakels ) {
      val peaks = peakel.peaks
      for( idx <- 0 until nbPeaks ) {
        val peak = peaks(idx)
        if( peak != None ) {
          xValues(idx) = peak.get.getLcContext.getElutionTime
          yValues(idx) += peak.get.getIntensity()
        }
      }
    }
    
    (xValues,yValues)
  }
  
  
  override def toString() : String = {
    "" + this.mz + "/" + this.elutionTime
  }
  //def eachIsotopicPattern
  
  // TODO: use Savitzky-Golay filter to produce a smoothed XIC,
  // then try to deconvolute signal if overlapping features
  // Return a raw XIC if not enough peaks
  //def computeFilteredXIC() { throw new Exception("NYI") }
  
}