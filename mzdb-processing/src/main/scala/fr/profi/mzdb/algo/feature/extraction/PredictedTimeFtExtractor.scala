package fr.profi.mzdb.algo.feature.extraction

import collection.mutable.HashMap
import util.control.Breaks._
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.IsotopicPattern
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.algo.signal.detection.CwtPeakel
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.utils.math.wavelet.Ridge
import fr.profi.mzdb.utils.math.wavelet.RidgesFinder
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder

/**
 * Try to select the best peakel in cross assignment
 * in order to extract with the ms2DrivenExtractor
 *
 *
 * Workflow
 *  Peakel detection using wavelet approaches
 *          then
 *  check if they are monoisotopic (overlapping test)
 *  keep only ones that are monoisotopic
 *          then
 *  compute apex deviation
 *  keep the longest path between monoisotopic through isotopic peakels
 *          then
 *  compute rmsd, keep the path with the best average rmsd
 *          then
 *  return the apex scanId of the best monoisotopic peakel, then perform
 *  extraction with ms2DrivenExtractor
 */
class PredictedTimeFtExtractor(
  override val scanHeaderById: Map[Int, ScanHeader],
  override val nfByScanId: Map[Int, Float],
  val minConsecutiveScans: Int = 4,
  val predictedTimeTol: Int = 90,
  val xtractConfig: FeatureExtractorConfig,
  val overlapXtractConfig: OverlappingFeatureExtractorConfig)
  extends AbstractSupervisedFtExtractor(xtractConfig, overlapXtractConfig)
  with IExtractorHelper {

  /**
   * use wavelet technique to dertermine starting point to extract
   */
  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    def getScanId(peak: Peak) = peak.getLcContext().getScanId()
    // Extract some vars
    val elutionTime = putativeFt.elutionTime

    //get the scanHeaders
    val curScanH = this.getScanHeaderForTime(elutionTime, 1)
    var leftmostScanH = this.getScanHeaderForTime(elutionTime - predictedTimeTol, 1)
    var rightmostScanH = this.getScanHeaderForTime(elutionTime + predictedTimeTol, 1)

    //checks scanHeaders
    if (leftmostScanH == null)
      leftmostScanH = this.scanHeaders.head

    if (rightmostScanH == null)
      rightmostScanH = this.scanHeaders.last

    if (leftmostScanH.getId == rightmostScanH.getId)
      return Option.empty[Feature]

    val scanIds = pklTree.scansIDs
    val ids = (for (i <- leftmostScanH.getId to rightmostScanH.getId) yield i) toArray
    val selectedScanIds = ids.filter(scanIds.contains(_))

    val maxTheoreticalPeakelIndex = this._getTheoriticalMaxPeakelIndex(putativeFt.theoreticalIP)

    val ips = selectedScanIds.map { id =>
      pklTree.extractIsotopicPattern(
        this.scanHeaderById(id),
        putativeFt.theoreticalIP,
        this.mzTolPPM,
        maxNbPeaksInIP = Some(3),
        maxTheoreticalPeakelIndex = maxTheoreticalPeakelIndex).orNull
    }
    val filteredIps = ips.filter(ip => ip != null && ip.peaks.count(_ != null) > 0) // FIXME: should never happen but still have a bug

    // --- FIXME: old implementation
    //val features = this._detectFeaturesFromExtractedIPs(putativeFt, filteredIps, maxTheoreticalPeakelIndex)

    val peakelsIndexes = this._getPeakelsIndexesFromExtractedIPs(putativeFt, filteredIps, maxTheoreticalPeakelIndex)

    val features = peakelsIndexes.map {
      case (minIdx, maxIdx) =>

        //inside map  
        val (realMinIdx, realMaxIdx) = (ips.indexOf(filteredIps(minIdx)), ips.indexOf(filteredIps(maxIdx)))
        //--- find apexIdx TODO can be retrieved by the waveletpeakelfinder
        val maxIP:IsotopicPattern = ips.slice(realMinIdx, realMaxIdx + 1).maxBy { ip =>
          if (ip.peaks(maxTheoreticalPeakelIndex) != null)
            ip.peaks(maxTheoreticalPeakelIndex).getIntensity
          else 0.0
        }
        
        val apexIdx = ips.indexOf(maxIP) 

        val selectedIPsIdx = new ArrayBuffer[Int]
        var consecutiveGaps: Int = 0
        //--- go threw the left first
        var i: Int = apexIdx - 1
        while (i >= realMinIdx && consecutiveGaps <= this.xtractConfig.maxConsecutiveGaps) {
          val currIP = ips(i)
          if (currIP == null) {
            consecutiveGaps += 1
          } else
            selectedIPsIdx.+=:(i)
        }
        //--- go threw the right second
        i = apexIdx + 1
        consecutiveGaps = 0
        while (i <= realMaxIdx && consecutiveGaps <= this.xtractConfig.maxConsecutiveGaps) {
          val currIP = ips(i)
          if (currIP == null) {
            consecutiveGaps += 1
          } else
            selectedIPsIdx += i
        }

        var ft: Feature = null
        if (selectedIPsIdx.isEmpty == false) {
          val f = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, selectedIPsIdx.map(ips(_))
            .filter(x => x != null && x.peaks.count(_ != null) > 0)
            .toArray)
          if (f.peakelsCount > 0 && f.peakels(0).peaks.length > 5)
            ft = f
        }
        ft // return ft
        
    }.filter(_ != null)

    //--- filter features
    this._getBestMatchingFeatures(putativeFt, features, pklTree)
  }

  def _getBestMatchingFeatures(putativeFt: PutativeFeature, features: Array[Feature], pklTree: PeakListTree): Option[Feature] = {

    val elutionTime = putativeFt.elutionTime
    val mz = if (putativeFt.mozs != null) putativeFt.mozs.sum / putativeFt.mozs.length else putativeFt.mz

    //val area = if (putativeFt.areas != null) putativeFt.areas.sum / putativeFt.areas.length else 0f
    //val minFold = area * 1.45 

    val charge = putativeFt.charge
    val (minDuration, maxDuration) = if (putativeFt.durations != null) (putativeFt.durations.min, putativeFt.durations.max)
    else (0f, 0f)
    if (features.isEmpty)
      return Option.empty[Feature]

    //check detected features contains a real monoisotope as first elution peak 
    val nonAmbiguousFeatures = features.filter { ft =>
      //val gapRespect = if (this._nbGapInMaxPeakelRespectful(ft, maxTheoreticalPeakelIndex, 2)) true else false
      val overlapStatus = this.overlappingFeaturesExtractor.extractOverlappingFeatures(ft, putativeFt.theoreticalIP, pklTree)
      (ft.isRelevant) //&& gapRespect)
    }

    if (nonAmbiguousFeatures.isEmpty)
      return Option.empty[Feature]

    // filter the feature based duration...     
    val durationFilteredFts = if (minDuration != 0f && maxDuration != 0f) {
      features.filter { f =>
        val duration = f.scanHeaders.last.getElutionTime - f.scanHeaders.head.getElutionTime
        duration >= minDuration - (minDuration * 0.5) && duration <= maxDuration + (maxDuration * 0.5)
      }
    } else nonAmbiguousFeatures

    if (durationFilteredFts.isEmpty)
      return Option.empty[Feature]

    //little cheat filter on intensity
    /*val variablesFts = if (area != 0 ) durationFilteredFts.filter{ f => f.area > area + minFold || f.area < area - minFold }
                         else  Array.empty[Feature]

      val intensityFilteredFts = if (variablesFts.isEmpty) durationFilteredFts else variablesFts*/

    //finally filter the feature based on the mass (since we have no information on intensity, duration...)
    val bestFeature = durationFilteredFts.sortBy(f => math.abs(f.getElutionTime - elutionTime)).slice(0, 3) // take the top 3 feature
      .minBy(f => math.abs(f.mz - mz))
    Some(bestFeature)

  }

  /**
   * ****************************************************************************************
   * UTILITY FUNCTIONS
   * ****************************************************************************************
   */
  def _detectFeaturesFromExtractedIPs(putativeFt: PutativeFeature,
                                      ips: Array[IsotopicPattern],
                                      maxPeakelIndex: Int): Array[Feature] = {
    // if no ip detected
    if (ips.isEmpty)
      return Array.empty[Feature]

    // build a tmpFt 
    val peakels = Feature.buildPeakels(ips)
    if (peakels.isEmpty)
      return Array.empty[Feature]
    val tmpFt = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, peakels)

    // determine maxrelative intensity peakel index
    val peakelIndex = if (maxPeakelIndex < tmpFt.peakelsCount) maxPeakelIndex else 0
    val maxIntensityPeakel = tmpFt.peakels(peakelIndex)

    // ensure peakel duration  is at least 5 scans
    if (this._isPeakelGoodForPeakDetection(maxIntensityPeakel) == false)
      return Array.empty[Feature]

    val (peaks, definedPeaks) = (maxIntensityPeakel.peaks, maxIntensityPeakel.definedPeaks)
    // launch peak detection
    val peakelIndexes = this._findPeakelsIndexes(definedPeaks, method = 2, minSNR = 1.0f)

    val detectedFts = new ArrayBuffer[Feature]

    peakelIndexes.foreach {
      case (minIdx, maxIdx) =>
        val ipsIndexes = (peaks.indexOf(definedPeaks(minIdx)), peaks.indexOf(definedPeaks(maxIdx)))
        val ft = this._buildFeatureFromIPsIdx(putativeFt, tmpFt, ipsIndexes)

        if (ft != null)
          detectedFts += ft
    }
    detectedFts.toArray
  }

  /**
   * Same as above but return an
   */
  def _getPeakelsIndexesFromExtractedIPs(putativeFt: PutativeFeature,
                                         ips: Array[IsotopicPattern],
                                         maxPeakelIndex: Int): Array[(Int, Int)] = {
    // if no ip detected
    if (ips.isEmpty)
      return Array.empty[(Int, Int)]

    // build a tmpFt 
    val peakels = Feature.buildPeakels(ips)
    if (peakels.isEmpty)
      return Array.empty[(Int, Int)]
    val tmpFt = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, peakels)

    // determine maxrelative intensity peakel index
    val peakelIndex = if (maxPeakelIndex < tmpFt.peakelsCount) maxPeakelIndex else 0
    val maxIntensityPeakel = tmpFt.peakels(peakelIndex)

    // ensure peakel duration  is at least 5 scans
    if (this._isPeakelGoodForPeakDetection(maxIntensityPeakel) == false)
      return Array.empty[(Int, Int)]

    val (peaks, definedPeaks) = (maxIntensityPeakel.peaks, maxIntensityPeakel.definedPeaks)
    // launch peak detection
    val peakelIndexes = this._findPeakelsIndexes(definedPeaks, method = 2, minSNR = 1.0f)
    peakelIndexes
  }

  /**
   *
   */
  def _forcePeakelExtraction(minMaxScanIds: Pair[Int, Int], peaks: Array[Peak]): Peakel = {
    //can be more accurate if we knew the putativeFt duration ?

    new Peakel(0, peaks.filter { p =>
      val scanId = p.getLcContext().getScanId()
      minMaxScanIds._1 <= scanId && minMaxScanIds._2 >= scanId
    })
  }
}
