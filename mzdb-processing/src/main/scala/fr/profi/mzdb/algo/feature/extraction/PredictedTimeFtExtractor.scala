package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model._
import fr.profi.mzdb.algo.signal.detection._
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.utils.math.wavelet._
import FeatureExtractionUtils._

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
  val xtractConfig: FeatureExtractorConfig,
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
) extends AbstractSupervisedFtExtractor {

  /** use wavelet technique to dertermine starting point to extract */
  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    def getScanId(peak: Peak) = peak.getLcContext().getScanId()

    // Retrieve some vars
    val pftTime = putativeFt.elutionTime
    val predictedTimeTol = this.xtractConfig.predictedTimeTol

    // Get the scanHeaders
    val curScanHOpt = this.getScanHeaderForTime(pftTime, 1)
    val leftMostScanH = this.getScanHeaderForTime(pftTime - predictedTimeTol, 1).getOrElse(this.scanHeaders.head)
    val rightMostScanH = this.getScanHeaderForTime(pftTime + predictedTimeTol, 1).getOrElse(this.scanHeaders.last)

    // Checks scanHeaders
    if (leftMostScanH.getId == rightMostScanH.getId)
      return Option.empty[Feature]

    val scanIds = pklTree.scanIds
    val ids = (for (i <- leftMostScanH.getId to rightMostScanH.getId) yield i) toArray
    val selectedScanIds = ids.filter(scanIds.contains(_))

    val maxTheoreticalPeakelIndex = putativeFt.theoreticalIP.getTheoriticalMaxPeakelIndex()

    val ips = selectedScanIds.map { id =>
      pklTree.extractIsotopicPattern(
        this.scanHeaderById(id),
        putativeFt.theoreticalIP,
        xtractConfig.mzTolPPM,
        xtractConfig.maxNbPeaksInIP,
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
        val maxIP: IsotopicPattern = ips.slice(realMinIdx, realMaxIdx + 1).maxBy { ip =>
          if (ip == null)
            0.0
          else if (maxTheoreticalPeakelIndex < ip.peaks.length && ip.peaks(maxTheoreticalPeakelIndex) != null)
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
          i -= 1
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
          i += 1
        }

        var ft: Feature = null
        if (selectedIPsIdx.isEmpty == false) {
          val peakels = Feature.buildPeakels(selectedIPsIdx.map(ips(_)).filter(_ != null))

          if (peakels.isEmpty == false && peakels(0) != null) {
            val f = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, peakels)
            if (f.peakelsCount > 0 && f.peakels(0).peaks.length > 5)
              ft = f
          }
        }
        ft // return ft

    }.filter(_ != null) // remove null features

    //--- filter features
    this._getBestMatchingFeature(putativeFt, features, pklTree)
  }

  def _getBestMatchingFeature(putativeFt: PutativeFeature, features: Array[Feature], pklTree: PeakListTree): Option[Feature] = {

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
  def _detectFeaturesFromExtractedIPs(
    putativeFt: PutativeFeature,
    ips: Array[IsotopicPattern],
    maxPeakelIndex: Int
  ): Array[Feature] = {

    // Returns if no ip detected
    if (ips.isEmpty) return Array.empty[Feature]

    // Build a tmpFt 
    val peakels = Feature.buildPeakels(ips)
    if (peakels.isEmpty) return Array.empty[Feature]

    val tmpFt = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, peakels)

    // Determine maxrelative intensity peakel index
    val peakelIndex = if (maxPeakelIndex < tmpFt.peakelsCount) maxPeakelIndex else 0
    val maxIntensityPeakel = tmpFt.peakels(peakelIndex)

    // Ensure peakel duration  is at least 5 scans
    if (maxIntensityPeakel.isGoodForPeakDetection(this.xtractConfig.minConsecutiveScans) == false)
      return Array.empty[Feature]

    // Launch peak detection
    val (peaks, definedPeaks) = (maxIntensityPeakel.peaks, maxIntensityPeakel.definedPeaks)

    val peakelIndexes = findPeakelsIndexes(
      definedPeaks,
      xtractConfig.predictedTimeXtractConfig.detectionAlgorithm,
      xtractConfig.predictedTimeXtractConfig.minSNR
    )

    val detectedFts = new ArrayBuffer[Feature](peakelIndexes.length)

    for ((minIdx, maxIdx) <- peakelIndexes) {
      val ipsIndexes = (peaks.indexOf(definedPeaks(minIdx)), peaks.indexOf(definedPeaks(maxIdx)))
      val ft = tmpFt.restrictToPeakelIdxRange(ipsIndexes)

      if (ft != null) detectedFts += ft
    }

    detectedFts.toArray
  }

  /** Same as above but return an */
  def _getPeakelsIndexesFromExtractedIPs(
    putativeFt: PutativeFeature,
    ips: Array[IsotopicPattern],
    maxPeakelIndex: Int
  ): Array[(Int, Int)] = {
    
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
    if (maxIntensityPeakel.isGoodForPeakDetection(xtractConfig.minConsecutiveScans) == false)
      return Array.empty[(Int, Int)]

    val (peaks, definedPeaks) = (maxIntensityPeakel.peaks, maxIntensityPeakel.definedPeaks)
    // launch peak detection
    val peakelIndexes = findPeakelsIndexes(definedPeaks, xtractConfig.predictedTimeXtractConfig.detectionAlgorithm,
      xtractConfig.predictedTimeXtractConfig.minSNR)
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
