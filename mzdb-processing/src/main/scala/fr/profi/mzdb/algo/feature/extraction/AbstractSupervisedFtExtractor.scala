package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._

import fr.profi.mzdb.algo.signal.detection.AbstractWaveletPeakelFinder
import fr.profi.mzdb.model._
import fr.profi.mzdb.utils.ms.MsUtils
import FeatureExtractionUtils._

abstract class AbstractSupervisedFtExtractor() extends AbstractFeatureExtractor {
  
  // Required attributes
  val xtractConfig: FeatureExtractorConfig
  val peakelDetectionConfig: PeakelDetectionConfig
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
  
  val overlappingFeaturesExtractor = new OverlappingFeaturesExtractor(
    this.scanHeaderById,
    Map(),
    this
  )
  
  /** Method to be implemented in concreted classes */
  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature]
  
  def searchAndExtractFeature(
    putativeFt: PutativeFeature,
    pklTree: PeakListTree,
    ftXtractConfig: FeatureExtractorConfig = this.xtractConfig,
    ftXtractAlgoConfig: PeakelDetectionConfig = this.peakelDetectionConfig
  ): Option[Feature] = {
    
    //unpack some config parameters
    val minConsecutiveScans = ftXtractConfig.minConsecutiveScans
    
    // Retrieve the scan header corresponding to the starting scan id
    val scanHeaderOpt = this.scanHeaderById.get(putativeFt.scanId)

    val scanHeader = scanHeaderOpt.get
    val ftTime = scanHeader.getElutionTime()

    val maxTheoreticalPeakelIndex = putativeFt.theoreticalIP.theoreticalMaxPeakelIndex

    // Extract isotopic patterns around the starting scan, by default extract a maxNbPeaks given by the averagine
    // ips never null
    val ips = this.extractIsotopicPatterns(putativeFt, maxTheoreticalPeakelIndex, pklTree, scanHeader, xtractConfig) 

    if (ips.isEmpty)
      return Option.empty[Feature]

    // Normalize the intensity of extracted IPs
    //this.normalizeIPs( ips )

    // Build a peakels, automatically remove empty peakels
    val peakels = Feature.buildPeakels(ips)

    if (peakels.isEmpty)
      return Option.empty[Feature]

    var tmpFt = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, peakels)
    
    // Find maxpeakelIndex
    val maxPeakelIndex = if (maxTheoreticalPeakelIndex < tmpFt.peakelsCount) maxTheoreticalPeakelIndex else 0

    // Get the defined peaks
    val maxPeakel = tmpFt.peakels(maxPeakelIndex)

    // Check definedPeaks length > 3 and peaks length >= 5
    if ( maxPeakel.isGoodForPeakDetection(minConsecutiveScans) == false )
      return Option.empty[Feature]

    //-------- REFINE PEAKEL OPTIONAL STEP --------
    if ( ftXtractAlgoConfig.refineDetection ) {

      val (peaks, definedPeaks) = (maxPeakel.peaks, maxPeakel.definedPeaks)

      // Detect peaks
      val peakelIndexes = findPeakelsIndices(definedPeaks, ftXtractAlgoConfig.detectionAlgorithm, ftXtractAlgoConfig.minSNR)

      // Treat matching Idx
      var matchingPeakIdx: (Int, Int) = null

      // Note if we are not good at peak extraction, no matching peak will be found, ms2 event outside of xic
      val filteredIndexes = peakelIndexes.filter(idx => ftTime >= definedPeaks(idx._1).getLcContext.getElutionTime &&
        ftTime <= definedPeaks(idx._2).getLcContext.getElutionTime)

      if (!filteredIndexes.isEmpty) {
        // Find the closest peakel in time domain of the ms2 event ?
        matchingPeakIdx = filteredIndexes.minBy { idx =>
          val apex = definedPeaks.slice(idx._1, math.min(idx._2, definedPeaks.length - 1)).maxBy(_.getIntensity)
          math.abs(ftTime - apex.getLcContext.getElutionTime)
        }
      }

      // If not matching peaks
      if (matchingPeakIdx == null)
        return Option.empty[Feature]

      val ipsIndexes = (peaks.indexOf(definedPeaks(matchingPeakIdx._1)), peaks.indexOf(definedPeaks(matchingPeakIdx._2)))

      val ft = tmpFt.restrictToPeakelIdxRange(ipsIndexes)

      if (ft == null)
        return Option.empty[Feature]

      Some(ft)

    } else { Some(tmpFt)  }
  } //end extractFeature
  
  /**
   * extract all the isotopicPatterns of the XIC
   * consecutiveGap is increased when it fails to extract ip or ip.intensity < percentage of maxInt ip
   */
  protected def extractIsotopicPatterns(
    putativeFt: PutativeFeature, 
    maxTheoreticalPeakelIndex: Int, 
    pklTree: PeakListTree, 
    startingScanHeader: ScanHeader, 
    extractionConf : FeatureExtractorConfig
  ): Array[IsotopicPattern] = {
      
    // unpack some data
    val maxConsecutiveGaps = extractionConf.maxConsecutiveGaps
    val mzTolPPM = extractionConf.mzTolPPM
    val maxTimeWindow = extractionConf.maxTimeWindow
    
    val ips = new ListBuffer[IsotopicPattern]()

    val cycleNum = startingScanHeader.getCycle
    var apexTime = startingScanHeader.getTime

    val theoIP = putativeFt.theoreticalIP

    // Determine intensity ascendant direction
    val range = Pair(1, 10)
    var ascDirection = this.getIntensityAscendantDirection(putativeFt, pklTree, mzTolPPM, cycleNum, range, minNbPeaks = 1)
      
    // 1 => right, -1 => left
    if (ascDirection == 0) // if no signal found on both sides
      return ips.toArray

    // --- Extract isotopic patterns ---
    var curMaxIntensity = 0.0

    // Iterate until left and right directions have been analyzed
    var numOfAnalyzedDirections = 0

    while (numOfAnalyzedDirections < 2) {

      var timeOverRange = false
      var consecutiveGapCount = 0
      var cycleShift = 0

      // Stop extraction in this direction if we have too much gaps or if we exceed run time range
      breakable {
        while (consecutiveGapCount <= maxConsecutiveGaps && !timeOverRange) {

          // Decrease cycle shift if LEFT direction
          if (ascDirection == -1) {
            cycleShift -= 1
          }

          // Determine current cycle number
          val curCycleNum = cycleNum + cycleShift

          // Try to retrieve the scan id
          var curScanHOpt = Option.empty[ScanHeader]
          if (this.ms1ScanIdByCycleNum.contains(curCycleNum)) {
            // Retrieve the wanted scan header
            curScanHOpt = this.scanHeaderById.get(this.ms1ScanIdByCycleNum(curCycleNum))
          }

          if (curScanHOpt.isEmpty) {
            //if wrong scanID
            timeOverRange = true
            break
          } else {
            val curScanH = curScanHOpt.get
            val curTime = curScanH.getTime

            // check if total time does not exceed the provided threshold
            if (maxTimeWindow > 0 && math.abs(curTime - apexTime) > maxTimeWindow / 2) {
              timeOverRange = true
              break
            }

            //FIXME: Warning called is maxNbPeaksInIp = null !
            //Note:we extract at least until maxTheoreticalPeakelIndex to get not only the monoisotopic peak but further peakels (in case of high
            //mass). The idea is not to miss the highest elution peak
            val ipOpt = pklTree.extractIsotopicPattern(curScanH, theoIP, mzTolPPM, extractionConf.maxNbPeaksInIP, maxTheoreticalPeakelIndex)

            // Check if an isotopic pattern has been found
            if (ipOpt.isDefined) {
              val ip = ipOpt.get
              val intensity = ip.getIntensity()

              // Add the isotopic pattern to the list of extracted IPs
              if (ascDirection == 1)
                ips += ip // append IP
              else if (ascDirection == -1)
                ips.+=:(ip) // prepend IP

              // Analysis of the isotopic pattern intensity
              if (intensity > curMaxIntensity) {
                // Update information about the apex
                curMaxIntensity = intensity
                apexTime = curTime
              }

              // test intensity < intensityThreshold
              if (intensity == 0 || intensity < curMaxIntensity * extractionConf.minPercentageOfMaxInt) {
                consecutiveGapCount += 1
                if (consecutiveGapCount > maxConsecutiveGaps)
                  break
              } else {
                consecutiveGapCount = 0
              }

              //ip not defined
            } else {
              consecutiveGapCount += 1
              if (consecutiveGapCount > maxConsecutiveGaps)
                break
            }

            // Increase cycle shift if right direction
            if (ascDirection == 1)
              cycleShift += 1

          } // END OF ELSE
        } // END OF WHILE
      }
      ascDirection *= -1
      numOfAnalyzedDirections += 1
    }
    ips.toArray
  }
  
  protected def getIntensityAscendantDirection(
    putativeFt: PutativeFeature, 
    pklTree: PeakListTree, 
    mzTolPPM: Float, 
    cycleNum: Int, 
    range: Pair[Int, Int], 
    minNbPeaks: Int // minimm peak in IP to sum
  ): Int = {

    var (firstCycleNum, lastCycleNum) = (0, 0)
    // Left check
    firstCycleNum = cycleNum - range._2
    lastCycleNum = cycleNum - range._1
    val leftIntSum = this.integrateIntensity(putativeFt, pklTree, mzTolPPM, 
        firstCycleNum, lastCycleNum, minNbPeaks)

    // Right check
    firstCycleNum = cycleNum + range._1
    lastCycleNum = cycleNum + range._2
    val rightIntSum = this.integrateIntensity(putativeFt, pklTree, mzTolPPM, 
        firstCycleNum, lastCycleNum, minNbPeaks
    )
    // Determine the direction by comparing the summed intensities
    var ascDirection = 0
    if (leftIntSum >= 0 || rightIntSum >= 0) {
      if (rightIntSum > leftIntSum) {
        ascDirection = 1
      } else {
        ascDirection = -1
      }
    }
    ascDirection
  }
  
  protected def integrateIntensity(
    putativeFt: PutativeFeature, 
    pklTree: PeakListTree, 
    mzTolPPM: Float, 
    firstCycle: Int, 
    lastCycle: Int, 
    minNbPeaks: Int
  ): Double = {

    val theoIP = putativeFt.theoreticalIP
    var intensitySum = 0.0

    // Sum the forward isotopic profile intensities
    breakable {
      for (curCycleNum <- firstCycle to lastCycle) {

        if (this.ms1ScanIdByCycleNum.contains(curCycleNum) == false)
          break

        val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
        val curScanH = this.scanHeaderById(curScanId)

        val ip = pklTree.extractIsotopicPattern(curScanH, putativeFt.theoreticalIP, mzTolPPM, maxNbPeaksInIP = Some(2))

        if (ip.isDefined && ip.get.peaks.length >= minNbPeaks) {
          intensitySum += ip.get.intensity
        }
      }
    }

    intensitySum
  }

}