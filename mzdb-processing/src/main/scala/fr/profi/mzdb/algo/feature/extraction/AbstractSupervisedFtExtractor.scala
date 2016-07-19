package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LongMap
import scala.util.control.Breaks._

import fr.profi.mzdb.algo.signal.detection.AbstractWaveletPeakelFinder
import fr.profi.mzdb.model._
import fr.profi.mzdb.util.ms.MsUtils
import FeatureExtractionUtils._

abstract class AbstractSupervisedFtExtractor() extends AbstractFeatureExtractor {
  
  // Required attributes
  val xtractConfig: FeatureExtractorConfig
  val peakelDetectionConfig: PeakelDetectionConfig
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
  
  val overlappingFeaturesExtractor = new OverlappingFeaturesExtractor(
    this.spectrumHeaderById,
    LongMap.empty[Float],
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
    val minConsecutiveSpectra = ftXtractConfig.minConsecutiveSpectra
    
    // Retrieve the spectrum header corresponding to the starting spectrum id
    val spectrumHeaderOpt = this.spectrumHeaderById.get(putativeFt.spectrumId)

    val spectrumHeader = spectrumHeaderOpt.get
    val ftTime = spectrumHeader.getElutionTime()

    val maxTheoreticalPeakelIndex = putativeFt.theoreticalIP.theoreticalMaxPeakelIndex

    // Extract isotopic patterns around the starting spectrum, by default extract a maxNbPeaks given by the averagine
    // ips never null
    val ips = this.extractIsotopicPatterns(putativeFt, maxTheoreticalPeakelIndex, pklTree, spectrumHeader, xtractConfig) 

    if (ips.isEmpty)
      return Option.empty[Feature]

    // Normalize the intensity of extracted IPs
    //this.normalizeIPs( ips )

    // Build a peakels, automatically remove empty peakels
    val indexedPeakelBuilders = Feature.ipsToIndexedPeakelBuilders(ips)

    if (indexedPeakelBuilders.isEmpty)
      return Option.empty[Feature]
    
    // Find maxpeakelIndex
    val maxPeakelIndex = if (maxTheoreticalPeakelIndex < indexedPeakelBuilders.length) maxTheoreticalPeakelIndex else 0

    // Get the defined peaks
    val maxPeakelBuilder = indexedPeakelBuilders(maxPeakelIndex)._1

    // Check definedPeaks length > 3 and peaks length >= 5
    if ( maxPeakelBuilder.hasEnoughPeaks(minConsecutiveSpectra) == false )
      return Option.empty[Feature]

    //-------- REFINE PEAKEL OPTIONAL STEP --------
    val newIndexedPeakelBuilders = if ( ftXtractAlgoConfig.refineDetection == false ) indexedPeakelBuilders
    else {
      
      // Detect peaks
      val peakelsIndices = findPeakelsIndices(
        maxPeakelBuilder,
        ftXtractAlgoConfig.detectionAlgorithm,
        ftXtractAlgoConfig.minSNR,
        spectrumHeaderById
      )
      val elutionTimes = maxPeakelBuilder.elutionTimes
      
      // Treat matching Idx
      var matchingPeakelIdxPair: (Int, Int) = null

      // Note if we are not good at peak extraction, no matching peak will be found, ms2 event outside of xic
      val filteredIndices = peakelsIndices.filter { idx => 
        ftTime >= elutionTimes(idx._1) &&
        ftTime <= elutionTimes(idx._2)
      }

      if (!filteredIndices.isEmpty) {
        val maxPeakelIndices = elutionTimes.indices
        
        // TODO: Find the closest peakel in time domain of the MS2 event ?
        matchingPeakelIdxPair = filteredIndices.minBy { idxPair =>
          
          // Search for the apex index
          val filteredMaxPeakelIndices = maxPeakelIndices.slice(idxPair._1, math.min(idxPair._2, peakelsIndices.length - 1) + 1)
          if( filteredMaxPeakelIndices.isEmpty ) {
            logger.error(s"can't retrieve apex using peakel indices: ${idxPair}")
            Float.MaxValue
          } else {
            val apexIdx = filteredMaxPeakelIndices.maxBy( maxPeakelBuilder.intensityValues(_) )
            
            // Compute the absolute time diff between feature and peakel apex
            math.abs(ftTime - maxPeakelBuilder.elutionTimes(apexIdx) )
          }
        }
      }

      // If not matching peaks
      if (matchingPeakelIdxPair == null)
        return Option.empty[Feature]

      // TODO: check what was the purpose of this
      //val ipsIndexes = (peaks.indexOf(definedPeaks(matchingPeakIdx._1)), peaks.indexOf(definedPeaks(matchingPeakIdx._2)))

      val maxPeakelSpectrumIds = maxPeakelBuilder.getSpectrumIds()
      
      val(firstPeakelIdx, lastPeakelIdx) = matchingPeakelIdxPair
      val(firstSpectrumId, lastSpectrumId) = (maxPeakelSpectrumIds(firstPeakelIdx), maxPeakelSpectrumIds(lastPeakelIdx) )
      
      this.restrictPeakelBuildersToSpectrumInitialIdRange(indexedPeakelBuilders, firstSpectrumId, lastSpectrumId)
    }
    
    if( newIndexedPeakelBuilders.isEmpty )
      return Option.empty[Feature]
    
    Some(
      new Feature(
        putativeFt.id,
        putativeFt.mz,
        putativeFt.charge,
        newIndexedPeakelBuilders.map( ipb => ipb._1.result() -> ipb._2 ),
        isPredicted = putativeFt.isPredicted
      )
    )
    
  } //end extractFeature
  
  /**
   * Creates a feature object from ips and averagine
   * @param minLcContext minimum LC context used to restrict the length of peakels
   * @param maxLcContext maximum LC context used to restrict the length of peakels
   * @retrun a new Feature or null if there are no peaks in the provided index range
   */
  protected def restrictPeakelBuildersToSpectrumInitialIdRange(
    indexedPeakelBuilders: Array[(PeakelBuilder, Int)],
    firstSpectrumId: Long,
    lastSpectrumId: Long
  ): Array[(fr.profi.mzdb.model.PeakelBuilder, Int)] = {
    val restrictedIndexedPeakels = new ArrayBuffer[(PeakelBuilder,Int)]()
    
    breakable {
      for ( (peakelBuilder,idx) <- indexedPeakelBuilders) {
        
        val slicedPeakelOpt = peakelBuilder.restrictToSpectrumIdRange(firstSpectrumId, lastSpectrumId)
        
        if ( slicedPeakelOpt.isDefined )
          restrictedIndexedPeakels += slicedPeakelOpt.get -> idx
        else
          break
      }
    }

    restrictedIndexedPeakels.toArray
  }
  
  
  /**
   * extract all the isotopicPatterns of the XIC
   * consecutiveGap is increased when it fails to extract ip or ip.intensity < percentage of maxInt ip
   */
  protected def extractIsotopicPatterns(
    putativeFt: PutativeFeature, 
    maxTheoreticalPeakelIndex: Int, 
    pklTree: PeakListTree, 
    startingSpectrumHeader: SpectrumHeader, 
    extractionConf : FeatureExtractorConfig
  ): Array[IsotopicPattern] = {
      
    // unpack some data
    val maxConsecutiveGaps = extractionConf.maxConsecutiveGaps
    val mzTolPPM = extractionConf.mzTolPPM
    val maxTimeWindow = extractionConf.maxTimeWindow
    
    val ips = new ListBuffer[IsotopicPattern]()

    val cycleNum = startingSpectrumHeader.getCycle
    var apexTime = startingSpectrumHeader.getTime

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

          // Try to retrieve the spectrum id
          var curSpectrumHOpt = Option.empty[SpectrumHeader]
          if (this.ms1SpectrumIdByCycleNum.contains(curCycleNum)) {
            // Retrieve the wanted spectrum header
            curSpectrumHOpt = this.spectrumHeaderById.get(this.ms1SpectrumIdByCycleNum(curCycleNum))
          }

          if (curSpectrumHOpt.isEmpty) {
            //if wrong spectrumID
            timeOverRange = true
            break
          } else {
            val curSpectrumH = curSpectrumHOpt.get
            val curTime = curSpectrumH.getTime

            // check if total time does not exceed the provided threshold
            if (maxTimeWindow > 0 && math.abs(curTime - apexTime) > maxTimeWindow / 2) {
              timeOverRange = true
              break
            }

            //FIXME: Warning called is maxNbPeaksInIp = null !
            //Note:we extract at least until maxTheoreticalPeakelIndex to get not only the monoisotopic peak but further peakels (in case of high
            //mass). The idea is not to miss the highest elution peak
            val ipOpt = pklTree.extractIsotopicPattern(curSpectrumH, theoIP, mzTolPPM, extractionConf.maxNbPeaksInIP, maxTheoreticalPeakelIndex)

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

        if (this.ms1SpectrumIdByCycleNum.contains(curCycleNum) == false)
          break

        val curSpectrumId = this.ms1SpectrumIdByCycleNum(curCycleNum)
        val curSpectrumH = this.spectrumHeaderById(curSpectrumId)

        val ip = pklTree.extractIsotopicPattern(curSpectrumH, putativeFt.theoreticalIP, mzTolPPM, maxNbPeaksInIP = Some(2))

        if (ip.isDefined && ip.get.peaks.length >= minNbPeaks) {
          intensitySum += ip.get.intensity
        }
      }
    }

    intensitySum
  }

}