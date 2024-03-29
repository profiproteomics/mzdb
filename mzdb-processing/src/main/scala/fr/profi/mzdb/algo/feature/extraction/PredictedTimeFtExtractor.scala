package fr.profi.mzdb.algo.feature.extraction

import fr.profi.ms.algo.IsotopePatternInterpolator
import fr.profi.mzdb.algo.feature.extraction.FeatureExtractionUtils._
import fr.profi.mzdb.model._

import scala.collection.mutable.{ArrayBuffer, LongMap}

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
 *  return the apex spectrumId of the best monoisotopic peakel, then perform
 *  extraction with ms2DrivenExtractor
 */
class PredictedTimeFtExtractor(
  override val spectrumHeaderById: LongMap[SpectrumHeader],
  override val nfBySpectrumId: LongMap[Float],
  val xtractConfig: FeatureExtractorConfig,
  val peakelDetectionConfig: PeakelDetectionConfig = PeakelDetectionConfig(DetectionAlgorithm.BASIC),
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
) extends AbstractSupervisedFtExtractor { //with ProgressComputing {
  
//  final case object STEP0 extends IProgressStepIdentity {
//    val stepDescription = "fake initialization step"
//  }
//  final case object STEP0b extends IProgressStepIdentity {
//    val stepDescription = "before before extraction step"
//  }
//  final case object STEP1 extends IProgressStepIdentity {
//    val stepDescription = "before extraction step"
//  }  
//  final case object STEP2 extends IProgressStepIdentity {
//    val stepDescription = "ExtractIsotopicPattern step"
//  }  
//  final case object STEP3 extends IProgressStepIdentity {
//    val stepDescription = "GetPeakelsIndexesFromExtractedIPs step"
//  }
//  final case object STEP4 extends IProgressStepIdentity {
//    val stepDescription = "Feature building step"
//  }
//  final case object STEP5 extends IProgressStepIdentity {
//    val stepDescription = "GetBestMatchingFeature step"
//  }
//  
//  trait PredictedTimeFtExtractorSequence extends IProgressPlanSequence
//  val progressPlan = ProgressPlan[PredictedTimeFtExtractorSequence](
//    name = "PredictedTimeFtExtractor progression",
//    steps = Seq(
//      ProgressStep( STEP0, maxCount = 1, weight = 1),
//      ProgressStep( STEP0b, maxCount = 1, weight = 1),
//      ProgressStep( STEP1, maxCount = 1, weight = 1),
//      ProgressStep( STEP2, maxCount = 1, weight = 1),
//      ProgressStep( STEP3, maxCount = 1, weight = 1),
//      ProgressStep( STEP4, maxCount = 1, weight = 1),
//      ProgressStep( STEP5, maxCount = 1, weight = 1)
//    )
//  )
  
  //for debug purpose
//  val printWriter = new PrintWriter(new File("timing_xics.txt"))
//  val printWriter2 = new PrintWriter(new File("timing_detection.txt"));
//  
//  def close() = {
//    printWriter.close()
//    printWriter2.close()
//  }
  
  /** use wavelet technique to dertermine starting point to extract */
  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {
    //progressPlan( STEP0 ).incrementAndGetCount(1)

    // Retrieve some vars
    val pftTime = putativeFt.elutionTime
    val predictedTimeTol = this.xtractConfig.predictedTimeTol

    // Get the spectrumHeaders
    val curSpectrumHOpt = this.getSpectrumHeaderForTime(pftTime, 1)
    val leftMostSpectrumH = this.getSpectrumHeaderForTime(pftTime - predictedTimeTol, 1).getOrElse(this.spectrumHeaders.head)
    val rightMostSpectrumH = this.getSpectrumHeaderForTime(pftTime + predictedTimeTol, 1).getOrElse(this.spectrumHeaders.last)
    //progressPlan( STEP0b ).incrementAndGetCount(1)

    // Checks spectrumHeaders
    if (leftMostSpectrumH.getId == rightMostSpectrumH.getId)
      return Option.empty[Feature]

    val cycles = ( leftMostSpectrumH.getCycle() to rightMostSpectrumH.getCycle() ).toArray
    val selectedSpectrumIds = cycles.map( c => this.ms1SpectrumHeaderByCycleNum(c).getId )

    val maxTheoreticalPeakelIndex = putativeFt.theoreticalIP.theoreticalMaxPeakelIndex
    
    //progressPlan( STEP1 ).incrementAndGetCount(1)
    
    val ips = selectedSpectrumIds.map { id =>
      pklTree.extractIsotopicPattern(
        this.spectrumHeaderById(id),
        putativeFt.theoreticalIP,
        xtractConfig.mzTolPPM,
        xtractConfig.maxNbPeaksInIP,
        maxTheoreticalPeakelIndex = maxTheoreticalPeakelIndex).orNull
    }
    
    
    //progressPlan( STEP2 ).incrementAndGetCount(1)
    
    val filteredIps = ips.filter(ip => ip != null && ip.peaks.count(_ != null) > 0) // FIXME: should never happen but still have a bug

    // --- FIXME: old implementation
    //val features = this._detectFeaturesFromExtractedIPs(putativeFt, filteredIps, maxTheoreticalPeakelIndex)
    
    val peakelsIndexes = this._getPeakelsIndexesFromExtractedIPs(putativeFt, filteredIps, maxTheoreticalPeakelIndex)
    //progressPlan( STEP3 ).incrementAndGetCount(1)
    
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
        //--- go through the left first
        var i: Int = apexIdx - 1
        while (i >= realMinIdx && consecutiveGaps <= this.xtractConfig.maxConsecutiveGaps) {
          val currIP = ips(i)
          if (currIP == null) {
            consecutiveGaps += 1
          } else
            selectedIPsIdx.+=:(i)
          i -= 1
        }
        //--- go through the right second
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
          val peakels = Feature.ipsToIndexedPeakels(selectedIPsIdx.map(ips(_)).filter(_ != null))

          if (peakels.isEmpty == false && peakels(0) != null) {
            val f = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, peakels)
            // TODO: parameterize the min number of peakels count and LC contexts
            if (f.getPeakelsCount > 0 && f.getFirstPeakel().spectrumIds.length > 5)
              ft = f
          }
        }
        ft // return ft

    }.filter(_ != null) // remove null features
    //progressPlan( STEP4 ).incrementAndGetCount(1)
    

    //--- filter features
    val bestFt = this._getBestMatchingFeature(putativeFt, features.filter(_.getPeakelsCount > 1), pklTree)    
    //progressPlan( STEP5 ).incrementAndGetCount(1)
    
    bestFt
  }

  def _getBestMatchingFeature(putativeFt: PutativeFeature, features: Array[Feature], pklTree: PeakListTree): Option[Feature] = {
    
    if (features.isEmpty)
      return Option.empty[Feature]
    
    val elutionTime = putativeFt.elutionTime
    val mz = if (putativeFt.mozs != null) putativeFt.mozs.sum / putativeFt.mozs.length else putativeFt.mz

//    val area = if (putativeFt.areas != null) putativeFt.areas.sum / putativeFt.areas.length else 0f
//    val minFold = area * 1.45 

    val charge = putativeFt.charge
//    val (minDuration, maxDuration) = if (putativeFt.durations != null) (putativeFt.durations.min, putativeFt.durations.max)
//    else (0f, 0f)
    
    val matchingFts = if(xtractConfig.maxIPDeviation.isDefined) {
      val maxIPDeviation = xtractConfig.maxIPDeviation.get
      features
      .map { ft =>
        val theoAbundances = IsotopePatternInterpolator.getTheoreticalPattern(ft.mz, ft.charge).abundances
        val peakelApexIntensities = ft.indexedPeakels.map(_._1.getApexIntensity)
        val rmsd = IsotopePatternInterpolator.calcAbundancesRmsd(theoAbundances, peakelApexIntensities)
        (ft,rmsd)
      }
      .filter { case(ft,rmsd) => ( rmsd < maxIPDeviation ) }
      .sortBy( _._2 )
      .map( _._1 )

    } else Array.empty[Feature]

    //check detected features contains a real monoisotope as first elution peak 
    val nonAmbiguousFeatures = matchingFts.filter { ft =>
//      val gapRespect = if (this._nbGapInMaxPeakelRespectful(ft, maxTheoreticalPeakelIndex, 2)) true else false
      val overlapStatus = this.overlappingFeaturesExtractor.extractOverlappingFeatures(ft, putativeFt.theoreticalIP, pklTree)
//      if (! ft.hasMonoPeakel)
//        this.logger.info("Detected feature does not have monoisotopic elution peak")
      (ft.hasMonoPeakel) //&& gapRespect)
    }


// ---filter the feature based duration...deprecated
//    val durationFilteredFts = if (minDuration != 0f && maxDuration != 0f) {
//      features.filter { f =>
//        val duration = f.spectrumHeaders.last.getElutionTime - f.spectrumHeaders.head.getElutionTime
//        duration >= minDuration - (minDuration * 0.5) && duration <= maxDuration + (maxDuration * 0.5)
//      }
//    } else nonAmbiguousFeatures
//
//    if (durationFilteredFts.isEmpty)
//      return Option.empty[Feature]

// ---little cheat filter on intensity...deprecated
//    val variablesFts = if (area != 0 ) durationFilteredFts.filter{ f => f.area > area + minFold || f.area < area - minFold }
//                         else  Array.empty[Feature]
//
//    val intensityFilteredFts = if (variablesFts.isEmpty) durationFilteredFts else variablesFts

    //finally filter the feature based on the mass (since we have no information on intensity, duration...)
//    val bestFeature = durationFilteredFts.sortBy(f => math.abs(f.getElutionTime - elutionTime)).slice(0, 3) // take the top 3 feature
//                                         .minBy(f => math.abs(f.mz - mz))
    /*val bestFeature = nonAmbiguousFeatures.sortBy(ovlft => math.abs(ovlft.getElutionTime - elutionTime)).take(3)
                                          .minBy(f => math.abs(f.mz - mz))*/
    nonAmbiguousFeatures.headOption
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
    val peakelBuilders = Feature.ipsToIndexedPeakelBuilders(ips)
    if (peakelBuilders.isEmpty) return Array.empty[Feature]
    
    // Determine maxrelative intensity peakel index
    val peakelIndex = if (maxPeakelIndex < peakelBuilders.length) maxPeakelIndex else 0
    val maxPeakelBuilder = peakelBuilders(peakelIndex)._1

    // Ensure peakel duration  is at least 5 spectra
    if (maxPeakelBuilder.hasEnoughPeaks(this.xtractConfig.minConsecutiveSpectra) == false)
      return Array.empty[Feature]

    // Launch peak detection
    //val (peaks, definedPeaks) = (maxIntensityPeakel.peaks, maxIntensityPeakel.definedPeaks)

    val peakelIndices = findPeakelsIndices(
      maxPeakelBuilder,
      peakelDetectionConfig.detectionAlgorithm,
      peakelDetectionConfig.minSNR,
      spectrumHeaderById
    )

    val detectedFts = new ArrayBuffer[Feature](peakelIndices.length)
    val spectrumIds = maxPeakelBuilder.getSpectrumIds().result()
      
    for ( (minIdx, maxIdx) <- peakelIndices ) {
      
      val restrictedPeakelBuilders = this.restrictPeakelBuildersToSpectrumInitialIdRange(peakelBuilders, spectrumIds(minIdx), spectrumIds(maxIdx))

      if (restrictedPeakelBuilders.isEmpty == false) {
        detectedFts += new Feature(
          putativeFt.id,
          putativeFt.mz,
          putativeFt.charge,
          restrictedPeakelBuilders.map( ipb => ipb._1.result() -> ipb._2 ),
          isPredicted = true
        )
      }
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
    val peakels = Feature.ipsToIndexedPeakels(ips)
    if (peakels.isEmpty)
      return Array.empty[(Int, Int)]
    
    val tmpFt = new Feature(putativeFt.id, putativeFt.mz, putativeFt.charge, peakels)

    // determine maxrelative intensity peakel index
    val peakelIndex = if (maxPeakelIndex < tmpFt.getPeakelsCount) maxPeakelIndex else 0
    val maxIntensityPeakel = tmpFt.getPeakel(peakelIndex)

    // ensure peakel duration  is at least 5 spectra
    if (maxIntensityPeakel.hasEnoughPeaks(xtractConfig.minConsecutiveSpectra) == false)
      return Array.empty[(Int, Int)]
    
    //val (peaks, definedPeaks) = (maxIntensityPeakel.peaks, maxIntensityPeakel.definedPeaks)
    
    // Launch peakel detection
    val peakelIndexes = findPeakelsIndices(
      maxIntensityPeakel,
      peakelDetectionConfig.detectionAlgorithm,
      peakelDetectionConfig.minSNR,
      spectrumHeaderById
    )
    
    peakelIndexes
  }
  
  //protected def getSpectrumId(peak: Peak) = peak.getLcContext().getSpectrumId()

  /**
   *
   */
  /*def _forcePeakelExtraction(minMaxSpectrumIds: Pair[Int, Int], peaks: Array[Peak]): Peakel = {
    //can be more accurate if we knew the putativeFt duration ?

    new Peakel(0, peaks.filter { p =>
      val spectrumId = p.getLcContext().getSpectrumId()
      minMaxSpectrumIds._1 <= spectrumId && minMaxSpectrumIds._2 >= spectrumId
    })
  }*/
  
}


