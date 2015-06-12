package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.mzdb.model._
import fr.profi.ms.algo.IsotopePatternInterpolator
import fr.profi.ms.model.TheoreticalIsotopePattern
import fr.profi.mzdb.algo.feature.scoring.FeatureScorer
import java.util.concurrent.atomic.AtomicLong

case class OverlappingStatus(
    
  // true if there is an overlapping feature detected
  overlapEvidence: Boolean,
  
  // true if an overlapping with the monoisotopic peak is affected
  overlapWithMonoEvidence: Boolean,
  
  // all overlapping features
  overlappingFeatures: Array[Feature],
  
  // overlapping features with the monoisotopic
  overlappingFeaturesWithMono: Array[OverlappingFeature]
)


object OverlappingFeaturesExtractor {
  var nonMonoCount = new AtomicLong
}

/**
 * Straightforward implementation
 * 
 * @author Marco
 * params: set as var, so parameters values can be changed after OverlappingFeaturesExtractor creation
 * inherit from Ms2DrivenExtractor essentially to fetch parameters
 */
class OverlappingFeaturesExtractor(
  val scanHeaderById: Map[Long, ScanHeader],
  val nfByScanId: Map[Long,Float] = Map(),
  val ftExtractor: AbstractSupervisedFtExtractor
) extends Logging {
  
  val xtractConfig: FeatureExtractorConfig = ftExtractor.xtractConfig
  //val xtractAlgoConfig: ExtractionAlgorithmConfig = ftExtractor.xtractAlgoConfig
  val overlapXtractConfig: OverlappingFeatureExtractorConfig = ftExtractor.overlapXtractConfig
  
  /*def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {
    ftExtractor.extractFeature(putativeFt, pklTree, xtractConfig, ExtractionAlgorithm.MS2_DRIVEN)
  }*/
  
  /**
   * **********************************************************************************************
   * EXPOSED FUNCTIONS
   * **********************************************************************************************
   */
  /* *
   * fill feature overlapping related attributes 
   *
  def extractOverlappingFeatures(ft: Feature, theoIP: TheoreticalIsotopePattern, pklTree: PeakListTree) : OverlapStatus = {
      if (ft.peakels.isEmpty)
        throw new Exception("can not extract overlapping features of an empty feature. Returning Error")
      
      ft.getIsotopicPatterns.par.map( ip => this._extractOverlappingIps(ip, theoIP, pklTree) )
      val ovlFeatures = this._buildOverlappingFeatures( ft )
      this._evaluateOverlappingStatus( ft, ovlFeatures )
  }*/

  def extractOverlappingFeatures(
    ft: Feature,
    theoIP: TheoreticalIsotopePattern,
    pklTree: PeakListTree
  ): OverlappingStatus = {
    
    require(
      ft.indexedPeakels.isEmpty == false,
      "can not extract overlapping features of an empty feature. Returning Error"
    )

    val ovlFeatures = this._extractOverlappingFeatures(ft, ft.charge, pklTree)
    this._evaluateOverlappingStatus(ft, ovlFeatures)
  }

  /** EXTRACTION OVERLAPPING FEATURES */
  private def _extractOverlappingFeatures(ft: Feature, ftZ: Int, pklTree: PeakListTree): Array[Feature] = {

    // Unpack parameters
    val minZ = 2 //if (this.overlapXtractConfig.extractAllOvlFts) this.overlapXtractConfig.minZ else ft.charge
    val maxZ = 3 //if (this.overlapXtractConfig.extractAllOvlFts) this.overlapXtractConfig.maxZ else ft.charge
    val maxIpShift = if (this.overlapXtractConfig.extractAllOvlFts) this.overlapXtractConfig.maxIpShift else 1

    val mzTolPPM = this.xtractConfig.mzTolPPM

    // Some require statements
    require(maxZ > 0, "maximum charge must be strictly positive")
    require(maxIpShift > 0, "maximum IP shift must be strictly positive")
    
    // retrive first peakel
    val firstFtPeakel = ft.getFirstPeakel()
    val firstFtMz = firstFtPeakel.getMz
    //getting the maxScanId, the idea is to use extractIsotopicPattern and use only first peakel
    val maxScanId = firstFtPeakel.getApexScanId()

    // if we do not have a defined scanId, we stop
    require(maxScanId != 0)

    //val threshMzMin = ft.mz  - ( (ft.mz * this.mzTolPPM ) / 1e6 )
//    val maxMz = if (this.overlapXtractConfig.extractAllOvlFts) ft.mz + (1.0027 / ft.charge) * ft.peakelsCount else ft.mz
//    val threshMzMax = maxMz + ((maxMz * mzTolPPM) / 1e6)

    //avoid many same peakels
    //val peakelByMass = new HashMap[Int, Peakel]

    //val allPeakelMass = collection.mutable.Set[Int]()

    val ovlFts = new ArrayBuffer[Feature]

    for (z <- minZ to maxZ) {
      breakable {
        for (ipShift <- -maxIpShift to 0) if (ipShift != 0) { //z != ft.charge -> if we exclude this case fail to extract possible monoistope
          
          //this.logger.debug(s"z: ${z}, ipShift: ${ipShift}");
          //try to restrict nb features to check
          val mzToExtract = firstFtMz + ((ipShift + ipShift * 0.0027).toDouble / z)

          //if (mzToExtract > threshMzMax)
          //  break

          //if ( allPeakelMass.contains( (mzToExtract * 1000).toInt ) == false ) {

          //build fictive putative feature
          val putativeFt = new PutativeFeature(-1, mzToExtract, z, maxScanId, evidenceMsLevel = 1)
          
          // Extract feature, can eventually skip the refine extraction to gain time
          val ftXtractAlgoConfig = ftExtractor.peakelDetectionConfig.copy( detectionAlgorithm=DetectionAlgorithm.BASIC, refineDetection=true) 
          
          val featureAsOpt = ftExtractor.searchAndExtractFeature(putativeFt, pklTree, ftXtractAlgoConfig=ftXtractAlgoConfig)

          if (featureAsOpt.isDefined) {
            val ovlFt = featureAsOpt.get

            //val abundances = IsotopicPatternLookup.getTheoreticalPattern(mzToExtract, z).getRelativeAbundances.filter(_ > 5)
            //val maxPeakelIndex = abundances.indexOf(abundances.max)

            //basic if extracted peakel is relevant
            //if ( this._nbGapInMaxPeakelRespectful(ovlFt, maxPeakelIndex, 2) && ovlFt.peakels(ovlFt.peakelsCount - 1).mz > threshMzMin) {// && //ovlFt.peakels(0).mz < threshMzMax )
            //for (peakel <- ovlFt.peakels) {
            //  allPeakelMass.add( (peakel.mz * 1000).toInt )
            //}
            ovlFts += ovlFt
            // }
          }
          //}
        }
      }
    }
    
    ovlFts.toArray
  }

  /**
   * **********************************************************************************************
   * OVERLAPPING STATUS EVALUATION
   * **********************************************************************************************
   */

  /**
   *  return overlapping status
   *  look for overlapping feature with the same charge than the considered feature
   *  and check intensity and apex deviation of the considered peakel
   */
  private def _evaluateOverlappingStatus(ft: Feature, ovlFeatures: Array[Feature]): OverlappingStatus = {
    //simple case
    if (ovlFeatures.isEmpty) {
      //this.logger.info(s"No overlapping features for feature ${ft.getId()}")
      return OverlappingStatus(overlapEvidence = false, false, null, null)
    }
    //this.logger.info(s"Found # ${ovlFeatures.length} overlapping features for feature ${ft.getId()}")
    //difficult cases
    //apply peak detection on the first peakel
    //could be optional, or using the basic peakel finder
    //build an inverse mapping

    val overlapEvidence = true

    //setting overlappingFeatures
    //ft.overlappingFeatures = ovlFeatures

    //determining best overlappingFeature

    val bestOvlFts = this._selectBestOverlappingFeatures(ft, ovlFeatures)
    
    val ftOverlapProperties = if( ft.overlapProperties != null ) ft.overlapProperties
    else {
      ft.overlapProperties = FeatureOverlapProperties()
      ft.overlapProperties
    }
    ftOverlapProperties.overlappingFeatures = bestOvlFts
    
    if (!bestOvlFts.isEmpty) {
      
      // Sort overlapping features by ascending area of peakel preceding the overlapping peakel
      // TODO: find an other way to select the best one
      val sortedOvlFts = bestOvlFts.sortBy { ovlFt =>
        ovlFt.feature.getPeakel( math.max(ovlFt.overlappingPeakelIndex - 1,0) ).area
      }
      
      ftOverlapProperties.bestOverlappingFeature = sortedOvlFts.last
    }

    OverlappingStatus(overlapEvidence = true, !bestOvlFts.isEmpty, ovlFeatures, bestOvlFts)
  }

  private def _selectBestOverlappingFeatures(ft: Feature, ovlFts: Array[Feature]): Array[OverlappingFeature] = {

    val bestOvlFeatures = new ArrayBuffer[OverlappingFeature]
    val monoFtPeakel = ft.getFirstPeakel()
    val currFtMonoMz = monoFtPeakel.getMz
    val minCorr = this.overlapXtractConfig.minPeakelCorrToMono
    val minApexDistance = this.overlapXtractConfig.minApexDistance
    val minAveragineRatio = this.overlapXtractConfig.minAveragineRatio
    
    //val filteredOvlFts = if (this.overlapXtractConfig.extractAllOvlFts) ovlFts else ovlFts.filter(_.charge == ft.charge)
    
    ovlFts.foreach { ovlFt =>
      //find closest peakel to current mono of considered ft
      //val insideTolPeakels = ovlFt.peakels.filter(p => math.abs(p.mz + (1.0027 / ovlFt.getCharge) - currFtMonoMz) < this.xtractConfig.mzTolPPM * p.mz / 1e6)

      // Found interesting peakels with the mono
      //if (insideTolPeakels.isEmpty == false) {

//        logger.trace(
//          "Several possible elution peak in overlap with the monoisotope of feature of mass: "+
//         s"${ft.mz} and charge: ${ft.charge}. \nConsidering the closest in mz range..."
//        )
        
        val prevOvlFtPeakelWithIdx = ovlFt.indexedPeakels.minBy( pWithIdx => math.abs(pWithIdx._1.getMz + (1.0027 / ovlFt.getCharge) - currFtMonoMz))//insideTolPeakels.minBy(p => math.abs(currFtMonoMz - p.mz))
        val( prevOvlFtPeakel, prevOvlFtPeakelIdx ) = prevOvlFtPeakelWithIdx

        //assume they are the same XIC now
        //val previousOvlFtIndex = math.min(math.max(previousOvlFtPeakelIndex - 1, 0), ovlFt.peakelsCount - 1)
        val monoisotopicIndex = 0

//        if (previousOvlFtIndex == 0) {
//          //we have rextract the same feature
//          //println("OvlFt and considered Feature seem to have the same monoisotopicPeakel")
//        } else {
//          val previousOvlFtPeakel = ovlFt.peakels(previousOvlFtIndex)

        val apexDistanceInCycle = math.abs(
          this.scanHeaderById(prevOvlFtPeakel.getApexScanId).getCycle -
          this.scanHeaderById(monoFtPeakel.getApexScanId).getCycle
        )

        val correlation = FeatureScorer.calcPeakelCorrelation(prevOvlFtPeakel, monoFtPeakel).toFloat
        //experimental intensity quotient vs averagine
        val theoIP = IsotopePatternInterpolator.getTheoreticalPattern(ovlFt.mz, ovlFt.charge)
        val abundances = theoIP.abundances
//        if (previousOvlFtIndex + 1 > abundances.length) {
//          logger.trace("Reached max peakel, pass")
//        }
//        else {
        
        // Compute averagine quotient between the the 'new' monoisotope and currentMonoisotope
        val theoriticalQuotient = abundances(monoisotopicIndex) / abundances(monoisotopicIndex + 1)
        val observedQuotient = prevOvlFtPeakel.area / monoFtPeakel.area
        val quotient = if (observedQuotient > theoriticalQuotient) observedQuotient / theoriticalQuotient else theoriticalQuotient / observedQuotient

        //sign of death for the feature
        if (apexDistanceInCycle <= minApexDistance && 
            correlation != Double.NaN && correlation > minCorr &&
            quotient < minAveragineRatio && quotient > 0.75) {
          
          //logger.info("Wrong feature in a cross-assignment (wrong monoisotopic selection). Ignore it...")
          ft.hasMonoPeakel = false          
          this.logger.trace(s" apexDist ${apexDistanceInCycle}, correlation: ${correlation}, average ratio: ${quotient}")
          OverlappingFeaturesExtractor.nonMonoCount.incrementAndGet()
          //println(s"nonMonoCount : ${OverlappingFeaturesExtractor.nonMonoCount}")
          // 0 means mono-isotopic in overlapped feature
          bestOvlFeatures += OverlappingFeature(ovlFt, 0, prevOvlFtPeakelIdx, apexDistanceInCycle, correlation, quotient) //ovlFt
        }
//        }
//        }
//      } //end if inside tol
//    } else {
//      this.logger.info("No peakel close to the current monoisotopique")
//    }
    }//end for each
    
    bestOvlFeatures.toArray
  }

}//end stuff