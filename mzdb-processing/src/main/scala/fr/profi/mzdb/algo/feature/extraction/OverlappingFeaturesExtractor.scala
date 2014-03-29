package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks._

import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.mzdb.model._
import fr.profi.mzdb.utils.ms.IsotopicPatternLookup

/**
 * StraitForward implementation
 * @author Marco
 * params: set as var, so parameters values can be changed after OverlappingFeaturesExtractor creation
 * inherit from Ms2DrivenExtractor essentially to fetch parameters
 */
class OverlappingFeaturesExtractor(
  val scanHeaderById: Map[Int, ScanHeader],
  val ms1ScanIdByCycleNum: Map[Int, Int],
  val xtractConfig: FeatureExtractorConfig,
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
) extends IExtractorHelper with Logging {

  /**
   * EXTRACTION OVERLAPPING FEATURES
   */
  protected def _extractOverlappingFeatures(ft: Feature, ftZ: Int, pklTree: PeakListTree): Array[Feature] = {

    // Unpack parameters
    val minZ = if (this.overlapXtractConfig.extractAllOvlFts) this.overlapXtractConfig.minZ else ft.charge
    val maxZ = if (this.overlapXtractConfig.extractAllOvlFts) this.overlapXtractConfig.maxZ else ft.charge
    val maxIpShift = if (this.overlapXtractConfig.extractAllOvlFts) this.overlapXtractConfig.maxIpShift else 1

    val mzTolPPM = this.xtractConfig.mzTolPPM

    // Some require statements
    require(maxZ > 0, "maximum charge must be strictly positive")
    require(maxIpShift > 0, "maximum IP shift must be strictly positive")

    val firstFtPeakel = ft.peakels.head
    //getting the maxScanId
    //the idea is to use extractIsotopicPattern and use only first peakel
    val maxScanId = firstFtPeakel.definedPeaks.maxBy(x => x.getIntensity())
      .getLcContext().getScanId()

    //if we do not have a defined scanId, we stop                                          
    require(maxScanId != 0)

    //val threshMzMin = ft.mz  - ( (ft.mz * this.mzTolPPM ) / 1e6 )
    val maxMz = if (this.overlapXtractConfig.extractAllOvlFts) ft.mz + (1.0027 / ft.charge) * ft.peakelsCount else ft.mz
    val threshMzMax = maxMz + ((maxMz * mzTolPPM) / 1e6)

    //avoid many same peakels
    //val peakelByMass = new HashMap[Int, Peakel]

    //val allPeakelMass = collection.mutable.Set[Int]()

    val ovlFts = new ArrayBuffer[Feature]

    for (z <- minZ to maxZ) {
      breakable {
        for (ipShift <- -maxIpShift to maxIpShift) if (z != ft.charge && maxIpShift != 0) {

          //try to restrict nb features to check
          val mzToExtract = firstFtPeakel.mz + (ipShift.toDouble / z)

          //if (mzToExtract > threshMzMax)
          //  break

          //if ( allPeakelMass.contains( (mzToExtract * 1000).toInt ) == false ) {

          //build fictive putative feature
          val putativeFt = new PutativeFeature(-1, mzToExtract, z, maxScanId, evidenceMsLevel = 1)
          //extract feature
          val conf = this.xtractConfig.ms2DrivenExtraction.copy(refineDetection = false) // skip the refine extraction to gain time
          this.xtractConfig.ms2DrivenExtraction = conf
          val featureAsOpt = this._extractFeature(putativeFt, pklTree, xtractConfig, ExtractionAlgorithm.MS2_DRIVEN)

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
  def _evaluateOverlappingStatus(ft: Feature, ovlFeatures: Array[Feature]): OverlapStatus = {
    //simple case
    if (ovlFeatures.isEmpty) {
      return OverlapStatus(overlapEvidence = false, false, null, null)
    }
    //difficult cases
    //apply peak detection on the first peakel
    //could be optional, or using the basic peakel finder
    //build an inverse mapping

    val overlapEvidence = true

    //setting overlappingFeatures
    ft.overlappingFeatures = ovlFeatures

    //determining best overlappingFeature

    val bestOvlFts = this._selectBestOverlappingFeatures(ft, ovlFeatures)
    if (!bestOvlFts.isEmpty)
      ft.bestOverlappingFeature = bestOvlFts(0).ft

    OverlapStatus(overlapEvidence = true, !bestOvlFts.isEmpty, ovlFeatures, bestOvlFts)

  }

  def _selectBestOverlappingFeatures(ft: Feature, ovlFts: Array[Feature]): Array[ProvenOverlappingFeaturesWithMono] = {

    val bestOvlFeatures = new ArrayBuffer[ProvenOverlappingFeaturesWithMono]
    val monoFtPeakel = ft.peakels.head
    val currFtMonoMz = monoFtPeakel.mz

    val filteredOvlFts = if (this.overlapXtractConfig.extractAllOvlFts) ovlFts else ovlFts.filter(_.charge == ft.charge)

    filteredOvlFts.foreach { ovlFt =>
      //find closest peakel to current mono of considered ft
      val insideTolPeakels = ovlFt.peakels.filter(p => math.abs(p.mz - currFtMonoMz) < this.xtractConfig.mzTolPPM * p.mz / 1e6)

      //found interesting peakels with the mono
      if (insideTolPeakels.isEmpty == false) {

        println(s"Several possible elution peak in overlap with the monoisotope of feature of mass: ${ft.mz} and charge: ${ft.charge}. \nConsidering the closest in mz range...")
        val closestPeakel = insideTolPeakels.minBy(p => math.abs(currFtMonoMz - p.mz))

        val closestPeakelIndex = closestPeakel.index

        //assume they are the same XIC now
        val previousOvlFtIndex = math.min(math.max(closestPeakelIndex - 1, 0), ovlFt.peakelsCount - 1)

        if (previousOvlFtIndex == 0) {
          //we have rextract the same feature
          //println("OvlFt and considered Feature seem to have the same monoisotopicPeakel")
        } else {
          val previousOvlFtPeakel = ovlFt.peakels(previousOvlFtIndex)

          val apexDistanceInCycle = math.abs(this.scanHeaderById(previousOvlFtPeakel.getApexScanContext.getScanId).getCycle -
            this.scanHeaderById(monoFtPeakel.getApexScanContext.getScanId).getCycle)

          val correlation = previousOvlFtPeakel.computeCorrelationWith(monoFtPeakel) toFloat
          //experimental intensity quotient vs averagine
          val theoIP = IsotopicPatternLookup.getTheoreticalPattern(ovlFt.mz, ovlFt.charge)
          val abundances = theoIP.getRelativeAbundances()
          if (previousOvlFtIndex + 1 > abundances.length)
            println("Reached max peakel, pass")
          else {
            val theoriticalQuotient = abundances(previousOvlFtIndex) / abundances(previousOvlFtIndex + 1)
            val observedQuotient = previousOvlFtPeakel.area / monoFtPeakel.area
            val quotient = if (observedQuotient > theoriticalQuotient) observedQuotient / theoriticalQuotient else theoriticalQuotient / observedQuotient

            //sign of death for the feature
            if (apexDistanceInCycle <= 20 && //quotient < 2
              correlation != Double.NaN && correlation > this.overlapXtractConfig.minPeakelCorrToMono) {
              println("Wrong feature in a cross-assignment (wrong monoisotopic selection). Ignore it...")
              ft.isRelevant = false
              bestOvlFeatures += ProvenOverlappingFeaturesWithMono(ovlFt, closestPeakelIndex, apexDistanceInCycle, correlation, quotient) //ovlFt
            }
          }
        }
      } //end if inside tol
    }
    bestOvlFeatures.toArray
  }

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

  def extractOverlappingFeatures(ft: Feature,
                                 theoIP: TheoreticalIsotopePattern,
                                 pklTree: PeakListTree): OverlapStatus = {
    if (ft.peakels.isEmpty)
      throw new Exception("can not extract overlapping features of an empty feature. Returning Error")

    val ovlFeatures = this._extractOverlappingFeatures(ft, ft.charge, pklTree)
    this._evaluateOverlappingStatus(ft, ovlFeatures)
  }

}//end stuff