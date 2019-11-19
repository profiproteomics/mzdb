package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.LongMap
import scala.util.control.Breaks._

import com.typesafe.scalalogging.LazyLogging

import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.SpectrumHeader

object Ms2DrivenFtExtractor {
  var wrong: Int = 0
}

class Ms2DrivenFtExtractor(
 val spectrumHeaderById: LongMap[SpectrumHeader],
 val nfBySpectrumId: LongMap[Float],
 val xtractConfig: FeatureExtractorConfig = FeatureExtractorConfig(mzTolPPM = 15),
 val peakelDetectionConfig: PeakelDetectionConfig = PeakelDetectionConfig(DetectionAlgorithm.BASIC),
 val overlapXtractConfig: OverlappingFeatureExtractorConfig = OverlappingFeatureExtractorConfig()
) extends AbstractSupervisedFtExtractor with LazyLogging {
  
  override def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    // Retrieve the spectrum header corresponding to the starting spectrum id
    val ftAsopt = this.searchAndExtractFeature(putativeFt, pklTree)
    
    //Actually never seen error with our parameters monisotopes detection, remove it
    //extract overlapping features
    if ( ! ftAsopt.isDefined) {
      return Option.empty[Feature]
    }
    
    val ft = ftAsopt.get
    if (ft.getPeakelsCount <= 1)
      return Option.empty[Feature]
    
//    val overlapStatus = this.overlappingFeaturesExtractor.extractOverlappingFeatures(ft, putativeFt.theoreticalIP, pklTree)
//      
//    if (  ft.hasMonoPeakel == false ) {
//      this.logger.trace(s"Possible wrong monoisotope selection for feature with mz:${ft.mz}.\n Ignoring it...")
//      println(s"Wrong monoisotopes:${Ms2DrivenFtExtractor.wrong}")
//      return Option.empty[Feature]
//    }
    ftAsopt
  }
  
  protected def refinePrecursorMz(mz: Double, pklTree: PeakListTree, spectrumId: Int): Option[Double] = {

    val nearestPeak = pklTree.getNearestPeak(spectrumId, mz, this.xtractConfig.mzTolPPM)

    var newMz = Option.empty[Double]
    if (nearestPeak != null) {
      newMz = Some(nearestPeak.getMz)
    } else {
    }

    newMz
  }

}