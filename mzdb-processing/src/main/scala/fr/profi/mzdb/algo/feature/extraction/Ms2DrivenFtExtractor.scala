package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.control.Breaks._
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.utils.ms.MsUtils
import fr.profi.mzdb.algo.signal.detection.AbstractWaveletPeakelFinder
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.ElutionTimeContext
import fr.profi.mzdb.algo.signal.detection.RidgeFilteringParameters
import fr.profi.mzdb.model.Peakel

object Ms2DrivenFtExtractor {
  var wrong: Int = 0
}

class Ms2DrivenFtExtractor(
 val scanHeaderById: Map[Int,ScanHeader],
 val nfByScanId: Map[Int,Float],
 val xtractConfig: FeatureExtractorConfig = FeatureExtractorConfig(mzTolPPM = 15),
 val peakelDetectionConfig: PeakelDetectionConfig = PeakelDetectionConfig(DetectionAlgorithm.BASIC),
 val overlapXtractConfig: OverlappingFeatureExtractorConfig = OverlappingFeatureExtractorConfig()
) extends AbstractSupervisedFtExtractor with Logging {
  
  override def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    // Retrieve the scan header corresponding to the starting scan id
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
  
  protected def refinePrecursorMz(mz: Double, pklTree: PeakListTree, scanId: Int): Option[Double] = {

    val nearestPeak = pklTree.getNearestPeak(scanId, mz, this.xtractConfig.mzTolPPM)

    var newMz = Option.empty[Double]
    if (nearestPeak != null) {
      newMz = Some(nearestPeak.getMz)
    } else {
    }

    newMz
  }

}