package fr.profi.mzdb.algo.feature.extraction

import collection.mutable.ArrayBuffer
import util.control.Breaks._
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.utils.ms.MsUtils
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.OverlappingIsotopicPattern
import fr.profi.mzdb.model.TheoreticalIsotopePattern
import fr.profi.mzdb.model.Peakel
import scala.collection.mutable.HashMap
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.model.FullLcContext
import fr.profi.mzdb.model.ILcContext

abstract class AbstractSupervisedFtExtractor(
  xtractConfig: FeatureExtractorConfig,
  ovlerlapXtractConfig: OverlappingFeatureExtractorConfig
) extends AbstractFeatureExtractor {
  
  // Build scanIdByCycleNum
  val ms1ScanIdByCycleNum = scanHeaderById.values
    .withFilter( _.getMsLevel == 1 )
    .map( sh => sh.getCycle -> sh.getId )
    .toMap
  
  val overlappingFeaturesExtractor = new OverlappingFeaturesExtractor(
    this.scanHeaderById, 
    this.ms1ScanIdByCycleNum,
    xtractConfig, 
    ovlerlapXtractConfig
  )
      
  /**
   * Abstract 
   */
  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature]

}