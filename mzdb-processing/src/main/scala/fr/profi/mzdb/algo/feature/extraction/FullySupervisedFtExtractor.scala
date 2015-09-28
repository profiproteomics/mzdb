package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.SpectrumHeader
             
class FullySupervisedFtExtractor(
  val spectrumHeaderById: Map[Long,SpectrumHeader],
  val nfBySpectrumId: Map[Long,Float],
  val xtractConfig: FeatureExtractorConfig,
  val peakelDetectionConfig: PeakelDetectionConfig = PeakelDetectionConfig(DetectionAlgorithm.BASIC),
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
) extends AbstractSupervisedFtExtractor {

  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree) : Option[Feature] = {
    
    // Retrieve a list of sorted spectrum ids
    // TODO: check if cycles are a better option
    val sortedSpectrumIds = putativeFt.firstSpectrumId to putativeFt.lastSpectrumId
    
    val theoIP = putativeFt.theoreticalIP
    val ips = new ArrayBuffer[IsotopicPattern]( sortedSpectrumIds.length )
      
    // Iterate over spectrum ids sorted in an ascendant way
    for( spectrumId <- sortedSpectrumIds ) {
      
      // Try to retrive the spectrum header
      for( curSpectrumH <- this.spectrumHeaderById.get(spectrumId) ) {
        //println(curSpectrumH.msLevel);
        
        val ipOpt = pklTree.extractIsotopicPattern( curSpectrumH, theoIP, xtractConfig.mzTolPPM )
        
        // Check if an isotopic pattern has been found
        if( ipOpt != None  ) {
          val ip = ipOpt.get            
          // Add current isotope pattern the IP list
          ips += ip
        }
      }
    }
    
    
    // Append extracted feature to the existing list
    val ft = new Feature( putativeFt.id, putativeFt.mz, putativeFt.charge, ips.toArray, isPredicted = false )
    
    //try to extract overlappingFeatures
    this.overlappingFeaturesExtractor.extractOverlappingFeatures(ft, theoIP, pklTree)
    
  
    Some(ft)
  }
}