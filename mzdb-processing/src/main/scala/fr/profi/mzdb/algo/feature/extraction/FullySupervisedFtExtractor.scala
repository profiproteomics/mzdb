package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
             
class FullySupervisedFtExtractor(
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val xtractConfig: FeatureExtractorConfig,
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
) extends AbstractSupervisedFtExtractor {

  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree) : Option[Feature] = {
    
    // Retrieve a list of sorted scan ids
    // TODO: check if cycles are a better option
    val sortedScanIds = putativeFt.firstScanId to putativeFt.lastScanId
    
    val theoIP = putativeFt.theoreticalIP
    val ips = new ArrayBuffer[IsotopicPattern]( sortedScanIds.length )
      
    // Iterate over scan ids sorted in an ascendant way
    for( scanId <- sortedScanIds ) {
      
      // Try to retrive the scan header
      for( curScanH <- this.scanHeaderById.get(scanId) ) {
        //println(curScanH.msLevel);
        
        val ipOpt = pklTree.extractIsotopicPattern( curScanH, theoIP, xtractConfig.mzTolPPM )
        
        // Check if an isotopic pattern has been found
        if( ipOpt != None  ) {
          val ip = ipOpt.get            
          // Add current isotope pattern the IP list
          ips += ip
        }
      }
    }
    
    
    // Append extracted feature to the existing list
    val ft = new Feature( putativeFt.id, putativeFt.mz, putativeFt.charge, ips.toArray )
    
    //try to extract overlappingFeatures
    this.overlappingFeaturesExtractor.extractOverlappingFeatures(ft, theoIP, pklTree)
    
  
    Some(ft)
  }
}