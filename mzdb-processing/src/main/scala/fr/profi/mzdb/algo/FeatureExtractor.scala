package fr.profi.mzdb.algo

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import fr.profi.mzdb.algo.feature.extraction._
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakList
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.utils.ms.MsUtils

class FeatureExtractor(
  val mzDbReader: MzDbReader,
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val xtractConfig: FeatureExtractorConfig = FeatureExtractorConfig( mzTolPPM = 10 ),
  val overlapXtractConfig: OverlappingFeatureExtractorConfig = OverlappingFeatureExtractorConfig()
) extends AbstractSupervisedFtExtractor(xtractConfig, overlapXtractConfig) with IExtractorHelper {

  protected lazy val fullySupervisedFtExtractor = new FullySupervisedFtExtractor( scanHeaderById, nfByScanId, xtractConfig, overlapXtractConfig );
  protected lazy val ms2DrivenFtExtractor = new Ms2DrivenFtExtractor( scanHeaderById, nfByScanId, xtractConfig, overlapXtractConfig );
  protected lazy val predictedTimeFtExtractor = new PredictedTimeFtExtractor( scanHeaderById, nfByScanId, xtractConfig, overlapXtractConfig );
  protected lazy val predictedMzFtExtractor = new PredictedMzFtExtractor( scanHeaderById, nfByScanId, xtractConfig, overlapXtractConfig );
    
  def extractFeatures( putativeFeatures: Seq[PutativeFeature], 
                       extractedFeatures: ArrayBuffer[Feature], // store newly extracted features
                       pklTree: PeakListTree ) {
    
    for( putativeFt <- putativeFeatures ) {      
      var ft = this.extractFeature(putativeFt,pklTree)      
      if( ft != None ) extractedFeatures += ft.get
    }
  }
  
  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree ): Option[Feature] = {
    val mzTolPPM = this.xtractConfig.mzTolPPM
    var ft = Option.empty[Feature]
    if( putativeFt.isPredicted == false ) { // we know that signal is there
      
      if( putativeFt.firstScanId > 0 && putativeFt.lastScanId > 0 ) { // have a full feature knowledge
        ft = this.fullySupervisedFtExtractor.extractFeature(putativeFt, pklTree )

      }  else if( putativeFt.scanId > 0 ) { // only know feature m/z and a related MS scan event
        ft = this.ms2DrivenFtExtractor.extractFeature(putativeFt, pklTree )
        
      }
    }  else { // don't know if signal is there
      
      if( putativeFt.elutionTime > 0 ) { // only know m/z, elution time is predicted
        ft = this.predictedTimeFtExtractor.extractFeature(putativeFt, pklTree );
        
      }  else if( putativeFt.mz > 0 ) { // search best feature for this m/z
        ft = this.predictedMzFtExtractor.extractFeature(putativeFt, pklTree );
      }
    }
    
    // Update MS2 scan ids of the feature
    for( foundFt <- ft ) {

      val ms2ScanIds = new ArrayBuffer[Int]
      // Retrieve the first peakel

      val firstPeakel = foundFt.peakels(0)

      // Iterate over each peak of this peakel

      for( peak <- firstPeakel.definedPeaks ) {
        // Retrieve the cycles surrounding the next MS2 scans
        val thisScanId = peak.getLcContext.getScanId
        val thisCycleNum = scanHeaderById(thisScanId).getCycle
        val nextCycleNum = thisCycleNum + 1

        // Do the job only if next cycle can be found
        if( ms1ScanHeaderByCycleNum.contains(nextCycleNum) ) {
          val nextCycleScanId = ms1ScanHeaderByCycleNum(nextCycleNum).getId
          // Iterate over MS2 scans
          for( scanId <- thisScanId until nextCycleScanId ) {
            val scanH = this.scanHeaderById(scanId)
            // TODO: log charge conflicts
            if( scanH.getMsLevel == 2 && scanH.getPrecursorCharge() == foundFt.charge ) {
              // Compute m/z difference between the current peak and MS2 scan precursor m/z
              val mzDiffPPM = MsUtils.DaToPPM(peak.getMz, (scanH.getPrecursorMz - peak.getMz).abs )
              if( mzDiffPPM < mzTolPPM ) {
                ms2ScanIds += scanId
              }
            }
          }
        }
      }
      foundFt.ms2ScanIds = ms2ScanIds.toArray
    }

    ft

  }

  

}