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
  val mzTolPPM: Float,
  val maxNbPeaksInIP: Int,
  val minNbOverlappingIPs: Int
) extends AbstractFeatureExtractor {

  protected lazy val fullySupervisedFtExtractor = new FullySupervisedFtExtractor( scanHeaderById, nfByScanId, mzTolPPM, maxNbPeaksInIP, minNbOverlappingIPs );
  protected lazy val ms2DrivenFtExtractor = new Ms2DrivenFtExtractor( scanHeaderById, nfByScanId, mzTolPPM, maxNbPeaksInIP, minNbOverlappingIPs );
  protected lazy val predictedTimeFtExtractor = new PredictedTimeFtExtractor( scanHeaderById, nfByScanId, mzTolPPM, maxNbPeaksInIP, minNbOverlappingIPs );
  protected lazy val predictedMzFtExtractor = new PredictedMzFtExtractor( scanHeaderById, nfByScanId, mzTolPPM, maxNbPeaksInIP, minNbOverlappingIPs );
    
  def extractFeatures( putativeFeatures: Seq[PutativeFeature], 
                       extractedFeatures: ArrayBuffer[Feature], // store newly extracted features
                       pklTree: PeakListTree ) {
    
    for( putativeFt <- putativeFeatures ) {      
      var ft = this.extractFeature(putativeFt,pklTree)      
      if( ft != None ) extractedFeatures += ft.get
    }
  }
  
  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree ): Option[Feature] = {
      
    var ft = Option.empty[Feature]
    if( putativeFt.isPredicted == false ) { // we know that signal is there
      
      if( putativeFt.firstScanId > 0 && putativeFt.lastScanId > 0 ) { // have a full feature knowledge
        //println("Fully supervised")
        ft = this.fullySupervisedFtExtractor.extractFeature(putativeFt, pklTree )

      }  else if( putativeFt.scanId > 0 ) { // only know feature m/z and a related MS scan event
        //println("MS2 driven")
        ft = this.ms2DrivenFtExtractor.extractFeature(putativeFt, pklTree )
        
      }

    }  else { // don't know if signal is there
      
      if( putativeFt.elutionTime > 0 ) { // only know m/z, elution time is predicted
        //println("Predicted time")

        ft = this.predictedTimeFtExtractor.extractFeature(putativeFt, pklTree );
        
      }  else if( putativeFt.mz > 0 ) { // search best feature for this m/z
        //println("Predicted Mz")
        ft = this.predictedMzFtExtractor.extractFeature(putativeFt, pklTree );
      }
    }
    
    // Update MS2 scan ids of the feature
    for( foundFt <- ft ) {  
      
      val ftScanHeaders = foundFt.getScanHeaders
      
      val firstScanId = ftScanHeaders.head.getId
      val lastCycleNum = ftScanHeaders.last.getCycle + 1 // jump to next cycle
        
      val lastScanId = if( scanHeaderByCycleNum.contains(lastCycleNum) ) scanHeaderByCycleNum(lastCycleNum).getId
      else scanHeaderByCycleNum(lastCycleNum - 1).getId
      
      val ms2ScanIds = new ArrayBuffer[Int]
      for( scanId <- firstScanId until lastScanId ) {
        val scanH = this.scanHeaderById(scanId)
        if( scanH.getMsLevel == 2 ) {
          val mzDiffPPM = MsUtils.DaToPPM(foundFt.mz, (scanH.getPrecursorMz - foundFt.mz).abs )
          if( mzDiffPPM < mzTolPPM ) {
            ms2ScanIds += scanId
          }
        }
      }
      
      foundFt.ms2ScanIds = ms2ScanIds.toArray
    }

    ft
  }
  
}