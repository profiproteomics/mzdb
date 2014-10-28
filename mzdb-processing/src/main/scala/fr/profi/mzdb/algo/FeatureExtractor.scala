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
import fr.proline.api.progress._

class FeatureExtractor(
  val mzDbReader: MzDbReader,
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val xtractConfig: FeatureExtractorConfig = FeatureExtractorConfig( mzTolPPM = 10 ),
  val overlapXtractConfig: OverlappingFeatureExtractorConfig = OverlappingFeatureExtractorConfig()
) extends AbstractFeatureExtractor { //with ProgressComputing {
  
  final case object STEP1 extends IProgressStepIdentity {
    val stepDescription = "Feature extraction step"
  }  
  
  trait FeatureExtractorSequence extends IProgressPlanSequence
  val progressPlan = ProgressPlan[FeatureExtractorSequence](
    name = "FeatureExtractor progression",
    steps = Seq(
      ProgressStep( STEP1, maxCount = 1, weight = 1)
    )
  )
 
  protected lazy val fullySupervisedFtExtractor = new FullySupervisedFtExtractor(
    scanHeaderById,
    nfByScanId,
    xtractConfig = xtractConfig,
    overlapXtractConfig = overlapXtractConfig
  )
  protected lazy val ms2DrivenFtExtractor = new Ms2DrivenFtExtractor(
    scanHeaderById,
    nfByScanId,
    xtractConfig = xtractConfig,
    overlapXtractConfig = overlapXtractConfig
   )
  protected lazy val predictedTimeFtExtractor = new PredictedTimeFtExtractor(
    scanHeaderById,
    nfByScanId,
    xtractConfig = xtractConfig,
    overlapXtractConfig = overlapXtractConfig
  )
  protected lazy val predictedMzFtExtractor = new PredictedMzFtExtractor(
    scanHeaderById,
    nfByScanId,
    xtractConfig = xtractConfig,
    overlapXtractConfig = overlapXtractConfig
  )
  
  // Set current progress updater as the predicted time feature extrctor one
//  this.progressPlan(STEP1).setProgressUpdater(predictedTimeFtExtractor.progressComputer.getOnProgressUpdatedListener())
//
//  def registerOnPredictedTimeProgressUpdatedAction( action: (IProgressStepIdentity,Float) => Unit ) = {    
//    predictedTimeFtExtractor.progressComputer.registerOnProgressUpdatedAction( (stepIdentity,progress) => {      
//      action(stepIdentity,progress)
//    })
//  }
//  
//  def setProgressPlanMaxCount( maxCount: Int ) = {
//    this.progressComputer.getCurrentStep().setMaxCount(maxCount)
//    for (step <- predictedTimeFtExtractor.progressPlan.steps) {
//      step.setMaxCount(maxCount)
//    }
//  }
  
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
        //this.logger.info(s"Predicted Time call for feature at ${putativeFt.mz}, ${putativeFt.elutionTime}")
        ft = this.predictedTimeFtExtractor.extractFeature(putativeFt, pklTree );
        
      }  else if( putativeFt.mz > 0 ) { // search best feature for this m/z
        ft = this.predictedMzFtExtractor.extractFeature(putativeFt, pklTree );
      }
    }
    
    
    val ftsByMs2ScanId = new HashMap[Long,ArrayBuffer[Feature]]
    ftsByMs2ScanId.sizeHint(scanHeaderById.size)
    
    // Update MS2 scan ids of the feature
    for( foundFt <- ft ) {

      val ms2ScanIds = new ArrayBuffer[Int]
      
      // Retrieve the first peakel
      val firstPeakel = foundFt.getFirstPeakel()

      // Iterate over each peak of this peakel
      val peakCursor = firstPeakel.getNewCursor()
      while( peakCursor.next() ) {
        
        val thisScanId = peakCursor.getScanId()
        val peakMz = peakCursor.getMz
        
        // Retrieve the cycles surrounding the next MS2 scans
        val scanHeader = scanHeaderById(thisScanId)
        val thisCycleNum = scanHeaderById(thisScanId).getCycle
        val nextCycleNum = thisCycleNum + 1

        // Do the job only if next cycle can be found
        if( ms1ScanHeaderByCycleNum.contains(nextCycleNum) ) {
          val nextCycleScanId = ms1ScanHeaderByCycleNum(nextCycleNum).getId
          
          // Iterate over MS2 scans
          for( scanId <- thisScanId + 1 until nextCycleScanId ) {
            val scanH = this.scanHeaderById(scanId)
            
            // TODO: log charge conflicts
            if( scanH.getMsLevel == 2 && scanH.getPrecursorCharge() == foundFt.charge ) {
              // Compute m/z difference between the current peak and MS2 scan precursor m/z
              val mzDiffPPM = MsUtils.DaToPPM(peakMz, (scanH.getPrecursorMz - peakMz).abs )
              if( mzDiffPPM < mzTolPPM ) {
                ms2ScanIds += scanId
              }
            }
          }
        }
      }
      
      foundFt.ms2ScanIds = ms2ScanIds.toArray
      
      ms2ScanIds.foreach(ftsByMs2ScanId.getOrElseUpdate(_, new ArrayBuffer[Feature]) += foundFt)
    }

    def _getIntensitySumOfSurroundingPeak(ms2scanID: Long, f: Feature): Float = {
      var( intensityBeforeMs2, intensityAfterMs2 ) = (0f,0f)
      val peakCursor = f.getFirstPeakel().getNewCursor()
      
      while( peakCursor.next() ) {
        
        val intensity = peakCursor.getIntensity
        val scanId = peakCursor.getScanId

        if( scanId < ms2scanID ) intensityBeforeMs2 = intensity
        else if( scanId > ms2scanID ) intensityAfterMs2 = intensity
      }
      
      intensityBeforeMs2 + intensityAfterMs2
    }
    
    // TODO: filter out ms2 events linked to multiple features
    // Keep only a single link with the feature having two peaks surrounding the MS2 event with the highest intensity
    ftsByMs2ScanId.foreach{ case (ms2ScanId, features) =>
      if (features.size > 1) {
         val surrPeaksIntByFt = features.map{f => f -> _getIntensitySumOfSurroundingPeak(ms2ScanId, f)} toMap
         val winningFt = surrPeaksIntByFt.maxBy(_._2)
         for (f <- features if f != winningFt) {
           val scanIds = f.ms2ScanIds.toBuffer
           scanIds -= ms2ScanId.toInt
           f.ms2ScanIds = scanIds toArray
         }
      }
    }
    
    ft
  }

  

}