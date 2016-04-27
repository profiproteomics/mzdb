package fr.profi.mzdb.algo

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LongMap

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.algo.feature.extraction._
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.SpectrumHeader
import fr.profi.mzdb.utils.ms.MsUtils
import fr.proline.api.progress._

class FeatureExtractor(
  val mzDbReader: MzDbReader,
  val spectrumHeaderById: LongMap[SpectrumHeader],
  val nfBySpectrumId: LongMap[Float],
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
    spectrumHeaderById,
    nfBySpectrumId,
    xtractConfig = xtractConfig,
    overlapXtractConfig = overlapXtractConfig
  )
  protected lazy val ms2DrivenFtExtractor = new Ms2DrivenFtExtractor(
    spectrumHeaderById,
    nfBySpectrumId,
    xtractConfig = xtractConfig,
    overlapXtractConfig = overlapXtractConfig
   )
  protected lazy val predictedTimeFtExtractor = new PredictedTimeFtExtractor(
    spectrumHeaderById,
    nfBySpectrumId,
    xtractConfig = xtractConfig,
    overlapXtractConfig = overlapXtractConfig
  )
  protected lazy val predictedMzFtExtractor = new PredictedMzFtExtractor(
    spectrumHeaderById,
    nfBySpectrumId,
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
      
      if( putativeFt.firstSpectrumId > 0 && putativeFt.lastSpectrumId > 0 ) { // have a full feature knowledge
        ft = this.fullySupervisedFtExtractor.extractFeature(putativeFt, pklTree )

      }  else if( putativeFt.spectrumId > 0 ) { // only know feature m/z and a related MS spectrum event
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
    
    
    val ftsByMs2SpectrumId = new LongMap[ArrayBuffer[Feature]](spectrumHeaderById.size)
    
    // Update MS2 spectrum ids of the feature
    for( foundFt <- ft ) {

      val ms2SpectrumIds = new ArrayBuffer[Long]()
      
      // Retrieve the first peakel
      val firstPeakel = foundFt.getFirstPeakel()

      // Iterate over each peak of this peakel
      val peakCursor = firstPeakel.getNewCursor()
      while( peakCursor.next() ) {
        
        val thisSpectrumId = peakCursor.getSpectrumId()
        val peakMz = peakCursor.getMz
        
        // Retrieve the cycles surrounding the next MS2 spectra
        val spectrumHeader = spectrumHeaderById(thisSpectrumId)
        val thisCycleNum = spectrumHeaderById(thisSpectrumId).getCycle
        val nextCycleNum = thisCycleNum + 1

        // Do the job only if next cycle can be found
        if( ms1SpectrumHeaderByCycleNum.contains(nextCycleNum) ) {
          val nextCycleSpectrumId = ms1SpectrumHeaderByCycleNum(nextCycleNum).getId
          
          // Iterate over MS2 spectra
          for( spectrumId <- thisSpectrumId + 1 until nextCycleSpectrumId ) {
            val spectrumH = this.spectrumHeaderById(spectrumId)
            
            // TODO: log charge conflicts
            if( spectrumH.getMsLevel == 2 && spectrumH.getPrecursorCharge() == foundFt.charge ) {
              // Compute m/z difference between the current peak and MS2 spectrum precursor m/z
              val mzDiffPPM = MsUtils.DaToPPM(peakMz, (spectrumH.getPrecursorMz - peakMz).abs )
              if( mzDiffPPM < mzTolPPM ) {
                ms2SpectrumIds += spectrumId
              }
            }
          }
        }
      }
      
      foundFt.ms2SpectrumIds = ms2SpectrumIds.toArray
      
      ms2SpectrumIds.foreach(ftsByMs2SpectrumId.getOrElseUpdate(_, new ArrayBuffer[Feature]) += foundFt)
    }

    def _getIntensitySumOfSurroundingPeak(ms2spectrumID: Long, f: Feature): Float = {
      var( intensityBeforeMs2, intensityAfterMs2 ) = (0f,0f)
      val peakCursor = f.getFirstPeakel().getNewCursor()
      
      while( peakCursor.next() ) {
        
        val intensity = peakCursor.getIntensity
        val spectrumId = peakCursor.getSpectrumId

        if( spectrumId < ms2spectrumID ) intensityBeforeMs2 = intensity
        else if( spectrumId > ms2spectrumID ) intensityAfterMs2 = intensity
      }
      
      intensityBeforeMs2 + intensityAfterMs2
    }
    
    // TODO: filter out ms2 events linked to multiple features
    // Keep only a single link with the feature having two peaks surrounding the MS2 event with the highest intensity
    ftsByMs2SpectrumId.foreach{ case (ms2SpectrumId, features) =>
      if (features.size > 1) {
         val surrPeaksIntByFt = features.map{f => f -> _getIntensitySumOfSurroundingPeak(ms2SpectrumId, f)} toMap
         val winningFt = surrPeaksIntByFt.maxBy(_._2)._1
         for (f <- features if f != winningFt) {
           val spectrumIds = f.ms2SpectrumIds.toBuffer
           spectrumIds -= ms2SpectrumId.toInt
           f.ms2SpectrumIds = spectrumIds toArray
         }
      }
    }
    
    ft
  }

  

}