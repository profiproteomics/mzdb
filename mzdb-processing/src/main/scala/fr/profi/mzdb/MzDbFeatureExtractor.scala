package fr.profi.mzdb

import java.io.File
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.immutable.TreeMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._
import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.algo.FeatureExtractor
import fr.profi.mzdb.algo.ms.normalization.MsSpectrumNormalizer
import fr.profi.mzdb.io.reader.provider.RunSliceDataProvider
import fr.profi.mzdb.model._
import fr.proline.api.progress._
import scala.collection.mutable.HashSet
import fr.profi.mzdb.algo.feature.extraction.FeatureExtractorConfig

/**
 *
 * @author David Bouyssie
 *
 */
class MzDbFeatureExtractor(
  mzDbReader: MzDbReader,
  maxNbPeaksInIP: Int = 3,
  minNbOverlappingIPs: Int = 3,
  xtractConfig: FeatureExtractorConfig = FeatureExtractorConfig( mzTolPPM = 10 )
) extends LazyLogging { //with ProgressComputing {
  
//  final case object MZFT_STEP0 extends IProgressStepIdentity {
//    val stepDescription = "fake initialization step"
//  }
//  final case object MZFT_STEP1 extends IProgressStepIdentity {
//    val stepDescription = "Create putativeFtByRunSlice"
//  }
//  final case object MZFT_STEP2 extends IProgressStepIdentity {
//    val stepDescription = "before runSlice iteration"
//  }  
//  final case object MZFT_STEP3 extends IProgressStepIdentity {
//    val stepDescription = "Remove aged runSlices"
//  }  
//  final case object MZFT_STEP4_0 extends IProgressStepIdentity {
//    val stepDescription = "Create pkltree 0"
//  }
//  final case object MZFT_STEP4_1 extends IProgressStepIdentity {
//    val stepDescription = "Create pkltree 1"
//  }
//  final case object MZFT_STEP4_2 extends IProgressStepIdentity {
//    val stepDescription = "Create pkltree 2"
//  }
//    final case object MZFT_STEP4_3 extends IProgressStepIdentity {
//    val stepDescription = "Create pkltree 3"
//  }
//  final case object MZFT_STEP5 extends IProgressStepIdentity {
//    val stepDescription = "Extraction"
//  }
//  final case object MZFT_STEP6 extends IProgressStepIdentity {
//    val stepDescription = "Filter features with the same apex"
//  }
// 
//  
//  trait MzDbFeatureExtractorProgressSequence extends IProgressPlanSequence
//  val progressPlan = ProgressPlan[MzDbFeatureExtractorProgressSequence](
//    name = "MzDbFeatureExtractor progression",
//    steps = Seq(
//      ProgressStep( MZFT_STEP0, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP1, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP2, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP3, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP4_0, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP4_1, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP4_2, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP4_3, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP5, maxCount = 1, weight = 1),
//      ProgressStep( MZFT_STEP6, maxCount = 1, weight = 1)
//    )
//  )
  

  class RichRunSliceData(self: RunSliceData) {
    def getPeakListBySpectrumId(): Map[Long, PeakList] = {
      val mapBuilder = Map.newBuilder[Long, PeakList]
      mapBuilder.sizeHint(self.getSpectrumSliceList.length)
      
      for( ss <- self.getSpectrumSliceList ) {
        // FIXME: remove the hardcoded 0.1 index value
        mapBuilder += ss.getSpectrumId() -> new PeakList( ss.toPeaks() )
      }
      
      mapBuilder.result
    }
  }
  implicit def rsdToRichRsd(rsd: RunSliceData) = new RichRunSliceData(rsd)
  
  
//  val mzFtStepTimers = new ArrayBuffer[StepTimer]
//  val t0 = System.currentTimeMillis().toDouble
//  var lastStep1 = t0
//  var i = 0
//  this.progressComputer.registerOnProgressUpdatedAction( (stepIdentity, progress) => {
//    val timeAfterStep = System.currentTimeMillis().toDouble
//    val deltaTimeBetweenSteps = timeAfterStep - lastStep1
//    lastStep1 = timeAfterStep
//    mzFtStepTimers += StepTimer(i, stepIdentity, deltaTimeBetweenSteps)
//    i+= 1
//  })
  
  def extractFeatures(rsdProvider: RunSliceDataProvider, putativeFeatures: Seq[PutativeFeature], mzTolPPM: Float): Seq[Feature] = {
    
    // FIXME: reset timings related vars
    
    //val putativeFeatures = putativeFeatures1.take(10000)
        
    //progressPlan( MZFT_STEP0 ).incrementAndGetCount(1)

    // Retrieve run slices and map them by their number
    val rsHeaders = mzDbReader.getRunSliceHeaders(1)
    val rsHeaderByNumber = rsHeaders.map { rsh => rsh.getNumber -> rsh }.toMap
    
    // Set the maxCount for run slice related steps
    val rsCount = rsHeaders.length
    logger.info(s"Processing ${rsCount} run slices...")
//    val rsSteps = Array(MZFT_STEP3,MZFT_STEP4_0,MZFT_STEP4_1,MZFT_STEP4_2,MZFT_STEP4_3,MZFT_STEP5)
//    rsSteps.foreach { rsStep => progressPlan(rsStep).setMaxCount(rsCount)}

    if (rsHeaders.length != rsHeaderByNumber.size)
      throw new Exception("run slice headers must have a unique number")

    // Define a putative feature map
    val putativeFtsByRsNumber = new HashMap[Int, ArrayBuffer[PutativeFeature]]

    // Group putative features by run slice id
    for (pft <- putativeFeatures) {
      for (rsh <- rsHeaders if rsh.getMsLevel == 1 && rsh.getBeginMz <= pft.mz && rsh.getEndMz > pft.mz) {
        putativeFtsByRsNumber.getOrElseUpdate(rsh.getId, new ArrayBuffer[PutativeFeature]) += pft
      }
    }
    
    //progressPlan( MZFT_STEP1 ).incrementAndGetCount(1)

    // Retrieve spectra mapped by their id
    val spectrumHeaderById = collection.immutable.Map() ++ mzDbReader.getSpectrumHeaderById.map { case (i, sh) => i.toLong -> sh }
 
    // Compute MS spectra normalization factors
    //val nfBySpectrumId = MsSpectrumNormalizer.computeNfBySpectrumId(mzDbReader)

    // Define a peaklist map (first level = runSliceId, second level =spectrumId )
    val pklBySpectrumIdAndRsId = new HashMap[Int, Map[Long, PeakList]]()

    // Define an array of features to be extracted
    val extractedFeatures = new ArrayBuffer[Feature](putativeFeatures.length)
    
    // Instantiate a feature extractor
    val ftExtractor = new FeatureExtractor(
      mzDbReader,
      spectrumHeaderById,
      null, //nfBySpectrumId,  
      xtractConfig
    )
    
    // Parameterize the ftExtractor in order to manage its progression
    //ftExtractor.setProgressPlanMaxCount(putativeFeatures.length)
        
//    val stepTimers = new ArrayBuffer[StepTimer]
//    
//    this.synchronized {
//      
//      val t0 = System.currentTimeMillis().toDouble
//      var lastStep1 = t0
//      var i = 0
//      ftExtractor.registerOnPredictedTimeProgressUpdatedAction( (stepIdentity, progress) => {
//        val timeAfterStep = System.currentTimeMillis().toDouble
//        val deltaTimeBetweenSteps = timeAfterStep - lastStep1
//        lastStep1 = timeAfterStep
//        stepTimers += StepTimer(i, stepIdentity, deltaTimeBetweenSteps)
//        i+= 1
//        
//        
//      
//      })
//    }
//    
//    progressPlan( MZFT_STEP2 ).incrementAndGetCount(1)


    // Iterate over run slice headers
    var (prevRSH, nextRSH) = (Option.empty[RunSliceHeader], Option.empty[RunSliceHeader])
    for (rsh <- rsHeaders if rsh.getMsLevel == 1) {
      this.logger.info("processing run slice with id =" + rsh.getId);
      //println("processing run slice with id =" + rsh.getId);
      // Retrieve run slices and their corresponding id
      val rsNum = rsh.getNumber

      prevRSH = rsHeaderByNumber.get(rsNum - 1)
      val prevRsNumber = if (prevRSH == None) 0 else prevRSH.get.getNumber

      nextRSH = rsHeaderByNumber.get(rsNum + 1)
      val nextRsNumber = if (nextRSH == None) 0 else nextRSH.get.getNumber

      // Build the list of obsolete run slices
      val runSlicesToRemove = for (
        processedRsId <- pklBySpectrumIdAndRsId.keys 
        if processedRsId != rsNum &&
          processedRsId != prevRsNumber &&
          processedRsId != nextRsNumber
      ) yield processedRsId

      // Clean the peaklist map => remove obsolete run slices
      runSlicesToRemove.foreach { pklBySpectrumIdAndRsId -= _ }
      //progressPlan(MZFT_STEP3).incrementAndGetCount(1)

      
      // Retrieve putative features corresponding to the current run slice
      val rsPutativeFts = putativeFtsByRsNumber.get(rsNum)

      if (rsPutativeFts != None) {

        // System.out.println("run slice id =" +runSlice.id +
        // " ; putative features count=" +
        // runSlicePutativeFeatures.size() );
        
        val emptyMap = new HashMap[Int,Map[Long,fr.profi.mzdb.model.PeakList]]()

        // Retrieve previous run slice peaklist
        if (prevRSH.isDefined) {
          if (pklBySpectrumIdAndRsId.contains(prevRsNumber) == false) {
            pklBySpectrumIdAndRsId += (prevRsNumber -> this._getRSD(rsdProvider, prevRsNumber).getPeakListBySpectrumId)
          }
        }

        // Retrieve current run slice peakList
        if (pklBySpectrumIdAndRsId.contains(rsNum) == false) {
          pklBySpectrumIdAndRsId += (rsNum -> this._getRSD(rsdProvider, rsNum).getPeakListBySpectrumId)
        }

        // Retrieve current next slice peaklist
        if (nextRSH.isDefined) {
          if (pklBySpectrumIdAndRsId.contains(nextRsNumber) == false) {
            pklBySpectrumIdAndRsId += (nextRsNumber -> this._getRSD(rsdProvider, nextRsNumber).getPeakListBySpectrumId)
          }
        }

        //progressPlan(MZFT_STEP4_0).incrementAndGetCount(1)
        
        // Group run slice peakLists into a single map (key = spectrum id)
        val peakListsBySpectrumId = new HashMap[Long, ArrayBuffer[PeakList]]()
        pklBySpectrumIdAndRsId.values.foreach {
          _.foreach {
            case (spectrumId, pkl) =>
              peakListsBySpectrumId.getOrElseUpdate(spectrumId, new ArrayBuffer[PeakList]) += pkl
          }
        }
        //progressPlan(MZFT_STEP4_1).incrementAndGetCount(1)

        // Use the map to instantiate a peakList tree which will be used for peak extraction
        val pklGroupBySpectrumId = Map() ++ peakListsBySpectrumId.map { kv => kv._1 -> new PeakListGroup(kv._2) }
        //progressPlan(MZFT_STEP4_2).setAsCompleted()
        
        val ms1SpectrumHeaderById = spectrumHeaderById.filter(_._2.getMsLevel() == 1)
        val pklTree = new PeakListTree(pklGroupBySpectrumId,ms1SpectrumHeaderById)
         
        //progressPlan(MZFT_STEP4_3).incrementAndGetCount(1)
        
        // Extract features using parallelization
        var error: Throwable = null

        val tmpXFts = rsPutativeFts.get.par.map { pft =>
          var xft = Option.empty[fr.profi.mzdb.model.Feature]

          try {
            xft = ftExtractor.extractFeature(pft, pklTree)
          } catch {
            case e: Throwable => error = e
          }

          xft
        } toArray;

        if (error != null)
          throw error
        else
          for (xft <- tmpXFts if xft != None) extractedFeatures += xft.get
        
        //progressPlan(MZFT_STEP5).incrementAndGetCount(1)


      }
    }

    this.logger.debug("nb features before identity filtering:" + extractedFeatures.length);
    
//    extractedFeatures.foreach{ f =>
//      val defPeaks = f.peakels(0).definedPeaks
//      for (i <- 0 until (defPeaks.length - 1)) {
//        val p = defPeaks(i)
//        for (j <- i + 1 until defPeaks.length ) {
//          val p_ = defPeaks(j)
//          if ( math.abs(p_.getIntensity - p.getIntensity) < Float.MinValue )
//            println("HEYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY")
//        }
//      }
//      
//    }

    //val featuresByApex = extractedFeatures.groupBy(_.peakels(0).getApex().toString() )

    
//    val ftSetByPeak = new HashMap[Peak,HashSet[Feature]]
//    for (ft <- extractedFeatures) {
//      val firstPeakel = ft.peakels(0)
//      val firstPeakelApexIdx = firstPeakel.getApexIndex()
//      val top3Peaks = if( firstPeakel.peaks.length < 2 || firstPeakelApexIdx == 0 || firstPeakelApexIdx == (firstPeakel.peaks.length-1) ) {
//        Array(firstPeakel.getApex())
//      }
//      else {
//        Array(firstPeakel.peaks(firstPeakelApexIdx-1),firstPeakel.peaks(firstPeakelApexIdx), firstPeakel.peaks(firstPeakelApexIdx +1))
//      }
//      
//      for( peak <- top3Peaks ) {
//        ftSetByPeak.getOrElseUpdate(peak, new HashSet[Feature] ) += ft
//      }
//    }
//    
//    val featuresByApex = new HashMap[Peak, HashSet[Feature]]()
//    featuresByApex.sizeHint(extractedFeatures.length)
//
//    for (ft <- extractedFeatures) {
      /*val firstPeakel = ft.peakels(0)
      val firstPeakelApex = firstPeakel.getApex()
      val sameIntApexes = firstPeakel.definedPeaks.filter( p => p.getIntensity() == firstPeakelApex.getIntensity() )
      if( sameIntApexes.length > 1) {
        this.logger.debug("sameIntApexes.length: "+ sameIntApexes.length)
      }
      val apexWithLowestMz = sameIntApexes.sortBy(_.getMz()).head
      featuresByApex.getOrElseUpdate(apexWithLowestMz, new ArrayBuffer[Feature]) += ft*/
      
//      val firstPeakel = ft.peakels(0)
//      val firstPeakelApex = firstPeakel.getApex()
//      
//      ftSetByPeak(firstPeakelApex)
      
      //val apexKey = ipApex.spectrumHeader.getId + "%" + ipApex.peaks(0).getMz + "%" + ipApex.peaks(0).getIntensity      
      
//    }
    
    val featuresByApex = extractedFeatures.groupBy { ft =>
      val firstPeakel = ft.getFirstPeakel
      val apexIdx = firstPeakel.apexIndex
      ( firstPeakel.spectrumIds(apexIdx), firstPeakel.mzValues(apexIdx) )
    }//new HashMap[String, ArrayBuffer[Feature]]
//    extractedFeatures.foreach{ f =>
//      featuresByApex.getOrElseUpdate(f.peakels(0).getApex().toString(), new ArrayBuffer[Feature]()) += f
//    }
    
    this.logger.debug("nb features after duplicated apexes filtering:" + featuresByApex.size)

    val filteredFeatures = new ArrayBuffer[Feature](featuresByApex.size)

    for (duplicatedFts <- featuresByApex.values) {
      // Sort duplicatedFts by descending elution duration
      val sortedFts = duplicatedFts.sortBy( - _.getFirstPeakel.calcDuration )
      filteredFeatures += sortedFts.head
    }

    //progressPlan(MZFT_STEP6).incrementAndGetCount(1)

    //    //print some statistics on time
    //    val meanTimeByStep = stepTimers.groupBy(_.stepIdentity).map{case (step, timers) => 
    //        step -> timers.map(_.executionTime).sum
    //      }
    //    val tot = meanTimeByStep.values.sum
    //    
    //    meanTimeByStep.foreach{ case (step, time) =>
    //      println(s"${step.stepName}: ${time/tot * 100} % of time")
    //    }
    //    
    //    println

    /*
    val meanTimeByStepFt = mzFtStepTimers.groupBy(_.stepIdentity).map {
      case (step, timers) =>
        step.stepName -> timers.map(_.executionTime).sum
    }
    val tot2 = meanTimeByStepFt.values.sum

    meanTimeByStepFt.keys.toList.sorted.foreach { stepName =>
      val time = meanTimeByStepFt(stepName)
      println(s"${stepName}: ${time / tot2 * 100} % of time")
    }
    */
    
//    val PercentageCycleByStep = new HashMap[IProgressStepIdentity, Double]()
//    for (i <- 0 until stepTimers.size by 4) {
//      val sum = stepTimers.slice(i, i + 4).map(_.executionTime).sum
//    }
    

    filteredFeatures
  }

  private def _getRSD(rsdProvider: RunSliceDataProvider, rsNum: Int): RunSliceData = {
    val rsd = rsdProvider.getRunSliceData(rsNum)//Option(rsdProvider.getRunSliceData(rsNum))
//    if (rsd == None) {
//      throw new Exception("run slice id '" + rsNum + "' is out of range")
//    }
    rsd //.get
  }

}