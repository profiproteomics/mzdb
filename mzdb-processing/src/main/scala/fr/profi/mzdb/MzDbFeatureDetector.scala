package fr.profi.mzdb

import java.util.Iterator
import java.util.concurrent.Executors
import java.util.concurrent.ThreadPoolExecutor

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LongMap
import scala.collection.mutable.Queue
import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.control.Breaks._

import com.typesafe.scalalogging.LazyLogging

import fr.profi.ms.algo.IsotopePatternInterpolator
import fr.profi.mzdb.algo.feature.extraction.UnsupervisedPeakelDetector
import fr.profi.mzdb.algo.signal.detection.IPeakelFinder
import fr.profi.mzdb.algo.signal.detection.SmartPeakelFinder
import fr.profi.mzdb.model._
import fr.profi.mzdb.util.misc.SetClusterer
import fr.profi.mzdb.util.ms.MsUtils
import fr.profi.util.collection._
import fr.profi.util.stat._
import fr.proline.api.progress._

import rx.lang.scala.Observable
import rx.lang.scala.Subject
import rx.lang.scala.Subscriber
import rx.lang.scala.schedulers.ExecutionContextScheduler
import rx.lang.scala.subjects.PublishSubject

abstract class PeakelFinderConfig

case class SmartPeakelFinderConfig(
  minPeaksCount: Int = 5,
  miniMaxiDistanceThresh: Int = 3,
  useOscillationFactor: Boolean = false,
  maxOscillationFactor: Int = 10,
  usePartialSGSmoother: Boolean = false,
  useBaselineRemover: Boolean = false
) extends PeakelFinderConfig

case class FeatureDetectorConfig(
  msLevel: Int = 1,
  mzTolPPM: Float = 10,
  minNbOverlappingIPs: Int = 3,
  peakelFinderConfig: PeakelFinderConfig = SmartPeakelFinderConfig()
)

object BuildPeakelFinder {
  def apply(config: PeakelFinderConfig): IPeakelFinder = {
    config match {
      case smf: SmartPeakelFinderConfig => new SmartPeakelFinder(
        minPeaksCount = smf.minPeaksCount,
        miniMaxiDistanceThresh = smf.miniMaxiDistanceThresh,
        useOscillationFactor = smf.useOscillationFactor,
        maxOscillationFactor = smf.maxOscillationFactor,
        usePartialSGSmoother = smf.usePartialSGSmoother,
        useBaselineRemover = smf.useBaselineRemover
      )
    }
  }
}


class PeakelDetectorConsumer(
  val consumerNumber: Int,
  val peakelDetector: UnsupervisedPeakelDetector,
  val detectorQueue: PeakelDetectorQueue
  //val queue: ArrayBlockingQueue[Option[PeakelDetectorQueueEntry]]
)(implicit execCtx: ExecutionContextExecutor) extends LazyLogging {
  
  // Here is the PeakListTree consumer code written in an Observable block
  // Note that exceptions will be automatically managed by RxJava
  // and can be intercepted by the observable subscriber (onError method)
  private val coldObservable = Observable[Array[Peakel]] { subscriber =>
    
    try {
      var hasFinished = false
      
      // Consume the queue while the file is being read and until queue is empty
      while (hasFinished == false) {
        val queueEntry = detectorQueue.dequeue()
        //val queueEntryOpt = queue.take()
        
        // TODO: find a better way to exit the consumer
        // This example could help: http://stackoverflow.com/questions/16009837/how-to-cancel-future-in-scala
        if( queueEntry == null ) {
          hasFinished = true
          logger.debug("exiting feature detector consumer "+ consumerNumber)
        } else {
          
          //val queueEntry = queueEntryOpt.get
          val rsNumber = queueEntry.rsNumber
          val pklTree = queueEntry.pklTree
          val curPeaklistBySpectrumId = queueEntry.curPeaklistBySpectrumId
          
          this.logger.debug(s"unsupervised processing of run slice $rsNumber in consumer $consumerNumber")
          
          // Create a peaklist group that is only used to call getPeaksCoordsSortedByDescIntensity
          // TODO: create a static method for that ???
          val curRsPeakLists = pklTree.spectrumIds.map( specId => curPeaklistBySpectrumId.getOrNull(specId) )
          val curRsPeakListColl = PeakListCollection(curRsPeakLists)
          
          // Sort the peaks by descending intensity
          this.logger.debug(s"sorting #${curRsPeakListColl.peaksCount} peaks by descending intensity in run slice " + rsNumber)
          
          // Sort cursRsPeakRefs according to cursRsIntensityList
          val curRsPeaksCoords = curRsPeakListColl.getPeaksCoordsSortedByDescIntensity()
          
          val highestPeakCoords = curRsPeaksCoords.head
          val lowestPeakCoords = curRsPeaksCoords.last
          val highestIntensity = curRsPeakListColl.getPeakAt(highestPeakCoords(0), highestPeakCoords(1)).getIntensity
          val lowestIntensity = curRsPeakListColl.getPeakAt(lowestPeakCoords(0), lowestPeakCoords(1)).getIntensity
          
          this.logger.debug( s"Peak intensity range in run slice $rsNumber = "+ lowestIntensity+" to "+highestIntensity)
          
          // Create a HashMap to memorize which peaks have been already used
          // Note: in previous implementation we included peaks of the previous run slice
          // TODO: check if not doing that has an effect on the quality of detection
          //val usedPeakMap = new HashMap[Peak,Boolean]() // true if used in last detection false if previous detection
          
          // Detect peakels in pklTree by using curRsPeaks as starting points
          val peakels = pklTree.synchronized {
            peakelDetector.detectPeakels(
              pklTree,
              curRsPeakListColl,
              curRsPeaksCoords
            )
          }
          
          subscriber.onNext(peakels)
          
          this.logger.debug( s"found ${peakels.length} peakels in run slice "+rsNumber)
        }
      }
      
      // Terminate the observable
      subscriber.onCompleted()
      
    } catch {
      case t: Throwable => subscriber.onError(t)
    }

  }
  
  private val execCtxScheduler = ExecutionContextScheduler(execCtx) // for subscribeOn
  private val hotObservable = coldObservable.subscribeOn(execCtxScheduler).publish
  hotObservable.connect // Execute the observable
  
  def observable = hotObservable
}

case class PeakelDetectorQueueEntry(
  rsNumber: Int,
  pklTree: PeakListTree, // contains peaks of [previous,current,next] RunSlices
  curPeaklistBySpectrumId: LongMap[PeakList] // contains only peaks of the current RunSlice
)

// Code inspired from: http://studio.cs.hut.fi/snippets/producer.html
class PeakelDetectorQueue(maxSize: Int) extends LazyLogging {
  
  private var isStopped = false

  // Here is the queue - not nothing fancy about it. Just a normal scala.collection.mutable.queue
  // In addition to being a queue, it is also the lock for our PeakelDetectorQueue
  // It is also the resource we are trying to protect using our PeakelDetectorQueue
  protected val peakelDetectorQueue = new Queue[PeakelDetectorQueueEntry]()
  
  def clear() = peakelDetectorQueue.clear()
  def hasEntries(): Boolean = ! peakelDetectorQueue.isEmpty
  def stop() = synchronized {
    // Dequeue all entries
    /*while( this.hasEntries() ) {
      this.dequeue()
    }*/
    isStopped = true
  }

  // This method is used to dequeue a PeakListTree
  // Take attentation to the peaklistTreeQueue.synchronized
  // This says that to enter the code inside synchronized you should holds the lock PeakelDetectorQueue    
  def dequeue(): PeakelDetectorQueueEntry = peakelDetectorQueue.synchronized {
    if( isStopped ) return null
      
    // This is a classic way to implement waiting on a resource.
    // The check is enclosed in a while loop so that if the thread is woken
    // but if there is nothing to do, it goes back to waiting
    
    // If the queue is empty (no PeakelDetectorQueueEntry in the queue)
    while (peakelDetectorQueue.isEmpty) {
      logger.debug("waiting for new entries in the queue...")
      
      // As it is empty we cannot do anything now and must go to waiting
      // This is done by calling the wait method of the lock object of the PeakelDetectorQueue
      peakelDetectorQueue.wait()
    }
    
    // If we are here, it must be possible to pick up a PeakelDetectorQueueEntry
    // This will notify a consumer that may waiting for
    peakelDetectorQueue.notifyAll()
    
    // Now let's just return our PeakelDetectorQueueEntry and leave the method (and the synchronized piece of code)
    // so that other threads can enter.
    peakelDetectorQueue.dequeue()
  }

  // This is the exact same thing but for bringing queue entries.
  // We don't have to repeat the explanations here
  def enqueue(queueEntry: PeakelDetectorQueueEntry): Unit = peakelDetectorQueue.synchronized {
    if( isStopped ) return ()
    
    while (peakelDetectorQueue.size >= maxSize) {
      peakelDetectorQueue.wait()
    }
    
    peakelDetectorQueue.notifyAll()
    
    // Puts a PeakelDetectorQueueEntry into the queue
    peakelDetectorQueue.enqueue(queueEntry)
  }
  
  protected val exceptionQueue = new Queue[Throwable]()
  def hasExceptions(): Boolean = ! exceptionQueue.isEmpty
  
  def dequeueException(): Throwable = exceptionQueue.synchronized {

    while (exceptionQueue.isEmpty) {
      logger.debug("Watching for new exception in the queue...")
      exceptionQueue.wait()
    }
    
    exceptionQueue.notifyAll()
    exceptionQueue.dequeue()
  }
  
  def enqueueException(t: Throwable): Unit = exceptionQueue.synchronized {
    exceptionQueue.notifyAll()
    exceptionQueue.enqueue(t)
  }

}

object MzDbFeatureDetector {
  
  trait MzDbPeakelDetectorSequence extends IProgressPlanSequence
  
  final case object RUN_SLICE_PEAKEL_DETECTION_STEP1 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectPeakels -> run slice loading"
  }
  final case object RUN_SLICE_PEAKEL_DETECTION_STEP2 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectPeakels -> PeakListTree update"
  }
  final case object RUN_SLICE_PEAKEL_DETECTION_STEP3 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectPeakels -> peaks sorting"
  }
  final case object RUN_SLICE_PEAKEL_DETECTION_STEP4 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectPeakels -> peakel detection"
  }
  final case object RUN_SLICE_PEAKEL_DETECTION_STEP5 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectPeakels -> update used peaks"
  }
  
  def newRunSlicePeakelDetectionProgressPlan() = {
    ProgressPlan[MzDbFeatureDetectorSequence](
      name = "Unsupervised peakel detection progression in run slice",
      steps = Seq(
        ProgressStep( RUN_SLICE_PEAKEL_DETECTION_STEP1 ),
        ProgressStep( RUN_SLICE_PEAKEL_DETECTION_STEP2 ),
        ProgressStep( RUN_SLICE_PEAKEL_DETECTION_STEP3 ),
        ProgressStep( RUN_SLICE_PEAKEL_DETECTION_STEP4 ),
        ProgressStep( RUN_SLICE_PEAKEL_DETECTION_STEP5 )
      )
    )
  }
  
  
  trait MzDbFeatureDetectorSequence extends IProgressPlanSequence
  
  final case object FEATURE_DETECTION_STEP1 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectFeatures -> peakel detection in mzDB file"
  }
  final case object FEATURE_DETECTION_STEP2 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectFeatures -> peakels correlation"
  }
  final case object FEATURE_DETECTION_STEP3 extends IProgressStepIdentity {
    val stepDescription = "MzDbFeatureDetector.detectFeatures -> peakel patterns computation "
  }
  
  def newFeatureDetectionProgressPlan() = {
    ProgressPlan[MzDbFeatureDetectorSequence](
      name = "Unsupervised feature detection progression in mzDB file",
      steps = Seq(
        ProgressStep( FEATURE_DETECTION_STEP1 ),
        ProgressStep( FEATURE_DETECTION_STEP2 ),
        ProgressStep( FEATURE_DETECTION_STEP3 )
      )
    )
  }
}

private object Wrapper {
  // TODO: factorize this code
  // BEGIN OF STOLEN FROM MzDbFeatureExtractor
  implicit class RichRunSliceData(val self: RunSliceData) extends AnyVal {
    def getPeakListBySpectrumId(): LongMap[PeakList] = {
      self.getSpectrumSliceList.filter(_.getData.getPeaksCount > 0).toLongMapWith { ss => 
        ss.getSpectrumId -> new PeakList(ss)
      }
    }
  }
  // END OF STOLEN FROM MzDbFeatureExtractor
}

/**
 * @author David Bouyssie
 *
 */
class MzDbFeatureDetector(
  mzDbReader: MzDbReader,
  @BeanProperty var ftDetectorConfig: FeatureDetectorConfig = FeatureDetectorConfig()
) extends LazyLogging {
  
  import Wrapper._
  
  val ms1SpectrumHeaderById = mzDbReader.getMs1SpectrumHeaders().toLongMapWith( sh => sh.getId.toLong -> sh )
  val ms2SpectrumHeaders = mzDbReader.getMs2SpectrumHeaders()
  //val ms2SpectrumHeaderById = ms2SpectrumHeaders.map( sh => sh.getId.toInt -> sh ).toMap
  val ms2SpectrumHeadersByCycle = ms2SpectrumHeaders.groupByLong(_.getCycle.toInt)
  val ms2SpectrumHeadersById = ms2SpectrumHeaders.toLongMapWith( sh => sh.getId.toLong -> sh )
  
  /**
   * Detect peakels using the unsupervised peakel detector
   */
  def detectPeakels(rsIter: Iterator[RunSlice], nbThreads: Option[Int] = None): Array[Peakel] = {
    
    val promise = Promise[Array[Peakel]]
    val onDetectedPeakels = { observablePeakels: Observable[Array[Peakel]] =>
      
      // Listen for peakels in a distinct thread
      val futurePeakels = Future {
        // Create a buffer that contains the detected peakels
        // Note: the buffer must be synchronized when accessed by multiple threads
        val peakelsBuffer = new ArrayBuffer[Peakel](50000) // we assume to have at least 50000 peakels
        
        observablePeakels
          .toBlocking
          .subscribe({ peakels =>
            peakelsBuffer ++= peakels
          })
        
        promise.success(peakelsBuffer.toArray)
        
      }(scala.concurrent.ExecutionContext.Implicits.global)
    }
    
    this.detectPeakelsAsync(rsIter, onDetectedPeakels, nbThreads)
    
    Await.result(promise.future,Duration.Inf)
  }
  
  /*def detectPeakelsAsync(
    rsIter: Iterator[RunSlice],
    nbThreads: Option[Int] = None
  ): Unit = {
    
    val peakelsPromise = Promise[Array[Peakel]]
    val onDetectedPeakels = { observablePeakels: Observable[Array[Peakel]] =>
      peakelsPromise.success(observablePeakels)
    }
    
    this.detectPeakelsAsync(rsIter, onDetectedPeakels, nbThreads)
  }*/
  
  def detectPeakelsAsync[Unit](
    rsIter: Iterator[RunSlice],
    onDetectedPeakels: Observable[Array[Peakel]] => Unit,
    nbThreads: Option[Int] = None
  ) {
    
    // Determine the thread pool size
    val poolSize = if (nbThreads.isDefined) nbThreads.get
    else {
      val nbProcessors = Runtime.getRuntime().availableProcessors()
      math.max( 1, (nbProcessors / 2).toInt )
    }
    
    // Create an implicit execution context for the execution of the consumers
    val threadPool = Executors.newFixedThreadPool( poolSize ).asInstanceOf[ThreadPoolExecutor]
    
    try {
      val spectrumHeaderById = if (ftDetectorConfig.msLevel == 1) { ms1SpectrumHeaderById } else { ms2SpectrumHeadersById }
      
      val peakelDetector = new UnsupervisedPeakelDetector(
        spectrumHeaderById = spectrumHeaderById,
        nfBySpectrumId = LongMap.empty[Float],
        mzTolPPM = ftDetectorConfig.mzTolPPM,
        peakelFinder = BuildPeakelFinder(ftDetectorConfig.peakelFinderConfig)
      )
      
      // Create a queue to parallelize the peakel detection process
      val nbConsumers = threadPool.getMaximumPoolSize
      logger.info(s"Will perform detection using $nbConsumers cores")
      val detectorQueue = new PeakelDetectorQueue( nbConsumers ) // nbConsumers * 2
      //val queue = new ArrayBlockingQueue[Option[PeakelDetectorQueueEntry]](nbConsumers)
      
      // Create an execution context for the execution of the consumers
      val execCtx = ExecutionContext.fromExecutor(threadPool)
      
      // Create as many consumers as nbConsumers
      val consumers = (for( consumerNumber <- 1 to nbConsumers ) yield {
        new PeakelDetectorConsumer(
          consumerNumber = consumerNumber,
          peakelDetector = peakelDetector,
          detectorQueue = detectorQueue
          //queue = queue
        )(execCtx)
      }) toList
      
      // Merge the consumers observable into a single one and then redirect detected peakels to the PublishSubject
      val observablePeakels = consumers.map( _.observable ).toObservable.flatten.doOnError( { error: Throwable =>
        logger.error(s"exiting consumers with error", error)
        detectorQueue.enqueueException(error)
      })
      
      onDetectedPeakels( observablePeakels )
      
      // Produce run slices for asynchronous peakel detection 
      _produceRunSlicesForPeakelDetection(rsIter, detectorQueue, consumers, spectrumHeaderById)
      
      ()
      
    } finally {
      logger.debug( "Shutting down thread pool used for peakel detection..." )
      if(threadPool.isShutdown() == false ) threadPool.shutdownNow()
    }
    
    //System.gc()
    
    //Thread.sleep(10000)
  }
  
  private def _produceRunSlicesForPeakelDetection(
    rsIter: Iterator[RunSlice],
    detectorQueue: PeakelDetectorQueue,
    //queue: ArrayBlockingQueue[Option[PeakelDetectorQueueEntry]],
    consumers: List[PeakelDetectorConsumer],
    spectrumHeaderById: LongMap[SpectrumHeader]
  ): Unit = {
    
    // Define a peaklist map (first level = runSliceNumber, second level =spectrumId )
    val pklBySpectrumIdAndRsNumber = new LongMap[LongMap[PeakList]]()
    
    // Execute the detection loop in a protected try/catch block
    try {
      
      // Retrieve run slices headers
      val rsHeaders = mzDbReader.getRunSliceHeaders(ftDetectorConfig.msLevel)
      val rsHeaderByNumber = rsHeaders.map { rsh => rsh.getNumber -> rsh } toMap
      
      // Define some vars for the run slice iteration
      var( prevRsNumber, nextRsNumber ) = (0,0)
      var rsOpt = if( rsIter.hasNext) Some(rsIter.next) else None
      
      // Iterate over run slice headers
      // This loops acts as a producer for the detectorQueue
      while( (rsIter.hasNext || rsOpt.isDefined ) && detectorQueue.hasExceptions() == false ) { // && pklBySpectrumIdAndRsNumber.size < 3
        
        val rs = rsOpt.get
        val rsh = rs.getHeader
        val rsd = rs.getData
        val rsNumber = rsh.getNumber
        val nextRsNumber = rsNumber + 1
        
        val peaksCountSum = rsd.getSpectrumSliceList.view.map(_.getData.getPeaksCount).sum
        if( peaksCountSum == 0 ) {
          logger.warn(s"Run slice $rsNumber (${rsh.getBeginMz},${rsh.getEndMz}) is empty")
        }
        else {
          
          val curPeaklistBySpectrumId = rsd.getPeakListBySpectrumId
         
          // Retrieve run slices and their corresponding id
          this.logger.debug(s"will process run slice $rsNumber (${rsh.getBeginMz},${rsh.getEndMz})")
          
          // Build the list of obsolete run slices
          val rsNumbersToRemove = for(
            processedRsNumber <- pklBySpectrumIdAndRsNumber.keys
            if processedRsNumber != rsNumber &&
               processedRsNumber != prevRsNumber &&
               processedRsNumber != nextRsNumber
          ) yield processedRsNumber
    
          // Clean the peaklist map => remove obsolete run slices
          rsNumbersToRemove.foreach { pklBySpectrumIdAndRsNumber -= _ }
          
          // Add current run slice peakList to pklBySpectrumIdAndRsNumber
          if ( pklBySpectrumIdAndRsNumber.contains(rsNumber) == false ) {
            pklBySpectrumIdAndRsNumber.put( rsNumber, curPeaklistBySpectrumId )
          }
          
          // Add next run slice peaklist to pklBySpectrumIdAndRsNumber
          val nextRsOpt = if (rsIter.hasNext == false) None
          else {
            this.logger.debug(s"loading run slice ${rsNumber+1}...")
            val nextRs = rsIter.next
            val nextRsh = nextRs.getHeader
            this.logger.debug(s"run slice ${rsNumber+1} (${nextRsh.getBeginMz},${nextRsh.getEndMz}) has been loaded !")
            
            pklBySpectrumIdAndRsNumber.put( nextRsNumber, nextRs.getData.getPeakListBySpectrumId )
            Some(nextRs)
          }
          
          // Group run slice peakLists into a single map (key = spectrum id)
          val peakListsBySpectrumId = new LongMap[ArrayBuffer[PeakList]]()
          pklBySpectrumIdAndRsNumber.values.foreach { pklBySpectrumId =>
            pklBySpectrumId.foreach { case (spectrumId, pkl) =>
              peakListsBySpectrumId.getOrElseUpdate(spectrumId, new ArrayBuffer[PeakList]) += pkl
            }
          }
          
          // Use the map to instantiate a peakList tree which will be used for peak extraction
          val pklTripletBySpectrumId = peakListsBySpectrumId.map { case (spectrumId, pkls) =>
            Tuple2(spectrumId, new PeakListTriplet( pkls.toArray ) )
          }
          val pklTree = new PeakListTree( pklTripletBySpectrumId, spectrumHeaderById )
          
          this.logger.debug(s"sending run slice $rsNumber (${rsh.getBeginMz},${rsh.getEndMz}) to the consumer")
          
          // Enqueue loaded PeakListTree to send it to the consumer
          // Note that the detectorQueue will wait if it is full
          detectorQueue.enqueue(
            PeakelDetectorQueueEntry(
              rsNumber = rsNumber,
              pklTree = pklTree,
              curPeaklistBySpectrumId = curPeaklistBySpectrumId
            )
          )
          /*queue.put(
            Some(PeakelDetectorQueueEntry(
              rsNumber = rsNumber,
              pklTree = pklTree,
              curPeaklistBySpectrumId = curPeaklistBySpectrumId
            ))
          )*/
          
          // Update some vars
          prevRsNumber = rsNumber
          rsOpt = nextRsOpt
        }
      }
      
      logger.debug( "End of run slices iteration !" )
      
    } catch {
      case e: Throwable => {
        logger.error("Exception raised from peakel detector producer",e)
        throw e
      }
    } finally {
      
      // Small hack: enqueue null values to exit the consumers
      for( consumer <- consumers ) {
        detectorQueue.enqueue(null)
        //queue.put(None)
      }
      
      // Update the progress computer
      //progressComputer.setCurrentStepAsCompleted()
      
      logger.debug( "Detector queue has been stopped !" )
    }
    
    // Throw an exception if the queue has exceptions (its means that a consumer has thrown an exception)
    if (detectorQueue.hasExceptions()) {
      logger.error("Exception caught in detector queue !")
      throw detectorQueue.dequeueException()
    }
    
    ()
  }
  
  /**
   * Performs a peakel detection, then look for grouping peakels
   * in order to have feature (identify peakels belonging to an
   * isotopic pattern
   */
  def detectFeatures(): Array[Feature] = { // LC-MS maps
    val mzTolPPM = ftDetectorConfig.mzTolPPM
    val msLevel = ftDetectorConfig.msLevel
    
    val detectedPeakels = this.detectPeakels(mzDbReader.getLcMsRunSliceIterator())
    val nbDetectedPeakels = detectedPeakels.length
    
    this.logger.debug(s"Has detected # ${nbDetectedPeakels} peakels")
    
    // TODO: configure the duration threshold
    val filteredPeakels = detectedPeakels
      //.sortWith( (a,b) => a.area > b.area )
      //.take(nbDetectedPeakels / 2)
      .filter(_.calcDuration > 5 )
    
    this.logger.debug(s"Has filtered # ${filteredPeakels.length} peakels")

    val detectedFeatures = this._deisotopePeakelsV2(filteredPeakels)
    
    // Link MS2 spectra to features
    for( ft <- detectedFeatures ) {
      
      // Find MS2 spectra concurrent with the detected feature
      val putativeMs2Spectra = new ArrayBuffer[SpectrumHeader]
      for(
        spectrumId <- ft.getSpectrumIds;
        sh = ms1SpectrumHeaderById(spectrumId);
        ms2SpectrumHeaders <- ms2SpectrumHeadersByCycle.get(sh.getCycle)
      ) {
        putativeMs2Spectra ++= ms2SpectrumHeaders
      }
      
      // Compute the m/z tolerance in Daltons
      val mzTolDa = MsUtils.ppmToDa( ft.mz, ftDetectorConfig.mzTolPPM )
    
      // Keep only MS2 spectra having a precursor m/z close to the feature one
      ft.ms2SpectrumIds = putativeMs2Spectra.distinct
        .withFilter( sh => sh.getPrecursorCharge == ft.charge )
        .withFilter( sh => math.abs(sh.getPrecursorMz - ft.mz) <= mzTolDa )
        .map(_.getId)
        .toArray
    }
    
    detectedFeatures
  }
  
  case class PeakelGraphNode(
    charge: Int,
    peakel: Peakel,
    isotopeLevel: Int,
    parentNode: Option[PeakelGraphNode],
    childNodes: ArrayBuffer[PeakelGraphNode] = new ArrayBuffer[PeakelGraphNode]
  ) {
    def isLeaf() = childNodes.isEmpty
    
    def getLeaves( nodeBuffer: ArrayBuffer[PeakelGraphNode] ) {
      for( node <- childNodes ) {
        if( node.isLeaf ) {
          nodeBuffer += node
        }
        else node.getLeaves( nodeBuffer )
      }
    }
    
    def getAllParents( nodeBuffer: ArrayBuffer[PeakelGraphNode] ) {      
      if( parentNode.isDefined ) {
        nodeBuffer += parentNode.get
        parentNode.get.getAllParents(nodeBuffer)
      }
    }
    
    def getLongestPartialPeakelPatterns( leaveBuffer: ArrayBuffer[PeakelGraphNode], areLeftPeakels: Boolean ): Array[PartialPeakelPattern] = {
      this.getLeaves( leaveBuffer)
      if( leaveBuffer.isEmpty ) return Array()
      
      val maxIsotopeLevel = leaveBuffer.maxBy( _.isotopeLevel ).isotopeLevel
      val farthestLeaves = leaveBuffer.filter( _.isotopeLevel == maxIsotopeLevel )
      
      for( farthestLeaf <- farthestLeaves.toArray ) yield {
        val nodeBuffer = new ArrayBuffer[PeakelGraphNode]()
        nodeBuffer += farthestLeaf
        farthestLeaf.getAllParents(nodeBuffer)
        
        PartialPeakelPattern(
          charge,
          nodeBuffer.toArray.map(_.peakel).reverse,
          areLeftPeakels
        )
      }
    }
  }
  
  case class PeakelGraph(
    apex: Peakel,
    leftPeakelNodes: ArrayBuffer[PeakelGraphNode] = new ArrayBuffer[PeakelGraphNode],
    rightPeakelNodes: ArrayBuffer[PeakelGraphNode] = new ArrayBuffer[PeakelGraphNode]    
  ) {
    require( apex != null, "apex is null")
    
    def hasNodes() = !leftPeakelNodes.isEmpty || !rightPeakelNodes.isEmpty
    
    def getBestPeakelPattern(): Option[PeakelPattern] = {
      if( hasNodes == false ) return None
      
      // Retrieve the peakel patten with the minimal RMSD
      val bestPeakelPattern = this.getLongestPeakelPatterns.minBy { peakelPattern =>
        
        val theoPattern = IsotopePatternInterpolator.getTheoreticalPattern(peakelPattern.getMz, peakelPattern.charge)
        val theoAbundances = theoPattern.abundances
        val peakelApexIntensities = peakelPattern.peakels.map(_.area)
        
        IsotopePatternInterpolator.calcAbundancesRmsd(theoAbundances, peakelApexIntensities)
      }
      
      Some(bestPeakelPattern)
    }
    
    def getLongestPeakelPatterns(): Array[PeakelPattern] = {
      if( hasNodes == false ) return Array()
      
      val partialPeakelPatternBuffer = new ArrayBuffer[PartialPeakelPattern]()
      
      for( leftPeakelNode <- leftPeakelNodes ) {
        val leaveBuffer = new ArrayBuffer[PeakelGraphNode]()
        if( leftPeakelNode.isLeaf() ) leaveBuffer += leftPeakelNode
        partialPeakelPatternBuffer ++= leftPeakelNode.getLongestPartialPeakelPatterns( leaveBuffer, areLeftPeakels = true)
      }
      
      for( rightPeakelNode <- rightPeakelNodes ) {
        val leaveBuffer = new ArrayBuffer[PeakelGraphNode]()
        if( rightPeakelNode.isLeaf() ) leaveBuffer += rightPeakelNode
        partialPeakelPatternBuffer ++= rightPeakelNode.getLongestPartialPeakelPatterns( leaveBuffer, areLeftPeakels = false)
      }
      
      // Group partial patterns by charge
      val partialPeakelPatternsGroupedByCharge = partialPeakelPatternBuffer.groupBy(_.charge)
      
      val peakelPatternBuffer = new ArrayBuffer[PeakelPattern]()
      
      for( (charge,partialPeakelPatterns) <- partialPeakelPatternsGroupedByCharge ) {
        val (leftPartialPatterns,rightPartialPatterns) = partialPeakelPatterns.partition(_.areLeftPeakels)
 
        // Combine partial peakel patterns
        // Note: that a null value is introdcued to be sure we iterate on both arrays
        for(
          leftPartialPattern <- (if( leftPartialPatterns.isEmpty ) ArrayBuffer[PartialPeakelPattern](null) else leftPartialPatterns);
          rightPartialPattern <- (if( rightPartialPatterns.isEmpty ) ArrayBuffer[PartialPeakelPattern](null) else rightPartialPatterns)
        ) {
          
          val peakels = (
            Option(leftPartialPattern).map(_.peakels).getOrElse(Array()) ++ 
            Array(apex) ++
            Option(rightPartialPattern).map(_.peakels).getOrElse(Array())
          )
          
          peakelPatternBuffer += PeakelPattern(
            apex = apex,
            peakels = peakels.sortBy(_.getMz),
            charge = charge
          )
        }    
      }
      
      peakelPatternBuffer.toArray
    }
  }
  
  case class PartialPeakelPattern(
    charge: Int,
    peakels: Array[Peakel],
    areLeftPeakels: Boolean    
  )
  
  case class PeakelPattern(
    apex: Peakel,
    peakels: Array[Peakel],
    charge: Int
  ) {
    require( apex != null, "apex is null")
    require( peakels != null, "peakels is null")
    require( peakels.isEmpty == false, "peakels is empty")
    
    lazy val abundance = peakels.foldLeft(0f)( (s,p) => s + p.area )
    def getMz = peakels.head.getMz
  }
  
  def _deisotopePeakelsV2(filteredPeakels: Array[Peakel]): Array[Feature] = {
    
    val mzTolPPM = ftDetectorConfig.mzTolPPM
    
    // --- Combine peakels to obtain features ---
    val peakelsGroupedByTime = this.groupCorrelatedPeakels(filteredPeakels)    
    this.logger.debug(s"has correlated ${peakelsGroupedByTime.length} peakels groups" )
    
    val peakelPatternsBuffer = new ArrayBuffer[PeakelPattern]()
    
    for( (groupTime,peakelGroup) <- peakelsGroupedByTime.par ) {
      
      val tmpPeakelPatternsBuffer = new ArrayBuffer[PeakelPattern]()
      val usedPeakelSetByCharge = new LongMap[HashSet[Peakel]]()
      
      // Sort peakels by desc area
      //val sortedPeakels = uniqueMzPeakels.sortWith { (a,b) => a.area > b.area }
      val sortedPeakels = peakelGroup.sortWith { (a,b) => a.area > b.area }
      val peakelsCount = sortedPeakels.length
      
      // Iterate over peakels to build graph of peakels
      for( apexPeakelIdx <- 0 until peakelsCount ) {
        
        val apexPeakel = sortedPeakels(apexPeakelIdx)
        val apexPeakelMz = apexPeakel.getMz
        val otherPeakels = sortedPeakels.slice(apexPeakelIdx + 1, peakelsCount)
        
        // Sort other peakels by ascending m/z value
        val otherPeakelsSortedByMz = otherPeakels.sortBy(_.getMz)
        
        // Partition peakels with a m/z lower or higher than current apex        
        val( leftPeakels, rightPeakels ) = otherPeakelsSortedByMz.partition(_.getMz < apexPeakelMz)
        
        // Create a new peakel graph for this apex peakel
        val peakelGraph = PeakelGraph( apex = apexPeakel )
        
        // Expand peakel graph in the left direction
        this._expandPeakelGraph(
          peakelGraph = peakelGraph,
          // Reverse the left array to iterate peakels in m/z descending order
          mzSortedPeakels = leftPeakels.reverse,
          areLeftPeakels = true,
          usedPeakelSetByCharge = usedPeakelSetByCharge
        )
        
        // Expand peakel graph in the right direction
        this._expandPeakelGraph(
          peakelGraph = peakelGraph,
          mzSortedPeakels = rightPeakels,
          areLeftPeakels = false,
          usedPeakelSetByCharge = usedPeakelSetByCharge
        )
        
        if( peakelGraph.hasNodes() ) {
          val bestPeakelPatternOpt = peakelGraph.getBestPeakelPattern()
          require( bestPeakelPatternOpt.isDefined, "best peakel pattern should be defined for a graph containing nodes")
          
          // Skip features with one unique isotope
          if( bestPeakelPatternOpt.get.peakels.length > 1 ) {
            // Append best peakel pattern into a TEMP buffer
            tmpPeakelPatternsBuffer += bestPeakelPatternOpt.get
          }
        }
      }
      
      // Append tmpPeakelPatternsBuffer to peakelPatternsBuffer
      peakelPatternsBuffer.synchronized {
        peakelPatternsBuffer ++= tmpPeakelPatternsBuffer
      }
      
      //logger.debug("peakel graphs count=" + peakelGraphBuffer.length )
    }
    
    // --- Clusterize peakel patterns using SetClusterer fork ---
    
    logger.info( s"obtained ${peakelPatternsBuffer.length} peakel patterns before clustering" )
    
    // Map peakel index by each peakel
    val peakelIdxByPeakel = filteredPeakels.zipWithIndex.toMap
    
    // Map peakel indices by each found peakel pattern
    // Note that parallel processing seems to lead to the insertion of null values in peakelPatternsBuffer
    val peakelIndexSetByPeakelPattern = peakelPatternsBuffer.withFilter(_ != null ).map { peakelPattern =>
      val peakels = peakelPattern.peakels
      val peakelIndexSet = peakels.map( peakelIdxByPeakel(_) ).toSet
      peakelPattern -> peakelIndexSet
    } toMap

    // Apply the peakel pattern clustering to remove duplicated patterns (due to sliding window)
    val clusters = SetClusterer.clusterizeMappedSets(peakelIndexSetByPeakelPattern)
    val supersetClusterCount = clusters.count( _.isSubset == false )
    logger.info( s"obtained ${supersetClusterCount} peakel pattern clusters after clustering" )
    
    val detectedFeatures = new ArrayBuffer[Feature]()
    
    for( cluster <- clusters; if cluster.isSubset == false ) {
      
      val peakelPattern = cluster.samesetsKeys.head
      val charge = peakelPattern.charge
      
      detectedFeatures += new Feature(
        Feature.generateNewId(),
        peakelPattern.getMz,
        peakelPattern.charge,
        peakelPattern.peakels.zipWithIndex //Feature.alignPeakels(peakelPattern.peakels)
      )
      
      val values = peakelPattern.peakels.flatMap( peakel => 
        List(peakel.getMz.toString, peakel.area.toString)
      )
      
      val patternTime = peakelPattern.peakels.head.calcWeightedAverageTime
      val patternDuration = peakelPattern.peakels.head.calcDuration
      
    }

    detectedFeatures.toArray
  }
  
  private def _expandPeakelGraph(
    peakelGraph: PeakelGraph,
    mzSortedPeakels: Array[Peakel],
    areLeftPeakels: Boolean,
    usedPeakelSetByCharge: LongMap[HashSet[Peakel]]
  ) {
    
    // Retrieve some values
    val mzTolPPM = ftDetectorConfig.mzTolPPM
    val maxCharge = 10 // TODO: retrieve from config
    val avgIsotopeMassDiff = PeakListTree.avgIsotopeMassDiff
    val apexMz = peakelGraph.apex.getMz
    val apexArea = peakelGraph.apex.area
    
    // Iterate over sorted peakels
    var peakelOffset = 0
    breakable {
      for( peakel <- mzSortedPeakels ) {
        peakelOffset += 1
  
        val peakelMz = peakel.getMz
        val( absMzDiff, putativeZ ) = calcIsotopeAbsMzDiffAndCharge( apexMz, peakelMz )
        
        // If putativeZ is greater than zero and ot too high
        if ( putativeZ > 0 && putativeZ <= maxCharge ) {
          val mzTolDa = MsUtils.ppmToDa(peakelMz, mzTolPPM)
          
          val( firstArea, secondArea ) = if( areLeftPeakels ) (peakel.area,apexArea) else (apexArea, peakel.area )
          
          val isIsotopeRatioOK = this._checkIsotopeRatio(
            putativeZ,
            apexMz,
            firstIntensity = firstArea,
            secondIntensity = secondArea,
            maxDiffFactor = 5 // TODO: put in config ???
          )
          
          // If this peakel is not already used, area is isotopically valid and m/z diff is matching 
          if(
            !(usedPeakelSetByCharge.contains(putativeZ) &&
            usedPeakelSetByCharge(putativeZ).contains(peakel)) &&
            peakel.area < apexArea && isIsotopeRatioOK &&
            math.abs(absMzDiff - (avgIsotopeMassDiff/putativeZ) ) < mzTolDa
          ) {
            
            // Memorize we used this peakel with this charge state
            usedPeakelSetByCharge.getOrElseUpdate(putativeZ, new HashSet[Peakel]) += peakel
            
            // Create a new graph node
            val peakelGraphNode = new PeakelGraphNode(
              charge = putativeZ,
              peakel = peakel,
              isotopeLevel = 1,
              parentNode = Option.empty[PeakelGraphNode]
            )
            
            if( areLeftPeakels )
              peakelGraph.leftPeakelNodes += peakelGraphNode
            else
              peakelGraph.rightPeakelNodes += peakelGraphNode
            
            this._expandPeakelGraphNode(peakelGraphNode, mzSortedPeakels, areLeftPeakels, peakelOffset, usedPeakelSetByCharge)
            
            // Break to stop on first peakel path match
            break
          }
        }
      }
    }
    
  }

  private def _expandPeakelGraphNode(
    peakelGraphNode: PeakelGraphNode,
    mzSortedPeakels: Array[Peakel],
    areLeftPeakels: Boolean,
    peakelOffset: Int,
    usedPeakelSetByCharge: LongMap[HashSet[Peakel]]
  ) {
    if( peakelOffset == mzSortedPeakels.length ) return
    
    // Retrieve some values
    val avgIsotopeMassDiff = PeakListTree.avgIsotopeMassDiff
    val isotopeLevel = peakelGraphNode.isotopeLevel
    val charge = peakelGraphNode.charge
    val avgIsoMzDiff = avgIsotopeMassDiff / charge
    val refPeakel = peakelGraphNode.peakel
    val refPeakelMz = refPeakel.getMz
    val mzThreshold = if( areLeftPeakels ) refPeakelMz - avgIsoMzDiff else refPeakelMz + avgIsoMzDiff
    val mzTolDa = MsUtils.ppmToDa(refPeakelMz, ftDetectorConfig.mzTolPPM)
    
    // Iterate over sorted peakels 
    breakable {
      for( peakelIdx <- peakelOffset until mzSortedPeakels.length ) {
        
        val peakel = mzSortedPeakels(peakelIdx)
        val peakelMz = peakel.getMz
        val absMzDiffWithRefPeakel = math.abs(refPeakelMz - peakel.getMz)
        
        // Break if we are too far in m/z values
        if( (areLeftPeakels && peakelMz < mzThreshold) || (! areLeftPeakels && peakelMz > mzThreshold) ){
          break
        }
        
        // Must have area lower than the reference peakel and a compatible m/z
        // Note that we do not check isotope ratios when expanding nodes (it is done only in expandPeakeGraph)
        if( peakel.area < refPeakel.area && math.abs(absMzDiffWithRefPeakel - avgIsoMzDiff) < mzTolDa ) {

          // Memorize we used this peakel with this charge state
          usedPeakelSetByCharge.getOrElseUpdate(charge, new HashSet[Peakel]) += peakel
          
          val childNode = new PeakelGraphNode(
            charge = charge,
            peakel = peakel,
            isotopeLevel = isotopeLevel + 1,
            parentNode = Some(peakelGraphNode)
          )
          peakelGraphNode.childNodes += childNode
          
          //println(s"en charge=$charge refPeakelMz=$refPeakelMz and peakelMz=$peakelMz (left=$areLeftPeakels)")
          
          this._expandPeakelGraphNode(childNode, mzSortedPeakels, areLeftPeakels, peakelIdx + 1, usedPeakelSetByCharge)
        }
      }
    }
    
  }
  
  private def _checkIsotopeRatio(
    charge: Int,
    firstMz: Double,
    firstIntensity: Float,
    secondIntensity: Float,
    maxDiffFactor: Float // 0.7 is ln(2)
  ): Boolean = {
    
    // Check isotopic pattern abundance ratio before prepending the peakel
    val theoPattern = IsotopePatternInterpolator.getTheoreticalPattern(firstMz, charge)
    val theoAbundances = theoPattern.abundances
    val theoIsoRatio2_1 = theoAbundances(1) / theoAbundances(0)
    val obsIsoRatio2_1 = secondIntensity / firstIntensity
    
    //println(s"theoIsoRatio2_1=$theoIsoRatio2_1 obsIsoRatio2_1=$obsIsoRatio2_1")
    
    // Check the range of observed ratio is valid compared to theoretical one
    ( math.abs(math.log(obsIsoRatio2_1) - math.log(theoIsoRatio2_1)) < math.log(maxDiffFactor) )
  }
  
  def _deisotopePeakelsV1(filteredPeakels: Array[Peakel]): Array[Feature] = {
    
    val mzTolPPM = ftDetectorConfig.mzTolPPM
    
    // --- Combine peakels to obtain features ---
    val peakelsGroupedByTime = this.groupCorrelatedPeakels(filteredPeakels)    
    this.logger.debug(s"has correlated ${peakelsGroupedByTime.length} peakels groups" )
    
    // Generate a sequence of possible isotope diffs for x charge states
    val maxZ = 10
    val maxIsotopesCount = 5 // TODO: use averagine ???
    val isotopeDiffTol = 0.01 // TODO: make some advanced statistics to infer this value
    val avgIsotopeMassDiff = PeakListTree.avgIsotopeMassDiff
    val peakelIdxByPeakel = filteredPeakels.zipWithIndex.toMap
    
    val peakelPatternsBuffer = new ArrayBuffer[PeakelPattern]
    
    peakelPatternsBuffer.synchronized {
      
      for( (groupTime,peakelGroup) <- peakelsGroupedByTime.par ) {
        
        // FIXME: filter out peakels having the same m/z value
        
        logger.trace(s"grouped ${peakelGroup.length} peakel(s) at average time ="+groupTime)
        
        // Compute an histogram of peakels by m/z BIN ???
        
        // Sort peakels by desc area
        val sortedPeakels = peakelGroup.sortWith { (a,b) => a.area > b.area }
        val peakelPatternBuffersByCharge = new LongMap[HashMap[Peakel,ListBuffer[Peakel]]]
        
        // Iterate over tested charge states
        for( z <- 1 to maxZ ) {
          val avgIsoMzDiff = avgIsotopeMassDiff / z
          val peakelPatternBufferByApex = peakelPatternBuffersByCharge.getOrElseUpdate(z, new HashMap[Peakel,ListBuffer[Peakel]])
          
          // Try to find compatible peakels using combinatorics approach
          // We assume to find only peakels of lowest intensity when looking over sibling isotopes
          for( peakel <- sortedPeakels ) {
            
            val mzTolDa = MsUtils.ppmToDa(peakel.getMz, mzTolPPM)
            
            val (minIsoMzDiff, maxIsoMzDiff) = (avgIsoMzDiff - mzTolDa, avgIsoMzDiff + mzTolDa)
            
            // Create a new putative peakel group for this peakel
            val peakelPatternBuffer = new ListBuffer[Peakel]()       
            peakelPatternBuffer += peakel
            peakelPatternBufferByApex += peakel -> peakelPatternBuffer
            
            // Sort other peakels by ascending m/z difference
            val otherPeakelsSortedByMzDiff = sortedPeakels.sortWith { (a,b) =>
              math.abs(a.getMz - peakel.getMz) < math.abs(b.getMz - peakel.getMz)
            }
            
            // Iterate over neighboring peakels (take care to skip peakels having the same m/z)
            breakable {
              // FIXME: remove this workaround for filtering peakels of same m/z value
              for( neighboringPeakel <- otherPeakelsSortedByMzDiff if peakel.getMz != neighboringPeakel.getMz) {
              //for( neighboringPeakel <- otherPeakelsSortedByMzDiff if peakel != neighboringPeakel) {
                
                val( prevPeakel, prevPeakelIdx ) = if( neighboringPeakel.getMz > peakelPatternBuffer.last.getMz ) {
                  (peakelPatternBuffer.last, peakelPatternBuffer.length - 1)
                }
                else if (neighboringPeakel.getMz < peakelPatternBuffer.head.getMz ) {
                  (peakelPatternBuffer.head, 0)
                }
                else {
                  throw new Exception(
                    "invalid neighboring peakel m/z" + s"""neighboringPeakel.mz: ${neighboringPeakel.getMz}
                    peakelPatternBuffer.last.mz:  ${ peakelPatternBuffer.last.getMz}
                    peakelPatternBuffer.head.mz:  ${ peakelPatternBuffer.head.getMz}"""
                  )
                }
                
                // Compute m/z diff with reference peakel
                // TODO check if first peakel is better
                val absMzDiffWithRefPeakel = math.abs(neighboringPeakel.getMz - peakel.getMz)

                // Break if neighboring peakel m/z is too far
                val isotopesDiffCount = math.round(absMzDiffWithRefPeakel * z)
                if( isotopesDiffCount > maxIsotopesCount ) break
                
                // Compute m/z diff with previous peakel
                val absMzDiffWithPrevPeakel = math.abs(neighboringPeakel.getMz - prevPeakel.getMz)
                
                var addNeighboringPeakel = false
                
                // Check if the new neighboring peakel has a lower intensity than previous one
                if( neighboringPeakel.area < prevPeakel.area ) {
                  
                  // Check if we are still on the same isotope than with the previous peakel (except for the reference peakel)
                  if( prevPeakel != peakel && absMzDiffWithPrevPeakel < isotopeDiffTol ) {
                    
                    // We have to chose if this new peakel should replace the previous appended one
                    val absMzDiffBetweenPrevAndRefPeakels = math.abs(prevPeakel.getMz - peakel.getMz)
                    
                    val expectedMzDiffFromRefPeakel = isotopesDiffCount * avgIsoMzDiff
                    
                    if( math.abs(absMzDiffBetweenPrevAndRefPeakels - expectedMzDiffFromRefPeakel) >
                        math.abs(absMzDiffWithRefPeakel - expectedMzDiffFromRefPeakel)
                      ) {
                      peakelPatternBuffer.remove(prevPeakelIdx)
                      addNeighboringPeakel = true
                    }
                  // Check the result is under the isotope diff tolerance
                  // if( math.abs(absMzDiffWithPrevPeakel - avgIsoMzDiff) < isotopeDiffTol )
                  } else if (absMzDiffWithPrevPeakel <= maxIsoMzDiff && absMzDiffWithPrevPeakel >= minIsoMzDiff) {
                    addNeighboringPeakel = true
                  }                    
                }
                
                if( addNeighboringPeakel ) {
                  // If previous peakel is at the end of the pattern
                  // If new peakel has m/z greater than the highest one
                  if( neighboringPeakel.getMz > peakelPatternBuffer.last.getMz ) {
                    // Append neighboring peakel to the buffer
                    peakelPatternBuffer.append(neighboringPeakel)
                  }
                  // Else we assume previous peakel is at the beginning of the pattern
                  else if( neighboringPeakel.getMz < peakelPatternBuffer.head.getMz ) {
                    
                    // Check isotopic pattern abundance ratio before prepending the peakel
                    val theoPattern = IsotopePatternInterpolator.getTheoreticalPattern(neighboringPeakel.getMz, z)
                    val theoAbundances = theoPattern.abundances
                    val theoIsoRatio2_1 = theoAbundances(1) / theoAbundances(0)
                    val obsIsoRatio2_1 = peakelPatternBuffer.head.getApexIntensity / neighboringPeakel.getApexIntensity
                    //val peakelApexIntensities = ft.peakels.map(_.getApex().getIntensity)
                    //val rmsd = IsotopePatternInterpolator.calcAbundancesRmsd(theoAbundances, peakelApexIntensities)
                    
                    // Check the range of observed ratio is valid compared to theoretical one
                    if( math.abs(math.log(obsIsoRatio2_1) - math.log(theoIsoRatio2_1)) < 0.7 ) {// 0.7 is ln(2)
                      // Prepend neighboring peakel to the buffer
                      peakelPatternBuffer.prepend(neighboringPeakel)
                    }
                  }
                  else {
                    this.logger.debug("neighboringPeakel.mz: "+ neighboringPeakel.getMz)
                    this.logger.debug("peakelPatternBuffer.last.mz:" + peakelPatternBuffer.last.getMz)
                    this.logger.debug("peakelPatternBuffer.head.mz:" + peakelPatternBuffer.head.getMz)
                    
                    throw new Exception("invalid neighboring peakel m/z")
                  }
                }
  
              } // End of sibling peakels loop
            } // End of breakable
          } // End of peakel loop
        } // End of Z loop
        
        // Convert peakel pattern buffers into peakel patterns
        val newPeakelPatterns = for(
          (charge,peakelPatternBufferByApex) <- peakelPatternBuffersByCharge;
          (apexPeakel,peakelPatternBuffer) <- peakelPatternBufferByApex;
          if peakelPatternBuffer.length > 1 // remove features with one unique peakel
        ) yield PeakelPattern(apexPeakel,peakelPatternBuffer.toArray,charge.toInt)
        
        logger.trace( s"found ${newPeakelPatterns.size} new peakel patterns" )
        
        // Clusterize peakel patterns using SetClusterer fork
        val peakelIndexSetByNewPeakelPattern = newPeakelPatterns.map { peakelPattern =>
          val peakelIndexSet = peakelPattern.peakels.map( peakelIdxByPeakel(_) ).toSet
            peakelPattern -> peakelIndexSet
        } toMap
    
        val newPeakelClusters = SetClusterer.clusterizeMappedSets(peakelIndexSetByNewPeakelPattern)
        logger.trace( s"obtained ${newPeakelClusters.length} new pattern clusters after clustering" )
        
        // Remove subsets and map found clusters by peakel index
        val oversetPeakelPatternByPeakelIdx = new HashMap[Int,ArrayBuffer[PeakelPattern]]
        for( newPeakelCluster <- newPeakelClusters; if newPeakelCluster.isSubset == false ) {
          if( newPeakelCluster.samesetsKeys.length > 1 ) {
            this.logger.trace( "L1 charges = "+newPeakelCluster.samesetsKeys.map(_.charge).mkString(";") )
            this.logger.trace( "cluster length = " + newPeakelCluster.samesetsKeys.length)
            this.logger.trace( "indices ="+newPeakelCluster.samesetsValues.mkString(";") )
          }
          
          val peakelPattern = newPeakelCluster.samesetsKeys.head
          for( peakelIdx <- newPeakelCluster.samesetsValues)
            oversetPeakelPatternByPeakelIdx.getOrElseUpdate(peakelIdx, new ArrayBuffer[PeakelPattern]) += peakelPattern
        }
        
        // Search for peakel pattern clusters having a shared peakel
        val peakelPatternGroups = new ArrayBuffer[List[PeakelPattern]]()
        val assignedPeakelPatterns = new HashSet[PeakelPattern]()
        
        for( newPeakelCluster <- newPeakelClusters;
          if newPeakelCluster.isSubset == false;
          peakelPattern <- newPeakelCluster.samesetsKeys;
          if !assignedPeakelPatterns.contains(peakelPattern)
        ) {
 
          val peakelPatternGroup = SetClusterer.getAllKeysHavingRelatedValues[PeakelPattern,Int](
            peakelIndexSetByNewPeakelPattern,
            oversetPeakelPatternByPeakelIdx.toMap,
            newPeakelCluster.samesetsValues,
            new collection.mutable.HashSet[Int]
          )
          
          if( peakelPatternGroup.isEmpty == false ) {
            peakelPatternGroups += peakelPatternGroup
            assignedPeakelPatterns ++= peakelPatternGroup
          }
        }
        
        // Create a competition between the peakel patterns sharing at least one peakel
        val bestPeakelPatterns = peakelPatternGroups.map { peakelPatternGroup =>
          
          // Minimize the RMSD with the theoretical isotope pattern
          peakelPatternGroup.minBy { peakelPattern =>
            val isotopePattern = IsotopePatternInterpolator.getTheoreticalPattern(
              peakelPattern.getMz, peakelPattern.charge
            )
            val obsAbundances = peakelPattern.peakels.map( _.getApexIntensity )
            IsotopePatternInterpolator.calcAbundancesRmsd(isotopePattern.abundances, obsAbundances)
          }
        }
        
        peakelPatternsBuffer ++= bestPeakelPatterns
      }
    } // end synchronized block
    
    logger.info( s"found a total of ${peakelPatternsBuffer.length} peakel patterns" )
    
    // --- Clusterize peakel patterns using SetClusterer fork ---
    
    // Map peakel indices by each found peakel pattern
    // FIXME: handle nullity before this step
    val peakelIndexSetByPeakelPattern = peakelPatternsBuffer.withFilter(_ != null).map { peakelPattern =>
      val peakelIndexSet = peakelPattern.peakels.withFilter(_ != null).map( peakelIdxByPeakel(_) ).toSet
      peakelPattern -> peakelIndexSet
    } toMap

    // Apply the peakel pattern clustering to remove duplicated patterns (due to sliding window)
    val clusters = SetClusterer.clusterizeMappedSets(peakelIndexSetByPeakelPattern)
    val supersetClusterCount = clusters.count( _.isSubset == false )
    logger.info( s"obtained ${supersetClusterCount} peakel pattern clusters after clustering" )
    
    val detectedFeatures = new ArrayBuffer[Feature]()
    
    for( cluster <- clusters; if cluster.isSubset == false ) {
      
      val peakelPattern = cluster.samesetsKeys.head
      val charge = peakelPattern.charge
      
      detectedFeatures += new Feature(
        Feature.generateNewId(),
        peakelPattern.getMz,
        peakelPattern.charge,
        peakelPattern.peakels.zipWithIndex //Feature.alignPeakels(peakelPattern.peakels)
      )
      
      val values = peakelPattern.peakels.flatMap( peakel => 
        List(peakel.getMz.toString, peakel.area.toString)
      )
      
      val patternTime = peakelPattern.peakels.head.calcWeightedAverageTime
      val patternDuration = peakelPattern.peakels.head.calcDuration
      
    }
    
    detectedFeatures.toArray
  }
  
  /** Group peakels by time using EntityHistogramComputer
   *  A peakel could belong to several bins of the histogram
   *  TODO: check if it is problem for swath detection
   */
  def groupCorrelatedPeakels(peakels: Array[Peakel]): Array[(Float,Array[Peakel])] = {
    
    logger.info("correlating peakels in intensity/time dimensions...")
    
    val peakelTimeByPeakel = Map() ++ peakels.map( peakel => peakel -> peakel.calcWeightedAverageTime )
    
    // Clusterize peakels having an apex separated by a given time value (10 secs)
    val histoComputer = new EntityHistogramComputer( peakels, { peakel: Peakel =>
      peakelTimeByPeakel(peakel)
    })
    
    val peakelTimes = peakelTimeByPeakel.values
    val timeRange = peakelTimes.max - peakelTimes.min
    val peakelBins = histoComputer.calcHistogram( nbins = (timeRange / 6f).toInt )
    
    // TODO: keep the information about the sliding to avoid redundant results
    // Starting points should always come from the second bin
    val peakelsGroupedByTime = new ArrayBuffer[(Float,Array[Peakel])]()
    peakelBins.sliding(3).foreach { peakelBinGroup =>
      
      val peakelGroup = peakelBinGroup.flatMap( _._2 )
      if( peakelGroup.isEmpty == false ) {
        val meanTime = peakelBinGroup.map( _._1.center ).sum / peakelBinGroup.length
        val times = peakelGroup.map( peakelTimeByPeakel(_) )
        logger.debug( s"min time is ${times.min} and max time is ${times.max}")
        peakelsGroupedByTime += meanTime.toFloat -> peakelGroup
      }
    }
    
    peakelsGroupedByTime.toArray
  }
  
  def calcIsotopeAbsMzDiffAndCharge( aMz: Double, bMz: Double): (Double,Int) = {
    val mzDiff = math.abs( aMz - bMz )
    val approxZ = (1.0 / mzDiff).toFloat
    val roundedApproxZ = math.round(approxZ)
    
    (mzDiff, roundedApproxZ)
  }

}