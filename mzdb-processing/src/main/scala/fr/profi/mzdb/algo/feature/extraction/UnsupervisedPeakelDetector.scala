package fr.profi.mzdb.algo.feature.extraction

import java.util.BitSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LongMap
import scala.util.control.Breaks._
import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.algo.signal.detection.HistogramBasedPeakelFinder
import fr.profi.mzdb.model._
import fr.profi.mzdb.util.ms.MsUtils
import fr.profi.api.progress._
import fr.profi.mzdb.algo.signal.detection.SmartPeakelFinder
import fr.profi.mzdb.algo.signal.detection.IPeakelFinder
import fr.profi.mzdb.algo.signal.detection.SmartPeakelFinder

object UnsupervisedPeakelDetector {
  
  // Detect peakels method progress
  trait UnsupervisedPeakelDetectionSequence extends IProgressPlanSequence
  
  final case object DETECTION_STEP1 extends IProgressStepIdentity {
    val stepDescription = "UnsupervisedPeakelDetector.detectPeakels"
  }
  
  def newDetectionProgressPlan() = {
    ProgressPlan[UnsupervisedPeakelDetectionSequence](
      name = "unsupervised peakel detector progression in peaklist tree",
      steps = Seq(
        ProgressStep( DETECTION_STEP1 )
      )
    )
  }
  
  // Extract peakels method progress
  trait UnsupervisedPeakelExtractionSequence extends IProgressPlanSequence
  
  final case object EXTRACTION_STEP1 extends IProgressStepIdentity {
    val stepDescription = "UnsupervisedPeakelDetector.extractPeakel -> peaks extraction in peaklist tree"
  }
  final case object EXTRACTION_STEP2 extends IProgressStepIdentity {
    val stepDescription = "UnsupervisedPeakelDetector.extractPeakel -> peakel detection"
  }
  final case object EXTRACTION_STEP3 extends IProgressStepIdentity {
    val stepDescription = "UnsupervisedPeakelDetector.extractPeakel -> update used peaks"
  }
  
  def newExtractionProgressPlan() = {
    ProgressPlan[UnsupervisedPeakelExtractionSequence](
      name = "unsupervised peakel extraction progression in peaklist tree",
      steps = Seq(
        ProgressStep( EXTRACTION_STEP1 ),
        ProgressStep( EXTRACTION_STEP2 ),
        ProgressStep( EXTRACTION_STEP3 )
      )
    )
  }
  
}

/**
 * @author David Bouyssie
 *
 */
// TODO: move to feature.detection package ???
class UnsupervisedPeakelDetector(
  val spectrumHeaderById: LongMap[SpectrumHeader],
  val nfBySpectrumId: LongMap[Float],
  // TODO: create configs for signal extraction
  val mzTolPPM: Float,
  val maxConsecutiveGaps: Int = 3,
  val maxTimeWindow: Float = 1200f,
  val intensityPercentile: Float = 0.9f,
  val peakelFinder: IPeakelFinder = new SmartPeakelFinder()
) extends LazyLogging {
  
  // TODO: add to config
  val minPeaksCount = peakelFinder.minPeaksCount  
  val minPeakelAmplitude = 1.5f
  
  private val maxHalfDuration = maxTimeWindow / 2
  
  def detectPeakels(
    pklTree: PeakListTree,
    curRsPklColl: PeakListCollection,
    intensityDescPeakCoords: Array[Array[Int]]
  ): Array[Peakel] = {
    
    val nbPeaks = intensityDescPeakCoords.length
    
    // Return if the number of peaks is too low
    if( nbPeaks < 10 ) return Array()
    
    // Instantiate a HashMap which will memorize used peaks
    val usedPeakMap = new HashMap[PeakList,BitSet]()
    usedPeakMap.sizeHint(curRsPklColl.peakListsCount)
    for ( (specId,pklTriplet) <- pklTree.pklTripletBySpectrumId; peaklist <- pklTriplet.peakLists) {
      usedPeakMap(peaklist) = new BitSet(peaklist.peaksCount)
    }
    
    val curRsPeakLists = curRsPklColl.peakLists
    val curRsPeaklistsCount = curRsPeakLists.length
    
    // Set up the progress computer
    val progressComputer = new ProgressComputer( UnsupervisedPeakelDetector.newDetectionProgressPlan() )
    val curStep = progressComputer.resetStepStartingTime(UnsupervisedPeakelDetector.DETECTION_STEP1)
    curStep.setMaxCount(nbPeaks)

    val lowestPeakCoords = intensityDescPeakCoords( Math.min(nbPeaks-1, (nbPeaks * intensityPercentile).toInt) )
    val lowestPeakListIdx = lowestPeakCoords(0)
    val lowestPeakIdx = lowestPeakCoords(1)
    val intensityThreshold = curRsPklColl.getPeakAt(lowestPeakListIdx, lowestPeakIdx).getIntensity
    
    logger.debug(s"detecting peakels using ${minPeaksCount} min peaks count")
    logger.debug(s"detecting peakels using intensity threshold = ${intensityThreshold}")
    
    // Get peaklist tree peaks sorted by m/z value
    val peakelBuffer = new ArrayBuffer[Peakel](20000)
    
    // Instantiate a shared PeakelCoordinates that will be used as a shared memory matrix
    //println("curRsPeaklistsCount: "+ curRsPeaklistsCount)
    val sharedPeakelCoordinates = new PeakelCoordinates(curRsPeaklistsCount)
    
    // Iterate over all peaks sorted by descending order
    breakable {
      var peakNum = 0
      for( peakCoords <- intensityDescPeakCoords ) {
        peakNum += 1
        
        // Update progress every 1000 peaks
        if( peakNum % 1000 == 0 ) {
          // Increment the step count
          curStep.incrementAndGetCount()
        }
        
        val peakListIdx = peakCoords(0)
        val peakIdx = peakCoords(1)
        val peakList = curRsPeakLists(peakListIdx)
        
        // Stop if we reach the lowest acceptable peak
        if (peakListIdx == lowestPeakListIdx && peakIdx == lowestPeakIdx) {
          break
        } else if( usedPeakMap(peakList).get(peakIdx) == false ) {
          
          // Retrieve corresponding spectrum slice
          val spectrumSlice = peakList.getSpectrum
          
          // Update apex peakList and the boundaries in the sharedPeakelCoordinates
          sharedPeakelCoordinates.peakLists(peakListIdx) = peakList
          sharedPeakelCoordinates.minIdx = peakListIdx
          sharedPeakelCoordinates.maxIdx = peakListIdx
          
          // Initiate a peakel extraction using this starting point
          val peakelOpt = this.extractPeakel(
            pklTree,
            usedPeakMap,
            peakListIdx,
            spectrumSlice,
            peakIdx,
            sharedPeakelCoordinates
          )
          
          // Check if we found one peakel
          if( peakelOpt.isDefined ) {
            val peakel = peakelOpt.get
            val apexIdx = peakel.apexIndex
            peakelBuffer += peakelOpt.get
          }
        }
      }
    } // END OF BREAKABLE
        
    // Increment the step count
    curStep.setAsCompleted()
    
    peakelBuffer.toArray
  }
  
  protected def extractPeakel(
    pklTree: PeakListTree,
    usedPeakMap: HashMap[PeakList,BitSet],
    spectrumIdx: Int,
    apexSpectrum: Spectrum,
    apexPeakIdx: Int,
    sharedPeakelCoordinates: PeakelCoordinates
  ): Option[Peakel] = {
    
    // Set up the progress computer
    // val progressComputer = new ProgressComputer( UnsupervisedPeakelDetector.newExtractionProgressPlan() )
    // progressComputer.beginStep(UnsupervisedPeakelDetector.EXTRACTION_STEP1)
    
    val pklTripletBySpectrumId = pklTree.pklTripletBySpectrumId
    val sharedPeakLists = sharedPeakelCoordinates.peakLists
    val sharedPeakIndices = sharedPeakelCoordinates.peakIndices
    
    // Retrieve the spectrum header map
    val pklTreeShMap = pklTree.spectrumHeaderMap
    val pklTreeSpecHeaders = pklTreeShMap.getSpectrumHeaders()
    val shCount = pklTreeSpecHeaders.length
    //println("sharedPeakLists.length "+sharedPeakLists.length)
    
    // Define some values
    val apexSpectrumHeader = apexSpectrum.getHeader
    val apexSpectrumData = apexSpectrum.getData
    val apexMz = apexSpectrumData.getMzList()(apexPeakIdx)
    val apexIntensity = apexSpectrumData.getIntensityList()(apexPeakIdx)
    
    //println("apexMz "+ apexMz)
    //println("apexIntensity "+ apexIntensity)

    val apexTime = apexSpectrumHeader.getTime
    val apexShPklTreeIdx = pklTreeShMap.getSpectrumHeaderIndex(apexSpectrumHeader.getId) //apexSpectrumHeader.getCycle    
    
    // Compute the m/z tolerance in Daltons
    val mzTolDa = MsUtils.ppmToDa( apexMz, mzTolPPM )

    // Define some vars
    var peakelPeaksCount = 0
    var numOfAnalyzedDirections = 0
    var isRightDirection = true
    
    // Loop until left and right directions have been analyzed
    while( numOfAnalyzedDirections < 2 ) {
      
      // Define or reset some vars for the current direction
      var timeOverRange = false
      var consecutiveGapCount = 0
      var shIdxShift = 0
      
      // Stop extraction in this direction if we have too much gaps or if we exceed run time range
      breakable {
        while( consecutiveGapCount <= maxConsecutiveGaps && !timeOverRange ) {
          
          // Decrease spectrum header index shift if LEFT direction
          if( isRightDirection == false ) shIdxShift -= 1
          
          // Determine current spectrum header index
          val curShIdx = apexShPklTreeIdx + shIdxShift
          
          // Try to retrieve the spectrum id
          if( curShIdx < 0 || curShIdx >= shCount ) {//if wrong spectrumID
            timeOverRange = true
            break
          } else {
            val curSpectrumH = pklTreeSpecHeaders(curShIdx)
            val curSpectrumId = curSpectrumH.getId
            val curTime = curSpectrumH.getTime
              
            // TODO: check if total time does not exceed the provided threshold
            if( this.maxHalfDuration > 0 && Math.abs(curTime - apexTime) > this.maxHalfDuration ) {
              timeOverRange = true
              break
            }
            
            // Try to retrieve a peaklist triplet for the current spectrum header
            val pklTriplet = pklTripletBySpectrumId.getOrNull(curSpectrumId)
            require( pklTriplet != null, "no pklTriplet for spectrum #" + curSpectrumId )
            
            // Try to retrieve the nearest peak
            val nearestPeakIdx = pklTriplet.addNearestPeakToPeakelCoordinates(
              apexMz,
              mzTolDa,
              sharedPeakelCoordinates,
              curShIdx
            )
            
            // Check if a peak has been found
            if( nearestPeakIdx != -1 && usedPeakMap(sharedPeakLists(curShIdx)).get(nearestPeakIdx) == false ) {
              consecutiveGapCount = 0
              peakelPeaksCount += 1
            // Else if peak is not defined
            } else {
              // Note that missing peaks are excluded => peakels will have some missing peaks
              consecutiveGapCount += 1
              sharedPeakelCoordinates.removePeak(curShIdx)
            }
  
            // Increase spectrum header index shift if right direction
            if (isRightDirection) {
              shIdxShift += 1
              sharedPeakelCoordinates.maxIdx = curShIdx
            } else {
              sharedPeakelCoordinates.minIdx = curShIdx
            }
            
          } // END OF ELSE
        } // END OF WHILE
      } // END OF BREAKABLE
      
      isRightDirection = false
      
      numOfAnalyzedDirections += 1
    }
    
    //progressComputer.setCurrentStepAsCompleted()
    //progressComputer.beginStep(UnsupervisedPeakelDetector.EXTRACTION_STEP2)
    
    // TODO: define a minimum number of peaks for a peakel in the config
    val peakelAndPeaksOpt = if( peakelPeaksCount < minPeaksCount ) {
      return None
    }
    
    // Retrieve RT / intensity pairs
    val rtIntPairs = sharedPeakelCoordinates.getElutionTimeIntensityPairs()
    require(
      rtIntPairs.length == peakelPeaksCount,
      s"invalid number of RT/intensity pairs for mz=$apexMz: got ${rtIntPairs.length} but expected $peakelPeaksCount"
    )
    //println("peakelPeaksCount: "+ peakelPeaksCount)
    //println("rtIntPairs length: "+ rtIntPairs.length)
    
    // Check if we have enough peaks for peakel detection
    val peakelsIndices = peakelFinder.findPeakelsIndices(rtIntPairs)
    
    // Retrieve the peakel corresponding to the feature apex
    // and memorize peak indices
    var matchingPeakelIdxOpt = Option.empty[(Int, Int)]
    val detectedPeaksIndices = new LongMap[Boolean](rtIntPairs.length)
    
    for( peakelIdx <- peakelsIndices ) {
      if(
        apexTime >= rtIntPairs(peakelIdx._1)._1 && 
        apexTime <= rtIntPairs(peakelIdx._2)._1
      ) matchingPeakelIdxOpt = Some(peakelIdx)
      
      var idx = peakelIdx._1
      while( idx <= peakelIdx._2) {
        detectedPeaksIndices.put(idx, true)
        idx += 1
      }
    }
    
    val peakListIdxByDefPeakIdx = sharedPeakelCoordinates.getDefinedPeakListIndexMapping()
    
    // Remove noisy peaks from future peakel extractions
    val rtIntPairsLength = rtIntPairs.length
    var i = 0
    while (i < rtIntPairsLength) {
      // Check if this peak has belong to a peakel
      if( detectedPeaksIndices.contains(i) == false ) {
        val peakListIdx = peakListIdxByDefPeakIdx(i)
        val peakList = sharedPeakLists(peakListIdx)
        val peakIdx = sharedPeakIndices(peakListIdx)
        // If this is not the case we add it to the usedPeakSet (it corresponds to noise)
        usedPeakMap(peakList).set(peakIdx, true)
      }
      i += 1
    }
    
    if( matchingPeakelIdxOpt.isEmpty ) {
      /*this.logger.warn(
        s"no peakel detected for peak with m/z=${apexMz} and spectrum id=${apexSpectrumHeader.getId}"
      )*/
      return None
    }
    
    // Retrieve peakel peaks
    val matchingPeakelIdx = matchingPeakelIdxOpt.get
    
    // Update sharedPeakelCoordinates boundaries
    sharedPeakelCoordinates.minIdx = peakListIdxByDefPeakIdx(matchingPeakelIdx._1)
    sharedPeakelCoordinates.maxIdx = peakListIdxByDefPeakIdx(matchingPeakelIdx._2)
    
    // Build the peakel
    //println("minIdx:"+sharedPeakelCoordinates.minIdx)
    //println("maxIdx:"+sharedPeakelCoordinates.maxIdx)
    val peakel = sharedPeakelCoordinates.toPeakel()
    
    //progressComputer.setCurrentStepAsCompleted()
    //progressComputer.beginStep(UnsupervisedPeakelDetector.EXTRACTION_STEP3)
    
    // Check that apex is not the first or last peak
//    if( peakel.apexIndex == 0 || peakel.apexIndex == (peakel.spectrumIds.length - 1) ) {
//      return None
//    }
    
    // Check peakel amplitude is big enough
    val minIntensity = peakel.intensityValues.min
    val maxIntensity = peakel.getApexIntensity()
    val intensityAmplitude = if( minIntensity == 0 ) 2f else maxIntensity / minIntensity
    
    if( intensityAmplitude < minPeakelAmplitude ) {
      return None
    }
    
    //println( "peakel.getApexMz(): "+peakel.getApexMz() )
    //println( "peakel.getPeaksCount(): "+peakel.getPeaksCount() )
    
    // Add matching peakel peaks to usedPeakMap
    i = sharedPeakelCoordinates.minIdx
    while (i <= sharedPeakelCoordinates.maxIdx) {
       val peakList = sharedPeakLists(i)
       if (peakList != null) {
         val peakIdx = sharedPeakIndices(i)
         usedPeakMap(peakList).set(peakIdx, true)
       }
      i += 1
    }
    
    //progressComputer.setCurrentStepAsCompleted()

    Some( peakel )
  }

}