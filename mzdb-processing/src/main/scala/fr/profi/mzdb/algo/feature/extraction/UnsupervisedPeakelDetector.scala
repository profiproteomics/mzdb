package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.mzdb.algo.signal.detection.HistogramBasedPeakelFinder
import fr.profi.mzdb.model._
import fr.profi.mzdb.utils.ms.MsUtils
import fr.proline.api.progress._

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
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  // TODO: create configs for signal extraction
  val mzTolPPM: Float,
  val maxConsecutiveGaps: Int = 2,
  val maxTimeWindow: Float = 1200f,
  val minPercentageOfMaxInt: Float = 0.005f
) extends Logging {
  
  /*val msLevel = 2
  
  val ms1ScanIdByCycleNum = scanHeaderById.values
    .withFilter( _.getMsLevel == this.msLevel )
    .map( sh => sh.getCycle -> sh.getId )
    .toMap*/
    
  /*def peakelsToFeatures(peakels: Array[Peakel] ): Array[Feature] = {
    
  }*/
  
  def detectPeakels(pklTree: PeakListTree, intensityDescPeaks: Array[Peak] ): Array[Peakel] = {
    // Return if the number of peaks is too low
    if( intensityDescPeaks.length < 10 ) return Array()
    
    val usedPeakSet = new HashSet[Peak]()
    
    val nbPeaks = intensityDescPeaks.length
    
    // Set up the progress computer
    val progressComputer = new ProgressComputer( UnsupervisedPeakelDetector.newDetectionProgressPlan() )
    val curStep = progressComputer.resetStepStartingTime(UnsupervisedPeakelDetector.DETECTION_STEP1)
    curStep.setMaxCount(nbPeaks)
    
    // Determine an intensity threshold based on quartiles
    //val q3 = math.log10( intensityDescPeaks( (nbPeaks * 0.25).toInt ).getIntensity )
    //val q2 = math.log10( intensityDescPeaks( (nbPeaks * 0.5).toInt ).getIntensity )
    //val q1 = math.log10( intensityDescPeaks( (nbPeaks * 0.75).toInt ).getIntensity )
    //val iqr = (q3 - q2) * 2
    //val intensityThreshold = math.pow(10, q1 - (1 * iqr) )
    val intensityThreshold = intensityDescPeaks( (nbPeaks * 0.9).toInt ).getIntensity
    logger.debug(s"detecting peakels using intensity threshold ="+ intensityThreshold)
    
    val peakelBuffer = new ArrayBuffer[Peakel]()
    
    // Iterate over all peaks sorted by descending order
    breakable {
      for( peak <- intensityDescPeaks ) {
        
        // Increment the step count
        curStep.incrementAndGetCount()
        
        if( peak.getIntensity() < intensityThreshold ) {
          break
        } else if( usedPeakSet.contains(peak) == false ) {
          
          // Retrieve corresponding scan header
          val scanHeader = this.scanHeaderById(peak.getLcContext.getScanId)
          
          // Initiate a peakel extraction using this starting point
          val peakelOpt = this.extractPeakel(pklTree, usedPeakSet, scanHeader, peak)
          
          // Check if we found one peakel
          if( peakelOpt.isDefined ) {
            val peakel = peakelOpt.get
            val apexIdx = peakel.apexIndex
            
            // Append peakel only if its apex is not at the extrema
            if( apexIdx > 0 && apexIdx < peakel.scanIds.length - 1 )
              peakelBuffer += peakelOpt.get
          }
        }
      }
    } // END OF BREAKABLE
    
    peakelBuffer.toArray
  }
  
  protected def extractPeakel(
    pklTree: PeakListTree,
    usedPeakSet: HashSet[Peak],
    apexScanHeader: ScanHeader,
    apexPeak: Peak
  ): Option[Peakel] = {
    
    // Set up the progress computer
    //val progressComputer = new ProgressComputer( UnsupervisedPeakelDetector.newExtractionProgressPlan() )
    //progressComputer.beginStep(UnsupervisedPeakelDetector.EXTRACTION_STEP1)
    
    // Retrieve the scan header map
    val pklTreeShMap = pklTree.scanHeaderMap
    
    // Define some values
    val apexMz = apexPeak.getMz
    val apexIntensity = apexPeak.getIntensity
    val apexTime = apexScanHeader.getTime
    val apexShPklTreeIdx = pklTreeShMap.getScanHeaderIndex(apexScanHeader) //apexScanHeader.getCycle    
    
    // Compute the m/z tolerance in Daltons
    val mzTolDa = MsUtils.ppmToDa( apexMz, mzTolPPM )
    
    // Define some vars
    var numOfAnalyzedDirections = 0
    var isRightDirection = true
    
    // Create a buffer for peaks
    val peaksBuffer = new ListBuffer[Peak]()
    
    // Loop until left and right directions have been analyzed
    while( numOfAnalyzedDirections < 2 ) {
      
      // Define or reset some vars for the current direction
      var timeOverRange = false
      var consecutiveGapCount = 0
      var shIdxShift = 0
      
      // Stop extraction in this direction if we have too much gaps or if we exceed run time range
      breakable {
        while( consecutiveGapCount <= maxConsecutiveGaps && !timeOverRange ) {
          
          // Decrease scan header index shift if LEFT direction
          if( isRightDirection == false ) shIdxShift -= 1
          
          // Determine current scan header index
          //val curCycleNum = cycleNum + cycleShift
          val curShIdx = apexShPklTreeIdx + shIdxShift
          
          // Try to retrieve the scan id
          var curScanHOpt = pklTreeShMap.getScanHeader(curShIdx)
          /*//if( this.ms1ScanIdByCycleNum.contains(curCycleNum) ) {
          // Check for out of bound indices
          if( curShIdx >= 0 && curShIdx < shCount ) {
            // Retrieve the wanted scan header
            //curScanHOpt = this.scanHeaderById.get(this.ms1ScanIdByCycleNum(curCycleNum))
            curScanHOpt = Some( pklTreeScanHeaders(curShIdx) )
          }*/
          
          if( curScanHOpt.isEmpty ) {//if wrong scanID
            timeOverRange = true
            break
          } else {
            val curScanH = curScanHOpt.get
            val curScanId = curScanH.getId
            val curTime = curScanH.getTime
              
            // TODO: check if total time does not exceed the provided threshold
            if( this.maxTimeWindow > 0 && (curTime - apexTime).abs > this.maxTimeWindow / 2 ) {
              timeOverRange = true
              break
            }
            
            // Try to retrieve a peaklist group for the current scan header
            val pklGroupOpt = pklTree.pklGroupByScanId.get(curScanId)    
            
            if( pklGroupOpt == None ) {
              return None
            }
            val pklGroup = pklGroupOpt.get
  
            // Try to retrieve the nearest peak
            val peak = pklGroup.getNearestPeak( apexMz, mzTolDa )
            
            // Check if a peak has been found
            if( peak != null && usedPeakSet.contains(peak) == false ) {
              
              // Retrieve some values
              val intensity = peak.getIntensity
              
              // If the peak intensity is higher than apex one
              if( intensity > apexIntensity ) {
                break
              }
              // Check if intensity equals zero
              else if( intensity == 0 ) consecutiveGapCount += 1
              // Add the peak to the peaks buffer
              // Note: before code
              //if( isRightDirection ) peaksBuffer += peak // append peak
              //else { peaksBuffer.+=:( peak ) } // prepend peak
              // New one => perform sort at the end => optimization
              else peaksBuffer += peak
              
            // Else if peak is not defined
            } else {
              // Note that null peaks are excluded => peakels will have some missing peaks
              consecutiveGapCount += 1
            }
  
            // Increase scan header index shift if right direction
            if( isRightDirection) shIdxShift += 1
            
          } // END OF ELSE
        } // END OF WHILE
      } // END OF BREAKABLE
      
      isRightDirection = false
      
      numOfAnalyzedDirections += 1
    }
    
    //progressComputer.setCurrentStepAsCompleted()
    //progressComputer.beginStep(UnsupervisedPeakelDetector.EXTRACTION_STEP2)
    
    // TODO: define a minimum number of peaks for a peakel in the config
    val peakelAndPeaksOpt = if( peaksBuffer.length < 5 ) {
      Option.empty[(Peakel,Array[Peak])]
    }
    else {
      
      // Sort peaks by ascending scan id
      val extractedPeaks = peaksBuffer.sortBy(_.getLcContext().getScanId())
      
      // Find all peakels in the extracted range of IPs
      val peakelsIndices = HistogramBasedPeakelFinder.findPeakelsIndices( extractedPeaks )
      
      // Retrieve the peakel corresponding to the feature apex
      val matchingPeakelIdxOpt = peakelsIndices.find { idx =>
        apexTime >= extractedPeaks(idx._1).getLcContext.getElutionTime && 
        apexTime <= extractedPeaks(idx._2).getLcContext.getElutionTime
      }
      
      if( matchingPeakelIdxOpt.isEmpty ) {
        /*this.logger.warn(
          s"no peakel detected for peak with m/z=${apexMz} and scan id=${apexScanHeader.getId}"
        )*/
        None
      } else {
        
        val matchingPeakelIdx = matchingPeakelIdxOpt.get
        val peakelPeaks = extractedPeaks.slice(matchingPeakelIdx._1, matchingPeakelIdx._2 + 1 ).toArray
        
        val peakel = new PeakelBuilder( peakelPeaks ).result()
        
        Some( peakel, peakelPeaks )
      }
    }
    
    //progressComputer.setCurrentStepAsCompleted()
    //progressComputer.beginStep(UnsupervisedPeakelDetector.EXTRACTION_STEP3)
    
    // Update usedPeakSet
    // Remove all extracted peaks from usedPeakSet
    //peaksBuffer.foreach { p => usedPeakSet -= p }
    
    if( peakelAndPeaksOpt.isEmpty ) {
      // Re-add input apexPeak to usedPeakSet
      // Marco: it may lead to missing peakel => we should not remove the apexPeak if no detected peakel
      //usedPeakSet += apexPeak
    } else {
      // Re-add peakel peaks to usedPeakMap
      val( peakel, extractedPeaks ) = peakelAndPeaksOpt.get
      for( peak <- extractedPeaks)
        usedPeakSet += peak
      
      // Check that apex is not the first or last peak
      if( peakel.apexIndex == 0 || peakel.apexIndex == (peakel.scanIds.length - 1) ) {
        return None
      }
      
      // Check peakel amplitude is big enough
      val peaksSortedByItensity = extractedPeaks.sortBy(_.getIntensity)
        
      // TODO: define a minimum amplitude for a peakel in the config
      val( minIntensity, maxIntensity ) = (peaksSortedByItensity.head.getIntensity, peaksSortedByItensity.last.getIntensity)
      val intensityAmplitude = if( minIntensity == 0 ) 1f else maxIntensity / minIntensity
      val minAmplitude = 1.5f
      
      if( intensityAmplitude < minAmplitude ) {
        return None
      }
    }
    
    //progressComputer.setCurrentStepAsCompleted()
    
    peakelAndPeaksOpt.map(_._1)
  }

}