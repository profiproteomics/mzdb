package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

import com.typesafe.scalalogging.slf4j.Logging

import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.model._
import fr.profi.mzdb.utils.ms.MsUtils


/**
 * @author David Bouyssie
 *
 */
// TODO: rename file and move to feature.detection package
class UnsupervisedPeakelDetector(
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  // TODO: create configs for signal extraction
  val mzTolPPM: Float,
  val maxConsecutiveGaps: Int = 2,
  val maxTimeWindow: Float = 1200f,
  val minPercentageOfMaxInt: Float = 0.005f
) extends Logging {
    
  /*@BeanProperty reader: MzDbReader, 
	  @BeanProperty nbConsecutiveScanMin: Int = 5,
	  @BeanProperty nbConsecutiveScanMax: Int = 100
	*/
  
  val ms1ScanIdByCycleNum = scanHeaderById.values
    .withFilter( _.getMsLevel == 1 )
    .map( sh => sh.getCycle -> sh.getId )
    .toMap
    
  /*def peakelsToFeatures(peakels: Array[Peakel] ): Array[Feature] = {
    
  }*/
  
  def detectPeakels(pklTree: PeakListTree, intensityDescPeaks: Array[Peak], usedPeakSet: HashSet[Peak] ): Array[Peakel] = {
    
    val peakelBuffer = new ArrayBuffer[Peakel]()
    
    // Iterate over all peaks sorted by descending order
    for( peak <- intensityDescPeaks ) {
      if( usedPeakSet(peak) == false ) {
        
        // Retrieve corresponding scan header
        val scanHeader = this.scanHeaderById(peak.getLcContext.getScanId)
        
        // Initiate a peakel extraction using this starting point
        val peakelOpt = this.extractPeakel(pklTree, usedPeakSet, scanHeader, peak)
        
        if( peakelOpt.isDefined ) peakelBuffer += peakelOpt.get
      }
    }
    
    peakelBuffer.toArray
  }
  
  def extractPeakel(
    pklTree: PeakListTree,
    usedPeakSet: HashSet[Peak],
    apexScanHeader: ScanHeader,
    apexPeak: Peak
  ): Option[Peakel] = {
    
    // Define some values
    val apexMz = apexPeak.getMz
    val apexIntensity = apexPeak.getIntensity
    val apexTime = apexScanHeader.getTime
    val cycleNum = apexScanHeader.getCycle
    
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
      var cycleShift = 0
      
      // Stop extraction in this direction if we have too much gaps or if we exceed run time range
      breakable {
        while( consecutiveGapCount <= maxConsecutiveGaps && !timeOverRange ) {
          
          // Decrease cycle shift if LEFT direction
          if( isRightDirection == false ) cycleShift -= 1
          
          // Determine current cycle number
          val curCycleNum = cycleNum + cycleShift
          
          // Try to retrieve the scan id
          var curScanHOpt = Option.empty[ScanHeader]
          if( this.ms1ScanIdByCycleNum.contains(curCycleNum) ) {
            // Retrieve the wanted scan header
            curScanHOpt = this.scanHeaderById.get(this.ms1ScanIdByCycleNum(curCycleNum))
          }
          
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
            if( pklGroupOpt == None ) return None
            val pklGroup = pklGroupOpt.get
  
            // Try to retrieve the nearest peak
            val peak = pklGroup.getNearestPeak( apexMz, mzTolDa)
            
            // Check if a peak has been found
            if( peak != null && usedPeakSet(peak) == false ) {
              
              // Retrieve some values
              val intensity = peak.getIntensity
              
              // Add the peak to the set of used peaks
              /*this.synchronized {
                usedPeakSet += peak
              }*/
              
              // Add the peak to the peaks buffer
              if( isRightDirection ) peaksBuffer += peak // append peak
              else { peaksBuffer.+=:( peak ) } // prepend peak
                
              // If the peak intensity is higher than apex one
              if( intensity > apexIntensity ) {
                break
              }
              
              // Check if intensity equals zero
              if( intensity == 0 ) consecutiveGapCount += 1
              
            // Else if peak is not defined
            } else {
              // Note that null peaks are excluded => peakels will have some missing peaks
              consecutiveGapCount += 1
            }
  
            // Increase cycle shift if right direction
            if( isRightDirection) cycleShift += 1
            
          } // END OF ELSE          
        } // END OF WHILE
      } // END OF BREAKABLE
      
      isRightDirection = false
      
      numOfAnalyzedDirections += 1
    }
    
    // TODO: define a minimum number of peaks for a peakel
    val peakelOpt = if( peaksBuffer.length < 5 ) None
    else {
      
      // Find all peakels in the extracted range of IPs
      val peakelsIndexes = BasicPeakelFinder.findPeakelsIndexes( peaksBuffer )
      
      // Retrieve the peakel corresponding to the feature apex
      val matchingPeakelIdxOpt = peakelsIndexes.find { idx =>
        apexTime >= peaksBuffer(idx._1).getLcContext.getElutionTime && 
        apexTime <= peaksBuffer(idx._2).getLcContext.getElutionTime
      }
      
      if( matchingPeakelIdxOpt.isEmpty ) {
        /*this.logger.warn(
          s"no peakel detected for peak with m/z=${apexMz} and scan id=${apexScanHeader.getId}"
        )*/
        None
      } else {
        
        val matchingPeakelIdx = matchingPeakelIdxOpt.get
        val peakelPeaks = ( matchingPeakelIdx._1 to matchingPeakelIdx._2 ).map( peaksBuffer(_) )
        
        // TODO: remove map when Marc has committed his update
        val peakel = Peakel(index = 0, peaks = peakelPeaks.toArray )
        
        Some( peakel )
      }
    }
    
    // Update usedPeakSet using a synchronized block
    usedPeakSet.synchronized {
      
      // Remove all extracted peaks from usedPeakSet
      //peaksBuffer.foreach { p => usedPeakSet -= p }
      
      if( peakelOpt.isEmpty ) {
        // Re-add input apexPeak to usedPeakSet
        usedPeakSet += apexPeak
      } else {
        // Re-add peakel peaks to usedPeakSet
        usedPeakSet ++= peakelOpt.get.definedPeaks
      }
      
    }
      
    peakelOpt
  }

}