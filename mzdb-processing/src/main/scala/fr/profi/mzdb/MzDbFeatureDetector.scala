package fr.profi.mzdb

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.mzdb.model._
import fr.profi.mzdb.algo.feature.extraction.UnsupervisedPeakelDetector

case class PeakelPattern(
  apex: Peakel,
  peakels: Array[Peakel],
  charge: Int
) {
  lazy val abundance = peakels.foldLeft(0f)( (s,p) => s + p.area ) 
}

/**
 * @author David Bouyssie
 *
 */
class MzDbFeatureDetector(
  mzDbReader: MzDbReader,
  minNbOverlappingIPs: Int = 3
) extends Logging {
  
  // TODO: factorize this code
  // BEGIN OF STOLEN FROM MzDbFeatureExtractor
  class RichRunSliceData(self: RunSliceData) {
    def getPeakListByScanId(): Map[Int,PeakList] = {
      Map() ++ self.getScanSliceList.map { ss => ss.getScanId -> new PeakList(ss.getPeaks(),0.1) }
    }
  }
  implicit def rsdToRichRsd(rsd: RunSliceData) = new RichRunSliceData(rsd)
  // END OF STOLEN FROM MzDbFeatureExtractor
  
  def detectFeatures( mzTolPPM: Float ): Array[Feature] = {
    
    // Retrieve scans mapped by their initial id
    val scanHeadersById = mzDbReader.getScanHeaderById.map { case(i,sh) => i.toInt -> sh } toMap
    
    val peakelDetector = new UnsupervisedPeakelDetector(
      scanHeadersById,
      Map.empty[Int,Float],
      mzTolPPM
    )
    
    // Define a peaklist map (first level = runSliceNumber, second level =scanId )
    val pklByScanIdAndRsNumber = new HashMap[Int, Map[Int, PeakList]]()
    
    // Instantiates some objects
    val rsIter = mzDbReader.getRunSliceIterator(1)
    val rsHeaders = mzDbReader.getRunSliceHeaders(1)
    val rsHeaderByNumber = rsHeaders.map { rsh => rsh.getNumber -> rsh } toMap
    
    // Define some vars
    var( prevRsNumber, nextRsNumber ) = (0,0)
    var rsOpt = if( rsIter.hasNext) Some(rsIter.next) else None
    val usedPeakSet = new HashSet[Peak]()
    val peakelsBuffer = new ArrayBuffer[Peakel]
    
    // Iterate over run slice headers
    while( rsIter.hasNext || rsOpt.isDefined ) {
      val rs = rsOpt.get
      val rsh = rs.getHeader
      val rsd = rs.getData
      val curPeaklistByScanId = rsd.getPeakListByScanId
      this.logger.debug("unsupervised processing of run slice with id =" + rsh.getId)

      // Retrieve run slices and their corresponding id
      val rsNumber = rsh.getNumber
      val nextRsNumber = rsNumber + 1     
      
      // Build the list of obsolete run slices
      val rsNumbersToRemove = for(
        processedRsNumber <- pklByScanIdAndRsNumber.keys
        if processedRsNumber != rsNumber &&
           processedRsNumber != prevRsNumber &&
           processedRsNumber != nextRsNumber
      ) yield processedRsNumber

      // Clean the peaklist map => remove obsolete run slices
      rsNumbersToRemove.foreach { pklByScanIdAndRsNumber -= _ }

      // Add current run slice peakList to pklByScanIdAndRsNumber
      if ( pklByScanIdAndRsNumber.contains(rsNumber) == false ) {
        pklByScanIdAndRsNumber += ( rsNumber -> curPeaklistByScanId )
      }

      // Add next run slice peaklist to pklByScanIdAndRsNumber
      val nextRsOpt = if (rsIter.hasNext == false) None
      else {
        val nextRs = rsIter.next
        pklByScanIdAndRsNumber += ( nextRsNumber -> nextRs.getData.getPeakListByScanId )
        Some(nextRs)
      }
      
      // Group run slice peakLists into a single map (key = scan id)
      val peakListsByScanId = new HashMap[Int,ArrayBuffer[PeakList]]()
      pklByScanIdAndRsNumber.values.foreach { pklByScanId =>
        pklByScanId.foreach { case (scanId, pkl) =>
          peakListsByScanId.getOrElseUpdate(scanId, new ArrayBuffer[PeakList]) += pkl
        }
      }
      
      // Use the map to instantiate a peakList tree which will be used for peak extraction
      val pklGroupByScanId = peakListsByScanId.map { kv => kv._1 -> new PeakListGroup( kv._2 ) } toMap
      val pklTree = new PeakListTree( pklGroupByScanId )
      
      // Retrieve all peaks incurPeaklistByScanId
      val curRsPeaks = curPeaklistByScanId.values.flatMap( _.getAllPeaks() ).toArray
      
      // Quick sort the peaks
      quickSortPeaksByDescIntensity(curRsPeaks)
      
      // Detect peakels in pklTree by using curRsPeaks as starting points
      val peakels = peakelDetector.detectPeakels(pklTree, curRsPeaks, usedPeakSet)
      println(peakels.length)
      
      // Add peakels to the global buffer
      peakelsBuffer ++= peakels
      
      // Remove used peaks from previous run slice
      if( prevRsNumber > 0 ) {
        usedPeakSet.synchronized {
          val prevPeaklistByScanId = pklByScanIdAndRsNumber(prevRsNumber)
          val prevRsPeaks = curPeaklistByScanId.values.flatMap( _.getAllPeaks() ).toArray
          usedPeakSet --= prevRsPeaks
        }
      }
  
      // Update some vars
      prevRsNumber = rsNumber
      rsOpt = nextRsOpt
    }
    
    // Combine peakels to obtain features
    logger.info("combining peakels into features...")
    
    val peakelsGroupedByApexScanId = peakelsBuffer.groupBy(_.apexScanContext.getScanId())
    
    // Generate a sequence of possible isotope diffs for x charge states
    val maxZ = 10
    val maxIsotopesCount = 5 // TODO: use averagine ???
    val isotopeDiffTol = 0.01 // TODO: make some advanced statistics to infer this value
    
    /*val chargesByIsotopeDiff = new HashMap[Double,ArrayBuffer[Int]]
    for(
      z <- 1 to maxZ;
      shift <- 1 to 5 // TODO: use averagine
    ) {
      val isoDiff = shift.toDouble / z
      chargesByIsotopeDiff.getOrElseUpdate(isoDiff, new ArrayBuffer[Int]) += z
    }
    val uniqIsotopeDiffs = chargesByIsotopeDiff.keys.toArray*/
    
    // FIXME: this grouping might not be appropriate
    for( (scanId,peakelGroup) <- peakelsGroupedByApexScanId ) {
      
      println(s"${peakelGroup.length} peakels in scan with id ="+scanId)
      
      // Compute an histogram of peakels by m/z BIN ???
      
      // Sort peakels by desc area
      val sortedPeakels = peakelGroup.sortWith { (a,b) => b.area > a.area }
      val peakelPatternBuffersByCharge = new HashMap[Int,HashMap[Peakel,ListBuffer[Peakel]]]
      
      // Iterate over tested charge states
      for( z <- 1 to maxZ ) {
        val isoDiff = 1 / z
        val peakelPatternBufferByApex = peakelPatternBuffersByCharge.getOrElseUpdate(z, new HashMap[Peakel,ListBuffer[Peakel]])
        
        // Try to find compatible peakels using combinatorics approach
        // We assume to find only peakels of lowest intensity when looking over sibling isotopes
        for( peakel <- sortedPeakels ) {
          
          // Create a new putative peakel group for this peakel
          val peakelPatternBuffer = peakelPatternBufferByApex.getOrElseUpdate(peakel, new ListBuffer[Peakel])
          peakelPatternBuffer += peakel
          
          // Sort other peakels by ascending m/z difference
          val otherPeakelsSortedByMzDiff = sortedPeakels.sortWith { (a,b) =>
            (a.mz - peakel.mz).abs < (b.mz - peakel.mz).abs
          }
          
          // Iterate over neighboring peakels
          breakable {
            for( neighboringPeakel <- otherPeakelsSortedByMzDiff if peakel != neighboringPeakel ) {
              
              val prevPeakel = if( neighboringPeakel.mz > peakelPatternBuffer.last.mz ) {
                peakelPatternBuffer.last
              }
              else if (neighboringPeakel.mz < peakelPatternBuffer.head.mz ) {
                peakelPatternBuffer.head
              }
              else throw new Exception("invalid neighboring peakel m/z")
              
              // Compute m/z diff with previous peakel
              val absMzDiff = (neighboringPeakel.mz - prevPeakel.mz).abs              
              
              // Break if neighboring peakel m/z is too far
              if( absMzDiff > maxIsotopesCount ) break
              
              // Compute absolute difference to expected isotope difference
              val absIsoMzDiff = (absMzDiff - isoDiff).abs
              
              // Check the result is under the isotope diff tolerance
              // And that new neighboring peakel as a lower intensity than previous one
              if( absIsoMzDiff < isotopeDiffTol && neighboringPeakel.area < prevPeakel.area ) {
                
                // If previous peakel is at the end of the pattern
                if( peakelPatternBuffer.last == prevPeakel ) {
                  // Append neighboring peakel to the buffer
                  peakelPatternBuffer.append(neighboringPeakel)
                }
                // Else we assume previous peakel is at the beginning of the pattern
                else {
                  // Prepend neighboring peakel to the buffer
                  peakelPatternBuffer.prepend(neighboringPeakel)
                }
              }
            } // End of sibling peakels loop
          } // End of breakable
        } // End of peakel loop
      } // End of Z loop
      
      // Convert peakel pattern buffers into peakel patterns
      val peakelPatterns = for(
        (charge,peakelPatternBufferByApex) <- peakelPatternBuffersByCharge;
        (apexPeakel,peakelPatternBuffer) <- peakelPatternBufferByApex
      ) yield PeakelPattern(apexPeakel,peakelPatternBuffer.toArray,charge)
      
      // TODO: clusterize peakel patterns using SetClusterer fork
    }
    
    Array()
  }
  
  def quickSortPeaksByDescIntensity(peaks: Array[Peak]) {
    
    // TODO: try to perform quickSort in parallel
    val peakDescOrdering = new Ordering[Peak] {
      def compare(a: Peak, b: Peak) = b.getIntensity compare a.getIntensity
    }
    util.Sorting.quickSort(peaks)(peakDescOrdering)
  }

}