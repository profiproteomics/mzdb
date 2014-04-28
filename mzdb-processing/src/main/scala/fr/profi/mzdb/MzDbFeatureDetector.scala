package fr.profi.mzdb

import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.ms.algo.IsotopePatternInterpolator
import fr.profi.mzdb.model._
import fr.profi.mzdb.algo.feature.extraction.UnsupervisedPeakelDetector
import fr.profi.mzdb.utils.ms.MsUtils
import fr.profi.mzdb.utils.misc.SetClusterer
import fr.profi.util.stat._

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
    while( (rsIter.hasNext || rsOpt.isDefined ) ) { // && pklByScanIdAndRsNumber.size < 3
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
      logger.debug( s"found ${peakels.length} peakels in run slice #"+rsNumber)
      
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
    
    // --- Compute some peakels statistics ---
    val scanHeaders = mzDbReader.getScanHeaders()
    val scanHeaderById = scanHeaders.map( sh => sh.getId -> sh ).toMap
    val ms1ScanHeaderByCycleNum = scanHeaders.withFilter(_.getMsLevel == 1 ).map(sh => sh.getCycle -> sh).toMap
    val matchedMs2ScanIdSet = new HashSet[Int]
    
    for( peakel <- peakelsBuffer ) {
      
      val ms2ScanIds = new ArrayBuffer[Int]
      
      // TODO: factorize this loop with the one from FeatureExtractor
      for( peak <- peakel.definedPeaks ) {
        
        // Retrieve the cycles surrounding the next MS2 scans
        val thisScanId = peak.getLcContext.getScanId
        val thisCycleNum = scanHeaderById(thisScanId).getCycle
        val nextCycleNum = thisCycleNum + 1
  
        // Do the job only if next cycle can be found
        if( ms1ScanHeaderByCycleNum.contains(nextCycleNum) ) {
          val nextCycleScanId = ms1ScanHeaderByCycleNum(nextCycleNum).getId
          
          // Iterate over MS2 scans
          for( scanId <- thisScanId + 1 until nextCycleScanId ) {
            val scanH = scanHeaderById(scanId)
            
            if( scanH.getMsLevel == 2 ) {
              // Compute m/z difference between the current peak and MS2 scan precursor m/z
              val mzDiffPPM = MsUtils.DaToPPM(peak.getMz, math.abs(scanH.getPrecursorMz - peak.getMz) )
              if( mzDiffPPM < mzTolPPM ) {
                ms2ScanIds += scanId
              }
            }
          }
        }
      }
      
      matchedMs2ScanIdSet ++= ms2ScanIds
    }
    
    println("number of MS2 scans ="+ scanHeaders.count(_.getMsLevel == 2))
    println("number of matched MS2 scans ="+matchedMs2ScanIdSet.size)
    // TODO: have a look to the unmatched features
    
    // --- Combine peakels to obtain features ---
    logger.info("combining peakels into features...")
    
    // Clusterize peakels having an apex separated by a given time value (10 secs)    
    val histoComputer = new EntityHistogramComputer( peakelsBuffer, { peakel: Peakel =>
      peakel.apexScanContext.getElutionTime
    })
    val peakelTimes = peakelsBuffer.map( _.weightedAverageTime )
    val timeRange = peakelTimes.max - peakelTimes.min
    val peakelBins = histoComputer.calcHistogram( nbins = (timeRange / 3f).toInt )
    
    // TODO: keep the information about the sliding to avoid redundant results
    // Starting points should always come from the second bin
    val peakelsGroupedByTime = new ArrayBuffer[(Float,Array[Peakel])]()
    peakelBins.sliding(3).foreach { peakelBinGroup =>
      
      val peakelGroup = peakelBinGroup.flatMap( _._2 )
      if( peakelGroup.isEmpty == false ) {
        val meanTime = peakelBinGroup.map( _._1.center ).sum / peakelBinGroup.length
        val times = peakelGroup.map(_.weightedAverageTime)
        logger.debug( s"min time is ${times.min} and max time is ${times.max}")
        peakelsGroupedByTime += meanTime.toFloat -> peakelGroup
      }
    }
    
    // Generate a sequence of possible isotope diffs for x charge states
    val maxZ = 10
    val maxIsotopesCount = 5 // TODO: use averagine ???
    val isotopeDiffTol = 0.01 // TODO: make some advanced statistics to infer this value
    val avgIsotopeMassDiff = PeakListTree.avgIsotopeMassDiff
    val peakelIdxByPeakel = peakelsBuffer.zipWithIndex.toMap
    
    val peakelPatternsBuffer = new ArrayBuffer[PeakelPattern]
    
    peakelPatternsBuffer.synchronized {
      
      for( (groupTime,peakelGroup) <- peakelsGroupedByTime.par ) {
        
        logger.debug(s"grouped ${peakelGroup.length} peakel(s) at average time ="+groupTime)
        
        // Compute an histogram of peakels by m/z BIN ???
        
        // Sort peakels by desc area
        val sortedPeakels = peakelGroup.sortWith { (a,b) => b.area > a.area }
        val peakelPatternBuffersByCharge = new HashMap[Int,HashMap[Peakel,ListBuffer[Peakel]]]
        
        // Iterate over tested charge states
        for( z <- 1 to maxZ ) {
          val avgIsoMzDiff = avgIsotopeMassDiff / z
          val peakelPatternBufferByApex = peakelPatternBuffersByCharge.getOrElseUpdate(z, new HashMap[Peakel,ListBuffer[Peakel]])
          
          // Try to find compatible peakels using combinatorics approach
          // We assume to find only peakels of lowest intensity when looking over sibling isotopes
          for( peakel <- sortedPeakels ) {
            
            // Create a new putative peakel group for this peakel
            val peakelPatternBuffer = new ListBuffer[Peakel]()       
            peakelPatternBuffer += peakel
            peakelPatternBufferByApex += peakel -> peakelPatternBuffer
            
            // Sort other peakels by ascending m/z difference
            val otherPeakelsSortedByMzDiff = sortedPeakels.sortWith { (a,b) =>
              math.abs(a.mz - peakel.mz) < math.abs(b.mz - peakel.mz)
            }
            
            // Iterate over neighboring peakels (take care to skip the current peakel)
            breakable {
              for( neighboringPeakel <- otherPeakelsSortedByMzDiff if peakel != neighboringPeakel ) {
                
                val( prevPeakel, prevPeakelIdx ) = if( neighboringPeakel.mz > peakelPatternBuffer.last.mz ) {
                  (peakelPatternBuffer.last, peakelPatternBuffer.length - 1)
                }
                else if (neighboringPeakel.mz < peakelPatternBuffer.head.mz ) {
                  (peakelPatternBuffer.head, 0)
                }
                else throw new Exception("invalid neighboring peakel m/z")
                
                // Compute m/z diff with reference peakel
                val absMzDiffWithRefPeakel = math.abs(neighboringPeakel.mz - peakel.mz)
                
                // Break if neighboring peakel m/z is too far
                val isotopesDiffCount = math.round(absMzDiffWithRefPeakel * z)
                if( isotopesDiffCount > maxIsotopesCount ) break
                
                // Compute m/z diff with previous peakel
                val absMzDiffWithPrevPeakel = math.abs(neighboringPeakel.mz - prevPeakel.mz)
                
                var addNeighboringPeakel = false
                
                if( neighboringPeakel.area < prevPeakel.area ) {
                  
                  // Check if we are still on the same isotope than with the previous peakel
                  if( absMzDiffWithPrevPeakel < isotopeDiffTol ) {
                    
                    // We have to chose if this new peakel should replace the previous appended one
                    val absMzDiffBetweenPrevAndRefPeakels = math.abs(prevPeakel.mz - peakel.mz)
                    
                    val expectedMzDiffFromRefPeakel = isotopesDiffCount * avgIsoMzDiff
                    
                    if( math.abs(absMzDiffBetweenPrevAndRefPeakels - expectedMzDiffFromRefPeakel) >
                        math.abs(absMzDiffWithRefPeakel - expectedMzDiffFromRefPeakel)
                      ) {
                      peakelPatternBuffer.remove(prevPeakelIdx)
                      addNeighboringPeakel = true
                    }
                  }
                  // Check the result is under the isotope diff tolerance
                  // And that new neighboring peakel as a lower intensity than previous one
                  else if( math.abs(absMzDiffWithPrevPeakel - avgIsoMzDiff) < isotopeDiffTol ) {
                    addNeighboringPeakel = true
                  }
                }
                
                if( addNeighboringPeakel ) {
                  // If previous peakel is at the end of the pattern
                  // If new peakel has m/z greater than hte highest one
                  if( neighboringPeakel.mz > peakelPatternBuffer.last.mz ) {
                    // Append neighboring peakel to the buffer
                    peakelPatternBuffer.append(neighboringPeakel)
                  }
                  // Else we assume previous peakel is at the beginning of the pattern
                  else if( neighboringPeakel.mz < peakelPatternBuffer.head.mz ) {
                    
                    // Check isotopic pattern abundance ratio before prepending the peakel                    
                    val theoPattern = IsotopePatternInterpolator.getTheoreticalPattern(neighboringPeakel.mz, z)
                    val theoAbundances = theoPattern.abundances
                    val theoIsoRatio2_1 = theoAbundances(1) / theoAbundances(0)
                    val obsIsoRatio2_1 = peakelPatternBuffer.head.getApex.getIntensity / neighboringPeakel.getApex.getIntensity
                    //val peakelApexIntensities = ft.peakels.map(_.getApex().getIntensity)
                    //val rmsd = IsotopePatternInterpolator.calcAbundancesRmsd(theoAbundances, peakelApexIntensities)
                    
                    // Check the range of observed ratio is valid compared to theoretical one
                    if( math.abs(math.log(obsIsoRatio2_1) - math.log(theoIsoRatio2_1)) < 0.7 ) {// 0.7 is ln(2)
                      // Prepend neighboring peakel to the buffer
                      peakelPatternBuffer.prepend(neighboringPeakel)
                    }
                  }
                  else {
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
        ) yield PeakelPattern(apexPeakel,peakelPatternBuffer.toArray,charge)
        
        // Clusterize peakel patterns using SetClusterer fork
        val peakelIndexSetByNewPeakelPattern = newPeakelPatterns.map { peakelPattern =>
          val peakelIndexSet = peakelPattern.peakels.map( peakelIdxByPeakel(_) ).toSet
            peakelPattern -> peakelIndexSet
        } toMap
    
        val newPeakelClusters = SetClusterer.clusterizeMappedSets(peakelIndexSetByNewPeakelPattern)
        
        // Remove subsets and map found clusters by peakel index
        val oversetPeakelPatternByPeakelIdx = new HashMap[Int,ArrayBuffer[PeakelPattern]]
        for( newPeakelCluster <- newPeakelClusters; if newPeakelCluster.isSubset == false ) {
          if( newPeakelCluster.samesetsKeys.length > 1 ) {
            println( "L1 charges = "+newPeakelCluster.samesetsKeys.map(_.charge).mkString(";") )
            println( "cluster length = " + newPeakelCluster.samesetsKeys.length)
            println( "indices ="+newPeakelCluster.samesetsValues.mkString(";") )
          }
          
          val peakelPattern = newPeakelCluster.samesetsKeys.head
          for( peakelIdx <- newPeakelCluster.samesetsValues)
            oversetPeakelPatternByPeakelIdx.getOrElseUpdate(peakelIdx, new ArrayBuffer[PeakelPattern]) += peakelPattern
        }
        
        // Search for peakel pattern clusters having a shared peakel
        val peakelPatternGroups = new ArrayBuffer[List[PeakelPattern]]
        val assignedPeakelPatterns = new HashSet[PeakelPattern]
        
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
              peakelPattern.peakels.head.mz, peakelPattern.charge
            )
            val obsAbundances = peakelPattern.peakels.map( _.getApex.getIntensity )            
            IsotopePatternInterpolator.calcAbundancesRmsd(isotopePattern.abundances, obsAbundances)
          }
        }
        
        peakelPatternsBuffer ++= bestPeakelPatterns
        
      }
      
    }
    
    // --- Clusterize peakel patterns using SetClusterer fork ---
    
    // Map peakel indices by each found peakel pattern
    val peakelIndexSetByPeakelPattern = peakelPatternsBuffer.map { peakelPattern =>
      val peakelIndexSet = peakelPattern.peakels.map( peakelIdxByPeakel(_) ).toSet
      peakelPattern -> peakelIndexSet
    } toMap

    // Apply the peakel pattern clustering to remove duplicated patterns (due to sliding window)
    val clusters = SetClusterer.clusterizeMappedSets(peakelIndexSetByPeakelPattern)
    val supersetClusterCount = clusters.count( _.isSubset == false )
    logger.info( s"obtained ${supersetClusterCount} peakel pattern clusters" )
    
    // Output results into a file
    val printWriter = new java.io.PrintWriter("detected_features.tsv")
    
    for( cluster <- clusters; if cluster.isSubset == false ) {
      
      val peakelPattern = cluster.samesetsKeys.head
      val charge = peakelPattern.charge
      /*val peakelIndices = cluster.samesetsValues
      val values = peakelIndices.toList
        .map( peakdelIdx => peakelsBuffer(peakdelIdx) )
        .sortBy( _.mz )*/
      val values = peakelPattern.peakels.flatMap( peakel => 
        List(peakel.mz.toString, peakel.area.toString)
      )
      
      val patternTime = peakelPattern.peakels.head.weightedAverageTime
      val patternDuration = peakelPattern.peakels.head.duration
      
      printWriter.println(patternTime + "\t" + patternDuration + "\t" +charge + "\t" + values.mkString("\t") )
      printWriter.flush()
    }
    
    printWriter.close()
    
    Array()
  }
  
  def quickSortPeaksByDescIntensity(peaks: Array[Peak]) {
    
    // TODO: try to perform quickSort in parallel
    val peakDescOrdering = new Ordering[Peak] {
      def compare(a: Peak, b: Peak) = b.getIntensity compare a.getIntensity
    }
    scala.util.Sorting.quickSort(peaks)(peakDescOrdering)
  }

}