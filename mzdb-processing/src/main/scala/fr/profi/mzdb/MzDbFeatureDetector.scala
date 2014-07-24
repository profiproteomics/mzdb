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
import scala.beans.BeanProperty
import fr.profi.mzdb.io.reader.iterator.RunSliceIterator

case class FeatureDetectorConfig(msLevel: Int=1, mzTolPPM: Float=10,minNbOverlappingIPs: Int=3)


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
  @BeanProperty var ftDetectorConfig: FeatureDetectorConfig = FeatureDetectorConfig()
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
  
  /** Group peakels by time using EntityHistogramComputer
   *  A peakel could belong to several bins of the histogram
   *  TODO: check if it is problem for swath detection
   */
  def groupCorrelatedPeakels(peakelsBuffer: Array[Peakel]):  Array[(Float,Array[Peakel])] = {
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
    peakelsGroupedByTime.toArray
  }
  
  /**
   * Detect peakels using the unsupervised peakel detector
   */
  def detectPeakels(minParentMz: Double=0, maxParentMz: Double=0): Array[Peakel] = {
        // Retrieve scans mapped by their initial? id
    val msLevel = ftDetectorConfig.msLevel
    val scanHeaderById = mzDbReader.getScanHeaderById.map { case (i, sh) => i.toInt -> sh } toMap
    
    val peakelDetector = new UnsupervisedPeakelDetector(
      scanHeaderById,
      Map.empty[Int,Float],
      ftDetectorConfig.mzTolPPM
    )
    
    // Define a peaklist map (first level = runSliceNumber, second level =scanId )
    val pklByScanIdAndRsNumber = new HashMap[Int, Map[Int, PeakList]]()
    
    // Instantiates some objects
    val rsIter = {
	if (msLevel > 1 && minParentMz != 0d && maxParentMz != 0d)
	    mzDbReader.getRunSliceIterator(msLevel, minParentMz, maxParentMz)
	else mzDbReader.getRunSliceIterator(msLevel)
    }
    val rsHeaders = mzDbReader.getRunSliceHeaders(msLevel)
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
      val pklGroupByScanId = peakListsByScanId.map { case (scanId, pkl) => scanId -> new PeakListGroup( pkl ) } toMap
      val pklTree = new PeakListTree( pklGroupByScanId, scanHeaderById )
      
      // Retrieve all peaks incurPeaklistByScanId
      var curRsPeaks = curPeaklistByScanId.values.flatMap( _.getAllPeaks() ).toArray
      
      // Quick sort the peaks
      this.logger.debug("QuickSort on #" + curRsPeaks.length + "peaks")
      curRsPeaks = curRsPeaks.sortWith((x,y) => x.getIntensity > y.getIntensity)
      //quickSortPeaksByDescIntensity(curRsPeaks)
      
      // Detect peakels in pklTree by using curRsPeaks as starting points
      val peakels = peakelDetector.detectPeakels(pklTree, curRsPeaks, usedPeakSet)
      this.logger.debug( s"found ${peakels.length} peakels in run slice #"+rsNumber)
      
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
    peakelsBuffer.toArray
  }
  
  /**
   * Performs a peakel detection, then look for grouping peakels
   * in order to have feature (identify peakels belonging to an
   * isotopic pattern
   */
  def detectFeatures(): Array[Feature] = { // LC-MS maps
    val mzTolPPM = ftDetectorConfig.mzTolPPM
    val msLevel = ftDetectorConfig.msLevel
    
    val peakelsBuffer = this.detectPeakels()
    
    // --- Compute some peakels statistics ---
    if (msLevel < 2) {
      val scanHeaders = mzDbReader.getScanHeaders()
      val scanHeaderById = scanHeaders.map( sh => sh.getId -> sh ).toMap
      val ms1ScanHeaderByCycleNum = scanHeaders.withFilter(_.getMsLevel == msLevel ).map(sh => sh.getCycle -> sh).toMap
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
      this.logger.info("number of MS2 scans ="+ scanHeaders.count(_.getMsLevel == msLevel))
      this.logger.info("number of matched MS2 scans ="+matchedMs2ScanIdSet.size)
      // TODO: have a look to the unmatched features
    } // end computing statistics
    
    // --- Combine peakels to obtain features ---
    val peakelsGroupedByTime = this.groupCorrelatedPeakels(peakelsBuffer)
    
    // Generate a sequence of possible isotope diffs for x charge states
    val maxZ = 10
    val maxIsotopesCount = 5 // TODO: use averagine ???
    val isotopeDiffTol = 0.01 // TODO: make some advanced statistics to infer this value
    val avgIsotopeMassDiff = PeakListTree.avgIsotopeMassDiff
    val peakelIdxByPeakel = peakelsBuffer.zipWithIndex.toMap
    
    val peakelPatternsBuffer = new ArrayBuffer[PeakelPattern]
    
    peakelPatternsBuffer.synchronized {
      
      for( (groupTime,peakelGroup) <- peakelsGroupedByTime.par ) {
        
        // FIXME: filter out peakels having the same m/z value
        
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
            
            val mzTolDa = MsUtils.ppmToDa(peakel.mz, mzTolPPM)
            
            val (minIsoMzDiff, maxIsoMzDiff) = (avgIsoMzDiff - mzTolDa, avgIsoMzDiff + mzTolDa)
            
            // Create a new putative peakel group for this peakel
            val peakelPatternBuffer = new ListBuffer[Peakel]()       
            peakelPatternBuffer += peakel
            peakelPatternBufferByApex += peakel -> peakelPatternBuffer
            
            // Sort other peakels by ascending m/z difference
            val otherPeakelsSortedByMzDiff = sortedPeakels.sortWith { (a,b) =>
              math.abs(a.mz - peakel.mz) < math.abs(b.mz - peakel.mz)
            }
            
            // Iterate over neighboring peakels (take care to skip peakels having the same m/z)
            breakable {
              // FIXME: remove this workaround for filtering peakels of same m/z value
              for( neighboringPeakel <- otherPeakelsSortedByMzDiff if peakel.mz != neighboringPeakel.mz) {
              //for( neighboringPeakel <- otherPeakelsSortedByMzDiff if peakel != neighboringPeakel) {
                
                val( prevPeakel, prevPeakelIdx ) = if( neighboringPeakel.mz > peakelPatternBuffer.last.mz ) {
                  (peakelPatternBuffer.last, peakelPatternBuffer.length - 1)
                }
                else if (neighboringPeakel.mz < peakelPatternBuffer.head.mz ) {
                  (peakelPatternBuffer.head, 0)
                }
                else {
                  //println("neighboringPeakel.mz: "+ neighboringPeakel.mz)
                  //println("peakelPatternBuffer.last.mz:" + peakelPatternBuffer.last.mz)
                  //println("peakelPatternBuffer.head.mz:" + peakelPatternBuffer.head.mz)
                  throw new Exception(
                    "invalid neighboring peakel m/z" + s"""neighboringPeakel.mz: ${neighboringPeakel.mz}
                    peakelPatternBuffer.last.mz:  ${ peakelPatternBuffer.last.mz}
                    peakelPatternBuffer.head.mz:  ${ peakelPatternBuffer.head.mz}"""
                  )
                }
                
                // Compute m/z diff with reference peakel
                // TODO check if first peakel is better
                val absMzDiffWithRefPeakel = math.abs(neighboringPeakel.mz - peakel.mz)

                // Break if neighboring peakel m/z is too far
                val isotopesDiffCount = math.round(absMzDiffWithRefPeakel * z)
                if( isotopesDiffCount > maxIsotopesCount ) break
                
                // Compute m/z diff with previous peakel
                val absMzDiffWithPrevPeakel = math.abs(neighboringPeakel.mz - prevPeakel.mz)
                
                var addNeighboringPeakel = false
                
                // Check if the new neighboring peakel has a lower intensity than previous one
                if( neighboringPeakel.area < prevPeakel.area ) {
                  
                  // Check if we are still on the same isotope than with the previous peakel (except for the reference peakel)
                  if( prevPeakel != peakel && absMzDiffWithPrevPeakel < isotopeDiffTol ) {
                    
                    // We have to chose if this new peakel should replace the previous appended one
                    val absMzDiffBetweenPrevAndRefPeakels = math.abs(prevPeakel.mz - peakel.mz)
                    
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
                      this.logger.debug("neighboringPeakel.mz: "+ neighboringPeakel.mz)
                      this.logger.debug("peakelPatternBuffer.last.mz:" + peakelPatternBuffer.last.mz)
                      this.logger.debug("peakelPatternBuffer.head.mz:" + peakelPatternBuffer.head.mz)
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
            this.logger.debug( "L1 charges = "+newPeakelCluster.samesetsKeys.map(_.charge).mkString(";") )
            this.logger.debug( "cluster length = " + newPeakelCluster.samesetsKeys.length)
            this.logger.debug( "indices ="+newPeakelCluster.samesetsValues.mkString(";") )
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
    val peakelIndexSetByPeakelPattern = peakelPatternsBuffer.withFilter(_ != null).map { peakelPattern =>
      val peakelIndexSet = peakelPattern.peakels.withFilter(_ != null).map( peakelIdxByPeakel(_) ).toSet
      peakelPattern -> peakelIndexSet
    } toMap

    // Apply the peakel pattern clustering to remove duplicated patterns (due to sliding window)
    val clusters = SetClusterer.clusterizeMappedSets(peakelIndexSetByPeakelPattern)
    val supersetClusterCount = clusters.count( _.isSubset == false )
    logger.info( s"obtained ${supersetClusterCount} peakel pattern clusters" )
    
    // Output results into a file
    val printWriter = new java.io.PrintWriter("detected_features.tsv")
    
    val detectedFeatures = new ArrayBuffer[Feature]()
    var i = 0
    for( cluster <- clusters; if cluster.isSubset == false ) {
      i -= 1
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
      
      detectedFeatures += Feature(i, peakelPattern.apex.mz, charge, peakelPattern.peakels)
    }
    
    printWriter.close()
    
    detectedFeatures.toArray
  }
  
  def quickSortPeaksByDescIntensity(peaks: Array[Peak]) {
    
    // TODO: try to perform quickSort in parallel
    val peakDescOrdering = new Ordering[Peak] {
      def compare(a: Peak, b: Peak) = b.getIntensity compare a.getIntensity
    }
    
    // Note: may result in StackOverFlowError (fixed in scala 2.11)
    scala.util.Sorting.quickSort(peaks)(peakDescOrdering)
  }

}