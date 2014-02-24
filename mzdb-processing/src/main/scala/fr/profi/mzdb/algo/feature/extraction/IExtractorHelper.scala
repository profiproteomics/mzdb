/**
 *
 */
package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.FullLcContext
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import util.control.Breaks._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.model.TheoreticalIsotopePattern
import fr.profi.mzdb.model.Peakel


/**
 * @author Marco
 *
 */
/**
 * @author David Bouyssie
 *
 */
trait IExtractorHelper {
  
  /**
   * required parameters
   */
  val xtractConfig: FeatureExtractorConfig
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
  val scanHeaderById: Map[Int, ScanHeader]
  val ms1ScanIdByCycleNum: Map[Int, Int]
  
  
  lazy val mzTolPPM = xtractConfig.mzTolPPM
  
  
   /**
   * 
   */
  def _findMaximaNaive( data: Array[Float]): Array[Int] = {
     val maxs= new ArrayBuffer[Int]
     for (i <- 0 until data.length) {
        if ( i == 0) {
            if (data(i + 1) < data(i))
                maxs += i;
        } else if ( i == data.length - 1) {
            if ( data(i - 1) < data(i) )
                maxs += i;
        } else {
            if (data(i - 1) < data(i) && data(i + 1) < data(i))
                maxs += i;
        }
     }
     maxs.toArray
  }
  
  /**
   * 
   */
  protected def _getIntensityAscendantDirection(putativeFt: PutativeFeature, pklTree: PeakListTree,
                                                cycleNum: Int, range: Pair[Int, Int], mzTolPPM: Float,
                                                minNbPeaks: Int): Int = {

    var (firstCycleNum, lastCycleNum) = (0, 0)

    // Left check
    firstCycleNum = cycleNum - range._2
    lastCycleNum = cycleNum - range._1
    val leftIntSum = this._integrateIntensity(
      putativeFt, pklTree, firstCycleNum, lastCycleNum, mzTolPPM, minNbPeaks
    )

    // Right check
    firstCycleNum = cycleNum + range._1
    lastCycleNum = cycleNum + range._2
    val rightIntSum = this._integrateIntensity(
      putativeFt, pklTree, firstCycleNum, lastCycleNum, mzTolPPM, minNbPeaks
    )

    // Determine the direction by comparing the summed intensities
    var ascDirection = 0
    if (leftIntSum >= 0 || rightIntSum >= 0) {
      if (rightIntSum > leftIntSum) {
        ascDirection = 1
      } else {
        ascDirection = -1
      }
    }
    ascDirection
  }
  
  /**
   * old way
   */
  protected def _integrateIntensity(putativeFt: PutativeFeature, pklTree: PeakListTree,
                                    firstCycle: Int, lastCycle: Int, mzTolPPM: Float,
                                    minNbPeaks: Int): Double = {

    val theoIP = putativeFt.theoreticalIP
    var intensitySum = 0.0

    // Sum the forward isotopic profile intensities
    breakable {
      for (curCycleNum <- firstCycle to lastCycle) {
        
        if (this.ms1ScanIdByCycleNum.contains(curCycleNum) == false) 
          break

        val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
        val curScanH = this.scanHeaderById(curScanId)

        val ip = pklTree.extractIsotopicPattern(curScanH, putativeFt.theoreticalIP, mzTolPPM, maxNbPeaksInIP = Some(2))

        if (ip.isDefined && ip.get.peaks.length >= minNbPeaks) {
          intensitySum += ip.get.intensity
        }
      }
    }

    intensitySum
  }
  
  
   /**
   * extract all the isotopicPatterns of the XIC
   * consecutiveGap is increased when it fails to extract ip or ip.intensity < percentage of maxInt ip
   */
  protected def _extractIsotopicPatterns( putativeFt: PutativeFeature,
		  							                      maxTheoreticalPeakelIndex: Int,
                                          pklTree: PeakListTree,
                                          startingScanHeader: ScanHeader,
                                          extractionConf : FeatureExtractorConfig,
                                          maxNbPeaksInIP:Option[Int] = null): Array[IsotopicPattern] = {
    
    val ips = new ListBuffer[IsotopicPattern]()
    
    val cycleNum = startingScanHeader.getCycle
    var apexTime = startingScanHeader.getTime
    
    val theoIP = putativeFt.theoreticalIP
    
    // Determine intensity ascendant direction
    val range = Pair(1, 10)
    var ascDirection = this._getIntensityAscendantDirection(putativeFt,
                                                            pklTree,
                                                            cycleNum,
                                                            range,
                                                            this.mzTolPPM,
                                                            minNbPeaks=1)
    // 1 => right
    // -1 => left
    if( ascDirection == 0 ) // if no signal found on both sides
      return ips.toArray
    
    // --- Extract isotopic patterns ---
    var curMaxIntensity = 0.0
    //var lastIntensity = 0f
    
    // Iterate until left and right directions have been analyzed
    var numOfAnalyzedDirections = 0
        
    while( numOfAnalyzedDirections < 2 ) {
      
      var timeOverRange = false
      var consecutiveGapCount = 0
      var cycleShift = 0
      
      // Stop extraction in this direction if we have too much gaps or if we exceed run time range
      breakable {
        while( consecutiveGapCount <= extractionConf.maxConsecutiveGaps && !timeOverRange ) {
          
          // Decrease cycle shift if LEFT direction
          if( ascDirection == -1 ) {
            cycleShift -= 1 
          }
          
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
            val curTime = curScanH.getTime
              
            // check if total time does not exceed the provided threshold
            if( extractionConf.maxTimeWindow > 0 && math.abs(curTime - apexTime) > extractionConf.maxTimeWindow / 2 ) {
              timeOverRange = true
              break
            }
            
            //TODO:Warning called is maxNbPeaksInIp = null !
            //Note:we extract at least until maxTheoreticalPeakelIndex to get not only the monoisotopic peak but further peakels (in case of high
            //mass). The idea is not to miss the highest elution peak
            val ipOpt = pklTree.extractIsotopicPattern( curScanH, theoIP, mzTolPPM, maxNbPeaksInIP, maxTheoreticalPeakelIndex )
            
            //WEIRD CASES
            //if( cycleShift == 0 && ipOpt.isEmpty ) {
              // Sometimes the precursor m/z is wrong => just skip these weird cases
              //this.logger.trace( "supervised ft extraction failed at scan id=%06d & mz=%f".format(curScanH.getId, putativeFt.getMz) )
            //}
            
            // Check if an isotopic pattern has been found
            if( ipOpt.isDefined ) {
              
              val ip = ipOpt.get
              
              val intensity = ip.getIntensity()
                 
              // Add the isotopic pattern to the list of extracted IPs
              if( ascDirection == 1 )
                ips += ip // append IP
              else if( ascDirection == -1 )
                ips.+=:(ip) // prepend IP
                
              // Analysis of the isotopic pattern intensity
              if( intensity > curMaxIntensity ) {
                // Update information about the apex
                curMaxIntensity = intensity
                apexTime = curTime
              }
                
              // test intensity < intensityThreshold
              if( intensity == 0 || intensity < curMaxIntensity * extractionConf.minPercentageOfMaxInt ) {
                consecutiveGapCount += 1
                if (consecutiveGapCount > extractionConf.maxConsecutiveGaps)
                  break
              } else { 
                consecutiveGapCount = 0
              }
              
            //ip not defined
            } else { 
              consecutiveGapCount += 1
              if (consecutiveGapCount > extractionConf.maxConsecutiveGaps)
                break
            }
  
            // Increase cycle shift if right direction
            if( ascDirection == 1 ) 
              cycleShift += 1
            
          }// END OF ELSE
        } // END OF WHILE
      }
      ascDirection *= -1
      numOfAnalyzedDirections += 1
    }
    
    ips.toArray
  }
  
  /**
   * 
   */
  def _getTheoriticalMaxPeakelIndex(theoIp: TheoreticalIsotopePattern): Int = {
    val relativeAbundances = theoIp.getRelativeAbundances
    val maxValue = relativeAbundances.maxBy(x=>x)
    relativeAbundances.indexOf( maxValue )  
  }
  
  /**
   * 
   */
  def _buildFeatureFromIPsIdx(pft:PutativeFeature, 
                              tmpFt: Feature, 
                              index:Pair[Int, Int] ) : Feature = {
    require( index._1 != index._2)
    val (minIdx, maxIdx) = (index._1, index._2)
    
    /*var ips: Array[IsotopicPattern]= null
    try {
      ips = (for ( i <- minIdx to maxIdx) for (p <- peakels) if yield tmpFt.getIsotopicPattern(i)).toArray
    } catch {
      case _ :Throwable => {println(s"minIdx:${minIdx}, maxIdx:${maxIdx}, tmpFt peakel length: ${tmpFt.peakels(0).peaks.length}")
        
      }   
    } 
    val filteredIps = ips.filter(_ != null )
    
    if (filteredIps.isEmpty)
      return null
     */
   val peakels = new ArrayBuffer[Peakel]()
   breakable {
     for (c <- 0 until tmpFt.peakelsCount) {
       val peakel = tmpFt.peakels(c) 
       val idx = peakel.index
       val peaks = peakel.peaks.slice(minIdx, maxIdx + 1)
       if (peaks.count(_ != null) > 0)
         peakels += peakel.copy(peaks=peaks)//Peakel(idx, peaks)
       else 
          break
      }
   }
    
   if (peakels.isEmpty || peakels.forall(_ == null) == true)
      return null 

    //remove empty isotopic pattern if any, or fail at isotopic pattern request construction
    //val nbPeaks = peakels(0).peaks.length
    //for (i <- 0 until nb)
    
   new Feature( pft.id, pft.mz, pft.charge, peakels.toArray )  
  }
  
 
  
  /**
   * BASIC OR WAVELET DETECTION ALGORITHM
   */
  def _findPeakelsIndexes(peaks: Array[Peak], method: Int = 1, minSNR: Float = 0f): Array[Pair[Int, Int]]  = {
    
    var peakelIndexes:Array[(Int, Int)] = null //new ArrayBuffer[(Int, Int)]
    /*val maxs = this._findMaximaNaive(peaks.map(_.getIntensity()))
    
    if (maxs.isEmpty) {
      println("no local maxima detected")
      return null
    } else if (maxs.length == 1) {
      peakelIndexes += Pair(0, peaks.length-1)
    } else {*/
      if (method == 1) {
        peakelIndexes = BasicPeakelFinder.findPeakelsIndexes(peaks.map(_.getIntensity toDouble), 2)//.foreach(peakelIndexes += _)
      } else {
        val wpf = new WaveletBasedPeakelFinder(peaks)
        wpf.ridgeFilteringParams.minSNR = minSNR
        peakelIndexes = wpf.findPeakelsIndexes(asScanId=false)//.foreach(peakelIndexes += _)
      }
    //}
    peakelIndexes
  }
  
  
  
   def _nbGapInMaxPeakelRespectful(ft:Feature, maxPredictedPeakelIndex:Int, maxConsecutiveGaps:Int): Boolean = {
     //should never happened
     if (ft.peakelsCount == 0)
      return false
      
     val maxIntensityPeakel = if (ft.peakelsCount > maxPredictedPeakelIndex) 
                                 ft.peakels(maxPredictedPeakelIndex) 
                              else ft.peakels.maxBy(_.getIntensity)
     var gap = 0
     for (p <- maxIntensityPeakel.peaks) {
      if (p == null)
        gap += 1
      else
        gap = 0
      if (gap > maxConsecutiveGaps)
        return false
    }
    return true
  }
   
   
  def _isPeakelGoodForPeakDetection( peakel: Peakel) : Boolean = {
     val peaks = peakel.peaks
    if (peaks.length < 5)
      return false
    
    val definedPeaks = peakel.definedPeaks
    if ( definedPeaks.length <= 3 )
      return false
    true
  }
  
 
 
   
/*******************************************************************************
 * EXPOSED FUNCTIONS
 *******************************************************************************/
  /**
   * will be used in TimeExtractor
   */
  
  
  
  /**
   *  
   */
  def _extractFeature(putativeFt: PutativeFeature, 
                      pklTree: PeakListTree, 
                      extractionConf:FeatureExtractorConfig, 
                      method:Int = 1,
                      minSNR :Float = 0f): Option[Feature] = {
    
    // Retrieve the scan header corresponding to the starting scan id
    val scanHeaderOpt = this.scanHeaderById.get(putativeFt.scanId) 
  
    val scanHeader = scanHeaderOpt.get
    val ftTime = scanHeader.getElutionTime()
    
    val maxTheoreticalPeakelIndex = this._getTheoriticalMaxPeakelIndex(putativeFt.theoreticalIP)
    
    // Extract isotopic patterns around the starting scan
    // by default extract a maxNbPeaks given by the averagine 
    // ips never null
    val ips = this._extractIsotopicPatterns(putativeFt, 
                                            maxTheoreticalPeakelIndex, 
                                            pklTree, 
                                            scanHeader, 
                                            extractionConf, 
                                            maxNbPeaksInIP = Some(3))//.filter(  _.peaks.count( _ != null) > 0 )
             
    if( ips.isEmpty )
     return Option.empty[Feature]
     
    // Normalize the intensity of extracted IPs
    //this.normalizeIPs( ips )
    
    // Build a peakels, automatically remove empty peakels
    val peakels = Feature.buildPeakels(ips)
    
    if (peakels.isEmpty)
      return Option.empty[Feature]
          
    var tmpFt = new Feature( putativeFt.id, putativeFt.mz, putativeFt.charge, peakels )
    
    //-------- REFINE PEAKEL OPTIONAL STEP --------
    if ( extractionConf.refineDetection ) {
      //find maxpeakelIndex
      val maxPeakelIndex = if ( maxTheoreticalPeakelIndex < tmpFt.peakelsCount ) maxTheoreticalPeakelIndex else 0
      
      //get the defined peaks
      val maxPeakel = tmpFt.peakels(maxPeakelIndex)
      
      //check definedPeaks length > 3 and peaks length >= 5
      if ( this._isPeakelGoodForPeakDetection(maxPeakel) == false)
        return Option.empty[Feature]
      
      val (peaks, definedPeaks) = (maxPeakel.peaks, maxPeakel.definedPeaks)
       
      //detect peaks
      val peakelIndexes = this._findPeakelsIndexes(definedPeaks, method, minSNR)
      
      //treat matching Idx
      var matchingPeakIdx:(Int, Int) = null
      
      //Note if we are not good at peak extraction, no matching peak will be found, ms2 event outside of xic
      val filteredIndexes = peakelIndexes.filter( idx => ftTime >= definedPeaks(idx._1).getLcContext.getElutionTime && 
                                                         ftTime <= definedPeaks(idx._2).getLcContext.getElutionTime )
        
       if (! filteredIndexes.isEmpty) {
         //find the closest peakel in time domain of the ms2 event ?
         matchingPeakIdx = filteredIndexes.minBy{ idx => val apex = definedPeaks.slice(idx._1, math.min(idx._2, definedPeaks.length - 1))
                                                                                .maxBy(_.getIntensity)
                                                         math.abs(ftTime - apex.getLcContext.getElutionTime)
                                                }
       }
      
       //If not matching peaks
      if (matchingPeakIdx == null)
        return Option.empty[Feature]
      
      val ipsIndexes = (peaks.indexOf(definedPeaks(matchingPeakIdx._1)), peaks.indexOf(definedPeaks(matchingPeakIdx._2)))
      
      val ft = this._buildFeatureFromIPsIdx( putativeFt, tmpFt, ipsIndexes)//definedPeaks, tmpFt.peakels(maxPeakelIndex).peaks )
      
      if (ft == null)
        return Option.empty[Feature]
      
      Some(ft)    
    
    } 
    
    else Some(tmpFt)
  
  }//end _extractFeature

}