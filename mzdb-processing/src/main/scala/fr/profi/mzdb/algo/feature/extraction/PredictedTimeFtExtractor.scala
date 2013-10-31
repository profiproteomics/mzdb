package fr.profi.mzdb.algo.feature.extraction

import collection.mutable.HashMap
import util.control.Breaks._
import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.IsotopicPattern
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.algo.signal.detection.CwtPeakel
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.utils.math.wavelet.Ridge
import fr.profi.mzdb.utils.math.wavelet.RidgesFinder
import fr.profi.mzdb.model.Peakel


class PredictedTimeFtExtractor(
  //override val mzDbReader: MzDbReader,
  override val scanHeaderById: Map[Int, ScanHeader],
  override val nfByScanId: Map[Int, Float],
  override val mzTolPPM: Float,
  override val maxNbPeaksInIP: Int,
  override val minNbOverlappingIPs: Int,
  val minConsecutiveScans: Int = 4,
  val predictedTimeTol: Int = 180) 
  extends Ms2DrivenFtExtractor(
  scanHeaderById,
  nfByScanId,
  mzTolPPM,
  maxNbPeaksInIP,
  minNbOverlappingIPs) { 

  override def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    val startingScanId = this._findStartingScanId(putativeFt, pklTree)
    //val startingScanIdCwt = _findStartingScanIDUsingCwt(putativeFt, pklTree)

    if (startingScanId > 0)
      return super.extractFeature(putativeFt, pklTree, startingScanId)
    else
      return Option.empty[Feature]

  }

  /**
   * use wavelet technique to dertermine starting point to extract
   */
  def extractFeatureCwt(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    val startingScanIdCwt = _findStartingScanIDUsingCwt(putativeFt, pklTree)
    if (startingScanIdCwt > 0)
      return super.extractFeature(putativeFt, pklTree, startingScanIdCwt)
    else
      return Option.empty[Feature]
  }

  /**
   * Try to select the best peakel in cross assignment
   * in order to extract with the ms2DrivenExtractor
   * 
   * 
   * Workflow
   *  Peakel detection using wavelet approaches
   *          then
   *  check if they are monoisotopic (overlapping test)
   *  keep only ones that are monoisotopic
   *          then
   *  compute apex deviation
   *  keep the longest path between monoisotopic through isotopic peakels
   *          then
   *  compute rmsd, keep the path with the best average rmsd
   *          then
   *  return the apex scanId of the best monoisotopic peakel, then perform
   *  extraction with ms2DrivenExtractor 
   */
  private def _findStartingScanIDUsingCwt(putativeFt: PutativeFeature, pklTree: PeakListTree): Int = {
    //extract some vars
    val elutionTime = putativeFt.elutionTime
    val mz = putativeFt.mz
    val charge = putativeFt.charge

    val curScanH = this.getScanHeaderForTime(elutionTime, 1)
    val leftmostScanH = this.getScanHeaderForTime(elutionTime - predictedTimeTol, 1)
    val rightmostScanH = this.getScanHeaderForTime(elutionTime + predictedTimeTol, 1)

    val scanIDs = pklTree.scansIDs().filter(x => x > (curScanH.getId - leftmostScanH.getId) && x < (curScanH.getId + rightmostScanH.getId)) toArray

    val deltaMass = 1.002f / charge

    val peakels = new ArrayBuffer[Array[CwtPeakel]] // nbPeakels 0 monoiso -> 5 maxIso
    var maxNbPeakels = 0
    
    breakable {
      for (c <- 0 to this.maxNbPeaksInIP) {

        val mzToCheck = (deltaMass * c) + mz
        //extractPeaks
        val peaks = _extractPeaks(putativeFt, pklTree, scanIDs, mzToCheck, mzTolPPM)
        //build cwt
        val peakelFinder = new WaveletBasedPeakelFinder(peaks) //mexh by default
        //by default minSnr = 3
        peakelFinder.ridgeFilteringParams.minSNR = 1.5f
        val peakelsInPredictedRange = peakelFinder.findCwtPeakels().filter(x => x.apexLcContext.getElutionTime() > leftmostScanH.getElutionTime() && 
                                                                                x.apexLcContext.getElutionTime() < rightmostScanH.getElutionTime())
        //we break if did not find any peakel ?
        if (peakelsInPredictedRange.isEmpty)
          break
          
        peakels += peakelsInPredictedRange
        maxNbPeakels = math.max(maxNbPeakels, peakelsInPredictedRange.length)
      }
    }
    
    // nothing found
    if (peakels.isEmpty) {
      return 0
    }
    
    //no computation of the correlation, only the monoistopic is found
    // TODO: we return the scanId of the maximum intensity ? or the closest in rt
    //this is most intense which is choosen
    if (peakels.length == 1) {
      return peakels(0).sortBy( _.intensityMax ).last.apexLcContext.getScanId()
    }
    
    //the value  we will return
    var scanId = 0

    //TODO: check if they are really the monoisotopic peakel of a feature (overlapping test)
    //(peakel: Peakel, pf: PutativeFeature, charge:Int, pklTree:PeakListTree)
    val monoIsos = peakels(0).filter(peakel => this._checkIsMonoisotopicPeakel(peakel, putativeFt, 
                                                                               putativeFt.charge, pklTree, 
                                                                               minNbOverlappingIPs));
    

    //find longest path between all peakels belonging to different isotopes lovels
    val longestRidges = this._distCalc(peakels).groupBy(_.length).maxBy(_._1)._2
    
    //should never append
    //algo failed to find something relevant
    if (longestRidges.isEmpty) {
      logger.warn("no signal found in selected region")
      return scanId
    }
    
    //calc rmsds
    val rmsdsBymonoIso = this._correlationCalc(longestRidges)
    
    //keep the best monisotopic peakel
    //compute the mean correlation for each path
    //keep the monoisotopicpeakel which the highest mean correlation
    val bestMonoIso = rmsdsBymonoIso.map{ case (cwtPeakel, rmsds) => cwtPeakel-> (rmsds.map(_._2).sum / rmsds.length) }.maxBy(_._2)._1
    
    //return its scanId
    scanId = bestMonoIso.apexLcContext.getScanId
    scanId
  }
  
  /**
   * get the most intense peak in the range mz - mzTol, mz + mzTol, equivalent to getXic(XicMethod.MAX)
   */
  private def _extractPeaks(putativeFt: PutativeFeature, pklTree: PeakListTree, selectedScanIDs: Array[Int], mz: Double, mzTol: Float): Array[Peak] = {
    var peaks = new ArrayBuffer[Peak]
    selectedScanIDs.foreach { x =>
      var minmz = putativeFt.mz - (mzTolPPM * putativeFt.mz / 1e6)
      var maxmz = putativeFt.mz + (mzTolPPM * putativeFt.mz / 1e6)
      val p = pklTree.getPeaksInRange(x, minmz, maxmz)
      if (p.length > 0) peaks +=  p.sortBy(_.getIntensity).last else peaks += new Peak(0,0)
    }
    peaks.toArray
  }
  

  /**
   * return the longest ridges ( we hope the good feature has its isotopes
   * belonging to one of the longest ridge ( better isotopic pattern...) )
   * TODO: have to see if it matches with rmsd calc
   * 
   */
  private def _distCalc(peakels: ArrayBuffer[Array[CwtPeakel]], maxScanDrift:Int =10 ) : Array[Array[CwtPeakel]] = {
    //should never append
    //tested before entering 
    if (peakels.length <= 1) {
      return null
    }
    val monoIsos = peakels(0);
    val ridges = new ArrayBuffer[ArrayBuffer[CwtPeakel]]
    for (monoIso <- monoIsos) {
     val currentRidge = new ArrayBuffer[CwtPeakel]() += monoIso
     breakable {
       for (i <- 1 until peakels.length) {
           val isoPeakels = peakels(i).filter(p => math.abs(p.index - monoIso.index ) < maxScanDrift )
           //update the currentRidge
           if ( isoPeakels.isEmpty)
             break
             
           currentRidge += isoPeakels(0)
           for (j <- 1 until isoPeakels.length) {
             val clonedRidge = currentRidge.clone
             clonedRidge += isoPeakels(j)
             ridges += clonedRidge
           }
       }
     }//end breakable
    }//end monoIsos
    ridges.map(_.toArray) toArray
  }

  
  /**
   * return a hashmap containing monoistopicpeakel as key and a hashmap containing best rmsd in each isotopic level
   */
  private def _correlationCalc(peakels : Array[Array[CwtPeakel]]): collection.mutable.Map[CwtPeakel, ArrayBuffer[Pair[CwtPeakel, Double]]] = {
    
    var monoIsos =  new HashMap[CwtPeakel, ArrayBuffer[Pair[CwtPeakel, Double]]] ++ //annoying...
                    peakels(0).map( _ -> new ArrayBuffer[Pair[CwtPeakel, Double]]()).toMap
                    
    for (monoiso <- monoIsos.keys) {
      for ( i <- 1 until peakels.length) {
        val isoPeakels = peakels(i)
        for (isoPeakel <- isoPeakels) {
          val rmsd =  monoiso.computeCorrelationWith(isoPeakel)
          monoIsos(monoiso) += Pair(isoPeakel, rmsd)
        }
      }
    }
    monoIsos
  }


  private def _findStartingScanId(putativeFt: PutativeFeature, pklTree: PeakListTree): Int = {

    // Retrieve some vars
    val theoIP = putativeFt.theoreticalIP
    val elutionTime = putativeFt.elutionTime
    var curScanH = this.getScanHeaderForTime(elutionTime, 1)

    // If no scan header for current time => take the last scan header
    if (curScanH == null) {
      val ftId = putativeFt.id
      this.logger.debug("feature (id=" + ftId + ") elution time is out of the range of the current raw file")

      curScanH = this.scanHeaders.last

      // Check that the feature elution time is greater than the last scan header
      if (elutionTime < curScanH.getElutionTime)
        throw new Exception("can't retrieve a scan header at time: " + elutionTime)
    }

    // Retrieve the cycle number
    var curCycleNum = curScanH.getCycle

    val leftmostScanH = Option(this.getScanHeaderForTime(elutionTime - predictedTimeTol, 1)).getOrElse(curScanH)
    val rightmostScanH = Option(this.getScanHeaderForTime(elutionTime + predictedTimeTol, 1)).getOrElse(curScanH)
    val leftmostCycleNum = leftmostScanH.getCycle
    val rightmostCycleNum = rightmostScanH.getCycle

    // Compute the cycle range which will be screened to search for signal
    var cycleShift = 0
    if (math.abs(rightmostCycleNum - curCycleNum) >= math.abs(leftmostCycleNum - curCycleNum)) {
      cycleShift = math.abs(rightmostCycleNum - curCycleNum)
    } else {
      cycleShift = math.abs(leftmostCycleNum - curCycleNum)
    }
    val range = Pair(0, cycleShift)

    // Search for signal while progressively decreasing the number of required peaks in extracted IPs
    var startingScanId = 0

    breakable {
      for (minNbPeaks <- this.maxNbPeaksInIP to 1 by -1) {

        // Search for the direction which exhibits the highest intensity
        val intensityAscDir = _getIntensityAscendantDirection(
          putativeFt,
          pklTree,
          curCycleNum,
          range,
          this.mzTolPPM,
          minNbPeaks
        )

        // Check if there is some signal in one of the left/right directions
        if (intensityAscDir != 0) {

          var timeShift = 0f
          var nbConsecutiveScans = 0
          var nbConsecutiveGaps = 0
          var breaked = false

          while (timeShift < predictedTimeTol && !breaked) {

            // Try to retrieve the scan id
            if (!this.ms1ScanIdByCycleNum.contains(curCycleNum)) breaked = true
            else {
              val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
              val startingScanH = this.scanHeaderById(curScanId)

              // Try to extract some signal
              val ipOpt = pklTree.extractIsotopicPattern(startingScanH, theoIP, mzTolPPM, 2)

              // Check if signal has been extracted
              if (ipOpt != None && ipOpt.get.peaks.length >= minNbPeaks) {
                nbConsecutiveScans += 1
                nbConsecutiveGaps = 0
                startingScanId = startingScanH.getId
              } else {
                startingScanId = 0
                nbConsecutiveGaps += 1

                if (nbConsecutiveGaps > this.maxConsecutiveGaps) nbConsecutiveScans = 0

              }

              // Stop the signal extractions if we have enough consecutive scans
              if (nbConsecutiveScans >= minConsecutiveScans) breaked = true
              else {
                curCycleNum += intensityAscDir
                timeShift = math.abs(startingScanH.getTime - elutionTime)
              }
            }
          }
        }

        if (startingScanId != 0) break

      }
    }
    startingScanId
  }

}
