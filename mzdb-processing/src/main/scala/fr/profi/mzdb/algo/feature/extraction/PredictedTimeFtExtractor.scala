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
  val predictedTimeTol: Int = 120) extends Ms2DrivenFtExtractor(
  scanHeaderById,
  nfByScanId,
  mzTolPPM,
  maxNbPeaksInIP,
  minNbOverlappingIPs
) with RidgesFinder {

  override def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    //System.out.println("extracting feature with m/z="+putativeFt.mz +" at time="+putativeFt.elutionTime);
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

  private def _findStartingScanIDUsingCwt(putativeFt: PutativeFeature, pklTree: PeakListTree): Int = {
    //extract some vars
    val elutionTime = putativeFt.elutionTime
    val mz = putativeFt.mz
    val charge = putativeFt.charge

    val curScanH = this.getScanHeaderForTime(elutionTime, 1)
    val leftmostScanH = this.getScanHeaderForTime(elutionTime - predictedTimeTol, 1)
    val rightmostScanH = this.getScanHeaderForTime(elutionTime + predictedTimeTol, 1)

    val scanIDs = pklTree.scansIDs().filter(x => x > (curScanH.getId - leftmostScanH.getId) && x < (curScanH.getId + rightmostScanH.getId)) toArray

    val NB_PEAKELS_TO_CHECK = 3

    val deltaMass = 1.002f / charge

    var peakels = new ArrayBuffer[Array[CwtPeakel]]
    var values = new ArrayBuffer[Array[Float]]
    var maxNbPeakels = 0
    
    breakable {
      for (c <- 0 to NB_PEAKELS_TO_CHECK) {

        var mzToCheck = (deltaMass * c) + mz
        //extractPeaks
        var peaks = _extractPeaks(putativeFt, pklTree, scanIDs, mzToCheck, mzTolPPM)
        values += peaks.map(_.getIntensity)
        //build cwt
        val peakelFinder = new WaveletBasedPeakelFinder(peaks, 
                                                        scales = (6f to 120f by 2f).toArray, 
                                                        wavelet = MexicanHat()) //mexh by default
        var peakelsInPredictedRange = peakelFinder.findCwtPeakels().filter(x => x.apexLcContext.getElutionTime() > leftmostScanH.getElutionTime() && 
                                                                                x.apexLcContext.getElutionTime() < rightmostScanH.getElutionTime())

        //we break if did not find any peakel ?
        if (peakelsInPredictedRange.isEmpty)
          break
        peakels += peakelsInPredictedRange
        maxNbPeakels = math.max(maxNbPeakels, peakelsInPredictedRange.length)
      }
    }
    
    if (peakels.isEmpty) // nothing found
      return 0
    
    //no computation of the correlation
    //we return the scanId of the maximum intensity ? or the closest in rt
    if (peakels.length == 1) {
      return peakels(0).sortBy( _.intensityMax ).last.apexLcContext.getScanId()
    } 
     
    //usual case 
    val monoIsosRmsd = _rmsdCalc(peakels, values)
    //val monoIsosEval = monoIsos 
    //retrieving the best solution
    val isos = monoIsosRmsd.map{ case (monoiso, mapping) => monoiso -> (mapping.map { case (index, rmsd) => rmsd._2 }).toBuffer.sum }
    val bestMonoIso = isos.maxBy(x=> x._2)._1
    bestMonoIso.apexLcContext.getScanId()
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
   * return a hashmap containing monoistopicpeakel as key and a hashmap containing best rmsd in isotopic level
   */
  private def _rmsdCalc(peakels : ArrayBuffer[Array[CwtPeakel]],
                        values: ArrayBuffer[Array[Float]],
                        scanDrift : Int = 5): collection.mutable.Map[CwtPeakel, HashMap[Int, Pair[CwtPeakel, Double]]] = {
    
    var monoIsos = HashMap[CwtPeakel, HashMap[Int, Pair[CwtPeakel, Double]]]() ++ peakels(0).map( _ -> new HashMap[Int,Pair[CwtPeakel, Double]]()).toMap
    for (monoiso <- monoIsos.keys) {
      val (minIdx, maxIdx) = (monoiso.minIdx, monoiso.maxIdx)
      var array = values(0).slice(minIdx, maxIdx).toBuffer
      for ( i <- 1 until peakels.length) {
        val closestPeakels = peakels(i).filter(x => math.abs(monoiso.apexLcContext.getScanId() - x.apexLcContext.getScanId()) < scanDrift)
        var peakelsWithRmsd = closestPeakels.map(x => x -> VectorSimilarity.rmsd(array.map(_.toDouble).toArray, values(i).slice(x.minIdx, x.maxIdx).map(_.toDouble).toArray)).toBuffer
        monoIsos(monoiso)(i) = peakelsWithRmsd.sortBy(_._2).last
      }
    }
    monoIsos
  }

  

  /**
   * use to make peakels contain same number of peaks, if shorter use zero padding,
   * else remove value to match min and max Idx of the monoisotopic peakel
   */
  private def zeroPad(peakel: CwtPeakel, values: Array[Float], minIdx: Int, maxIdx: Int): Array[Float] = {
    var (minIdx_, maxIdx_) = (peakel.minIdx, peakel.maxIdx)
    var output = values.slice(minIdx_, maxIdx_).toBuffer

    while (minIdx_ < minIdx) {
      output.remove(0)
      minIdx_ += 1
    }

    while (minIdx_ > minIdx) {
      output.insert(0, 0f)
      minIdx_ -= 1
    }

    while (maxIdx_ < maxIdx) {
      output += 0f
      maxIdx_ += 1
    }

    while (maxIdx_ > maxIdx) {
      output.remove(output.length - 1)
      maxIdx_ -= 1
    }

    output.toArray
  }

  private def _findStartingScanId(putativeFt: PutativeFeature, pklTree: PeakListTree): Int = {

    // Retrieve some vars
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
        val intensityAscDir = _getIntensityAscendantDirection(putativeFt, pklTree, curCycleNum, range,
          this.mzTolPPM, minNbPeaks, minNbPeaks)

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
              val ipOpt = pklTree.extractIsotopicPattern(startingScanH, putativeFt.mz, this.mzTolPPM,
                putativeFt.charge, this.maxNbPeaksInIP);

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
