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


 object Method extends Enumeration {
    type Method = Value
    val CLASSIC, RIDGE = Value
  }

import fr.profi.mzdb.algo.feature.extraction.Method._

class PredictedTimeFtExtractor(
  //override val mzDbReader: MzDbReader,
 
    
  override val scanHeaderById: Map[Int, ScanHeader],
  override val nfByScanId: Map[Int, Float],
  override val mzTolPPM: Float,
  override val maxNbPeaksInIP: Int,
  override val minNbOverlappingIPs: Int,
  val minConsecutiveScans: Int = 4,
  val predictedTimeTol: Int = 120,
  val method : Method = Method.RIDGE) extends Ms2DrivenFtExtractor(
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

    val NB_PEAKELS_TO_CHECK = 5

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
    var scanId = 0
    if (method == Method.CLASSIC) {
      //usual case 
      val monoIsosRmsd = _rmsdCalc(peakels, values)
      //retrieving the best solution
      val result = new HashMap[Int, ArrayBuffer[Pair[CwtPeakel, ArrayBuffer[Pair[CwtPeakel, Double]]]]]
      monoIsosRmsd.map{ case (peakel, mapping) => 
        result.getOrElseUpdate(mapping.size, new ArrayBuffer[Pair[CwtPeakel, ArrayBuffer[Pair[CwtPeakel, Double]]]]()) += Tuple2(peakel, mapping)}
      val longestMonoIsos = result.maxBy(_._1)._2
      val bestMonoIso = longestMonoIsos.map{ case (peakel, array) => peakel -> (array.map{ x=> x._2}).toBuffer.sum / array.length } maxBy(_._2)//.apexLcContext.getScanId()
      scanId = bestMonoIso._1.apexLcContext.getScanId()
      
    } else if (method == Method.RIDGE) {
      var ridges = ridgeCalc(peakels)
      if (ridges.isEmpty) {
        logger.warn("no signal found in selected region")
        return 0
      }
      var weightedRidges = _rmsdCalc(peakels, ridges, values)
      //we take the minimum
      var bestCandidateRidge = weightedRidges.map{ case (ridge, rmsds) => (ridge, rmsds._1, rmsds._2.sum[Double] / rmsds._2.length) }.toList.maxBy(_._3)
      //return the maxIdx (scanId) of the ridge that has the most intense value at monoistopic peakel 
      scanId = bestCandidateRidge._2.apexLcContext.getScanId()
    } else {
      throw new Exception("Improper method")
    }
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
   * return the most probable Ridges
   */
  private def ridgeCalc(peakels: ArrayBuffer[Array[CwtPeakel]] ) : Array[Ridge] = {
    //var apexes = new ArrayBuffer[ArrayBuffer[Int]]
    var apexes = peakels.map {x=> x.map {_.apex} } toArray
    //we find the ridges here, winLength is not so important since it will find the closest apex, and
    //we set the maxGap to 1 (the min possible) since we don't want hole in isotopicPattern
    var (ridges, orphanRidges) = this.findRidges(apexes.reverse, null, winLength = 10, maxGap = 1) //10scans aprroximatively 20-30 s
    var ridgesByLength = new HashMap[Int, ArrayBuffer[Ridge]]
    //TODO: check the filter here
    ridges.filter( !_.isEnded(1)).foreach( x => ridgesByLength.getOrElseUpdate(x.length, new ArrayBuffer[Ridge]) += x)
    var longestRidges = ridgesByLength(ridgesByLength.keys.toList.sortBy(x=>x).last)
    longestRidges.toArray
  }

   /**
   * lastScale correspond to the monoisotopic peakel
   * return an hashmap containing Ridge and an array of rmsd using monoistopic peakel
   */
  private def _rmsdCalc(peakels: ArrayBuffer[Array[CwtPeakel]], 
                        ridges: Array[Ridge], 
                        values: ArrayBuffer[Array[Float]]) : HashMap[Ridge, Pair[CwtPeakel, Array[Double]]] = {
    var rpeakels = peakels.reverse
    var rvalues = values.reverse
    var weightedRidges = new HashMap[Ridge, Pair[CwtPeakel,Array[Double]]]
    for (ridge <- ridges) {
      //var peaks = new HashMap[Int, Array[Float]]
      var correspondingPeakels = new HashMap[Int, CwtPeakel]
      ridge.maximaIndexPerScale.foreach{ case (scale, value)  =>  
        if (value != None) rpeakels(scale).foreach { cwtPeakel => 
          if (cwtPeakel.apex == value.get._1) 
            correspondingPeakels(scale) = cwtPeakel
            }
      }    
      var (monoisotopicScale, monoisotopicPeakel) = correspondingPeakels.maxBy(_._1)
      var (minIdx, maxIdx) = (monoisotopicPeakel.minIdx, monoisotopicPeakel.maxIdx)
      var monoisoArray = rvalues(monoisotopicScale).slice(minIdx, maxIdx).map(_.toDouble)
      var rmsds = new ArrayBuffer[Double]
      for ( (scale, peakel) <- correspondingPeakels if peakel != monoisotopicPeakel) {
        var array = zeroPad(peakel, values(scale), minIdx, maxIdx).map(_.toDouble)
        // calc rmsd
        rmsds += VectorSimilarity.rmsd(array, monoisoArray)
      }
      weightedRidges += ridge -> (monoisotopicPeakel, rmsds.toArray)
    }
    weightedRidges
  }

  
  /**
   * return a hashmap containing monoistopicpeakel as key and a hashmap containing best rmsd in each isotopic level
   */
  private def _rmsdCalc(peakels : ArrayBuffer[Array[CwtPeakel]],
                        values: ArrayBuffer[Array[Float]],
                        scanDrift : Int = 5): collection.mutable.Map[CwtPeakel, ArrayBuffer[Pair[CwtPeakel, Double]]] = {
    
    var monoIsos = HashMap[CwtPeakel, ArrayBuffer[Pair[CwtPeakel, Double]]]() ++ peakels(0).map( _ -> new ArrayBuffer[Pair[CwtPeakel, Double]]())
    for (monoiso <- monoIsos.keys) {
      val (minIdx, maxIdx) = (monoiso.minIdx, monoiso.maxIdx)
      var array = values(0).slice(minIdx, maxIdx).map(_.toDouble)
      for ( i <- 1 until peakels.length) {
        val closestPeakels = peakels(i).filter(x => math.abs(monoiso.apexLcContext.getScanId() - x.apexLcContext.getScanId()) < scanDrift)
        if (closestPeakels.isEmpty) {
          //monoIsos(monoiso)(i) = None //set to None
        }
        else if (closestPeakels.length == 1) {
          val x = closestPeakels(0)
          val rmsd =  VectorSimilarity.rmsd(array, values(i).slice(x.minIdx, x.maxIdx).map(_.toDouble))
          monoIsos(monoiso) += Tuple2(x, rmsd)
        } else {
          //take the best one
           var peakelsWithRmsd = closestPeakels.map(x => x -> VectorSimilarity.rmsd(array, values(i).slice(x.minIdx, x.maxIdx).map(_.toDouble)))
           monoIsos(monoiso) += peakelsWithRmsd.maxBy(_._2)
        }
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
