package fr.profi.mzdb.algo.feature.extraction

import collection.mutable.ArrayBuffer
import util.control.Breaks._
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.utils.ms.MsUtils
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.OverlappingIsotopicPattern

abstract class AbstractSupervisedFtExtractor extends AbstractFeatureExtractor {

  // Build scanIdByCycleNum
  val ms1ScanIdByCycleNum = this.scanHeaderById.values.filter { _.getMsLevel == 1 }.map { sh => sh.getCycle -> sh.getId } toMap

  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature]

  protected def _getIntensityAscendantDirection(putativeFt: PutativeFeature, pklTree: PeakListTree,
                                                cycleNum: Int, range: Pair[Int, Int], mzTolPPM: Float,
                                                minNbPeaks: Int, maxNbPeaks: Int): Int = {

    var (firstCycleNum, lastCycleNum) = (0, 0)

    // Left check
    firstCycleNum = cycleNum - range._2
    lastCycleNum = cycleNum - range._1
    val leftIntSum = this._integrateIntensity(putativeFt, pklTree, firstCycleNum, lastCycleNum,
      mzTolPPM, minNbPeaks, maxNbPeaks);

    // Right check
    firstCycleNum = cycleNum + range._1
    lastCycleNum = cycleNum + range._2
    val rightIntSum = this._integrateIntensity(putativeFt, pklTree, firstCycleNum, lastCycleNum,
      mzTolPPM, minNbPeaks, maxNbPeaks);

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

  protected def _integrateIntensity(putativeFt: PutativeFeature, pklTree: PeakListTree,
                                    firstCycle: Int, lastCycle: Int, mzTolPPM: Float,
                                    minNbPeaks: Int, maxNbPeaks: Int): Double = {

    var intensitySum = 0.0

    // Sum the forward isotopic profile intensities
    breakable {
      for (curCycleNum <- firstCycle to lastCycle) {

        if (this.ms1ScanIdByCycleNum.contains(curCycleNum) == false) break

        val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
        val curScanH = this.scanHeaderById(curScanId)

        val ip = pklTree.extractIsotopicPattern(curScanH, putativeFt.mz, mzTolPPM,
          putativeFt.charge, maxNbPeaks)

        if (ip != None && ip.get.peaks.length >= minNbPeaks) {
          intensitySum += ip.get.intensity
        }
      }
    }

    intensitySum
  }

  protected def _extractOverlappingIPs(ip: IsotopicPattern, pklTree: PeakListTree,
                                       maxZ: Int = 5, maxIpShift: Int = 3): Array[OverlappingIsotopicPattern] = {
    require(maxZ > 0, "maximum charge must be strictly positive")
    require(maxIpShift > 0, "maximum IP shift must be strictly positive")

    // Search for overlapping isotopic patterns
    val olpIPs = new ArrayBuffer[OverlappingIsotopicPattern]()

    // Try several charge states
    for (z <- 1 to maxZ) {

      // Try several m/z shifts
      for (ipShift <- (-maxIpShift) until 0) {

        // Skip current feature peaks
        if (ipShift != 0 && !(ipShift > 0 && z == ip.charge)) {

          val olpIpMz = ip.mz + (ipShift.toDouble / z)
          val olpIpNbPeaks = math.abs(ipShift);

          // Try to extract a putative overlapping isotopic pattern
          val tmpOlpIp = pklTree.extractOverlappingIsotopicPattern(scanHeader = ip.scanHeader,
            mz = olpIpMz,
            mzTolPPM = this.mzTolPPM,
            charge = z,
            maxNbPeaks = olpIpNbPeaks,
            overlapShift = ipShift)

          //System.out.println( "putativeFt.mz=" + putativeFt.mz + " z="  + z + " shift="+ ipShift + " olpIpMz="+ olpIpMz );

          // Check that we retrieved enough peaks
          if (tmpOlpIp != None && olpIpNbPeaks == tmpOlpIp.get.peaks.length) {
            // Set overlapping IP elution time
            //tmpOlpIp.elutionTime = ip.getElutionTime;
            olpIPs += tmpOlpIp.get
          }
        }

      }
    }
    olpIPs.toArray
  }

  protected def _checkIsMonoisotopicPeakel(f: Feature) {
    //TODO
  }
}