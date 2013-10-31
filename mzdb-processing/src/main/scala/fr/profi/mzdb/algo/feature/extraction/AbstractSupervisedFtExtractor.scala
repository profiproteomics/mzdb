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
import fr.profi.mzdb.model.TheoreticalIsotopePattern
import fr.profi.mzdb.model.Peakel

abstract class AbstractSupervisedFtExtractor extends AbstractFeatureExtractor {

  // Build scanIdByCycleNum
  val ms1ScanIdByCycleNum = this.scanHeaderById.values.filter { _.getMsLevel == 1 }.map { sh => sh.getCycle -> sh.getId } toMap

  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature]

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

  protected def _integrateIntensity(putativeFt: PutativeFeature, pklTree: PeakListTree,
                                    firstCycle: Int, lastCycle: Int, mzTolPPM: Float,
                                    minNbPeaks: Int): Double = {

    val theoIP = putativeFt.theoreticalIP
    var intensitySum = 0.0

    // Sum the forward isotopic profile intensities
    breakable {
      for (curCycleNum <- firstCycle to lastCycle) {

        if (this.ms1ScanIdByCycleNum.contains(curCycleNum) == false) break

        val curScanId = this.ms1ScanIdByCycleNum(curCycleNum)
        val curScanH = this.scanHeaderById(curScanId)

        val ip = pklTree.extractIsotopicPattern(curScanH,putativeFt.theoreticalIP,mzTolPPM,2)

        if (ip.isDefined && ip.get.peaks.length >= minNbPeaks) {
          intensitySum += ip.get.intensity
        }
      }
    }

    intensitySum
  }

  protected def _extractOverlappingIPs(ip: IsotopicPattern, theoIP: TheoreticalIsotopePattern, pklTree: PeakListTree,
                                       maxZ: Int = 5, maxIpShift: Int = 3): Array[OverlappingIsotopicPattern] = {
    
    require(maxZ > 0, "maximum charge must be strictly positive")
    require(maxIpShift > 0, "maximum IP shift must be strictly positive")

    // Search for overlapping isotopic patterns
    val olpIPs = new ArrayBuffer[OverlappingIsotopicPattern]()

    // Try several charge states
    for (z <- 1 to maxZ) {

      // Try several m/z shifts
      for (ipShift <- (-maxIpShift) until 0) {

          val olpIpMz = ip.mz + (ipShift.toDouble / z)
          val olpIpNbPeaksToReachMonoistopicPeakel = ipShift + 1
          
          // Configure a new theoretical isotope pattern
          val tmpTheoIP = theoIP.copy(
            mz = olpIpMz,
            charge = z,
            relativeAbundances = theoIP.relativeAbundances.take(olpIpNbPeaksToReachMonoistopicPeakel)//olpIpNbPeaks)
          )

          // Try to extract a putative overlapping isotopic pattern
          val tmpOlpIp = pklTree.extractOverlappingIsotopicPattern(
            scanHeader = ip.scanHeader,
            theoreticalIP = tmpTheoIP,
            mzTolPPM = this.mzTolPPM,
            nbPeaksToSum = 2,
            overlapShift = ipShift
          )

          // Check that we retrieved enough peaks
          if (tmpOlpIp.isDefined && olpIpNbPeaksToReachMonoistopicPeakel <= tmpOlpIp.get.peaks.length) {
            // Set overlapping IP elution time
            //tmpOlpIp.elutionTime = ip.getElutionTime;
            olpIPs += tmpOlpIp.get
          } 
      }
    }
    olpIPs.toArray
  }

  protected def _checkIsMonoisotopicPeakel(f: Feature, pf: PutativeFeature, pklTree:PeakListTree): Boolean = {
    f.getIsotopicPatterns.map(this._extractOverlappingIPs(_, pf.theoreticalIP , pklTree))
    true
  }
  
  protected def _checkIsMonoisotopicPeakel(peakel: Peakel, pf: PutativeFeature, charge:Int, pklTree:PeakListTree): Boolean = {
    val f = Feature(Feature.generateNewId, peakel.getMz, charge, Array[Peakel](peakel))
    this._checkIsMonoisotopicPeakel(f, pf, pklTree)
  }
  
  
}