package fr.profi.mzdb.algo.feature.extraction

import scala.Array.canBuildFrom
import scala.annotation.migration
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import com.fasterxml.jackson.annotation.JsonInclude
import com.weiglewilczek.slf4s.Logging

import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.IsotopicPatternLike
import fr.profi.mzdb.model.OverlappingIsotopicPattern
import fr.profi.mzdb.model.ScanHeader

abstract class AbstractFeatureExtractor extends Logging {

  val scanHeaderById: Map[Int, ScanHeader]
  val nfByScanId: Map[Int, Float]
  val mzTolPPM: Float
  val maxNbPeaksInIP: Int
  val minNbOverlappingIPs: Int

  val scanHeaders = scanHeaderById.values.toArray.sortBy(_.getId)
  val ms1ScanHeaderByCycleNum = Map() ++ scanHeaders.filter(_.getMsLevel() == 1 ).map(sh => sh.getCycle -> sh)

  private val TIME_INDEX_WIDTH = 15

  private val _scanIdsByTimeIndex: HashMap[Int, ArrayBuffer[Int]] = {

    val _tmpScanIdsMap = new HashMap[Int, ArrayBuffer[Int]]()

    for ((scanId, scanH) <- scanHeaderById) {
      val timeIndex = (scanH.getTime / TIME_INDEX_WIDTH).toInt
      _tmpScanIdsMap.getOrElseUpdate(timeIndex, new ArrayBuffer[Int]()) += scanH.getId
    }

    _tmpScanIdsMap
  }

  def getScanHeaderForTime(time: Float, msLevel: Int) = {

    val timeIndex = (time / TIME_INDEX_WIDTH).toInt

    var nearestScanHeader: ScanHeader = null
    val scanIdsByTimeIndex = this._scanIdsByTimeIndex

    for (index <- timeIndex - 1 to timeIndex + 1) {
      if (scanIdsByTimeIndex.contains(index)) {
        for (
          tmpScanId <- scanIdsByTimeIndex(index);
          scanH <- scanHeaderById.get(tmpScanId);
          if scanH.getMsLevel() == msLevel
        ) {
          if (nearestScanHeader == null || (scanH.getTime() - time).abs < (nearestScanHeader.getTime() - time).abs) {
            nearestScanHeader = scanH
          }
        }
      }
    }
    nearestScanHeader
  }

  protected def normalizeIPs(isotopicPatterns: Seq[IsotopicPatternLike]) {
    //isotopicPatterns.foreach( _.normalizeIntensities(nfByScanId) )
    ()
  }

  protected def updateFtOverlappingFeatures(ft: Feature, isotopicPatterns: Seq[IsotopicPattern], minNbIPs: Int) {

    // Check that the current features has peak
    if (ft.peakelsCount == 0)
      return

    // Group overlapping isotopic patterns by charge state and overlapShift of peaks
    val ovlIpsMap = new HashMap[Int, HashMap[Int, ArrayBuffer[OverlappingIsotopicPattern]]]()

    for (ip <- isotopicPatterns) if (ip.overlappingIps != null) {

      for (olpIp <- ip.overlappingIps) {
        val olpIpZ = olpIp.charge
        val sameChargeOlpIPsByOverlapShift = ovlIpsMap.getOrElseUpdate(olpIpZ, 
            new HashMap[Int, ArrayBuffer[OverlappingIsotopicPattern]])

        val olpIpNbPeaks = olpIp.peaks.length
        val sameFtOlpIPs = sameChargeOlpIPsByOverlapShift.getOrElseUpdate(olpIpNbPeaks, 
            new ArrayBuffer[OverlappingIsotopicPattern])

        sameFtOlpIPs += olpIp
      }
    }

    //build the peakels for each overlapping features (build feature  objects)
    val ovlFts = new ArrayBuffer[Feature] // hold
    ovlIpsMap.foreach {
      case (charge, mapping) =>
        mapping.foreach {
          case (nbPeaks, ftIps) =>
            if (ftIps.length >= this.minNbOverlappingIPs) {
              val ovlFt = new Feature(ftIps(0).mz, charge, ftIps)
              ovlFt.mz = ovlFt.peakels.head.getApex.getMz()
              ovlFts += ovlFt
            }
        }
    }
  
    val ftFirstPeakel = ft.peakels(0)
    val chargeStates = ovlIpsMap.keySet.toArray
    val olpFeatures = new ArrayBuffer[Feature](chargeStates.length)
    var bestOlpFt: Feature = null
    var highestPMCC = -1.0; // PMCC range is [-1,+1]
    for (olpFtCharge <- chargeStates) {
      val sameChargeIPs = ovlIpsMap(olpFtCharge)
      val nbPeaksList = sameChargeIPs.keySet.toArray
      var sameChargeHighestNbIPs = 0
      var sameChargeLongestOlpFt: Feature = null
      for (nbPeaks <- nbPeaksList) {
        val sameFtIps = sameChargeIPs(nbPeaks)
        this.normalizeIPs(sameFtIps)
        val olpFt = new Feature(sameFtIps(0).mz, olpFtCharge, sameFtIps)
        // Update feature properties using apex ones
        olpFt.mz = olpFt.peakels(0).getApex.getMz
        // Check if the current overlapping feature has a longer elution duration
        val nbIPs = olpFt.ms1Count
        if (nbIPs >= sameChargeHighestNbIPs) {
          sameChargeHighestNbIPs = nbIPs
          sameChargeLongestOlpFt = olpFt
        }
      }
      val nbOlpIPs = sameChargeLongestOlpFt.ms1Count
      if (nbOlpIPs >= minNbIPs) {
        // Search for the overlapping peakel having the highest correlation with the first feature peakel
        /*ArrayList<Peakel> overlappingPeakels = (ArrayList<Peakel>) sameChargeLongestOlpFt.getPeakels();
        int bestOlpPeakelIndex;
        for( Peakel overlappingPeakel: overlappingPeakels ) {          
          double overlappingPMCC = FeatureScorer.computePeakelCorrelation(overlappingPeakel, ftFirstPeakel);          
        }*/
        olpFeatures += sameChargeLongestOlpFt
        // Compute the correlation coefficient of this overlapping feature      
        val olpFtLastPeakel = sameChargeLongestOlpFt.peakels(sameChargeLongestOlpFt.peakelsCount - 1)
        sameChargeLongestOlpFt.overlapPMCC = olpFtLastPeakel.computeCorrelationWith(ftFirstPeakel).toFloat
        //overlapClonedPeakels.add( ftFirstPeakel );
        //double meanPMCC = FeatureScorer.computeMeanPeakelCorrelation(overlapClonedPeakels);
        // Check if the current overlapping feature has a higher correlation
        if (sameChargeLongestOlpFt.overlapPMCC >= highestPMCC) {
          highestPMCC = sameChargeLongestOlpFt.overlapPMCC
          bestOlpFt = sameChargeLongestOlpFt
        }
      }
    }
    if ( !olpFeatures.isEmpty) {
      ft.overlappingFeatures = olpFeatures.toArray
      ft.bestOverlappingFeature = bestOlpFt
      if (bestOlpFt != null) {
        val bestOlpFtInt = bestOlpFt.peakels(bestOlpFt.peakelsCount - 1).intensity
        ft.overlapRelativeFactor = bestOlpFtInt / ft.peakels(0).intensity
      }
    }
  }
}


  /*
    this.logger.debug("length ovl ft:" + ovlFts.length)
 
    //detect local maxima in each peakel of considered feature 
    val getLocalMaxima = (peakel : Peakel) => {
      val(nl,nr,order) = (5,5,4)
      val coeffs = SGFilter.computeSGCoefficients(nl,nr,order)
      val sgFilter = new SGFilter(5,5)
      val data = sgFilter.smooth(peakel.getDefinedPeaks map(_ getIntensity() toDouble), coeffs)
      //println("length1: " + peakel.getDefinedPeaks.length + ", length2: "+ data.length)
      var maxs = new ArrayBuffer[Int]
      if ( data.length >= 2 && data.head > data(1) ) maxs += 0
      if (data.length >= 2 && data.last > data(data.length - 2)) maxs += data.length - 1
      var i = 2
      while (i < data.length - 3) {
        if (data(i-1) < data(i) && data(i+1) < data(i))
          maxs += i
        i+=1
      }
      maxs
      /*val a = new WaveletBasedPeakelFinder(peakel.getDefinedPeaks, scales = (5f to peakel.duration.toInt by 1).toArray)
      var res = new ArrayBuffer[Int]
      res ++ a.findCwtPeakels(SmoothingMethod.None).map(_.apex).toArray
      res*/
    }

    //nbMax
    //clusterize peakels inside one feature by their nb local maxima
    
    def clusterizePeakelsFeatures(f: Feature) : HashMap[Int, Pair[Int, Peakel]] = {
       val m = new HashMap[Int, Pair[Int, Peakel]]

       //for (i  <- 0 until f.peakelsCount) {
       f.peakels(0).definedPeaks.foreach(x=> println(x.getLcContext().getElutionTime() + "\t" + x.getIntensity() ))
       println
       val localMax = getLocalMaxima(f.peakels(0))
       localMax.foreach(println(_))
       val apexIdx = localMax.indexOf(f.peakels(0).apexIndex)
       if (apexIdx != -1)
         localMax.remove(apexIdx)
       f.peakels(0).setLocalMaxima( localMax.toArray)
       m(localMax.length + 1) = Tuple2(0, f.peakels(0))
     //}
       m
    }
    
    if (ovlFts.isEmpty)
      return
      
    val refPeakByLocalMax = clusterizePeakelsFeatures(ft)
    refPeakByLocalMax.foreach{case (a, b)=> println(a)}
    //DataStructure holding
    val peakelsByLocalMax = new HashMap[Feature, HashMap[Int, Pair[Int, Peakel]]]()
    ovlFts.foreach(ft => peakelsByLocalMax(ft) = clusterizePeakelsFeatures(ft))*/