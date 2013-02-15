package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.Feature
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

trait IFeatureExtractor {

  val mzDbReader: MzDbReader
  val scanHeaderById: Map[Int,ScanHeader]
  val nfByScanId: Map[Int,Float]
  val mzTolPPM: Float
  val maxNbPeaksInIP: Int
  val minNbOverlappingIPs: Int
  
  protected def normalizeIPs( isotopicPatterns: Seq[IsotopicPattern] ) {
    //isotopicPatterns.foreach( _.normalizeIntensities(nfByScanId) )
    ()
  }
  
  protected def updateFtOverlappingFeatures( ft: Feature, isotopicPatterns: Seq[IsotopicPattern], minNbIPs: Int ) {
    
    // Check that the current features has peak
    if( ft.peakelsCount == 0 ) { return; }

    // Group overlapping isotopic patterns by charge state and number of peaks
    val olpIPsMap = new HashMap[Int, HashMap[Int,ArrayBuffer[IsotopicPattern]]]()

    for( ip <- isotopicPatterns ) {

      if( ip.overlappingIps != null ) {
        for( olpIp <- ip.overlappingIps ) {

          val olpIpZ = olpIp.charge          
          val sameChargeOlpIPsByNbPeaks = olpIPsMap.getOrElseUpdate(olpIpZ, new HashMap[Int,ArrayBuffer[IsotopicPattern]] )

          val olpIpNbPeaks = olpIp.peaks.length
          val sameFtOlpIPs = sameChargeOlpIPsByNbPeaks.getOrElseUpdate(olpIpNbPeaks, new ArrayBuffer[IsotopicPattern] )

          sameFtOlpIPs += olpIp
        }
      }
    }

    // Build a list of overlapping features    
    val ftFirstPeakel = ft.peakels(0)
    val chargeStates = olpIPsMap.keySet.toArray
    val olpFeatures = new ArrayBuffer[Feature]( chargeStates.length )
    
    var bestOlpFt: Feature = null
    var highestPMCC = -1.0; // PMCC range is [-1,+1]

    for( olpFtCharge <- chargeStates ) {

      val sameChargeIPs = olpIPsMap(olpFtCharge)
      val nbPeaksList = sameChargeIPs.keySet.toArray

      var sameChargeHighestNbIPs = 0
      var sameChargeLongestOlpFt: Feature = null

      for( nbPeaks <- nbPeaksList ) {
        val sameFtIps = sameChargeIPs(nbPeaks)
        this.normalizeIPs(sameFtIps)

        val olpFt = new Feature( sameFtIps(0).mz, olpFtCharge, sameFtIps )

        // Update feature properties using apex ones
        olpFt.mz = olpFt.peakels(0).getApex.getMz
        
        // Check if the current overlapping feature has a longer elution duration
        val nbIPs = olpFt.ms1Count
        if( nbIPs >= sameChargeHighestNbIPs ) {
          sameChargeHighestNbIPs = nbIPs
          sameChargeLongestOlpFt = olpFt
        }

      }

      val nbOlpIPs = sameChargeLongestOlpFt.ms1Count

      if( nbOlpIPs >= minNbIPs ) {

        // Search for the overlapping peakel having the highest correlation with the first feature peakel
        /*ArrayList<Peakel> overlappingPeakels = (ArrayList<Peakel>) sameChargeLongestOlpFt.getPeakels();

        int bestOlpPeakelIndex;
        for( Peakel overlappingPeakel: overlappingPeakels ) {          
          double overlappingPMCC = FeatureScorer.computePeakelCorrelation(overlappingPeakel, ftFirstPeakel);          
        }*/

        olpFeatures += sameChargeLongestOlpFt    

        // Compute the correlation coefficient of this overlapping feature      
        val olpFtLastPeakel = sameChargeLongestOlpFt.peakels( sameChargeLongestOlpFt.peakelsCount - 1 )
        sameChargeLongestOlpFt.overlapPMCC = olpFtLastPeakel.computeCorrelationWith( ftFirstPeakel ).toFloat

        //overlapClonedPeakels.add( ftFirstPeakel );
        //double meanPMCC = FeatureScorer.computeMeanPeakelCorrelation(overlapClonedPeakels);

        // Check if the current overlapping feature has a higher correlation
        if( sameChargeLongestOlpFt.overlapPMCC >= highestPMCC ) {
          highestPMCC = sameChargeLongestOlpFt.overlapPMCC
          bestOlpFt = sameChargeLongestOlpFt
        }

      }

    }

    if( olpFeatures.size > 0 ) {
      ft.overlappingFeatures = olpFeatures.toArray
      ft.bestOverlappingFeature = bestOlpFt

      val bestOlpFtInt = bestOlpFt.peakels( bestOlpFt.peakelsCount - 1 ).intensity
      ft.overlapRelativeFactor =  bestOlpFtInt / ft.peakels( 0 ).intensity
    }
    
  }
}