package fr.profi.mzdb.algo

import java.util.Arrays

import org.apache.commons.math3.stat.correlation.PearsonsCorrelation

import fr.profi.mzdb.model.Peakel

object PeakelCorrelationScorer {
  
    def findCorrelatingPeakel(ref: Peakel, peakels: Array[Peakel]): Option[Peakel] = {
    
    val correlations = peakels.map { peakel => 
      _computeCorrelation(ref,peakel) -> peakel
    }
    val (correlation, bestPeakel) = correlations.maxBy(_._1)
    
    // TODO: DBO => I don't like the result of the pearson correlation, 
    // I think we should not apply a filter based on this metric
    if (correlation > 0.6) Some(bestPeakel)
    else None
  }

  private def _computeCorrelation(p1: Peakel, p2: Peakel): Double = {
    var p1Offset = 0
    var p2Offset = 0

    // not clean : some RT values can be missing in elutionTime array when intensity = 0
    if (p1.getFirstElutionTime() < p2.getFirstElutionTime()) {
      // search p2.firstElutionTime index in p1
      val nearestIdx = Arrays.binarySearch(p1.getElutionTimes(), p2.getFirstElutionTime())
      p2Offset = if (nearestIdx < 0) ~nearestIdx else nearestIdx
    } else {
      // search p1.firstElutionTime in p2
      val nearestIdx = Arrays.binarySearch(p2.getElutionTimes(), p1.getFirstElutionTime())
      p1Offset = if (nearestIdx < 0) ~nearestIdx else nearestIdx
    }

    val p1Values = p1.getIntensityValues()
    val p2Values = p2.getIntensityValues()
    val length = Math.max(p1Values.length + p1Offset, p2Values.length + p2Offset)

    val y1 = new Array[Double](length)
    val y2 = new Array[Double](length)
    var k = 0
    while (k < length) {
      if (k >= p1Offset && k < p1Values.length) {
        y1(k) = p1Values(k - p1Offset)
      }
      if (k >= p2Offset && k < p2Values.length) {
        y2(k) = p2Values(k - p2Offset)
      }
      k += 1
    }

    val pearson = new PearsonsCorrelation()
    math.abs(pearson.correlation(y1, y2))
  }
}