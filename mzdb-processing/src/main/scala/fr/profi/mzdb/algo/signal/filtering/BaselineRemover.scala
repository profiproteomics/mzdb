package fr.profi.mzdb.algo.signal.filtering

import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math3.stat.StatUtils
import fr.profi.mzdb.utils.math.DerivativeAnalysis
import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
class BaselineRemover( gapTolerance: Int = 1, minPeaksCount: Int = 3 ) {
  
  require( gapTolerance >= 1, "gapTolerance must be strictly positive")
  
  def removeBaseLine( peaks: Array[Peak], noiseThresholdOpt: Option[Double] = None ): Array[Peak] = {
    require( peaks != null, "peaks is null")
    
    val rtIntPairs = peaks.map { peak =>
      peak.getLcContext().getElutionTime() -> peak.getIntensity().toDouble
    }
    
    val noiseThreshold = noiseThresholdOpt.getOrElse(calcNoiseThreshold(rtIntPairs))
    
    _removeBaseLine( peaks, rtIntPairs, noiseThreshold )
  }
  
  private def _removeBaseLine( peaks: Array[Peak], rtIntPairs: Array[(Float,Double)], noiseThreshold: Double ): Array[Peak] = {
    val noiseFreePeakGroupsIndices = findNoiseFreePeakGroupsIndices(rtIntPairs, noiseThreshold)
    
    val peaksAboveThreshold = new ArrayBuffer[Peak](peaks.length)
    
    for( noiseFreePeakGroupIndices <- noiseFreePeakGroupsIndices ) {
      val( firstIndex, lastIndex ) = noiseFreePeakGroupIndices
      peaksAboveThreshold ++= peaks.slice( firstIndex, lastIndex +1 )
    }

    peaksAboveThreshold.toArray
  }
  
  def removeBaseLine( rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)] = {
    require( rtIntPairs != null, "rtIntPairs is null")
    
    removeBaseLine( rtIntPairs, calcNoiseThreshold(rtIntPairs) )
  }
  
  def removeBaseLine( rtIntPairs: Array[(Float,Double)], noiseThreshold: Double ): Array[(Float,Double)] = {
    val noiseFreePeakGroupsIndices = findNoiseFreePeakGroupsIndices(rtIntPairs, noiseThreshold)
    
    val rtIntPairsAboveThreshold = new ArrayBuffer[(Float,Double)](rtIntPairs.length)
    
    for( noiseFreePeakGroupIndices <- noiseFreePeakGroupsIndices ) {
      val( firstIndex, lastIndex ) = noiseFreePeakGroupIndices
      rtIntPairsAboveThreshold ++= rtIntPairs.slice( firstIndex, lastIndex +1 )
    }

    rtIntPairsAboveThreshold.toArray
  }
  
  def findNoiseFreePeakGroupsIndices( rtIntPairs: Array[(Float,Double)], noiseThreshold: Double ): Array[(Int,Int)] = {
    require( rtIntPairs != null, "rtIntPairs is null")
    
    // Clusterize peaks being consecutively above the nosie thresholds
    val groupedRtIntPairIndices = new ArrayBuffer[ArrayBuffer[Int]]()
    var rtIntPairGroupIndices = new ArrayBuffer[Int]()
    var underThresholdCount = 0
    
    for( (rtIntPair,index) <- rtIntPairs.zipWithIndex ) {

      val isUnderThreshold = rtIntPair._2 < noiseThreshold
      if( isUnderThreshold ) underThresholdCount += 1
      else {
        if( underThresholdCount > gapTolerance ) {
          groupedRtIntPairIndices += rtIntPairGroupIndices
          rtIntPairGroupIndices = new ArrayBuffer[Int]()
        }
        
        rtIntPairGroupIndices += index
        underThresholdCount = 0
      }
    }
    
    // Add last detected group
    groupedRtIntPairIndices += rtIntPairGroupIndices
    
    // Keep only peak groups with a sufficient number of peaks (at least 3)
    // and extend group indices to maximize peak duration
    val rtIntPairsCount = rtIntPairs.length
    val rtIntPairsIndicesAboveThreshold = new ArrayBuffer[(Int,Int)]
    for(
      rtIntPairIndices <- groupedRtIntPairIndices;
      if rtIntPairIndices.length >= minPeaksCount
    ) {
      val firstIndex = rtIntPairIndices.head
      val lastIndex = rtIntPairIndices.last
      
      val extendedFirstIndex = if( firstIndex == 0 ) firstIndex else firstIndex - 1
      val extendedLastIndex = if( lastIndex == rtIntPairsCount - 1 ) lastIndex else lastIndex + 1
      
      rtIntPairsIndicesAboveThreshold += extendedFirstIndex -> extendedLastIndex
    }
 
    rtIntPairsIndicesAboveThreshold.toArray
  }

  def calcNoiseThreshold( rtIntPairs: Array[(Float,Double)] ): Double = {
    require( rtIntPairs != null, "rtIntPairs is null")
    
    // Compute histogram of observed intensities
    val intensityHistoComputer = new fr.profi.util.stat.EntityHistogramComputer(rtIntPairs, 
      { rtIntPair: (Float,Double) => rtIntPair._2 }
    )
    val intensityHisto = intensityHistoComputer.calcHistogram(20)
    val indexedHisto = intensityHisto.zipWithIndex
    
    // Search for first minimum frequency
    val firstTupleWithMinimumFreqOpt = indexedHisto.sliding(3).find { buffer =>
      val freq1 = buffer(0)._1._2.length
      val freq2 = buffer(1)._1._2.length
      val freq3 = buffer(2)._1._2.length
      if( freq2 < freq1 && freq3 >= freq2 ) true else false
    }
    
    // If no minimum found
    val firstMinimumFreq = if( firstTupleWithMinimumFreqOpt.isEmpty ) {
      return 0.0
      
      // Take the last frequency
      //indexedHisto.last
      
      //println("rtIntPairs : "+ rtIntPairs.map(p => p._1 + "\t" +p._2).mkString( "\n"))
      //println("intensityHisto L: "+ intensityHisto.length)
      //println("intensityHisto: "+ intensityHisto.map(_._2.length).toList)
    } else firstTupleWithMinimumFreqOpt.get(1)
    
    // Search for maximum frequency after first frequency minimum
    val maxFreqAfterFirstMinimum = intensityHisto
      .slice(firstMinimumFreq._2,intensityHisto.length)
      .maxBy(_._2.length)
      ._2.length
    
    // Determine the noise threshold
    intensityHisto
      .find( _._2.length <= maxFreqAfterFirstMinimum ).get
      ._1.lowerBound
    //val sortedIntensities = rtIntPairs.map(_._2).sorted
    /*val sortedFreqs = intensityHisto.map(_._2.length.toDouble)
    val binCount = sortedFreqs.length
    val intensityCvPairs = new ArrayBuffer[(Float,Double)](binCount)
    for( i <- 0 until binCount - 1 ) {
      val intensityHead = intensityHisto(i)._1.center
      val freqInclusiveTail = sortedFreqs.slice(i,binCount)
      val(mean,sd) = _calcMeanAndSd(freqInclusiveTail)
      intensityCvPairs += intensityHead.toFloat -> intensityHisto(i)._2.length.toDouble //(sd / mean)
      //println(intensityCvPairs.last)
    }*/
    
    //intensityCvPairs.toArray
  }
  
  private def _calcMeanAndSd( values: Array[Double] ): (Double,Double) = {   
    val mean = StatUtils.mean(values)
    val variance = StatUtils.variance(values, mean)
   
    (mean, math.sqrt(variance))
  }
  
}