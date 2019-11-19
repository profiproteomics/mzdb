package fr.profi.mzdb.algo.signal.filtering

import fr.profi.util.stat._

/**
 * @author bouyssie
 *
 */
class XicBinner( smoothingConfig: XicBinnerConfig ) {
  
  /*def smoothIntensities(values: Array[Double] ): Array[Double] = {
    throw new Exception("this method won't be implemented and should be removed from ISignalSmoother")
  }
  
  def smoothTimeIntensityPairs(rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)] = {
    val extendedBins = binRtIntPairs(rtIntPairs)
    extendedBins.map( extendedBin => extendedBin.bin.center.toFloat -> extendedBin.sum )
  }*/
  def binRtIntPairs( rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)] = {
    val extendedBins = calcBins(rtIntPairs)
    extendedBins.map( extendedBin => extendedBin.bin.center.toFloat -> extendedBin.sum )
  }
  
  def calcBins( rtIntPairs: Array[(Float,Double)] ): Array[ExtendedBin] = {
    
    val expectedBinDataPointsCount = smoothingConfig.expectedBinDataPointsCount
    val cycleTime = (rtIntPairs.last._1 - rtIntPairs.head._1) / rtIntPairs.length
    val binSize = cycleTime * expectedBinDataPointsCount
    
    // Instantiate an histogram computer
    val histoComputer = new EntityHistogramComputer(rtIntPairs, { rtIntPair: (Float,Double) => rtIntPair._1.toDouble } )
    
    // Compute the number of bins
    val timeRange = rtIntPairs.last._1 - rtIntPairs.head._1
    val nbBins = math.max(1,(timeRange / binSize).toInt)
    
    val rtIntPairsHisto = histoComputer.calcHistogram(nbBins)
    
    val extendedBins = rtIntPairsHisto.map { case (bin, dataPoints) =>
      val intSum = dataPoints.foldLeft(0.0) { (s,dp) => s + dp._2 }
      
      ExtendedBin(bin,intSum)
    }
    
    extendedBins
  }

}

case class ExtendedBin( bin: Bin, sum: Double )

case class XicBinnerConfig(
  expectedBinDataPointsCount: Int = 5
)
