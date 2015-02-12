package fr.profi.mzdb.algo.signal.detection

import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.IPeakelData
import fr.profi.mzdb.model.Peak
import fr.profi.util.math.getMedianObject
import fr.profi.util.stat._

/**
 * @author David Bouyssie
 *
 */
case class ExtendedBin( bin: Bin, sum: Double )

object HistogramBasedPeakelFinder extends IPeakelFinder {
  
  var sameSlopeCountThreshold = 2
  var expectedBinDataPointsCount = 5
  var debug = false
  
  def findPeakelsIndices(peaks: Seq[Peak] ): Array[Tuple2[Int,Int]] = {
    findPeakelsIndices( peaks.map( p => (p.getLcContext.getElutionTime , p.getIntensity.toDouble) ).toArray )
  }
  
  def findPeakelsIndices(peakel: IPeakelData): Array[Tuple2[Int,Int]] = {
    findPeakelsIndices( peakel.getElutionTimeIntensityPairs.toArray.map( p => (p._1 , p._2.toDouble) ).toArray )
  }
  
  // Note 1: consNbTimesThresh must equal 1 here because of the smoothing procedure (otherwise small peaks may be not detected)
  // Note 2: the binSize may be set to 2.5 seconds for an average cycle time of 0.5 s
  def findPeakelsIndices(rtIntPairs: Array[(Float,Double)] ): Array[Tuple2[Int,Int]] = {
    
    // Check we have at least 5 peaks before the filtering
    if( rtIntPairs.length < 5 ) return Array()
    
    // Applying a moving median filter while keeping first and last values
    /*
    val filteredRtIntPairs = Array(rtIntPairs.head) ++ rtIntPairs.sliding(3).map { buffer =>
      getMedianObject( buffer, { (a: (Float,Double), b: (Float,Double)) =>
        a._2 > b._2
      } )
    } ++ Array(rtIntPairs.last)
    require( filteredRtIntPairs.length == rtIntPairs.length, "filtered array must have the same length than unfiltered one")
    */
    
    val cycleTime = (rtIntPairs.last._1 - rtIntPairs.head._1) / rtIntPairs.length
    val binSize = cycleTime * expectedBinDataPointsCount
    val bins = _binRtIntPairs(rtIntPairs,binSize)
    val nbBins = bins.length

    // Check we have at least 3 peaks after the binning
    if( nbBins < 3 ) return Array()
    
    // Convert bins into array and add paddings
    val binnedAbValues = bins.map( _.sum )
    
    // Smooth bin values
    val nbSmoothingPoints = math.sqrt(nbBins).toInt
    val smoothedValues = this.smoothValues(
      binnedAbValues,
      smoothingConfig = SmoothingConfig(
        nbPoints = 3, //if( nbSmoothingPoints < 3 ) 3 else nbSmoothingPoints,
        times = 1
      )
    )
    
    /*if( debug ) {
      println( bins.map(_.bin.lowerBound / 60 ).mkString("\t"))
      println( binnedAbValues.mkString("\t") )
      println( smoothedValues.mkString("\t") )
    }*/
    
    // Make left to right analysis
    val leftToRightBinIndices = BasicPeakelFinder.findPeakelsIndicesFromSmoothedIntensities(
      smoothedValues,
      sameSlopeCountThresh = HistogramBasedPeakelFinder.sameSlopeCountThreshold
    )
    
    // Make right to left analysis
    val maxBinIdx = nbBins - 1
    val rightToLeftBinIndices = BasicPeakelFinder.findPeakelsIndicesFromSmoothedIntensities(
      // Reverse values
      smoothedValues.reverse,
      sameSlopeCountThresh = HistogramBasedPeakelFinder.sameSlopeCountThreshold
    ) map { rightToLeftBinIdx =>
      // Reverse bin indices
      ( maxBinIdx - rightToLeftBinIdx._2, maxBinIdx - rightToLeftBinIdx._1 )
    }
    
    // Check we have the same number of bins using left and right analyses
    val finalBinIndices = if( leftToRightBinIndices.length != rightToLeftBinIndices.length ) leftToRightBinIndices.toArray
    else {
      // Compute intersections of detected peakel indices using both analyses
      val allBinIndicesSorted = (leftToRightBinIndices ++ rightToLeftBinIndices).sortBy(_._1)
      val binIndicesIntersections = new ArrayBuffer[(Int, Int)]
      
      // Compute intersections of detected peakel indices using both analyses
      for( leftToRightBinIdx <- leftToRightBinIndices; rightToLeftBinIdx <- rightToLeftBinIndices ) {
        if( rightToLeftBinIdx._1 >= leftToRightBinIdx._1 && rightToLeftBinIdx._1 < leftToRightBinIdx._2 ) {
          val firstBinIdx = ( math.max(leftToRightBinIdx._1, rightToLeftBinIdx._1) )
          val lastBinIdx = ( math.min(leftToRightBinIdx._2, rightToLeftBinIdx._2) )
          
          /*if( lastBinIdx <= firstBinIdx) {
            println( leftToRightBinIdx )
            println( rightToLeftBinIdx )
            println( binnedAbValues.mkString("\t") )
            println( smoothedValues.mkString("\t") )
          }*/
          
          require( lastBinIdx > firstBinIdx, "error during computation of peakel indices intersection")
          
          binIndicesIntersections += ( firstBinIdx -> lastBinIdx )
        }
      }
      
      binIndicesIntersections.toArray
    }
    
    // Convert peakel indices of binned values into indices of input values
    val peakelIndices = new ArrayBuffer[Tuple2[Int,Int]]( finalBinIndices.length )
    val rtInPairsWithIndex = rtIntPairs.zipWithIndex
    
    for( binIdx <- finalBinIndices ) {
      val(firstBin,lastBin) = (bins(binIdx._1).bin, bins(binIdx._2).bin)
      val firstIdx = rtInPairsWithIndex.find( _._1._1 >= firstBin.lowerBound ).get._2
      val lastIdx = rtInPairsWithIndex.find( _._1._1 >= lastBin.upperBound ).map(_._2).getOrElse( rtIntPairs.length - 1 )
      
      // TODO: remove lowest peaks from peakels ??? (threshold on relative difference ?)
      
      peakelIndices += (firstIdx -> lastIdx)
    }
    
    peakelIndices.toArray
  }
  
  private def _binRtIntPairs( rtIntPairs: Array[(Float,Double)], binSize: Float ): Array[ExtendedBin] = {
    
    // Instantiate an histogram computer
    val histoComputer = new EntityHistogramComputer(rtIntPairs, { rtIntPair: (Float,Double) => rtIntPair._1.toDouble } )
    
    // Compute the number of bins
    val timeRange = rtIntPairs.last._1 - rtIntPairs.head._1
    val nbBins = (timeRange / binSize).toInt
    
    val rtIntPairsHisto = histoComputer.calcHistogram(nbBins)
    
    val extendedBins = rtIntPairsHisto.map { case (bin, dataPoints) =>
      val intSum = dataPoints.foldLeft(0.0) { (s,dp) => s + dp._2 }
      
      ExtendedBin(bin,intSum)
    }
    
    //extendedBins
    
    // Add some padding to the bins (needed for smoothing operation)
    Array( extendedBins.head.copy( sum = rtIntPairs.head._2 ) ) ++ 
      extendedBins ++ 
    Array( extendedBins.last.copy( sum = rtIntPairs.last._2 ) )
  }

}
