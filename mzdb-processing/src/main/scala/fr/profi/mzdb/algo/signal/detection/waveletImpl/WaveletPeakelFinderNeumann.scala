package fr.profi.mzdb.algo.signal.detection.waveletImpl

import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.utils.math.wavelet.Ridge
import scala.collection.mutable.HashMap
import fr.profi.mzdb.algo.signal.detection.CwtPeakel
import fr.profi.mzdb.algo.signal.detection.WaveletPeakelFinder
import fr.profi.mzdb.algo.signal.detection.CwtParameters
import fr.profi.mzdb.algo.signal.detection.RidgeFilteringParameters
import org.apache.commons.math.stat.descriptive.moment.StandardDeviation
import fr.profi.mzdb.utils.math.wavelet.Ridger
import fr.profi.mzdb.utils.math.wavelet.MexicanHat
import fr.profi.mzdb.algo.feature.extraction.FeatureExtractionUtils

class WaveletPeakelFinderNeumann(val peaks: Seq[Peak]) extends WaveletPeakelFinder(peaks) {
  
  def setCwtParametersParams() = {
    val scanTimeDiff = peaks.map(_.getLcContext().getElutionTime())
                             .sliding(2).withFilter(_.length == 2)
                             .map{ x=>x(1) - x(0)}
                             .toArray
    val scanTimeDiffSum = scanTimeDiff.sum
    
    if (scanTimeDiffSum == 0)
      throw new IllegalArgumentException("Error trying to compute scales")
    
    val scanTimeDiffMean = scanTimeDiffSum / scanTimeDiff.length;
   
    //NOTE: also the default in centwave paper
    this.minScale = math.round( ( 20 / scanTimeDiffMean ) / 2f ) 
    this.maxScale=  math.round( ( 110 / scanTimeDiffMean ) / 2f ) 
    
    /*baseline and noise estimation, based on CentWave*/
    val toBeTrimmed = math.round(0.1 * ydata.length) toInt
    val trimmedYdata = ydata.sorted.slice(toBeTrimmed, ydata.length - toBeTrimmed).map(_.toDouble)
    
    this.baseline = trimmedYdata.sum / trimmedYdata.length
    this.noise = new StandardDeviation().evaluate(trimmedYdata, 0, trimmedYdata.length)
      
    this.cwtParams = CwtParameters(scales = ( minScale to maxScale by 2f ) toArray, wavelet =  MexicanHat() )
  }
  
  
  /** */
  def setRidgesFilteringParams() {
    this.ridgeFilteringParams = RidgeFilteringParameters(minRidgeLength = 0,  
                                                         minSNR =  1.5f, 
                                                         minPeakWidth = 15,  
                                                         maxPeakWidth = 150, 
                                                         sizeNoise = ydata.length / 10,  
                                                         skipBoundaries = ydata.length / 10)
  }
  
  /*  
  Definition of the signal to noise ratio Before defining the SNR in
  the wavelet space, signal and noise must be defined first. Based on the
  assumption that the real MS peaks have an instrument-specific characteristic
  shape, the signal strength of a peak is defined as the maximum CWT coefficient
  on the ridge line within a certain scale range. As for the noise, we
  assume that it is composed of positive or negative peaks with very narrow
  width. Since the baseline drift has been removed by the transformation into
  the wavelet space, the CTW coefficients at the smallest scale are a
  good estimate of the noise level. The local noise level of a peak is defined as
  the 95-percentage quantile of the absolute CWT coefficient values
  within a local window surrounding the peak. A minimum noise level can be
  provided to avoid the noise level close to zero, which could happen when
  some region is very smooth. Thus, the SNR is defined as the ratio of the
  estimated peak signal strength and the local noise level of the peak. 
  */
  def computeSNR(r: Ridge) { 
    val sizeNoise = this.ridgeFilteringParams.sizeNoise
    val (firstScale, maxIdxAtFirstScale, maxValueAtFirstScale) = r.firstScaleMaxCoeffPos
    val centroidValue = math.abs(r.maxCoeffPos._3)
    val (mn, mx) = (math.max(maxIdxAtFirstScale - sizeNoise, 0), math.min(maxIdxAtFirstScale + sizeNoise, ydata.length - 1))
    val snrCoeffs = coeffs(this.minScale).map(math.abs(_)).slice(mn, mx).sortBy(x => x)
    val noiseValue = if (! snrCoeffs.isEmpty) snrCoeffs((0.95 * snrCoeffs.length) toInt) else 0
    val estimatedSNR = centroidValue / noiseValue
    r.SNR = estimatedSNR.toFloat
  }
  
  /*
  The identification of
  local maxima is similar to the method used in the PROcess R package in
  Bioconductor (www.bioconductor.org) (Gentleman, 2005). A sliding window
  is used, whose size is proportional to the wavelet support region at the
  scale.
  */
  def findMaxima(minWinSize: Int = 5) : HashMap[Float, Array[Int]] = {
    val maximaIndexesByScale = new HashMap[Float, Array[Int]]
    
    this.coeffs.foreach{ case(scale, coeff)  =>
      
      val winsize = math.max(minWinSize, scale * 2 + 1).toInt
      val localMax = new ArrayBuffer[Int]
      
      //find maxima in each window of length winSize
      //for (i <- 0 until coeff.length by winsize) {
      for (data <- coeff.sliding(winsize, winsize)) {
        val maxi = data.max  // coeff.slice(i, math.min(i + winsize, coeff.length)).max
        localMax += coeff.indexOf(maxi)
      }
      //shift by winSize /2
      for (i <- winsize / 2 until coeff.length by winsize) {
        val maxi = coeff.slice(i, math.min(i + winsize, coeff.length)).max
        localMax += coeff.indexOf(maxi)
      }
      
      //take the strongest maxima when 2 maxima indexes are close enough < to winSize
      for (i <- 0 until localMax.length - 1) {
        val (idx1, idx2) = (localMax(i), localMax(i + 1))
        if (math.abs(idx1 - idx2) <= winsize) {
          if (coeff(idx1) < coeff(idx2)) {
            localMax(i) = 0
          } else {
            localMax(i + 1) = 0
          }
        }
      }      
      maximaIndexesByScale(scale) = localMax.filter(_ != 0).toArray      
    }
    maximaIndexesByScale
  }
  
  /*
  Identify the peaks based on the ridge lines Three rules are defined
  to identify the major peaks:
  (1) The scale corresponding to the maximum amplitude on the ridge line,
  which is proportional to the width of the peak, should be within
  a certain range;
  (2) The SNR should be larger than a certain threshold;
  (3) The length of ridge lines should be larger than a certain threshold;
  */
  def ridgesToPeaks(ridges: Array[Ridge]): Array[CwtPeakel] = {
    // Group ridges by the max at first scale
    val ridgesByMaxIndexAtMaxScale = ridges.groupBy(ridge=>(ridge.maxCoeffPos._1, ridge.maxCoeffPos._2))
    // Keep only one ridge for maxCoeff idx at a max scale
    val validRidges = ridgesByMaxIndexAtMaxScale.map{case ((scale, idx), ridges) => ridges.maxBy( _.length)}.toArray
    
    // Unpack parameters
    val (minRidgeLength, sizeNoise, minSNR) = (ridgeFilteringParams.minRidgeLength, 
                                               ridgeFilteringParams.sizeNoise, ridgeFilteringParams.minSNR)
    // Compute SNR for each ridge
    validRidges.foreach(this.computeSNR(_))

    // Filter against maxScale position, SNR and length
    val filteredRidges = validRidges.filter(r => r.maxCoeffPos._2 > minRidgeLength && r.SNR > minSNR && 
                                                         r.length() > minRidgeLength ) 
    // PeakWidth estimation
    val peakels = filteredRidges.map{ ridge =>
     
      val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos
      // go back into real data, centroid calculation
      val approxPeakWidthHalf = maxScale / 2.0f
      var (minIdx, maxIdx) = (math.max(maxIdxAtMaxScale - approxPeakWidthHalf, 0).toInt, 
          math.min(maxIdxAtMaxScale + approxPeakWidthHalf, peaks.length - 1 ).toInt)
          
      val slicedPeaks = peaks.slice(minIdx, math.min(maxIdx + 1, peaks.length))
      val intensities = slicedPeaks.map(_.getIntensity)
      val xvalues = slicedPeaks.map(_.getLcContext.getElutionTime)
      val centroid =xvalues.zip(intensities).map { case (x, y) => x * y }.reduceLeft(_ + _) / intensities.reduceLeft(_ + _)
      val intensityMax = intensities.max 
      val xmax =  xvalues(intensities.indexOf(intensityMax)) 
      
      new CwtPeakel(index = intensities.indexOf(intensityMax),
                    peaks = slicedPeaks toArray, 
                    apexLcContext = this.peaks(intensities.indexOf(intensityMax)).getLcContext,
                    minIdx = minIdx, 
                    startLcContext = peaks(minIdx).getLcContext,  
                    maxIdx = maxIdx, 
                    endLcContext = peaks(maxIdx).getLcContext,
                    xMax = xmax toFloat,
                    intensityMax = intensityMax toFloat,
                    centroid = centroid toFloat,
                    snr = ridge.SNR)                
    }
    // remove a detected peakel which is contained in another peakel and merge overlapping peakels
    val filteredPeakels = this._filterOverlappingPeakels(peakels);
    //val mergedPeakels = this._mergeOverlappingPeakels(peakels)
    // Filter the two small peakel
    filteredPeakels.filter{ x => val diff = math.abs(peaks(x.maxIdx).getLcContext().getElutionTime() - peaks(x.minIdx).getLcContext().getElutionTime())
                               diff > this.ridgeFilteringParams.minPeakWidth &&
                               diff < this.ridgeFilteringParams.maxPeakWidth}
  }
  
  // main function
  def findCwtPeakels() : Array[CwtPeakel] = { 
    if (! cwtParams.wavelet.isInstanceOf[MexicanHat]) {
      logger.info("Neumann method was selectionned, setting wavelet to MexicanHat (it was a GaussianFirstDerivative or Ridger)")
      cwtParams.wavelet =  MexicanHat()
    }
    this.computeCoeffs
    val (ridges, orphanRidges) = this.findRidges(this._findMaximaNaive(5), winLength = 5, maxGap = 3)
    
    this.ridgesToPeaks(ridges)
    
  }
}