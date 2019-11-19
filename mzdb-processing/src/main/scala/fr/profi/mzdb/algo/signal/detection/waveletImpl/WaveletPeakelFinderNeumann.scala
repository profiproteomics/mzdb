package fr.profi.mzdb.algo.signal.detection.waveletImpl

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import org.apache.commons.math3.stat.descriptive.moment.StandardDeviation

import fr.profi.mzdb.algo.signal.detection._
import fr.profi.mzdb.model.Peak

import fr.profi.mzdb.util.math.wavelet.MexicanHat
import fr.profi.mzdb.util.math.wavelet.Ridge

class WaveletPeakelFinderNeumann(val peaks: Seq[Peak]) extends AbstractWaveletPeakelFinder(peaks) {
  
  def setCwtParametersParams() = {
    val spectrumTimeDiff = peaks.map(_.getLcContext().getElutionTime())
                             .sliding(2).withFilter(_.length == 2)
                             .map{ x=>x(1) - x(0)}
                             .toArray
    val spectrumTimeDiffSum = spectrumTimeDiff.sum
    
    if (spectrumTimeDiffSum == 0)
      throw new IllegalArgumentException("Error trying to compute scales")
    
    val spectrumTimeDiffMean = spectrumTimeDiffSum / spectrumTimeDiff.length;
   
    //NOTE: also the default in centwave paper
    this.minScale = math.round( ( 20 / spectrumTimeDiffMean ) / 2f ) 
    this.maxScale=  math.round( ( 110 / spectrumTimeDiffMean ) / 2f ) 
    
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
                                                         minSNR =  0.2f, 
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
  
  def ridgesToPeaks(ridges: Array[Ridge]): Array[CwtPeakel] = {
      this._ridgeToPeaks(ridges)
  }

  protected def _ridgeToPeaks(ridges: Array[Ridge]): Array[CwtPeakel] = {
    val (minRidgeLength, sizeNoise, minSNR) = (ridgeFilteringParams.minRidgeLength, 
                                               ridgeFilteringParams.sizeNoise, 
                                               ridgeFilteringParams.minSNR)
    val peakels = new ArrayBuffer[CwtPeakel]

//    filter against selected criteria
//    1: the ridge do not have to be ended
//    2: it must begin at a scale > 2
//    3: its length must be > minRidgeLength
    var filteredRidges = ridges.filter { x => (!x.isEnded() && x.length >= 0) } //minRidgeLength) }

    /*group ridges by the max at first scale*/
    val ridgesByMaxIndexAtMaxScale = new HashMap[Pair[Float, Int], ArrayBuffer[Ridge]]()
    filteredRidges.foreach { r =>
      val pair = (r.maxCoeffPos._1, r.maxCoeffPos._2) // (maxScale, maxIndexAtMaxScale)
      ridgesByMaxIndexAtMaxScale.getOrElseUpdate(pair, new ArrayBuffer[Ridge]) += r
    }
    filteredRidges = ridgesByMaxIndexAtMaxScale.map { case (i, ridges) => ridges.maxBy(_.length) } toArray

    /*compute SNR for each ridge*/
    val minimaByRidges = new HashMap[Ridge, Option[(Int, Int)]]
    filteredRidges.foreach { ridge =>
      val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos //firstScaleMaxCoeffPos
      val sideIndexesAsOpt = this._findLocalMinima(maxScale, maxIdxAtMaxScale) //
      
      //update hashmap
      minimaByRidges(ridge) = sideIndexesAsOpt

      if (sideIndexesAsOpt.isDefined) {
        val (minIdx, maxIdx) = sideIndexesAsOpt.get
        if (minIdx < maxIdx) {
          val sliced = this.ydata.slice(minIdx, math.min(maxIdx + 1, ydata.length))
          if (!sliced.isEmpty)
            ridge.SNR = (sliced.max - this.baseline) / this.noise toFloat
          else
            ridge.SNR = -1000
        } else {
          ridge.SNR = -1000
        }
      } else {
        ridge.SNR = -1000
      }
    }

    /*filter against SNR criteria*/
    filteredRidges = filteredRidges.filter(ridge => ridge.SNR > minSNR)

    /*filter against maxScale position*/
    //filteredRidges = filteredRidges.filter(_.maxCoeffPos._1 >= 4) //put in parameters ?

    /*peakWidth estimation*/
    filteredRidges.foreach { ridge =>

      val (maxScale, maxIdxAtMaxScale, maxValueAtMaxScale) = ridge.maxCoeffPos

      val sideIndexesAsOpt = minimaByRidges(ridge) //this._findLocalMinima(maxScale, maxIdxAtMaxScale) // WAZRNING BEFORE maxScale 
      //other option seems to be less performant
      //val sideIndexesAsOpt  = this._findPeakBoundariesXCMS(maxScale, maxIdxAtMaxScale)

      if (sideIndexesAsOpt.isDefined) {
        // go back into real data, centroid calculation
        val sidesIndexes = sideIndexesAsOpt.get
        var (minIdx, maxIdx) = (math.max(sidesIndexes._1, 0), math.min(sidesIndexes._2, peaks.length - 1))
        //val p = this._findLocalMinimaRealSpaceFromWaveletBounds(minIdx, maxIdx)
        //minIdx = p._1
        //maxIdx = p._2
        val slicedPeaks = peaks.slice(minIdx, math.min(maxIdx + 1, peaks.length))
        val intensities = slicedPeaks.map(_.getIntensity)
        val xvalues = slicedPeaks.map(_.getLcContext.getElutionTime)
        val centroid = xvalues.zip(intensities).map { case (x, y) => x * y }.reduceLeft(_ + _) / intensities.reduceLeft(_ + _)
        val intensityMax = intensities.max
        val xmax = xvalues(intensities.indexOf(intensityMax))

        peakels += new CwtPeakel(
          peaks = slicedPeaks toArray,
          apexIndex = intensities.indexOf(intensityMax),
          apexLcContext = this.peaks(intensities.indexOf(intensityMax)).getLcContext,
          minIdx = minIdx,
          startLcContext = peaks(minIdx).getLcContext,
          maxIdx = maxIdx,
          endLcContext = peaks(maxIdx).getLcContext,
          xMax = xmax toFloat,
          intensityMax = intensityMax toFloat,
          centroid = centroid toFloat,
          snr = ridge.SNR
        )
      }

    }
    /*filter peakels really too small*/
    //peakels = peakels.filter( x => math.abs(x.maxIdx - x.minIdx) >= 6) // filter the two small peakel

    /*remove a detected peakel which is contained in another peakel*/
    val filteredPeakels = this._filterOverlappingPeakels(peakels);
    val mergedPeakels = this._mergeOverlappingPeakels(filteredPeakels)

    //peakels.toArray
    mergedPeakels
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