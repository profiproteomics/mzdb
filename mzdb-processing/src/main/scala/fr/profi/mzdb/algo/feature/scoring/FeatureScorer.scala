package fr.profi.mzdb.algo.feature.scoring

import collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.utils.math.VectorSimilarity
import fr.profi.mzdb.model.Feature
import scala.reflect.BeanProperty
import util.control.Breaks._
import fr.profi.mzdb.utils.ms.IsotopicPatternLookup
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.algo.signal.fitting.GaussFitter
import org.apache.commons.math.stat.descriptive.moment.StandardDeviation
import fr.profi.mzdb.algo.signal.fitting.PolyFitter
import fr.profi.mzdb.algo.signal.fitting.GaussLorentzFitter
import org.apache.commons.math.optimization.OptimizationException
import org.apache.commons.math.stat.StatUtils
import fr.profi.mzdb.utils.math.StatisticsConversion

object FeatureScorer {
  
  // TODO: ms1_count
  // Compute the distribution of log(ms1_count)
  // Then compute the bounds using the box plot method
  // First results min = 5 ; max = 110
	
  /*****************************************************************
   *  Estimation of the correlation factor
   ****************************************************************/
  /**mean correlation */
  def calcMeanPeakelCorrelation( peakels: Seq[Peakel] ): Float = {
    val peakelsCount = peakels.length 
    
    if( peakelsCount < 2 ) 
      return 0f
    
    var peakelCorrSum = 0d
    peakels.sliding(2).foreach { case Seq(p1, p2) => 
      peakelCorrSum += calcPeakelCorrelation(p1, p2)
    }
    peakelCorrSum / (peakelsCount-1) toFloat
  }
  
  /**correlation between to peakels*/
  def calcPeakelCorrelation( firstPeakel: Peakel, secondPeakel: Peakel ): Double = {
    val secondPeakelPeakByTime = secondPeakel.getPeakByElutionTime
    
    val firstPeakelIntensities = new ArrayBuffer[Double]()
    val secondPeakelIntensities = new ArrayBuffer[Double]()
    
    // Compute the peak intersection of the two peakels
    var peakIndex = 0
    for( peakelPeak <- firstPeakel.definedPeaks ) {
      
      val secondPeakelPeak = secondPeakelPeakByTime.get( peakelPeak.getLcContext.getElutionTime )
      
      if( secondPeakelPeak != None ) {
        firstPeakelIntensities += peakelPeak.getIntensity().toDouble
        secondPeakelIntensities += secondPeakelPeak.get.getIntensity().toDouble
      }
      peakIndex += 1
    }
    return VectorSimilarity.pearsonCorrelation( firstPeakelIntensities.toArray, secondPeakelIntensities.toArray );
  }
  
  
  /*****************************************************************
   *  Estimation of the overlapping factor
   *  For the moment, an overlapping factor is computed based 
   *  only the first peakel of the considered feature 
   ****************************************************************/
  def calcOverlappingFactor(f: Feature,  mzTolInPpm: Double) : Float = {
    if (f.overlappingFeatures == null) 
      return 0f
      
    var of = 0d
    val mz = f.getMz
    val mzTolInDa =  mz * mzTolInPpm / 1e6;
    val overlappingMap = new ArrayBuffer[ Tuple3[Peakel, Option[Peakel], Option[Peakel]] ]()
    val peakel = f.peakels(0) //monoistopic peakel
    val refScanID = peakel.getApexScanContext.getScanId()
    //get the best overlapping peakel in overlapping feature set
    var (leftOverlappingPeaks, rightOverlappingPeaks) = (Option.empty[Peakel], Option.empty[Peakel])
    
    f.overlappingFeatures.foreach{ ofeature => ofeature.getPeakels.foreach{ p => 
                                  if ( (p.getMz - mz) < mzTolInDa) {
                                    if (p.getApexScanContext.getScanId() < refScanID) {
                                      leftOverlappingPeaks = Some(p)
                                    }
                                    if (p.getApexScanContext.getScanId() > refScanID) {
                                      rightOverlappingPeaks = Some(p)
                                    }
                                  }
                             }
    overlappingMap +=  Tuple3(peakel, leftOverlappingPeaks, rightOverlappingPeaks)
    }
    calcMeanOverlappingFactor(overlappingMap) toFloat
  }
  
  
  def calcMeanOverlappingFactor( peakels: Seq[Tuple3[Peakel, Option[Peakel], Option[Peakel]]] ) : Float = {
    if (peakels.isEmpty)
      return 0f
    var m = 0f
    peakels.foreach(p => m += calcOverlappingFactor(p._1, p._2, p._3))
    m / peakels.length
  }
  
  
  /**
   * overlap factor weighted with the intensity of the overlapping peakel
   * (the overlapping factor for one peakel should be bigger when the overlapping
   * peakel is a lot more intense.)
   *  
   */
  def calcOverlappingFactor ( peakel : Peakel, 
                              leftOverlappingPeakel: Option[Peakel], 
                              rightOverlappingPeakel: Option[Peakel], baseline : Float = 0): Float = {
    
    /** almost the 2 same function ugly */
    var of = 0f // in case of no overlap
    
    val calcLeftOverlappingFactor = (p : Peakel, lop : Peakel) => {
      val (overlappingIntens, refIntens) = ( p.definedPeaks.head.getIntensity, p.definedPeaks.last.getIntensity)
       of += (overlappingIntens / refIntens) * (lop.getApex.getIntensity() / p.getApex.getIntensity())
       //of += 
    }
    
    val calcRightOverlappingFactor = (p : Peakel, rop : Peakel) => {
     val (overlappingIntens, refIntens) = ( p.definedPeaks.last.getIntensity, p.definedPeaks.head.getIntensity )
       of += (overlappingIntens / refIntens) * (rop.getApex.getIntensity() / p.getApex.getIntensity())
       //of += rop.getApex.getIntensity() / p.getApex.getIntensity()
    }
    
    val (mz, intens) = (peakel.getApex.getMz(), peakel.getApex.getIntensity())
    //the intersection point must at the end of the first peakel and at the beginning of the second peakel
    //this point is supposed to be shared by the two overlapping peakels.
    val lastIntens = peakel.definedPeaks.last.getIntensity()
    val firstIntens = peakel.definedPeaks.head.getIntensity()
    
    if (leftOverlappingPeakel == None && rightOverlappingPeakel == None) {
      //do nothing
    }else if (leftOverlappingPeakel != None && rightOverlappingPeakel == None) {
      calcLeftOverlappingFactor(peakel, leftOverlappingPeakel.get)
    } else if (leftOverlappingPeakel == None && rightOverlappingPeakel != None){
      calcRightOverlappingFactor(peakel, rightOverlappingPeakel.get)
      //several overlapping peaks detected
    } else {
      calcLeftOverlappingFactor(peakel, leftOverlappingPeakel.get)
      calcRightOverlappingFactor(peakel, rightOverlappingPeakel.get)
    }
    of
  }
  
  /*****************************************************************
   *  Estimation of the quality of the Isotopic Pattern
   *  rmsd of peakel's area observed vs peakel's area calculated
   ****************************************************************/
  def calcIsotopicDistance(f : Feature): Float = {
    if (f.peakelsCount < 2)
      return Float.NaN
      
    val mz = f.getMz
    
    // Retrieve theoretical and observed abundances
    val theoPattern = IsotopicPatternLookup.getTheoreticalPattern(mz,f.getCharge)
    val theoAbundances = theoPattern.relativeAbundances.map(_ / 100 toDouble)
    val obsAbundances = f.getPeakels.map(_.area.toDouble)
    
    // Normalize observed abundances
    val maxIntens = obsAbundances.max
    val normAbundances = obsAbundances.map( x=> x / maxIntens)
   
    val (shortest, longest) = if (theoAbundances.length < normAbundances.length) (theoAbundances, normAbundances)
    else (normAbundances, theoAbundances)
    
    VectorSimilarity.rmsd( shortest, longest.take(shortest.length) ) toFloat
  }
  
  
  
  /*****************************************************************
   *  Perform a local max detection on each peakels
   ****************************************************************/
  
  /** using the basic peakel finder.*/
  def calcSignalFluctuationByBasicPeakelFinder ( f: Feature): Float =  {
  // a perfect shape would be only one maximum index
    var shape = 0f
    f.getPeakels.foreach(p=> shape += BasicPeakelFinder.findPeakelsIndexes(p.definedPeaks).length)
    shape / f.peakelsCount
  }
  
  /**using the wavelet peakel finder*/
  def calcSignalFluctuationByWaveletBasedPeakelFinder ( f:Feature ): Float =  {
    var shape = 0f
    f.getPeakels.foreach(p=> shape +=  new WaveletBasedPeakelFinder(p.definedPeaks).findCwtPeakels().length)
    shape / f.peakelsCount
  }
  
  /*****************************************************************
   *  Estimation of the shape
   *  weighting of the rmsd value (fit vs observed) by the peakel area
   *  return the weighted mean 
   ****************************************************************/
  
  /**using gaussLorentz fitting*/
  def calcShapeByGaussLorentzFitting (f: Feature) : Float =  {
     val rmsds = f.peakels.map{ p =>
         val xObs = p.definedPeaks.map(_.getMz)
         val yObs = p.definedPeaks.map(_.getIntensity.toDouble)
         val gaussFitter = new GaussLorentzFitter(p.definedPeaks.map(_.getMz), yObs)
         gaussFitter.optimize()
         val refPeak = gaussFitter.peaks(0)
         val yModelized = refPeak.getFittedY(xObs)
         VectorSimilarity.rmsd(yObs, yModelized) * p.getArea
         }
       rmsds.sum / f.getPeakels.map(_.getArea).sum toFloat
  }
  
  /**using gaussian fitting*/
   def calcShapeByGaussFitting (f: Feature) : Float =  {
     val rmsds = f.getPeakels.map{ p =>
       val xObs = p.definedPeaks.map(_.getMz)
       val yObs = p.definedPeaks.map(_.getIntensity.toDouble)
       if (yObs.length >= 3) {//minimum points to perform a Non Linear Square Fitting (Apache Commons Math)
         val gaussFitter = new GaussFitter(p.definedPeaks.map(_.getMz), yObs)
         try {
           gaussFitter.optimize()
           val refPeak = gaussFitter.peaks.head //only one modelized peak !
           val yCalc = refPeak.getFittedY(xObs) //throw zero exception Apache commons math
           VectorSimilarity.rmsd(yObs, yCalc) * p.getArea
         }catch {
           case optimzExcept : OptimizationException => Float.NaN
           case _  => Float.NaN
         }
       }else {
         Float.NaN
       }
     }
     rmsds.filter(_ != Float.NaN).sum / math.pow(f.getPeakels.map(_.getArea).sum, 2) toFloat

   }
    
   /**using parabola fitting*/
   def calcShapeByParabolaFitting (f: Feature) : Float =  {
    val rmsds = f.peakels.map{ p =>
       val xObs = p.definedPeaks.map(_.getMz)
       val yObs = p.definedPeaks.map(_.getIntensity.toDouble)
       val gaussFitter = new PolyFitter(p.definedPeaks.map(_.getMz), yObs)
       gaussFitter.optimize()
       val refPeak = gaussFitter.peaks(0)
       val yModelized = refPeak.getFittedY(xObs)
       VectorSimilarity.rmsd(yObs, yModelized) * p.getArea
       }
     rmsds.sum / f.getPeakels.map(_.getArea).sum toFloat
  }
  
  /*****************************************************************
   *  Evaluation of the peakel width 
   ****************************************************************/
  
   /**mean of the medians of each peakel width*/
  def calcMeanOfMedianPeakelsWidth(f:Feature): Float = {
    var peakelsWidth = 0f
    for (peakel <- f.getPeakels) {
       val peaks = peakel.getDefinedPeaks.sortBy(x => x.getLeftHwhm() + x.getRightHwhm())
       val p = peaks((0.5 * peaks.length).toInt)
       peakelsWidth += p.getLeftHwhm() + p.getRightHwhm()
    }
    peakelsWidth / f.getPeakelsCount
  }
  
  /**compute median of all peakels width*/
  def calcMedianPeakelsWidth(f:Feature): Float = {
    var peakelsWidthMedian = new ArrayBuffer[Float]
    for (peakel <- f.getPeakels) {
       peakelsWidthMedian ++ peakel.getDefinedPeaks.map(x => x.getLeftHwhm() + x.getRightHwhm())
    }
    peakelsWidthMedian = peakelsWidthMedian.sorted
    peakelsWidthMedian((0.5f * peakelsWidthMedian.length) toInt)
  }
  
  /**
   * standard deviation on peak width is calculated for each peakel
   * it is weighted by the intensity of the evaluated peakel 
   */
  def calcStdDevPeakelsWidth (f : Feature) :Float = {
    var peakelsWidth = new ArrayBuffer[Double]
    for (peakel <- f.getPeakels) {
      //normalize by the mass  fwhm / m = 1 / reso
       val stdDev = new StandardDeviation().evaluate(peakel.getDefinedPeaks.map(x => (x.getLeftHwhm() + x.getRightHwhm()) toDouble))
       peakelsWidth += stdDev  * peakel.getArea //weight by the area
    }
    if (!peakelsWidth.isEmpty)
      ( peakelsWidth.sum / ( f.getMz * f.getPeakels.map(_.getArea).sum ) ) toFloat //divide by the sum of peakel areas
    else 
      Float.NaN
  }
  
  /**standard deviation on mz for each peakel ponderate by the area*/
  def calcStdDevPeakelsMzPrecision( f:Feature): Float = {
    var peakelsMzPrecision = new ArrayBuffer[Double]
    f.getPeakels.foreach{peakel => 
      val stdDev = new StandardDeviation().evaluate(peakel.getDefinedPeaks.map(_.getMz))
      peakelsMzPrecision += stdDev * peakel.getArea
    }
    if (!peakelsMzPrecision.isEmpty)
      ( peakelsMzPrecision.sum / (  f.getMz * f.getPeakels.map(_.getArea).sum ) ).toFloat
    else 
      Float.NaN
  }
  
  
  /**Peakel amplitude */
  def calcPeakelsAmplitude(f: Feature): Float = {
      if (f.getPeakels.isEmpty)
        return Float.NaN
      f.getPeakels.map{peakel => 
      val definedPeaks = peakel.getDefinedPeaks
      (definedPeaks.maxBy(_.getIntensity).getIntensity / definedPeaks.minBy(_.getIntensity).getIntensity) * peakel.getArea
      }.toList.sum / f.getPeakels.map(_.getArea).sum
    
  }
  
  /***********************************************************
   * Peakel velocity WILL NOT BE USED
   ***********************************************************/
  private def getDistanceBetweenTwoPoints(x1:Float, y1:Float, x2:Float, y2:Float): Float = {
    math.sqrt( math.pow(x2 - x1, 2) + math.pow(y2-y1, 2)) toFloat
  }
  
  private def getDistanceSum(peakel: Peakel): Float = {
    val definedPeaks = peakel.definedPeaks
    var sum = 0f
    definedPeaks.sliding(2).withFilter(_.size == 2).foreach{ p => 
      sum += FeatureScorer.getDistanceBetweenTwoPoints(p(0).getLcContext().getElutionTime(), p(0).getIntensity(), p(1).getLcContext().getElutionTime(), p(1).getIntensity())
    }
    sum
  }
  
  def calcDistanceOverArea(f:Feature): Float = {
    var m = 0f
    f.getPeakels.foreach(p => m += FeatureScorer.getDistanceSum(p) * p.getArea )
    m / math.pow(f.getPeakels.map(_.getIntensity).sum, 2) toFloat
  }
  
}



object SideEstimator extends Enumeration {
    type SideEstimator = Value
    val Q3_ESTIMATION, Q1_ESTIMATION = Value
}
import SideEstimator._
  
case class DistributionParameters(median: Double, q1: Double, q3:Double, iqr: Double, sigma: Double)

trait DistributionNormalizer {
  /**little bit tricky 
   * math.abs is used to transform all vectors values direction
   * in the same direction
   * */
  def normalize(x: Array[Double], distribParams: DistributionParameters) : Array[Double] = {
    x.map( v => math.abs(v - distribParams.median) / distribParams.sigma)
  }
}

trait DistributionParametrizer {
  
  def getParameters(values: Array[Double]): DistributionParameters = {
    val median = StatUtils.percentile(values, 50)
    val q3 = StatUtils.percentile(values, 75)
    val q1 = StatUtils.percentile(values, 25)
    val iqr = q3 - q1
    val sigma = iqr / 1.349
    DistributionParameters(median, q1, q3, iqr, sigma)
  }
  
  def getEstimatedParameters(values: Array[Double], side: SideEstimator): DistributionParameters = {
    val median = StatUtils.percentile(values, 50)
    var (q3, q1) = (0d, 0d)
    side match {
      case Q3_ESTIMATION =>  { 
        q1 = StatUtils.percentile(values, 25)
        q3 = median + (median - q1)
      }
      case Q1_ESTIMATION => {
        q3 = StatUtils.percentile(values, 75)
        q1 = median - (q3- median)
      }
    }
    val iqr = q3 - q1
    val sigma = iqr / 1.349
    DistributionParameters(median, q1, q3, iqr, sigma)
  }
  
}

trait IDataTransformer {
  protected def transformValues(): Array[Double]
}

abstract case class ZScoreEvaluator(featureQualityVectors: Array[FeatureQualityVector], sideEstimation: SideEstimator)  
extends DistributionParametrizer with DistributionNormalizer with IDataTransformer{
  
  var values: Array[Double] = null
  
  def getZScores(): Array[Double] = {
    values = this.transformValues()//featureQualityVectors.map(_.isotopesPattern toDouble) 
    val params = this.getEstimatedParameters(values, sideEstimation)
    val normalizedValues = this.normalize(values, params)
    normalizedValues
  }
  
  def getPonderatedZScores(): Pair[Array[Double], Double] = {
    values = this.transformValues()//featureQualityVectors.map(_.isotopesPattern toDouble) 
    val params = this.getEstimatedParameters(values, SideEstimator.Q1_ESTIMATION) 
    val weightingCoeff = if (sideEstimation == SideEstimator.Q1_ESTIMATION) values.filter(_ < params.q1).length 
                         else values.filter(_ > params.q3).length
    val normalizedValues = this.normalize(values, params)//pass to z-score 
    (normalizedValues.map(_ * weightingCoeff), weightingCoeff)
  }
}

case class IsotopesPatternZScoreScorer( override val featureQualityVectors: Array[FeatureQualityVector])
                                        extends ZScoreEvaluator(featureQualityVectors, SideEstimator.Q1_ESTIMATION) {
    
  def transformValues(): Array[Double] = {
      featureQualityVectors.map( x => if (x.isotopesPattern != Double.NaN) - math.log10(x.isotopesPattern toDouble)
                                      else Double.NaN )
    }

}


case class MzPrecisionZScoreZScorer( override val featureQualityVectors: Array[FeatureQualityVector])
                                     extends ZScoreEvaluator(featureQualityVectors, SideEstimator.Q3_ESTIMATION){
  
  /*uses ppm instead da, put this directly when calculating the score ?*/
  def transformValues(): Array[Double] = {
      featureQualityVectors.map(x => if (x.mzPrecision != Double.NaN) x.mzPrecision * 1e6
                                     else Double.NaN)
  }

}


case class PeakWidthZScoreZScorer( override val featureQualityVectors: Array[FeatureQualityVector])
                                   extends ZScoreEvaluator(featureQualityVectors, SideEstimator.Q3_ESTIMATION){
   def transformValues(): Array[Double] = {
      featureQualityVectors.map(x=> if (x.peakelsWidth != Double.NaN) x.peakelsWidth * 1e6
                                   else Double.NaN)
  }
}


case class FinalScoreComputer ( featureQualityVectors: Array[FeatureQualityVector]) {
  
  def compute(): Array[Double] = {
    val zscorers = Array[ZScoreEvaluator](IsotopesPatternZScoreScorer(featureQualityVectors),
                                           MzPrecisionZScoreZScorer(featureQualityVectors),
                                           PeakWidthZScoreZScorer(featureQualityVectors))
    
    val results = new ArrayBuffer[Pair[Array[Double], Double]]
    zscorers.foreach(results += _.getPonderatedZScores)
    //val sumCoeffs = results(0)._2 + results(1)._2 + results(2)._2
    val finalScores = results(0)._1.zip(results(1)._1).zip(results(2)._1).map { case y: Tuple2[Tuple2[Double, Double], Double] =>
      val x = Array[Double](y._1._1, y._1._2, y._2)
      var weightedMeanZScore = 0d
      var filtered = x.filter(_ != Double.NaN)
      var sumCoeffs = 0d
      var i = 0
      x.foreach{ x => if (x != Double.NaN) sumCoeffs += results(i)._2; i+=1 }
      weightedMeanZScore  = filtered.sum / sumCoeffs
      /*---transform en p-value*/
      val p_value = StatisticsConversion.zscoreToPvalue(weightedMeanZScore)
      println("p_value:" + p_value)
      val mascotStyleScore = -10 * math.log10(p_value)
      mascotStyleScore
    }
    finalScores toArray
  }
  
  
}






