package fr.profi.mzdb.algo.feature.scoring

import fr.profi.mzdb.model.Feature
import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder

/**
 * Metric to evaluate a feature quality
 */
case class FeatureQualityVector (
  ms1Count: Int,
  mzPrecision: Float,
  shape: Float, // RMSD
  signalFluctuation: Float, // derivative switch count over all peakels
  
  isotopesCount: Int, // nb peakels
  isotopesPattern: Float, //distance between theoretical and experimental patterns correlation ?
  isotopesRatios:Float,
  
  peakelsWidth: Float,
  peakelsCorrelation: Float,
  peakelsVelocity: Float,
  peakelsAmplitude: Float,
  
  overlappingPeakelsCorrelation: Float,
  overlappingFactor: Float
)

/**
 * each metric 
 */
case class FeatureQualityAssessment (
  evalThresholds : QualityVectorThresholds,
  
  ms1Count: Boolean,
  mzPrecision:Boolean,
  shape: Boolean,
  signalFluctuation: Boolean, // derivative switch count over all peakels
  
  isotopesCount: Boolean,
  isotopesPattern: Boolean,
  isotopesRatios:Boolean,

  peakelsWidth: Boolean,
  peakelsCorrelation: Boolean,
  peakelsVelocity: Boolean,
  peakelsAmplitude: Boolean,
  
  overlappingPeakelsCorrelation: Boolean,
  overlappingFactor: Boolean
) {
  def isOk(): Boolean = {
    ( ms1Count && shape && isotopesCount && signalFluctuation && peakelsWidth && 
        peakelsCorrelation && isotopesPattern && overlappingPeakelsCorrelation && overlappingFactor &&
        mzPrecision && isotopesRatios && peakelsVelocity && peakelsAmplitude) 
  }
}




case class FeatureEvaluation(
  feature: Feature,
  qualityCriteria: FeatureQualityVector,
  qualityAssessment: FeatureQualityAssessment,
  isOverlapping: Boolean) {
  
  lazy val isHighQuality: Boolean =  qualityAssessment.isOk   
}

class QualityVectorThresholds(min:FeatureQualityVector,
                              max:FeatureQualityVector) extends Tuple2[FeatureQualityVector, FeatureQualityVector](min, max) {
  
  def getMzPrecisionMinMax(): Pair[Float, Float] = (min.mzPrecision, max.mzPrecision)
  def getIsotopesRatios(): Pair[Float, Float] = (min.isotopesRatios, max.isotopesRatios)
  def getPeakelsAmplitude(): Pair[Float, Float] = (min.peakelsAmplitude, max.peakelsAmplitude)
  def getPeakelsVelocity(): Pair[Float, Float] = (min.peakelsVelocity, max.peakelsVelocity)
  def getMs1CountMinMax(): Pair[Int, Int] = (min.ms1Count,max.ms1Count)
  def getIsotopesCountMinMax(): Pair[Int,Int] = (min.isotopesCount,max.isotopesCount)
  def getIstopesPatternMinMax(): Pair[Float,Float] = (min.isotopesPattern,max.isotopesPattern)
  def getShapeMinMax(): Pair[Float,Float] = (min.shape,max.shape)
  def getSignalFluctuationMinMax(): Pair[Float, Float] = (min.signalFluctuation, max.signalFluctuation)
  def getPeakelsWidthMinMax(): Pair[Float,Float] = (min.peakelsWidth,max.peakelsWidth)
  def getPeakelsCorrelationtMinMax(): Pair[Float, Float] = (min.peakelsCorrelation,max.peakelsCorrelation)
  def getOverlappingFactorMinMax(): Pair[Float, Float] = (min.overlappingFactor,max.overlappingFactor)
  def getOverlappingPeakelsCorrelationMinMax(): Pair[Float, Float] = (min.overlappingPeakelsCorrelation,max.overlappingPeakelsCorrelation)
}


case class FeatureEvaluationThresholds(
   var qualityThresholds: QualityVectorThresholds, // min & max thresholds
   val ftScoringConfig: FeatureScoringConfig) {}

trait IFeatureThresholdsComputer {
  def getThresholds( features: Array[Feature]): Pair[FeatureQualityVector, FeatureQualityVector]
}

class FeatureThresholdsContainer( thresholds: Pair[FeatureQualityVector, FeatureQualityVector] ) extends IFeatureThresholdsComputer {
  
  def getThresholds( features: Array[Feature]): Pair[FeatureQualityVector, FeatureQualityVector] = {
    thresholds
  }
  
}

class ProbabilisticFeatureThresholdsComputer() extends IFeatureThresholdsComputer {
  
  def getThresholds( features: Array[Feature]): Pair[FeatureQualityVector, FeatureQualityVector] = {
    null
  }
  
}

/**
 * iqrFactor: 1.5 for alpha = 0.05 (95%) and 3 for alpha = 0.01 (99%)
 */
class BoxPlotFeatureThresholdsComputer( iqrFactor: Float = 1.5f ) extends IFeatureThresholdsComputer {
  
  def getThresholds( features: Array[Feature] ): Pair[FeatureQualityVector, FeatureQualityVector] = {
    
    val (mzPrecisionMin, mzPrecisionMax) = _getMzPrecisionBounds(features);
    val (ms1CountMin, ms1CountMax) = _getMS1CountBounds(features)
    val (isotopesCountMin, isotopesCountMax) = _getIsotopesCountBounds(features)
    val (isotopesPatternMin, isotopesPatternMax) = _getIsotopesPatternBounds(features)
    val (isotopesRatiosMin, isotopesRatiosMax) = _getIsotopesRatiosBounds(features);
    val (shapeMin, shapeMax) = _getShapeBounds(features)
    val (signalFluctuationMin, signalFluctuationMax) = _getSignalFluctuationBounds(features)
    val (peakelsWidthMin, peakelsWidthMax) = _getPeakelsWidthBounds(features)
    val (peakelsCorrelationMin, peakelsCorrelationMax) = _getPeakelsCorrelationBounds(features)
    val (peakelsVelocityMin, peakelsVelocityMax) = _getPeakelsVelocityBounds(features)
    val (peakelsAmplitudeMin, peakelsAmplitudeMax) = _getPeakelsAmplitudeBounds(features)
    val (overlappingFactorMin, overlappingFactorMax) = _getOverlappingFactorBounds(features)
    val (overlappingPeakelsCorrelationMin, overlappingPeakelsCorrelationMax) = _getOverlappingPeakelsCorrelationBounds(features)
    /*
     * ms1Count: Int,
  mzPrecision: Float,
  shape: Float, // RMSD
  signalFluctuation: Float, // derivative switch count over all peakels
  
  isotopesCount: Int, // nb peakels
  isotopesPattern: Float, //distance between theoretical and experimental patterns correlation ?
  isotopesRatios:Float,
  
  peakelsWidth: Float,
  peakelsCorrelation: Float,
  peakelsVelocity: Float,
  peakelsAmplitude: Float,
  
  overlappingPeakelsCorrelation: Float,
  overlappingFactor: Float
     */
    Pair(FeatureQualityVector(ms1CountMin,
                              mzPrecisionMin,
                              shapeMin,
                              signalFluctuationMin,
                              isotopesCountMin, 
                              isotopesPatternMin,
                              isotopesRatiosMin,
                              peakelsWidthMin,
                              peakelsCorrelationMin,
                              peakelsVelocityMin,
                              peakelsAmplitudeMin,
                              overlappingPeakelsCorrelationMin,
                              overlappingFactorMin),
         FeatureQualityVector(ms1CountMax,
                              mzPrecisionMax,
                              shapeMax,
                              signalFluctuationMax,
                              isotopesCountMax, 
                              isotopesPatternMax,
                              isotopesRatiosMax,
                              peakelsWidthMax,
                              peakelsCorrelationMax,
                              peakelsVelocityMax,
                              peakelsAmplitudeMax,
                              overlappingPeakelsCorrelationMax,
                              overlappingFactorMax))
  }
  
  private def _calcFeatureQ1Q3Indexes( values: Seq[Any] ): Pair[Int,Int] = {
    
    val N = values.length
    val iq1 = N * 0.25 toInt
    //val iq2 =  N * 0.5 toInt
    val iq3 = (N * 0.75).toInt
    
    (iq1,iq3)
  }
  
  private def _calcBounds( values: Array[Int] ): Pair[Int,Int] = {    
    val sortedValues = values.sorted
    val q1q3Indexes = _calcFeatureQ1Q3Indexes(sortedValues)
    
    val (iq1v, iq3v) = (sortedValues(q1q3Indexes._1), sortedValues(q1q3Indexes._2) )
    val iqr = iq3v - iq1v
    val fullIqr = iqrFactor * iqr
    
    ((iq1v - fullIqr).toInt ,(iq3v + fullIqr).toInt)
  }
  
  private def _calcBounds( values: Array[Float] ): Pair[Float,Float] = {    
    val sortedValues = values.sorted
    val q1q3Indexes = _calcFeatureQ1Q3Indexes(sortedValues)
    
    val (iq1v, iq3v) = (sortedValues(q1q3Indexes._1), sortedValues(q1q3Indexes._2) )
    val iqr = iq3v - iq1v
    val fullIqr = iqrFactor * iqr
    
    ((iq1v - fullIqr) ,(iq3v + fullIqr))
  }
  
  /*
  private def _calcBounds( values: Array[Double] ): Pair[Double,Double] = {
    val sortedValues = values.sorted
    val q1q3Indexes = _calcFeatureQ1Q3Indexes(sortedValues)
    
    val (iq1v, iq3v) = (sortedValues(q1q3Indexes._1), sortedValues(q1q3Indexes._2) )
    val iqr = iq3v - iq1v
    val fullIqr = iqrFactor * iqr
    
    ((iq1v - fullIqr) ,(iq3v + fullIqr))
  }*/
  
  
  private def _getMS1CountBounds(features : Array[Feature]): Pair[Int, Int] = {
    val values = features.map( f => f.ms1Count )    
    _calcBounds(values)
  }
  
  private def  _getIsotopesCountBounds (features : Array[Feature]) : Pair[Int, Int] ={
    (1,1)
  }
  private def  _getIsotopesPatternBounds (features : Array[Feature]) : Pair[Float, Float] = {
    (0f, 0f)
  }
  private def  _getShapeBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  private def  _getSignalFluctuationBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  private def  _getPeakelsWidthBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  private def _getMzPrecisionBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  
  private def _getIsotopesRatiosBounds(features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  
  private def _getPeakelsAmplitudeBounds(features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  
  private def _getPeakelsVelocityBounds(features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  
  private def  _getPeakelsCorrelationBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  private def  _getOverlappingFactorBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  private def  _getOverlappingPeakelsCorrelationBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  
}


object FeatureQualityEvaluator {
  
  
  def evaluate(f : FeatureQualityVector, qualityThresholds: QualityVectorThresholds) : FeatureQualityAssessment = {
    FeatureQualityAssessment(qualityThresholds,
                              checkParam( f.ms1Count, qualityThresholds.getMs1CountMinMax),
                              checkParam( f.mzPrecision, qualityThresholds.getMzPrecisionMinMax),
                              checkParam( f.shape, qualityThresholds.getShapeMinMax),
                              checkParam( f.signalFluctuation, qualityThresholds.getSignalFluctuationMinMax),
                              checkParam( f.isotopesCount, qualityThresholds.getIsotopesCountMinMax),
                              checkParam( f.isotopesPattern, qualityThresholds.getIstopesPatternMinMax),
                              checkParam ( f.isotopesRatios, qualityThresholds.getIsotopesRatios),
                              checkParam( f.peakelsWidth, qualityThresholds.getPeakelsWidthMinMax),
                              checkParam( f.peakelsCorrelation, qualityThresholds.getPeakelsCorrelationtMinMax),
                              checkParam( f.peakelsVelocity, qualityThresholds.getPeakelsVelocity),
                              checkParam( f.peakelsAmplitude, qualityThresholds.getPeakelsAmplitude),
                              checkParam( f.overlappingPeakelsCorrelation, qualityThresholds.getOverlappingPeakelsCorrelationMinMax),
                              checkParam( f.overlappingFactor, qualityThresholds.getOverlappingFactorMinMax))

  }
  
  def checkParam( param: Float, minMax: Pair[Float,Float] ): Boolean = {
    param <= minMax._2 && param >= minMax._1
  }
  
  def checkParam( param: Int, minMax: Pair[Int,Int] ): Boolean = {
     param <= minMax._2 && param >= minMax._1
  }
  
}

case class FeatureScoringConfig(
  val methods: Map[String, Any] = Map("signalFluctuation" -> "BasicPeakelFinder", "shape" -> "Gauss")//several could be employed for assessing the quality of a feature
)


//Fitting all the peakels ? if we do it for all the peakels it could be long
object FeatureEvaluator  {
  
  def evaluateFeatures(features: Seq[Feature], ftScoringConfig: FeatureScoringConfig, thresholdComputer: IFeatureThresholdsComputer ): Seq[FeatureEvaluation] = {
    null
  }
  
  def computeQualityVector(f: Feature, ftScoringConfig: FeatureScoringConfig = FeatureScoringConfig()) : FeatureQualityVector = {
    
    var (signalFluctuation, shape) = (0f, 0f)
    ftScoringConfig.methods("signalFluctuation") match {
      case "BasicPeakelFinder" => signalFluctuation = FeatureScorer.calcSignalFluctuationByBasicPeakelFinder(f)
      case "WaveletBasedPeakelFinder" => signalFluctuation = FeatureScorer.calcSignalFluctuationByWaveletBasedPeakelFinder(f)
      case _ => throw new Exception("Error when assigning a  shape to a feature")
    }
    
    ftScoringConfig.methods("shape") match {
      case "Gauss" => shape = FeatureScorer.calcShapeByGaussFitting(f)
      case "GaussLorentz" => shape = FeatureScorer.calcShapeByGaussLorentzFitting(f)
      case "Parabola" => shape = FeatureScorer.calcShapeByParabolaFitting(f)
        
    }
   
    val isotopesCount = f.getPeakelsCount
    val isotopesPattern = FeatureScorer.calcIsotopicDistance(f)
    val peakelsWidth = FeatureScorer.calcStdDevPeakelsWidth(f)
    val peakelsCorrelation = FeatureScorer.calcMeanPeakelCorrelation(f.getPeakels).toFloat
    //println("peakelsCorrelation :" + peakelsCorrelation)

    FeatureQualityVector (
      f.ms1Count,
      mzPrecision = FeatureScorer.calcStdDevPeakelsMzPrecision(f),
      shape,
      signalFluctuation,
      isotopesCount,
      isotopesPattern toFloat,
      0,
      peakelsWidth,
      peakelsCorrelation,
      peakelsVelocity = FeatureScorer.calcDistanceOverArea(f),
      peakelsAmplitude = FeatureScorer.calcPeakelsAmplitude(f),
      f.getOverlapPMCC,
      FeatureScorer.calcOverlappingFactor(f, 5).toFloat)
  }
  
  def evaluateQualityVector(f: Feature, qualityVector: FeatureQualityVector, thresholds: QualityVectorThresholds ): FeatureEvaluation = {               											  
    val assessment = FeatureQualityEvaluator.evaluate(qualityVector, thresholds)
    FeatureEvaluation(f, qualityVector, assessment, qualityVector.overlappingFactor == 0)
  }
  
 
 
}



