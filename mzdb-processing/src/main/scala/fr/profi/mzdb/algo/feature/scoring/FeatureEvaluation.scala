package fr.profi.mzdb.algo.feature.scoring

import scala.collection.mutable.LongMap
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.ILcContext

/**
 * Metric to evaluate a feature quality
 */
case class FeatureQualityVector (
  ms1Count: Int, // duration in nb spectra 
  mzPrecision: Float, //standard deviation in moz 
  shape: Float, // could also be called fitting RMSD 
  
  //signalFluctuation: Float, // derivative switch count over all peakels SO SO
  
  isotopesCount: Int, // nb peakels
  isotopesPattern: Float, //distance between theoretical and experimental patterns correlation ?
  
  //isotopesRatios:Float,
  
  peakelsWidth: Float, // left and right hwhm
  peakelsCorrelation: Float, // pearson correlation between peakels
  
  //peakelsVelocity: Float, // FAKE
  //peakelsAmplitude: Float, // kind of SNR ? FAKE TO ME
  
  peakelsApexDeviation: Float, //mean of the deviation in spectrum between peakel apexes
  
  overlappingPeakelsCorrelation: Float, // correlation with detected overlapping peakels
  overlappingFactor: Float // overlapping factor, metric to quantify how much a feature is affected by an overlapping peakel/feature 
)

/**
 * each metric 
 */
case class FeatureQualityAssessment (
  evalThresholds : QualityVectorThresholds,
  
  ms1Count: Boolean,
  mzPrecision:Boolean,
  shape: Boolean,
  
  //signalFluctuation: Boolean, // derivative switch count over all peakels
  
  isotopesCount: Boolean,
  isotopesPattern: Boolean,
  //isotopesRatios:Boolean,

  peakelsWidth: Boolean,
  peakelsCorrelation: Boolean,
  //peakelsVelocity: Boolean,
  //peakelsAmplitude: Boolean,
  
  peakelsApexDeviation:Boolean,
  
  overlappingPeakelsCorrelation: Boolean,
  overlappingFactor: Boolean
) {
  def isOk(): Boolean = {
    ( ms1Count && shape && isotopesCount  && peakelsWidth && peakelsCorrelation && 
     isotopesPattern && overlappingPeakelsCorrelation && overlappingFactor && mzPrecision ) 
  }
}



/**
 * 
 */
case class FeatureEvaluation(
  feature: Feature,
  qualityCriteria: FeatureQualityVector,
  qualityAssessment: FeatureQualityAssessment,
  isOverlapping: Boolean) {
  
  lazy val isHighQuality: Boolean =  qualityAssessment.isOk   
}

/**
 * Min and Max allowed values for each metric, min and max are FeatureQualityVector
 */
class QualityVectorThresholds(min:FeatureQualityVector,
                              max:FeatureQualityVector) {
  
  
  
  def getMzPrecisionMinMax(): (Float, Float) = (min.mzPrecision, max.mzPrecision)
  
  def getIsotopesRatios(): (Float, Float) = (min.isotopesPattern, max.isotopesPattern)
  //def getPeakelsAmplitude(): Pair[Float, Float] = (min.peakelsAmplitude, max.peakelsAmplitude)
  
  //def getPeakelsVelocity(): Pair[Float, Float] = (min.peakelsVelocity, max.peakelsVelocity)
  
  def getMs1CountMinMax(): (Int, Int) = (min.ms1Count,max.ms1Count)
  
  def getIsotopesCountMinMax(): (Int,Int) = (min.isotopesCount,max.isotopesCount)
  
  def getIstopesPatternMinMax(): (Float,Float) = (min.isotopesPattern,max.isotopesPattern)
  
  def getShapeMinMax(): (Float,Float) = (min.shape,max.shape)
  
  //def getSignalFluctuationMinMax(): Pair[Float, Float] = (min.signalFluctuation, max.signalFluctuation)
  
  def getPeakelsWidthMinMax(): (Float,Float) = (min.peakelsWidth,max.peakelsWidth)
  
  def getPeakelsCorrelationtMinMax(): (Float,Float) = (min.peakelsCorrelation,max.peakelsCorrelation)
  
  def getOverlappingFactorMinMax(): (Float,Float) = (min.overlappingFactor,max.overlappingFactor)
  
  def getOverlappingPeakelsCorrelationMinMax(): (Float,Float) = (min.overlappingPeakelsCorrelation,max.overlappingPeakelsCorrelation)
  
  def getPeakelsApexDeviationMinMax(): (Float,Float) = (min.peakelsApexDeviation, max.peakelsApexDeviation)

}

/**
 * FeatureEvaluationThresholds
 */
case class FeatureEvaluationThresholds(
   var qualityThresholds: QualityVectorThresholds, // min & max thresholds
   val ftScoringConfig: FeatureScoringConfig) {}

/**
 * 
 */
trait IFeatureThresholdsComputer {
  //def getThresholds( features: Array[Feature]): Pair[FeatureQualityVector, FeatureQualityVector]
  
  def getThresholds( qualityVec: Array[FeatureQualityVector]): QualityVectorThresholds
  
  //def getThresholds( qualityVec: ParArray[FeatureQualityVector]): QualityVectorThresholds


}


/**
 * iqrFactor: 1.5 for alpha = 0.05 (95%) and 3 for alpha = 0.01 (99%)
 */
class BoxPlotFeatureThresholdsComputer( iqrFactor: Float = 1.5f ) extends IFeatureThresholdsComputer {
  
  def getThresholds( qualityVec: Array[FeatureQualityVector]): QualityVectorThresholds = {
    
    val (mzPrecisionMin, mzPrecisionMax) = _getMzPrecisionBounds(qualityVec);
    
    val (ms1CountMin, ms1CountMax) = _getMS1CountBounds(qualityVec)
    
    val (isotopesCountMin, isotopesCountMax) = _getIsotopesCountBounds(qualityVec)
    
    val (isotopesPatternMin, isotopesPatternMax) = _getIsotopesPatternBounds(qualityVec)
    
    //val (isotopesRatiosMin, isotopesRatiosMax) = _getIsotopesRatiosBounds(features);
    
    val (shapeMin, shapeMax) = _getShapeBounds(qualityVec)
    
    //val (signalFluctuationMin, signalFluctuationMax) = _getSignalFluctuationBounds(features)
    
    val (peakelsWidthMin, peakelsWidthMax) = _getPeakelsWidthBounds(qualityVec)
    
    val (peakelsCorrelationMin, peakelsCorrelationMax) = _getPeakelsCorrelationBounds(qualityVec)
    
    val (peakelsApexDeviationMin, peakelsApexDeviationMax) = this._getPeakelsApexDeviationBounds(qualityVec)
    
    //val (peakelsVelocityMin, peakelsVelocityMax) = _getPeakelsVelocityBounds(features)
    
    //val (peakelsAmplitudeMin, peakelsAmplitudeMax) = _getPeakelsAmplitudeBounds(features)
    
    val (overlappingFactorMin, overlappingFactorMax) = _getOverlappingFactorBounds(qualityVec)
    
    val (overlappingPeakelsCorrelationMin, overlappingPeakelsCorrelationMax) = _getOverlappingPeakelsCorrelationBounds(qualityVec)
    
    new QualityVectorThresholds(FeatureQualityVector(ms1CountMin,
                                                    mzPrecisionMin,
                                                    shapeMin,
                                                    //signalFluctuationMin,
                                                    isotopesCountMin, 
                                                    isotopesPatternMin,
                                                    //isotopesRatiosMin,
                                                    peakelsWidthMin,
                                                    peakelsCorrelationMin,
                                                    
                                                    peakelsApexDeviationMin,
                                                    
                                                    //peakelsVelocityMin,
                                                    //peakelsAmplitudeMin,
                                                    overlappingPeakelsCorrelationMin,
                                                    overlappingFactorMin),
                                       FeatureQualityVector(ms1CountMax,
                                                            mzPrecisionMax,
                                                            shapeMax,
                                                            
                                                            //signalFluctuationMax,
                                                            
                                                            isotopesCountMax, 
                                                            isotopesPatternMax,
                                                            //isotopesRatiosMax,
                                                            peakelsWidthMax,
                                                            peakelsCorrelationMax,
                                                            
                                                            peakelsApexDeviationMax,
                                                            
                                                            //peakelsVelocityMax,
                                                            //peakelsAmplitudeMax,
                                                            overlappingPeakelsCorrelationMax,
                                                            overlappingFactorMax))
  }
  
  private def _calcFeatureQ1Q3Indexes( values: Seq[Any] ): (Int,Int) = {
    
    val N = values.length
    val iq1 = (N * 0.25).toInt
    //val iq2 =  N * 0.5 toInt
    val iq3 = (N * 0.75).toInt
    
    (iq1,iq3)
  }
  
  private def _calcBounds( values: Array[Int] ): (Int,Int) = {
    val sortedValues = values.sorted
    val q1q3Indexes = _calcFeatureQ1Q3Indexes(sortedValues)
    
    val (iq1v, iq3v) = (sortedValues(q1q3Indexes._1), sortedValues(q1q3Indexes._2) )
    val iqr = iq3v - iq1v
    val fullIqr = iqrFactor * iqr
    
    ((iq1v - fullIqr).toInt ,(iq3v + fullIqr).toInt)
  }
  
  private def _calcBounds( values: Array[Float] ): (Float,Float) = {
    val sortedValues = values.sorted
    val q1q3Indexes = _calcFeatureQ1Q3Indexes(sortedValues)
    
    val (iq1v, iq3v) = (sortedValues(q1q3Indexes._1), sortedValues(q1q3Indexes._2) )
    val iqr = iq3v - iq1v
    val fullIqr = iqrFactor * iqr
    
    ((iq1v - fullIqr) ,(iq3v + fullIqr))
  }

  
  private def _getMS1CountBounds(features : Array[FeatureQualityVector]): (Int, Int) = {
    val values = features.map( f => f.ms1Count )    
    _calcBounds(values)
  }
  
  private def  _getIsotopesCountBounds (features : Array[FeatureQualityVector]) : (Int, Int) ={
     val values = features.map( f => f.isotopesCount )    
     _calcBounds(values)
  }
  private def  _getIsotopesPatternBounds (features : Array[FeatureQualityVector]) : (Float, Float) = {
     val values = features.map( f => f.isotopesPattern )    
     _calcBounds(values)
  }
  private def  _getShapeBounds (features : Array[FeatureQualityVector]): (Float, Float) = {
     val values = features.map( f => f.shape )    
     _calcBounds(values)

  }
  /*private def  _getSignalFluctuationBounds (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }*/
  private def  _getPeakelsWidthBounds (features : Array[FeatureQualityVector]): (Float, Float) = {
     val values = features.map( f => f.peakelsWidth )    
     _calcBounds(values)

  }
  private def _getMzPrecisionBounds (features : Array[FeatureQualityVector]): (Float, Float) = {
     val values = features.map( f => f.mzPrecision )    
     _calcBounds(values)

  }
  
  private def _getPeakelsApexDeviationBounds(features : Array[FeatureQualityVector]): (Float, Float) = {
      val values = features.map( f => f.peakelsApexDeviation )    
     _calcBounds(values)

  }
  
  /*private def _getIsotopesRatiosBounds(features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)
  }*/
  
  /*private def _getPeakelsAmplitudeBounds(features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  
  private def _getPeakelsVelocityBounds(features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }*/
  
  private def  _getPeakelsCorrelationBounds (features : Array[FeatureQualityVector]): (Float, Float) = {
     val values = features.map( f => f.peakelsCorrelation )    
     _calcBounds(values)
  }
  private def  _getOverlappingFactorBounds (features : Array[FeatureQualityVector]): (Float, Float) = {
     val values = features.map( f => f.overlappingFactor )    
     _calcBounds(values)

  }
  private def  _getOverlappingPeakelsCorrelationBounds (features : Array[FeatureQualityVector]): (Float, Float) = {
     val values = features.map( f => f.overlappingPeakelsCorrelation )    
     _calcBounds(values)
  }
  
}



/**
 * 
 */
object FeatureQualityEvaluator {
  
  
  def evaluate(f : FeatureQualityVector, qualityThresholds: QualityVectorThresholds) : FeatureQualityAssessment = {
    FeatureQualityAssessment(qualityThresholds,
                              checkParam( f.ms1Count, qualityThresholds.getMs1CountMinMax),
                              checkParam( f.mzPrecision, qualityThresholds.getMzPrecisionMinMax),
                              checkParam( f.shape, qualityThresholds.getShapeMinMax),
                              //checkParam( f.signalFluctuation, qualityThresholds.getSignalFluctuationMinMax),
                              checkParam( f.isotopesCount, qualityThresholds.getIsotopesCountMinMax),
                              checkParam( f.isotopesPattern, qualityThresholds.getIstopesPatternMinMax),
                              //checkParam ( f.isotopesRatios, qualityThresholds.getIsotopesRatios),
                              checkParam( f.peakelsWidth, qualityThresholds.getPeakelsWidthMinMax),
                              checkParam( f.peakelsCorrelation, qualityThresholds.getPeakelsCorrelationtMinMax),
                              checkParam( f.peakelsApexDeviation, qualityThresholds.getPeakelsApexDeviationMinMax),

                              //checkParam( f.peakelsVelocity, qualityThresholds.getPeakelsVelocity),
                              //checkParam( f.peakelsAmplitude, qualityThresholds.getPeakelsAmplitude),
                              checkParam( f.overlappingPeakelsCorrelation, qualityThresholds.getOverlappingPeakelsCorrelationMinMax),
                              checkParam( f.overlappingFactor, qualityThresholds.getOverlappingFactorMinMax))

  }
  
  def checkParam( param: Float, minMax: (Float,Float) ): Boolean = {
    param <= minMax._2 && param >= minMax._1
  }
  
  def checkParam( param: Int, minMax: (Int,Int) ): Boolean = {
     param <= minMax._2 && param >= minMax._1
  }
  
}


/**
 * 
 */
case class FeatureScoringConfig(
  val methods: Map[String, Any] = Map("signalFluctuation" -> "BasicPeakelFinder", "shape" -> "Gauss")//several could be employed for assessing the quality of a feature
)


/**
 * 
 */
//Fitting all the peakels ? if we do it for all the peakels it could be long
object FeatureEvaluator  {
  
  def evaluateFeatures(
    features: Seq[Feature],
    ftScoringConfig: FeatureScoringConfig,
    thresholdComputer: IFeatureThresholdsComputer,
    lcContextBySpectrumId: LongMap[ILcContext]
  ): Seq[FeatureEvaluation] = {
    //fill data 
    val fq = features.par.map( f => computeQualityVector(f, ftScoringConfig, lcContextBySpectrumId)).toArray
    val qualThresholds = thresholdComputer.getThresholds(fq) 
    /* just one toArray does not work ? FIXME: */
    fq.zip(features).par.map{ case (fq, f) => evaluateQualityVector(f, fq, qualThresholds) }.toArray.toSeq
  }
  
  def computeQualityVector(
    f: Feature,
    ftScoringConfig: FeatureScoringConfig = FeatureScoringConfig(),
    lcContextBySpectrumId: LongMap[ILcContext]
  ) : FeatureQualityVector = {
    
    var (signalFluctuation, shape) = (0f, 0f)
    ftScoringConfig.methods("signalFluctuation") match {
      case "BasicPeakelFinder" => signalFluctuation = FeatureScorer.calcSignalFluctuationByBasicPeakelFinder(f,lcContextBySpectrumId)
      case "WaveletBasedPeakelFinder" => signalFluctuation = FeatureScorer.calcSignalFluctuationByWaveletBasedPeakelFinder(f,lcContextBySpectrumId)
      case _ => throw new Exception("Error when assigning a  shape to a feature")
    }
    
    // TODO: uncomment me when fitters have been upgraded to math3
    /*ftScoringConfig.methods("shape") match {
      case "Gauss" => shape = FeatureScorer.calcShapeByGaussFitting(f)
      case "GaussLorentz" => shape = FeatureScorer.calcShapeByGaussLorentzFitting(f)
      case "Parabola" => shape = FeatureScorer.calcShapeByParabolaFitting(f)
    }*/
   
    val isotopesCount = f.getPeakelsCount
    val isotopesPattern = FeatureScorer.calcRmsdIsotopicPattern(f)
    val peakelsWidth = 0f // FeatureScorer.calcStdDevPeakelsWidth(f)
    
    val peakels = f.getPeakels()
    val peakelsCorrelation = FeatureScorer.calcMeanPeakelCorrelation(peakels).toFloat
    val peakelsApexDeviation = FeatureScorer.calcMeanPeakelsApexDeviation(peakels)
    //println("peakelsCorrelation :" + peakelsCorrelation)

    FeatureQualityVector (
      f.getMs1Count,
      mzPrecision = FeatureScorer.calcStdDevPeakelsMzPrecision(f),
      shape,
      //signalFluctuation,
      isotopesCount,
      isotopesPattern.toFloat,
      //0,
      peakelsWidth,
      peakelsCorrelation,
      //peakelsVelocity = FeatureScorer.calcDistanceOverArea(f),
      //peakelsAmplitude = FeatureScorer.calcPeakelsAmplitude(f),
      peakelsApexDeviation,
      Option(f.overlapProperties).map(_.getOverlapPMCC).getOrElse(0f),
      FeatureScorer.calcOverlappingFactor(f, 5).toFloat
    )
  }
  
  def evaluateQualityVector(f: Feature, qualityVector: FeatureQualityVector, thresholds: QualityVectorThresholds ): FeatureEvaluation = {               											  
    val assessment = FeatureQualityEvaluator.evaluate(qualityVector, thresholds)
    FeatureEvaluation(f, qualityVector, assessment, qualityVector.overlappingFactor == 0)
  }
 
}



