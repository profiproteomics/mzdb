package fr.profi.mzdb.algo.feature.scoring

import fr.profi.mzdb.model.Feature
import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder




/**
 * @author David Bouyssie
 *
 */

case class FeatureQualityVector (
  ms1Count: Int,
  isotopesCount: Int, // nb peakels
  isotopesPattern: Float, //distance between theoretical and experimental patterns
  shape: Float, // RMSD
  signalFluctuation: Float, // derivative switch count over all peakels
  peakelsWidth: Float,
  peakelsCorrelation: Float  
)



/**
 * @author David Bouyssie
 *
 */
case class FeatureQualityAssessment (
  evalThresholds : QualityVectorThresholds,
  ms1Count: Boolean,
  isotopesCount: Boolean,
  isotopesPattern: Boolean,
  shape: Boolean,
  signalFluctuation: Boolean, // derivative switch count over all peakels
  peakelsWidth: Boolean,
  peakelsCorrelation: Boolean  
) {
  def isOk(): Boolean = {
    if( ms1Count && shape && isotopesCount && signalFluctuation && peakelsWidth && peakelsCorrelation && isotopesPattern ) true
    else false
  }
}

/**
 * @author David Bouyssie
 *
 */
case class FeatureOverlapVector (
  overlappingPeakelsCorrelation: Float,
  overlappingFactor: Float
)

/**
 * @author David Bouyssie
 *
 */
case class FeatureEvaluation(
  feature: Feature,
  qualityCriteria: FeatureQualityVector,
  qualityAssessment: Option[FeatureQualityAssessment],
  overlapCriteria: FeatureOverlapVector,
  isOverlapping: Boolean
) {
  
  lazy val isHighQuality: Boolean = if (qualityAssessment != None) qualityAssessment.get.isOk else false
  
}

class QualityVectorThresholds(min:FeatureQualityVector,max:FeatureQualityVector) extends Pair[FeatureQualityVector, FeatureQualityVector](min,max) {
  
  def getMs1CountMinMax(): Pair[Int, Int] = (min.ms1Count,max.ms1Count)
  def getIsotopesCountMinMax(): Pair[Int,Int] = (min.isotopesCount,max.isotopesCount)
  def getIstopesPatternMinMax(): Pair[Float,Float] = (min.isotopesPattern,max.isotopesPattern)
  def getShapeMinMax(): Pair[Float,Float] = (min.shape,max.shape)
  def getSignalFluctuationMinMax(): Pair[Float, Float] = (min.signalFluctuation, max.signalFluctuation)
  def getPeakelsWidthMinMax(): Pair[Float,Float] = (min.peakelsWidth,max.peakelsWidth)
  def getPeakelsCorrelationtMinMax(): Pair[Float, Float] = (min.peakelsCorrelation,max.peakelsCorrelation)
}



/**
 * @author David Bouyssie
 *
 */
case class FeatureEvaluationThresholds(
   var qualityThresholds: QualityVectorThresholds, // min & max thresholds
   var overlapThresholds: Pair[FeatureOverlapVector, FeatureOverlapVector], // min & max thresholds
   methods: Map[String, Any] = Map("signalFluctuation" -> "BasicPeakelFinder", "shape" -> "GaussFitting")//several could be employed for assessing the quality of a feature
) {
  
  
  
}

object FeatureEvaluationThresholdsGenerator {
  
  //val mapping = HashMap[String, ] 
  
  def autoQualityThresholdAdjustment( features : Array[Feature]) : Pair[FeatureQualityVector, FeatureQualityVector] = {
    val (ms1CountMin, ms1CountMax) = _getMS1CountQ1Q3(features)
    val (isotopesCountMin, isotopesCountMax) = _getIsotopesCountQ1Q3(features)
    val (isotopesPatternMin, isotopesPatternMax) = _getIsotopesPatternQ1Q3(features)
    val (shapeMin, shapeMax) = _getShapeQ1Q3(features)
    val (signalFluctuationMin, signalFluctuationMax) = _getSignalFluctuationQ1Q3(features)
    val (peakelsWidthMin, peakelsWidthMax) = _getPeakelsWidthQ1Q3(features)
    val (peakelsCorrelationMin, peakelsCorrelationMax) = _getPeakelsCorrelationQ1Q3(features)
    Pair(FeatureQualityVector(ms1CountMin,
                              isotopesCountMin, 
                              isotopesPatternMin,
                              shapeMin,
                              signalFluctuationMin,
                              peakelsWidthMin,
                              peakelsCorrelationMin),
         FeatureQualityVector(ms1CountMax,
                              isotopesCountMax,
                              isotopesPatternMax,
                              shapeMax,
                              signalFluctuationMax,
                              peakelsWidthMax,
                              peakelsCorrelationMax))
  }
  
  
  def _getMS1CountQ1Q3(features : Array[Feature]):Pair[Int, Int] = {
    val N = features.length
    val iq1 = N * 0.25 toInt
    val iq2 =  N * 0.5 toInt
    val iq3 = (N * 0.75).toInt
    features.sortBy(x => x.ms1Count)  
    val (iq1v, iq2v, iq3v) = (features(iq1).ms1Count, features(iq2).ms1Count, features(iq3).ms1Count)
    val iq = 1.5 * iq2v
    
    ((iq1v - iq).toInt ,(iq3v + iq).toInt)
    }
  
  def  _getIsotopesCountQ1Q3 (features : Array[Feature]) : Pair[Int, Int] ={
    (1,1)
  }
  def  _getIsotopesPatternQ1Q3 (features : Array[Feature]) : Pair[Float, Float] = {
    (0f, 0f)
  }
  def  _getShapeQ1Q3 (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  def  _getSignalFluctuationQ1Q3 (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  def  _getPeakelsWidthQ1Q3 (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

  }
  def  _getPeakelsCorrelationQ1Q3 (features : Array[Feature]): Pair[Float, Float] = {
        (0f, 0f)

}
}

object FeatureQualityEvaluator {
  
  
  def evaluate(f : FeatureQualityVector, qualityThresholds: QualityVectorThresholds) : FeatureQualityAssessment = {
    FeatureQualityAssessment(qualityThresholds,
                              checkParam( f.ms1Count, qualityThresholds.getMs1CountMinMax),
                              checkParam( f.isotopesCount, qualityThresholds.getIsotopesCountMinMax),
                              checkParam( f.isotopesPattern, qualityThresholds.getIstopesPatternMinMax),
                              checkParam( f.shape, qualityThresholds.getShapeMinMax),
                              checkParam( f.signalFluctuation, qualityThresholds.getSignalFluctuationMinMax),
                              checkParam( f.peakelsWidth, qualityThresholds.getPeakelsWidthMinMax),
                              checkParam( f.peakelsCorrelation, qualityThresholds.getPeakelsCorrelationtMinMax))

  }
  
  def checkParam( param: Float, minMax: Pair[Float,Float] ): Boolean = {
    param <= minMax._2 && param >= minMax._1
  }
  
  def checkParam( param: Int, minMax: Pair[Int,Int] ): Boolean = {
     param <= minMax._2 && param >= minMax._1
  }
  
}


//Fitting all the peakels ? if we do it for all the peakels it could be long
object FeatureEvaluator  {
  
  def evaluate (f : Feature, eval : Option[FeatureEvaluationThresholds]) : FeatureEvaluation = {

    val ms1Count = f.ms1Count    
 
    var (signalFluctuation, shape) = (0f, 0f)
    if (eval != None) {
      eval.get.methods("signalFluctuation") match {
        case "BasicPeakelFinder" => signalFluctuation = FeatureScorer.calcSignalFluctuationByBasicPeakelFinder(f)
        case "WaveletBasedPeakelFinder" => signalFluctuation = FeatureScorer.calcSignalFluctuationByWaveletBasedPeakelFinder(f)
        case _ => throw new Exception("Error when assigning a  shape to a feature")
      }
      
      var shape = 0f
      eval.get.methods("shape") match {
        case "Gauss" => shape = FeatureScorer.calcShapeByGaussFitting(f)
        case "GaussLorentz" => shape = FeatureScorer.calcShapeByGaussLorentzFitting(f)
        case "Parabola" => shape = FeatureScorer.calcShapeByParabolaFitting(f)
          
      }
    } else {
      signalFluctuation = FeatureScorer.calcSignalFluctuationByBasicPeakelFinder(f)
      shape = FeatureScorer.calcShapeByGaussFitting(f)
    }
    
    val isotopesCount = f.getPeakelsCount
    val isotopesPattern = FeatureScorer.calcIsotopicDistance(f)
    val peakelsWidth = FeatureScorer.calcStdDevPeakelsWidth(f)
    val peakelsCorrelation = FeatureScorer.calcMeanPeakelCorrelation(f.getPeakels).toFloat

    val featureQuality  = FeatureQualityVector (ms1Count, 
                        											  isotopesCount,
                        											  isotopesPattern toFloat,
                        											  shape,
                        											  signalFluctuation,
                        											  peakelsWidth,
                        											  peakelsCorrelation)
                        											  
    var featureQualityAssessment = Option.empty[FeatureQualityAssessment]                											  
    if (eval != None)
      featureQualityAssessment = Some(FeatureQualityEvaluator.evaluate(featureQuality, eval.get.qualityThresholds))
     
    val featureOverlapVector = FeatureOverlapVector(f.getOverlapPMCC, f.getOverlapRelativeFactor) //correlation , factor
    
    new FeatureEvaluation(f, featureQuality, featureQualityAssessment, featureOverlapVector, featureOverlapVector.overlappingFactor == 0)
  }
  
 
 
}

/*object Threshold extends Enumeration with Comp{
  type Threshold = Value
  val upperThreshold, lowerThreshold, lowerAndUpperThreshold = Value
}
import Threshold._

trait Comp {
  def comp(threshType: Threshold, value: Float, minVal: Float, maxVal: Float): Boolean = {
    this match {
      case upperThreshold => return value < maxVal
      case lowerThreshold => return value > minVal
      case lowerAndUpperThreshold => return value > minVal && value < maxVal
    }
  }
}

object Metric extends Enumeration {
  type Metric = Value
  case class Type(@BeanProperty val name : String, 
                  @BeanProperty val threshType: Threshold) {
    
    def compare(value:Float, f: FeatureEvaluationThresholds) : Boolean = {
      this match {
        case ms1Count => return comp(threshType,value, f.qualityThresholds._1.getMs1Count, f.qualityThresholds._2.getMs1Count)
        case isotopesCount => return comp(threshType,value, f.qualityThresholds._1.getIsotopesCount, f.qualityThresholds._2.getIsotopesCount)
        case isotopesPattern =>return comp(threshType,value, f.qualityThresholds._1.getIsotopesPattern, f.qualityThresholds._2.getIsotopesPattern)
        case shape => return comp(threshType,value, f.qualityThresholds._1.getShape, f.qualityThresholds._2.getShape)
        case signalFluctuation => return comp(threshType,value, f.qualityThresholds._1.getSignalFluctuation, f.qualityThresholds._2.getSignalFluctuation)
        case peakelsWidth => return comp(threshType,value, f.qualityThresholds._1.getPeakelsWidth, f.qualityThresholds._2.getPeakelsWidth)
        case peakelsCorrelation => return comp(threshType,value, f.qualityThresholds._1.getPeakelsCorrelation, f.qualityThresholds._2.getPeakelsCorrelation)
      }
    }
  }
  val ms1Count = Type("ms1Count", lowerThreshold)
  val isotopesCount = Type("isotopesCount", lowerThreshold)
  val isotopesPattern = Type("isotopesPattern", upperThreshold)
  val shape = Type("shape", upperThreshold)
  val signalFluctuation = Type("signalFluctuation", upperThreshold)
  val peakelsWidth = Type("peakelsWidth", upperThreshold)
  val peakelsCorrelation = Type("peakelsCorrelation", upperThreshold)
    
  
}
import Metric._


case class EvalParam(val metric: Metric.Type, var value : Float = 0f)  {
  
  def checkEval(fethresh:FeatureEvaluationThresholds) : Boolean = {
    return metric.compare(value,fethresh)
  }
  
}*/

/*case class FeatureQualityVector  {

 var ms1CountParam = EvalParam(Metric.ms1Count)
 val isotopesCountParam = EvalParam(Metric.isotopesCount)
 val isotopesPatternParam = EvalParam(Metric.isotopesPattern)
 val shapeParam = EvalParam(Metric.shape)
 val signalFluctuationParam = EvalParam(Metric.signalFluctuation)
 val peakelsWidthParam = EvalParam(Metric.peakelsWidth)
 val peakelsCorrelationParam = EvalParam(Metric.peakelsCorrelation)
     
  def setMs1Count(x:Float) {ms1CountParam.value = x}
  def getMs1Count() : Float = {ms1CountParam.value}
  
  def setIsotopesCount(x:Float) {isotopesCountParam.value = x}
  def getIsotopesCount() : Float = {isotopesCountParam.value}
  
  def setIsotopesPattern(x:Float) {isotopesPatternParam.value = x}
  def getIsotopesPattern(): Float = {isotopesPatternParam.value}

  def setShape(x:Float) {shapeParam.value = x}
  def getShape(): Float = {shapeParam.value }

  def setSignalFluctuation(x:Float) {signalFluctuationParam.value = x}
  def getSignalFluctuation() : Float ={signalFluctuationParam.value }

  def setPeakelsWidth(x:Float) {peakelsWidthParam.value = x}
  def getPeakelsWidth(): Float = {peakelsWidthParam.value}

  def setPeakelsCorrelation(x:Float) {peakelsCorrelationParam.value = x}
  def getPeakelsCorrelation() : Float ={peakelsCorrelationParam.value}

  def getFeatureQualityAssessment(eval:FeatureEvaluationThresholds) : FeatureQualityAssessment = {
    FeatureQualityAssessment(eval, 
                             ms1CountParam.checkEval(eval), 
                             isotopesCountParam.checkEval(eval),
                             isotopesPatternParam.checkEval(eval),
                             shapeParam.checkEval(eval),
                             signalFluctuationParam.checkEval(eval),
                             peakelsWidthParam.checkEval(eval),
                             peakelsCorrelationParam.checkEval(eval))
  }
}*/

