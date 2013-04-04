package fr.profi.mzdb.algo.feature.scoring

import fr.profi.mzdb.model.Feature
import scala.reflect.BeanProperty
import scala.collection.mutable.ArrayBuffer
//import fr.profi.mzdb.algo.signal.fitting.Optimizer
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.algo.FeatureScorer
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder

/**
 * @author David Bouyssie
 *
 */
case class FeatureQualityVector (
  var ms1Count: Int,
  shape: Float,
  peakelUnicity: Int,
  peakelsWidth: Float,
  peakelsCorrelation: Float,
  peakelsPattern: Float
)

/**
 * @author David Bouyssie
 *
 */
case class FeatureQualityAssessment (
  ms1Count: Boolean,
  shape: Boolean,
  peakelUnicity: Boolean,
  peakelsWidth: Boolean,
  peakelsCorrelation: Boolean,
  peakelsPattern: Boolean
) {
  def isOk(): Boolean = {
    if( ms1Count && shape && peakelUnicity && peakelsWidth && peakelsCorrelation && peakelsPattern ) true
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
  qualityAssessment: FeatureQualityAssessment,
  overlapCriteria: FeatureOverlapVector,
  isOverlapping: Boolean
) {
  
  lazy val isHighQuality: Boolean = qualityAssessment.isOk
  
}

/**
 * @author David Bouyssie
 *
 */
case class FeatureEvaluationThresholds(
   var qualityThresholds: Tuple2[FeatureQualityVector,FeatureQualityVector], // min & max thresholds
   var overlapThresholds: Tuple2[FeatureOverlapVector,FeatureOverlapVector], // min & max thresholds
   methods: Map[String, Any] = Map("shape" -> "BasicPeakelFinder")//several could be employed for assessing the quality of a feature
) {
  
  def setMS1CountQ1Q3(features : ArrayBuffer[Feature]) {
    // test if user is not cheating us
    if (features.isEmpty)
      return
    val N = features.length
    val iq1 = N * 0.25 toInt
    val iq2 =  N * 0.5 toInt
    val iq3 = (N * 0.75).toInt
	features.sortBy(x => x.ms1Count)  
	val (iq1v, iq2v, iq3v) = (features(iq1).ms1Count, features(iq2).ms1Count, features(iq3).ms1Count)
	val iq = 1.5 * iq2v
	
	qualityThresholds._1.ms1Count = (iq1v - iq).toInt
	qualityThresholds._2.ms1Count = (iq3v + iq).toInt

  }
}

object FeatureEvaluator  {
  /*ms1Count: Int,
  shape: Float,
  peakelNumber: Int,
  peakelsWidth: Float,
  peakelsCorrelation: Float,
  peakelsPattern: Float*/
  def evaluate (f : Feature, eval : FeatureEvaluationThresholds) : FeatureEvaluation = {
    //evaluate IP
    //evaluate
    val ms1Count = f.ms1Count
    //Fitting all the peakels ? a good estimator of the would the rmsd of the fitting, see if there is a convergence or no
    //but if we do it for all the peakels it could be long
    //just check local maxima in the region would be an indicator of the peak shape, basicPeakelDetection would be the simplest
 
    var shape = 0f
    eval.methods("shape") match {
      case "BasicPeakelFinder" => shape = _shapeByBasicPeakelFinder(f)
      case "WaveletBasedPeakelFinder" => shape = _shapeByBasicPeakelFinder(f)
      case "GaussLorentzFitting" => shape = _shapeByGaussLorentzFitting(f)
      case _ => throw new Exception("Error when assigning a  shape to a feature")
    }
    
    val peakelsNumber = f.getPeakels.length
    val peakelsWidth = _getMedianPeakelsWidth(f)
    val peakelsCorrelation = FeatureScorer.calcMeanPeakelCorrelation(f.getPeakels).toFloat
    val peakelsPattern = _getPeakelsPattern(f)
    
    val featureQuality  = FeatureQualityVector (ms1Count, 
    											shape,
    											peakelsNumber,
    											peakelsWidth,
    											peakelsCorrelation,
    											peakelsPattern)
    val featureQualityAssessment = FeatureQualityAssessment( ms1Count > eval.qualityThresholds._1.ms1Count && ms1Count < eval.qualityThresholds._2.ms1Count,
    														shape > eval.qualityThresholds._1.shape && shape < eval.qualityThresholds._2.shape,
    														peakelsNumber > eval.qualityThresholds._1.peakelUnicity && peakelsNumber < eval.qualityThresholds._2.peakelUnicity,
    														peakelsWidth> eval.qualityThresholds._1.peakelsWidth && peakelsWidth < eval.qualityThresholds._2.peakelsWidth,
    														peakelsCorrelation > eval.qualityThresholds._1.peakelsCorrelation && peakelsCorrelation < eval.qualityThresholds._2.peakelsCorrelation,
    														true)//peakelsNumber > eval.qualityThresholds._1.peakelUnicity && peakelsNumber < eval.qualityThresholds._2.peakelUnicity,


    val featureOverlapVector = FeatureOverlapVector(0,0)
    new FeatureEvaluation(f, 
    					  featureQuality, 
    					  featureQualityAssessment, 
    					  featureOverlapVector, 
    					  false)
  }
  
  private def _shapeByBasicPeakelFinder ( f: Feature): Float =  {
	// a perfect shape would be only one maximum index
    var shape = 0f
    for (peakel <- f.getPeakels) {
    	val results = BasicPeakelFinder.findPeakelsIndexes(peakel.definedPeaks)
    	if (results.length > 1 || results.isEmpty)
    	  shape += 1f
    }
    shape
  }
  
  private def _shapeByWaveletBasedPeakelFinder ( f:Feature ): Float =  {
    var shape = 0f
    for (peakel <- f.getPeakels) {
    	/*
    	val results = new WaveletBasedPeakelFinder(peakel.definedPeaks).findCwtPeakels()
    	if (results.length > 1 || results.isEmpty)
    	  shape += 1f*/
    }
    shape
  }
  
  private def _shapeByGaussLorentzFitting (f: Feature) : Float =  {
    //val initialSolution
    //val opt = Optimizer.optimize(10, new LevenbergMarquardtOptimizer, new GaussLorentzFitting, weights, initialSolution)
    0f

  }
  
  private def _getMedianPeakelsWidth(f:Feature): Float = {
    var peakelsWidth = 0f
    for (peakel <- f.getPeakels) {
       val peaks = peakel.getDefinedPeaks.sortBy(x => x.getLeftHwhm() + x.getRightHwhm())
       val p = peaks((0.5 * peaks.length).toInt)
       peakelsWidth += p.getLeftHwhm() + p.getRightHwhm()
    }
    peakelsWidth / f.getPeakelsCount
    peakelsWidth
  }
  
  private def _getPeakelsPattern(f :Feature) : Float = {
    0f
  }
  
 
}


