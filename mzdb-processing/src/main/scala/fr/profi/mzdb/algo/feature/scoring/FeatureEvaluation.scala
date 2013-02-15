package fr.profi.mzdb.algo.feature.scoring

import fr.profi.mzdb.model.Feature

/**
 * @author David Bouyssie
 *
 */
case class FeatureQualityVector (
  ms1Count: Int,
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
  qualityThresholds: Tuple2[FeatureQualityVector,FeatureQualityVector], // min & max thresholds
  overlapThresholds: Tuple2[FeatureOverlapVector,FeatureOverlapVector] // min & max thresholds
)


