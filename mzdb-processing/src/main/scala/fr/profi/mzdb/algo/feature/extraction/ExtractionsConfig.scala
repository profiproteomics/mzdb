package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.model.Feature

object DetectionAlgorithm extends Enumeration {
  type DetectionAlgorithm = Value
  val WAVELET, BASIC = Value
}
import DetectionAlgorithm._

object ExtractionAlgorithm extends Enumeration {
  type ExtractionAlgorithm = Value
  val MS2_DRIVEN, PREDICTED_TIME, FULLY_SPERVISED, UNSUPERVISED = Value
}

/** */
case class ExtractionAlgorithmConfig(var refineDetection: Boolean = true, // will launch peakel detection in the ms2DrivenAlgorihtm
                                     var detectionAlgorithm: DetectionAlgorithm, // use basic peakel finder if refine detection set to true
                                     var minSNR: Float = 3f) // if detect algo is set to wavelet must put a SNR

/** */
case class FeatureExtractorConfig(var mzTolPPM: Float, // ppm tolerance
                                  var ms2DrivenExtraction: ExtractionAlgorithmConfig = ExtractionAlgorithmConfig(detectionAlgorithm = BASIC),
                                  var predictedTimeExtraction: ExtractionAlgorithmConfig = ExtractionAlgorithmConfig(detectionAlgorithm = WAVELET),
                                  var minConsecutiveScans: Int = 5, // minimum scan duration to consider the detected feature (PredictedTime)
                                  var maxConsecutiveGaps: Int = 1, // maxConsectuvive gaps ( empty or null isotopic pattern) allowed in peak detection 
                                  var maxTimeWindow: Float = 1200f, // maximum duration for extraction (ms2Driven)
                                  var minPercentageOfMaxInt: Float = 0.005f, // minimum percentage of apex intensity to stop isotopic extraction
                                  var maxNbPeaksInIP: Option[Int] = Some(3), // by default the nb of peak of the averagine with relative intensity  >= 5%
                                  var predictedTimeTol: Int = 90) // predicted time 

/** */
case class OverlappingFeatureExtractorConfig(var extractAllOvlFts: Boolean = false, // if false will only check if an overlap exist on the monoistopic
                                             var minZ: Int = 1, // min charge to check
                                             var maxZ: Int = 6, // max charge to check
                                             var maxIpShift: Int = 3, // start to look for 3Da before the monoisotopic
                                             var minPeakelCorrToMono: Float = 0.6f, // minimum correlation to considerer a 'match'
                                             var minAvgQuot: Float = 1.5f) // quotient averagine
/** */
case class ProvedOverlappingFeaturesWithMono(ft: Feature, // feature with ambiguous monositopic peak
                                             peakelIndex: Int, // index of the peakel being  polluted
                                             apexDistanceInNbCycle: Int, // distance in cycle between overlapped peak and and overlapping peak
                                             pCorr: Float, // correlation between the peakel 'shape'
                                             avgQuot: Float) // quotient to check against the averagine

/** */
case class OverlapStatus(overlapEvidence: Boolean, // true if there is an overlapping feature detected
                         overlapWithMonoEvidence: Boolean, // true if an overlapping with the monoisotopic peak is affected
                         overlappingFeatures: Array[Feature], // all overlapping features
                         overlappingFeaturesWithMono: Array[ProvedOverlappingFeaturesWithMono]) // overlapping features with the monoisotopic
                          