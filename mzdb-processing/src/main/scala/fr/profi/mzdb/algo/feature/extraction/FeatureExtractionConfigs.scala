package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.model.Feature

object DetectionAlgorithm extends Enumeration {
  //type DetectionAlgorithm = Value
  val WAVELET, BASIC = Value
}

object ExtractionAlgorithm extends Enumeration {
  //type ExtractionAlgorithm = Value
  val MS2_DRIVEN, PREDICTED_TIME, FULLY_SPERVISED, UNSUPERVISED = Value
}

case class ExtractionAlgorithmConfig(
    
  // use basic peakel finder if refine detection set to true
  var detectionAlgorithm: DetectionAlgorithm.Value,
  
  // will launch peakel detection in the ms2DrivenAlgorihtm
  var refineDetection: Boolean = true,  
  
  // if detect algo is set to wavelet must put a SNR
  var minSNR: Float = 3f
)

case class FeatureExtractorConfig(
    
  // ppm tolerance
  var mzTolPPM: Float, 
  var ms2DrivenExtraction: ExtractionAlgorithmConfig = ExtractionAlgorithmConfig(detectionAlgorithm = DetectionAlgorithm.BASIC),
  var predictedTimeExtraction: ExtractionAlgorithmConfig = ExtractionAlgorithmConfig(detectionAlgorithm = DetectionAlgorithm.WAVELET),
  
  // minimum scan duration to consider the detected feature (PredictedTime)
  var minConsecutiveScans: Int = 5,
  
  // maxConsectuvive gaps ( empty or null isotopic pattern) allowed in peak detection
  var maxConsecutiveGaps: Int = 1,
  
  // maximum duration for extraction (ms2Driven)
  var maxTimeWindow: Float = 1200f,
  
  // minimum percentage of apex intensity to stop isotopic extraction
  var minPercentageOfMaxInt: Float = 0.005f,
  
  // by default the nb of peak of the averagine with relative intensity >= 5%
  var maxNbPeaksInIP: Option[Int] = Some(3),
  
  // predicted time 
  var predictedTimeTol: Int = 90
) 

case class OverlappingFeatureExtractorConfig(
    
  // if false will only check if an overlap exist on the monoistopic
  var extractAllOvlFts: Boolean = false, 
  
  // min charge to check
  var minZ: Int = 1, 
  
  // max charge to check
  var maxZ: Int = 6, 
  
  // start to look for 3Da before the monoisotopic
  var maxIpShift: Int = 3,
  
  // minimum correlation to considerer a 'match'
  var minPeakelCorrToMono: Float = 0.6f,
  
  // quotient averagine
  var minAvgQuot: Float = 1.5f
)


                          