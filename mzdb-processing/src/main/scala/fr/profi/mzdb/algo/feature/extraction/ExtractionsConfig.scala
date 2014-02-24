package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.model.Feature


/**
 * 
 */
case class FeatureExtractorConfig( mzTolPPM:Float,
                                  var refineDetection:Boolean = true,
                                  var maxConsecutiveGaps:Int = 0,
                                  var maxTimeWindow:Float = 1200f,
                                  var minPercentageOfMaxInt: Float = 0.005f,
                                  var forceExtractionWhenNotFound:Boolean= true)
                                  
                                  
case class FeatureTimeExtractorConfig( val minConsecutiveScans: Int = 4, val predictedTimeTol: Int = 120)

/**
 * 
 */
case class OverlappingFeatureExtractorConfig( var extractAllOvlFts :Boolean = false, // if false will only check if an overlap exist on the monoistopic
                                              var minNbOverlappingIPs: Int = 3,  //or peakel peaks length ?
                                              var minZ: Int = 1,
                                              var maxZ: Int = 6, 
                                              var maxIpShift: Int = 3,
                                              var minPeakelCorrToMono:Float = 0.6f,
                                              var minAvgQuot: Float = 1.5f)
/**
 * 
 */
case class ProvedOverlappingFeaturesWithMono(ft:Feature, peakelIndex: Int, apexDistanceInNbCycle:Int, pCorr: Float, avgQuot: Float)

/**
 * 
 */
case class OverlapStatus( overlapEvidence: Boolean,
                          overlapWithMonoEvidence: Boolean,
                          overlappingFeatures: Array[Feature],
                          overlappingFeaturesWithMono: Array[ProvedOverlappingFeaturesWithMono] )