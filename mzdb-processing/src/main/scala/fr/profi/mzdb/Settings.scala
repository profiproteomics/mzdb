package fr.profi.mzdb

import com.typesafe.config.ConfigFactory

object Settings {

  private val config = ConfigFactory.load()

  object SmartPeakelFinderConfig {
    val minPeaksCount = _smartPeakelFinderConfig.getInt("minPeaksCount")
    val miniMaxiDistanceThresh = _smartPeakelFinderConfig.getInt("miniMaxiDistanceThresh")
    val maxIntensityRelThresh = _smartPeakelFinderConfig.getDouble("maxIntensityRelThresh").toFloat
    val useOscillationFactor = _smartPeakelFinderConfig.getBoolean("useOscillationFactor")
    val maxOscillationFactor = _smartPeakelFinderConfig.getInt("maxOscillationFactor")
    val usePartialSGSmoother = _smartPeakelFinderConfig.getBoolean("usePartialSGSmoother")
    val useBaselineRemover = _smartPeakelFinderConfig.getBoolean("useBaselineRemover")
    val useSmoothing = _smartPeakelFinderConfig.getBoolean("useSmoothing")
  }

  object FeatureDetectorConfig {
    val msLevel = _featureDetectorConfig.getInt("msLevel")
    val mzTolPPM = _featureDetectorConfig.getInt("mzTolPPM").toFloat
    val minNbOverlappingIPs = _featureDetectorConfig.getInt("minNbOverlappingIPs")
    val intensityPercentile = _featureDetectorConfig.getDouble("intensityPercentile").toFloat
    val maxConsecutiveGaps = _featureDetectorConfig.getInt("maxConsecutiveGaps")
  }

  private val _smartPeakelFinderConfig = config.getConfig("SmartPeakelFinderConfig")
  private val _featureDetectorConfig = config.getConfig("FeatureDetectorConfig")

}
