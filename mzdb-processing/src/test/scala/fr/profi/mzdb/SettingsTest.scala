package fr.profi.mzdb

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.StrictLogging
import org.junit.Test

class SettingsTest extends StrictLogging {


  @Test
  def testConfig() = {

    assert(Settings.SmartPeakelFinderConfig.minPeaksCount == 5)
    assert(Settings.FeatureDetectorConfig.intensityPercentile == 0.9f)

  }


}
