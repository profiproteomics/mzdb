package fr.profi.mzdb

import scala.collection.mutable.ArrayBuffer

import com.typesafe.scalalogging.StrictLogging

import org.junit.Before
import org.junit.Test

import fr.profi.mzdb.algo.feature.extraction.FeatureExtractorConfig
import fr.profi.mzdb.io.reader.iterator.LcMsRunSliceIterator
import fr.profi.mzdb.io.reader.provider.RunSliceDataProvider
import fr.profi.mzdb.model.PutativeFeature

class MzDbFeatureDetectorTest extends StrictLogging {

  var mzDb: MzDbReader = _

  @Before
  @throws(classOf[Exception])
  def setUp() = {
    mzDb = new MzDbReader(MzDbFeatureDetectorTest.this.getClass.getResource("/OVEMB150205_12.raw.mzDB").getFile(), true)
  }

  @Test
  def testDetectPeakels() = {
    try {
      this.logger.debug("detecting peakels in raw MS survey...")
      val mzdbFtDetector = new MzDbFeatureDetector(
        mzDb,
        FeatureDetectorConfig(
          msLevel = 1,
          mzTolPPM = 5.0f,
          minNbOverlappingIPs = 5
        )
      )

      val runSliceIterator = new LcMsRunSliceIterator(mzDb, mzDb.getConnection(), 450, 452)
      val detectedPeakels = mzdbFtDetector.detectPeakels(runSliceIterator)
      logger.info("# of peakels detected : " + detectedPeakels.length)
      
      /*for (peakel <- detectedPeakels) {
        val intensities = peakel.getIntensityValues
        val apexPeakelIdx = peakel.apexIndex
      }*/
      
    } finally {
      mzDb.close()
    }
  }

  @Test
  def testExtractFeaturesFromMS2() = {

    val mzDbFts = try {

      val ftXtractConfig = FeatureExtractorConfig(
        mzTolPPM = 5.0f
      )

      val mzdbFtX = new MzDbFeatureExtractor(mzDb, 5, 5, ftXtractConfig)

      this.logger.info("retrieving spectrum headers...")
      val spectrumHeaders = mzDb.getSpectrumHeaders()
      val ms2SpectrumHeaders = spectrumHeaders.filter(_.getMsLevel() == 2)
      val pfs = new ArrayBuffer[PutativeFeature](ms2SpectrumHeaders.length)

      this.logger.debug("building a single putative features from MS2 spectrum events...")
      pfs += new PutativeFeature(
        id = PutativeFeature.generateNewId,
        mz = ms2SpectrumHeaders(10).getPrecursorMz,
        charge = ms2SpectrumHeaders(10).getPrecursorCharge,
        spectrumId = ms2SpectrumHeaders(10).getId,
        evidenceMsLevel = 2
      )

      // Instantiates a Run Slice Data provider
      val rsdProv = new RunSliceDataProvider(mzDb.getLcMsRunSliceIterator())

      // Extract features
      mzdbFtX.extractFeatures(rsdProv, pfs, 5.0f)

    } finally {
      mzDb.close()
    }

  }

}