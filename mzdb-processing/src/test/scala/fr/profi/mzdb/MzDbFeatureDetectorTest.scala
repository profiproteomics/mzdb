package fr.profi.mzdb

import com.typesafe.scalalogging.StrictLogging
import org.junit.AfterClass
import org.junit.Assert
import org.junit.BeforeClass
import org.junit.Test
import org.junit.Before
import fr.profi.mzdb.io.reader.iterator.LcMsRunSliceIterator
import fr.profi.mzdb.algo.feature.extraction.FeatureExtractorConfig
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.io.reader.iterator.LcMsRunSliceIterator
import fr.profi.mzdb.io.reader.provider.RunSliceDataProvider
import fr.profi.mzdb.model.PutativeFeature
import java.io.File
import org.junit.Ignore

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

   
  @Ignore
  @Test
  def testDetectPeakelsFromMS2() = {

      mzDb = new MzDbReader(new File("C:/Local/bruley/Data/mzDB/Wiff TTOF/TTOF2_01832.mzdb"), true ) 
      val mzDbFts = try {
        
      val ftDetectorConfig = FeatureDetectorConfig(
        msLevel = 2,
        mzTolPPM = 50.0f,
        minNbOverlappingIPs = 5,
        intensityPercentile = 0.9f,
        peakelFinderConfig = new SmartPeakelFinderConfig(5, 3, 0.66f, false, 10, false, false))
      val mzdbDetector = new MzDbFeatureDetector(mzDb, ftDetectorConfig)
      // detect peakels
      var rsIter = mzDb.getLcMsnRunSliceIterator(606.59,606.612)
      this.logger.info("iterating over run slices")
      while(rsIter.hasNext()) {
        val rs = rsIter.next()
        this.logger.info(s"RSlice header ${rs.getHeader.getNumber}, level = ${rs.getHeader.getMsLevel} " )
      }
      rsIter = mzDb.getLcMsnRunSliceIterator(606.59,606.612)
      val peakels = mzdbDetector.detectPeakels(rsIter)
      logger.info("Nb peakels detected : "+peakels.length)
      peakels
    } finally {
      mzDb.close()
    }
    
  }

}