package fr.profi.mzdb.featuredb

import fr.profi.mzdb.model.{Feature, Peakel}
import org.junit.{Ignore, Test}

import java.io.File

class FeatureDbWriterTest {

  @Ignore
  @Test
  def initDbTest() = {

    val testFile = new File("test.sqlite")
    if (testFile.exists()) { testFile.delete() }
    testFile.deleteOnExit()
    
    val featureFileConnection = FeatureDbWriter.initFeatureStore(testFile)


    val p1 = Peakel(id = 1,
                    spectrumIds = Array(1,2),
                    elutionTimes = Array(2.0f, 3.0f),
                    mzValues = Array(200.0, 200.02),
                    intensityValues = Array(1000, 12000))
    val p2 = p1.copy(id = 11, mzValues = Array(200.5, 200.52))

    val ft = Feature(id = 1, mz = 200.0, charge = 2, indexedPeakels = Array((p1, 1), (p2, 2)), isPredicted = false, ms2SpectrumIds = Array(2, 3))

    FeatureDbWriter.storeFeatures(featureFileConnection, Array(ft));
    FeatureDbWriter.storeFeatureReferences(featureFileConnection, Array((ft.id, 1001L)));


  }


}
