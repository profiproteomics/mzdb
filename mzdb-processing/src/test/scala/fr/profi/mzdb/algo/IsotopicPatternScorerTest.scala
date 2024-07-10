package fr.profi.mzdb.algo

import fr.profi.mzdb.model.SpectrumData
import org.junit.{Ignore, Test}

class IsotopicPatternScorerTest {

  @Test
  def testIsotopicPrediction() = {

    val mzList = Array(
      780.6512026096515,
      780.714133666967,
      780.9171988158956,
      781.0486749239027,
      781.382140629136,
      781.4185277581491,
      781.7248984478958,
      781.9208374435635,
      782.389877361993,
      782.4197550881104,
      782.9092877298076,
      783.4105573118388,
      783.8678134404207,
      783.913018578805,
      784.3691609078711,
      784.4065076903959,
      784.415159992602,
      784.9083961159358,
      785.4116733760561,
      785.9118976167387,
      786.4307000131101,
      786.9350081354768,
      787.4355355129171,
      788.4337450909248,
      788.9258970448492,
      789.1021835447112,
      789.4292058574449)
    val intensityList = Array(
      518417.22f,
      1365277.6f,
      1.2540963E7f,
      1029656.8f,
      694046.56f,
      1.1715607E7f,
      501973.62f,
      5517418.5f,
      373841.78f,
      1706445.6f,
      3256126.8f,
      2688709.8f,
      982912.25f,
      1142823.8f,
      914819.25f,
      751778.44f,
      741147.06f,
      609765.25f,
      3083592.8f,
      2492711.0f,
      1361910.9f,
      1496466.5f,
      610764.3f,
      543893.2f,
      890475.7f,
      503538.1f,
      684743.25f)
    val spectrumData = new SpectrumData(mzList, intensityList)


    val predictions = DotProductPatternScorer.calcIsotopicPatternHypotheses(spectrumData, 785.41167, 5)
    predictions.length
    ()
  }

  @Ignore
  def testFromSpectrumData() =  {

    val mzDb = new MzDbReader("C:/Local/bruley/Tests/MGF/Data/mzdb-converter-1.2.1/Xpl1_002787.mzdb", true)
    mzDb.enableScanListLoading()
    mzDb.enablePrecursorListLoading()
    val spectrum = mzDb.getSpectrum(122873)
    val putativePatterns = DotProductPatternScorer.calcIsotopicPatternHypotheses(spectrum.getData, 1322.7196, 10.0)
    val bestPattern = DotProductPatternScorer.selectBestPatternHypothese(putativePatterns)
    val spectrum2 = mzDb.getSpectrum(122930)
    val putativePatterns2 = DotProductPatternScorer.calcIsotopicPatternHypotheses(spectrum2.getData, 1322.7148, 10.0)
    val bestPattern2 = DotProductPatternScorer.selectBestPatternHypothese(putativePatterns2)
    mzDb.close()

//    val mzDb = new MzDbReader("C:/Local/bruley/Tests/MGF/Data/mzdb-converter-1.2.1/Xpl1_002787.mzdb", true)
//    mzDb.enableScanListLoading()
//    mzDb.enablePrecursorListLoading()
//    val spectrum = mzDb.getSpectrum(122873)
//    val putativePatterns = DotProductPatternScorer.calcIsotopicPatternHypotheses(spectrum.getData, 1322.2038, 10.0)
//    mzDb.close()

  }
}
