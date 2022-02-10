package fr.profi.mzdb.algo.signal.filtering

import fr.profi.mzdb.model.Peakel
import org.junit.Test

class ISignalSmootherTest {

  @Test
  def smoothPeakel() = {

    val spectrumIds = Array(1000L,1001,1002,1003,1004,1005,1006,1007,1008,1009,1010)
    val elutionTimes = Array(5000.0f, 5001.0f, 5002.0f, 5003.0f, 5004.0f, 5005.0f, 5006.0f, 5007.0f, 5008.0f, 5009.0f, 5010.0f)
    val mzValues = Array(800.0, 800.0,800.0,800.0,800.0,800.0,800.0,800.0,800.0, 800.0, 800.0)
    val intensityValues = Array(9549.227f, 10418.323f,  9897.820f, 10132.916f, 10163.847f, 10059.315f, 10189.407f,  9914.034f, 10223.851f, 10188.290f, 9878.704f)

    val p = new Peakel(
      id = Peakel.generateNewId(),
      spectrumIds = spectrumIds,
      elutionTimes = elutionTimes,
      mzValues = mzValues,
      intensityValues = intensityValues
    )

    val smoother = new SavitzkyGolaySmoother(SavitzkyGolaySmoothingConfig(nbPoints = 3, polyOrder = 2, iterationCount = 2))
    val smoothedPeakel = smoother.smoothPeakel(p)

    val f = true
  }
}
