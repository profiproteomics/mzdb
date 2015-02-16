/*package fr.profi.mzdb.utils.math

import scala.collection.immutable.HashMap
import scala.beans.BeanProperty
import fr.profi.mzdb.utils.math.wavelet.WaveletUtils

/** UWT equivalent to SWT*/
// TODO: implement this smoother in the filtering package
object UWTSmoother extends Smoother {
  
  @BeanProperty var smoothMethod = "Soft"
  @BeanProperty var nbIter = 6

  def smooth(data:Array[Double]): Array[Double] = {
    val coeffs = WaveletUtils.swt(data) //, args.getOrElse("iter", 6))
    WaveletUtils.denoiseSoft(coeffs) //, 6)
    WaveletUtils.iswt(coeffs) // , 6)
  }
}

}*/