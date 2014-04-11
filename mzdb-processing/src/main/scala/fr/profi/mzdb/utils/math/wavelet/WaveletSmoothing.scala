package fr.profi.mzdb.utils.math.wavelet

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Peak
import java.io.File
import fr.profi.mzdb.algo.signal.detection.WaveletPeakelFinder
import fr.profi.mzdb.algo.signal.detection.Method
import fr.profi.mzdb.algo.signal.detection.BasicPeakelFinder
object WaveletSmoothing extends App {
  
  
  /*val mzDbReader =new MzDbReader(new File("D:\\LCMS\\raw_files\\OENYD100205_16_medium.raw.mzdb"), true)
  val xic = mzDbReader.getXIC(411.5, 412.5, 1, MzDbReader.XicMethod.MAX)
  xic.foreach(x => println(x.getLcContext().getElutionTime()+ "\t" + x.getIntensity()))
  val y_data = xic.map(_.getIntensity().toDouble)
  val x_data = xic.map(_.getLcContext().getElutionTime())
  /*var coefficients = WaveletUtils.swt(y_data, 6)
  WaveletUtils.denoiseSoft(coefficients)
  val smoothed_y_data = WaveletUtils.iswt(coefficients)
  x_data.zip(smoothed_y_data).zip(y_data).foreach( x => println(x._1._1 + "\t" + x._1._2 + "\t" + x._2));*/
  /*var scales = (5f to 64f by 1f) toArray
  var wavelet = MexicanHat()*/
  //val coeffs = WaveletUtils.cwt(y_data, wavelet, scales)
  val slicedPeaks = xic.slice(800, 1000)
  println("minSLicesPeaks: "+  slicedPeaks(0).getLcContext().getElutionTime())
  var waveletPeakelFinder = new WaveletBasedPeakelFinder(slicedPeaks)
  val peakels = waveletPeakelFinder.findCwtPeakels(Method.Coombes)
  //val peakelsIndexes = BasicPeakelFinder.findPeakelsIndexes(xic)
  peakels.foreach(x=> println(x.xMax + "\t" + x.intensityMax))
  //val 
  //coeffs.last.zip(x_data).foreach(x=>println(x._2 + "\t" + x._1))*/
  val a = new Peak(0,0)
  val b = new Peak(0, 0)
  var h = new collection.immutable.HashMap[Peak, Int]()
  h += Pair(a, 0)
  h += Pair(b, 1)
  println("" + h(a))
  println(""+ h(b))
  
}