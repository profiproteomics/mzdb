import fr.profi.mzdb.model.MolecularFormula
import fr.profi.mzdb.model.Elements
import scala.collection.mutable.HashMap
import fr.profi.mzdb.model.Element
import scala.sys.process._
import java.io.OutputStreamWriter
import scala.sys.process.Process
import fr.profi.mzdb.model.MercuryLauncher
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import fr.profi.mzdb.algo.signal.generation.PeakelGenerator
import scala.util.Random
import fr.profi.mzdb.algo.signal.fitting.PeakShape
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.algo.signal.fitting.ParabolaFitter
import org.apache.commons.math.optimization.fitting.GaussianFunction
import fr.profi.mzdb.utils.math.pdf.ParametricParabola
import fr.profi.mzdb.algo.signal.fitting.GaussFitter
import fr.profi.mzdb.utils.math.pdf.ParametricParabola
import org.apache.commons.math.optimization.fitting.PolynomialFitter
import org.apache.commons.math.analysis.polynomials.PolynomialFunction
import org.apache.commons.math.optimization.general.GaussNewtonOptimizer
import org.junit.Assert

object Test extends App {
	
	def testMolecularFormula() {
		var m = MolecularFormula()
		m(Elements.H) = 34
		m(Elements.C) = 22
		Assert.assertTrue(m.toString == "C22H34")
		println(m.toString())
	}
	
	def testFitting() {
	  val peakel = PeakelGenerator.generate(525.12, 2000, 10000, 30, 1, 50)
	  val peaks = peakel.getDefinedPeaks
	  val noise = Seq.fill(1000)(Random.nextInt(2500))
	  var count = 0
	  var (rt, intens) = (peaks.map(x=> x.getLcContext().getElutionTime()), peaks.map{x=>x.getIntensity })
	  for (i <- 0 until intens.length) {
	    if (i == 4)
	      intens(i) += 5000
	    else
	    	intens(i) += noise(i)
	  }
	  val fitter = new org.apache.commons.math.optimization.fitting.GaussianFitter(new LevenbergMarquardtOptimizer)
	  for ((a, b) <- rt.zip(intens)) {
	    println("" + a+ "\t" + b)
	    fitter.addObservedPoint(a, b toDouble)
	  }
	  val v = fitter.fit()
	  println("A:" + v.getA())
	  val fitted_y = rt.map(x=> v.value(x))
	  for (i <- 0 until rt.length) {
	    println("" + rt(i) + "\t" + intens(i) +"\t" + fitted_y(i))
	  }
	  
	  //fitter.addObservedPoint(x, y)
	}
	
	def testLM() {
	  val peakel = PeakelGenerator.generate(525.12, 2000, 10000, 30, 1, 50)
	  val peaks = peakel.getDefinedPeaks
	  val noise = Seq.fill(1000)(Random.nextInt(2500))
	  var count = 0
	  var (rt, intens) = (peaks.map(x=> x.getLcContext().getElutionTime()), peaks.map{x=>x.getIntensity })
	  for (i <- 0 until intens.length) {
	    if (i == 4)
	      intens(i) += 5000
	    else
	    	intens(i) += noise(i)
	  }
	  val f = new PolynomialFitter(2, new LevenbergMarquardtOptimizer)
	  for ((a, b) <- rt.zip(intens)) {
	    f.addObservedPoint(1d, a, b toDouble)
	  }
	  val a = f.fit()
	 
	  for (i <- 0 until rt.length)  {
		//val xrt = rt(i) - rtn
	    //if (rt(i) < rtn) {
	      //var y =  shift + inte * math.exp(- xrt *xrt / (2 * sigmal * sigmal))
	      println("" + rt(i) + " \t" + intens(i) + "\t"+ a.value(rt(i)))
	    //}else {
	      //var y = shift + inte * math.exp(- xrt * xrt / (2 * sigmar * sigmar))
	    //  println("" + rt(i) + " \t" + intens(i) + "\t" +  v2.value(rt(i)))
	    //}
	  }
	}
	
	
	def testPolyFit() {
	  val peakel = PeakelGenerator.generate(525.12, 2000, 10000, 30, 1, 50)
	  val peaks = peakel.getDefinedPeaks
	  val noise = Seq.fill(1000)(Random.nextInt(2500))
	  var count = 0
	  var (rt, intens) = (peaks.map(x=> x.getLcContext().getElutionTime()), peaks.map{x=>x.getIntensity })
	  for (i <- 0 until intens.length) {
	    if (i == 4)
	      intens(i) += 5000
	    else
	    	intens(i) += noise(i)
	  }
	  val f = new GaussFitter(rt.map(x=>x toDouble)toArray, intens.map(x=>x toDouble)toArray)
	  val a = f.optimize()
	  val p = f.getPeaks
	  val peak = p(0)
	  val y = peak.getFittedY(rt.map(x=> x toDouble).toArray)
	  var count_ = 0
	  for ((rt_, intens_) <- rt.zip(y) ) {
	      println("" + rt_ + " \t" + intens(count_) + "\t" +  intens_)
	      count_ += 1
	    }
	  }
	
	
	testPolyFit
    //val (mz, intensities) = this.computeIsotopicDistribution(m, 0)
    //for ( (m, i) <- mz.zip(intensities)) {
    //  println("" + m +"\t" + i)
    //}
    //println("res" + res)
}