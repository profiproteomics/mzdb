


import fr.profi.mzdb.model.MolecularFormula
import fr.profi.mzdb.model.Elements
import scala.collection.mutable.HashMap
import fr.profi.mzdb.model.Element
import scala.sys.process._
import java.io.OutputStreamWriter
import scala.sys.process.Process
import fr.profi.mzdb.model.MercuryLauncher
import org.apache.commons.math.optimization.fitting.GaussianFitter
import org.apache.commons.math.optimization.general.LevenbergMarquardtOptimizer
import fr.profi.mzdb.algo.signal.generation.PeakelGenerator
import scala.util.Random
import fr.profi.mzdb.algo.signal.fitting.GaussianFitting
import fr.profi.mzdb.algo.signal.fitting.PeakShape
import scala.collection.mutable.ArrayBuffer

object Test extends App {
	
	def testMolecularFormula() {
		var m = MolecularFormula()
		m(Elements.H) = 34
		m(Elements.C) = 22
		assert(m.toString == "C22H34")
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
	  val fitter = new GaussianFitter(new LevenbergMarquardtOptimizer)
	  for ((a, b) <- rt.slice(0, 20).zip(intens.slice(0, 20))) {
	    println("" + a+ "\t" + b)
	    fitter.addObservedPoint(a, b toDouble)
	  }
	  val v = fitter.fit()
	  val fitted_y = rt.map(x=> v.value(x))
	  for (i <- 0 until rt.slice(0, 20).length) {
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
	  val f = new GaussianFitting(rt.map(x=>x toDouble)toArray, intens.map(x=>x toDouble)toArray,  (new ArrayBuffer[PeakShape]() += new PeakShape(1, 1, 1, 1))toArray)
	  val a = f.optimize(1000)._1
	  val inte = a.getPoint()(0)
	  val rtn = a.getPoint()(1)
	  val l = a.getPoint()(2)
	  val r = a.getPoint()(3)
	  
	  for (i <- 0 until rt.length)  {
	    if (rt(i) < rtn) 
	    	println("" + rt(i) + " \t" + inte * math.exp(-math.pow(rt(i) - rtn, 2)/ 2 * l * l))
	    else
	      println("" + rt(i) + " \t" + inte * math.exp(-math.pow(rt(i) - rtn, 2)/ 2 * r * r))
	  }
	    
	  
	}
	
	testLM
    //val (mz, intensities) = this.computeIsotopicDistribution(m, 0)
    //for ( (m, i) <- mz.zip(intensities)) {
    //  println("" + m +"\t" + i)
    //}
    //println("res" + res)
}