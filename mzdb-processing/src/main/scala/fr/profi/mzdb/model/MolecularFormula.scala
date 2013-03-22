package fr.profi.mzdb.model

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.sys.process.BasicIO
import java.io.InputStream
import scala.collection.generic.Shrinkable


case class Element (val abrev :String,
					val massMonoistopic : Double,
					val isotopesMasses : Array[Double] = null,
					val isotopesAbundance : Array[Double] = null, 
					val fullName : String ="")

object Elements{

  val C = Element("C", 12d, null, null, "Carbon")
  val H = Element("H", 1.007825035d, null, null, "Hydrogen")
  val N = Element("N", 14.003074d, null, null, "Azote")
  val O = Element("O", 15.99491463d, null, null, "Oxygen")
  val S = Element("S", 31.9720707d, null, null, "Sulfur")
}

object MolecularFormula {
  def apply(ts: (Element, Float)*): MolecularFormula = new MolecularFormula(new HashMap[Element, Float] ++ ts.toMap)
  def apply() : MolecularFormula = new MolecularFormula(new HashMap[Element, Float]())
  //def apply(t:HashMap[Element, Float]) : MolecularFormula = new MolecularFormula(t)
}

 class MolecularFormula (private var internalMap : HashMap[Element, Float]) extends MercuryLauncher {
  def apply(key:Element) : Float = internalMap(key)
  def update(key:Element, v: Float) : Unit = internalMap(key) = v
  
  override def toString() : String = {
    var s = ""
    internalMap map { case (element, nb) => s += element.abrev + nb.toInt.toString}
    s
  }
  
  def &() : HashMap[Element, Float] = internalMap
}
					
object AveragineComputer {
  val defaultAveragine =  MolecularFormula( Elements.C -> 4.9384f, 
		  								    Elements.H ->7.7577f, 
		  								    Elements.N ->1.3577f, 
		  								    Elements.O->1.4773f, 
		  								    Elements.S ->0.0417f) 
  val defaultMass = 111.1254f
  var averagine = defaultAveragine
  var mass = defaultMass
  
  /*
  def setAveragine(averagine_ : Map[String, Float]) {
    averagine = averagine_
    //mass = caclMassAveragine()
  }*/
  
  def computeAveragine( f : Feature) : MolecularFormula  = {
    val k = f.getMz * f.getCharge / mass //nb of overagine
    
    //use to calculate the number of hydrogen
    var diff =  averagine.& map{ case (element, nb) => (element, 0d) } //toMap
    var nbElements =  averagine.& map{ case(element, nb) => (element, k * averagine(element)) }
    var nbH = 0
    nbElements map { case (element, nb) => nbH -= math.floor(math.round(nbElements(element)).toDouble - nbElements(element)  * element.massMonoistopic).toInt }
    var finalElements = nbElements.map{ case (element, nb) => (element, math.round(nb) toFloat) }
    finalElements.update(Elements.H, finalElements.getOrElse(Elements.H, 0f) + nbH.toFloat)
    //var mass = 0d
    //finalElements map {case (element, nb) => mass += element.massMonoistopic * nb}
     new MolecularFormula(finalElements)
  }
}




trait MercuryLauncher {
  def computeIsotopicDistribution(mf :MolecularFormula, charge :Int = 0) : Pair[Array[Double], Array[Float]] = {
    import sys.process._
    var lines  = new ArrayBuffer[String]()
    val p = Process("D:\\Utilisateurs\\Marc\\user_data\\Desktop\\emass\\emass.exe")
    val io = new ProcessIO( in => { in.write (mf.toString getBytes "UTF-8"); in.close},
        				     out => { scala.io.Source.fromInputStream(out).getLines.foreach(lines += _) },
        				     err => {scala.io.Source.fromInputStream(err).getLines.foreach(println)})	
    p run io
    val i = p.!
	if (i != 0)
	  throw new Exception("mercury Program failed")
    //println("lines lenght:" + lines.length)
	val mass = new ArrayBuffer[Double]
    val relIntensities = new ArrayBuffer[Float]
 
    //parse output
    for (i  <- 1 until lines.length) {
      val splitted = lines(i).split(" ")
      val (m, in) = (splitted(0).toDouble, splitted(1) toFloat)
      mass += m; relIntensities += in
    }
    Pair[Array[Double], Array[Float]](mass.toArray, relIntensities.toArray)
  }

}



