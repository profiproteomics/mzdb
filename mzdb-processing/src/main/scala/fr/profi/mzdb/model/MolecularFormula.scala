package fr.profi.mzdb.model

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

//import java.io.InputStream
//import scala.collection.generic.Shrinkable

case class Element(
  val abrev: String,
  val massMonoistopic: Double,
  val isotopesMasses: Array[Double] = null,
  val isotopesAbundance: Array[Double] = null,
  val fullName: String = "")

// TODO: check elements table in python lib
object Elements {

  val C = Element("C", 12d, null, null, "Carbon")
  val H = Element("H", 1.007825035d, null, null, "Hydrogen")
  val N = Element("N", 14.003074d, null, null, "Azote")
  val O = Element("O", 15.99491463d, null, null, "Oxygen")
  val S = Element("S", 31.9720707d, null, null, "Sulfur")
}

object MolecularFormula {
  def apply(ts: (Element, Float)*): MolecularFormula = new MolecularFormula(new HashMap[Element, Float] ++ ts.toMap)
  def apply(): MolecularFormula = new MolecularFormula(new HashMap[Element, Float]())
  //def apply(t:HashMap[Element, Float]) : MolecularFormula = new MolecularFormula(t)
}

class MolecularFormula(private var internalMap: HashMap[Element, Float]) {
  def apply(key: Element): Float = internalMap(key)
  def update(key: Element, v: Float): Unit = internalMap(key) = v

  override def toString(): String = {
    var compAsStrings = internalMap.map { case (element, nb) => s"${element.abrev}(${nb.toInt.toString})" }
    compAsStrings.mkString(" ")
  }

  def &(): HashMap[Element, Float] = internalMap
}

class AveragineComputer(val averagine: MolecularFormula, val averageMass: Float)  {
  
//  var averagine = defaultAveragine
//  var averageMass = defaultMass

  /*
  def setAveragine(averagine_ : Map[String, Float]) {
    averagine = averagine_
    //mass = caclMassAveragine()
  }*/
  
  def computeAveragine(f: Feature): MolecularFormula = {
    computeAveragine(f.getMz, f.getCharge)
  }
  
  def computeAveragine(mz: Double, charge: Int): MolecularFormula = {
    computeAveragine(mz * charge)
  }

  def computeAveragine(mass: Double): MolecularFormula = {
    val k = mass / averageMass //nb of overagine

    // Use to calculate the number of hydrogen
    var diff = averagine.& map { case (element, nb) => (element, 0d) } //toMap
    var nbElements = averagine.& map { case (element, nb) => (element, k * averagine(element)) }
    
    /*var nbH = 0
    nbElements.map { case (element, nb) => 
      nbH -= math.floor(math.round(nbElements(element)).toDouble - nbElements(element) * element.massMonoistopic).toInt
    }*/
    
    var finalElements = nbElements.map { case (element, nb) => (element, math.round(nb) toFloat) }
    //finalElements(Elements.H) = nbH.toFloat

    new MolecularFormula(finalElements)
  }
}

object AveragineComputer extends AveragineComputer(MolecularFormula(
    Elements.C -> 4.9494f,
    Elements.H -> 7.8306f,
    Elements.N -> 1.3602f,
    Elements.O -> 1.4720f,
    Elements.S -> 0.0378f
  ), 111.0840f)

/*
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
*/


