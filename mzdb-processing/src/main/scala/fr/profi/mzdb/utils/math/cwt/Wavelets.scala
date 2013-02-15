package fr.profi.mzdb.utils.math.cwt
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.math.complex.Complex

trait WaveletInterface {
	def values() : Array[Complex]
	
	def isReal() : Boolean
}


abstract class MotherWavelet (val lowerBound:Int = -8, 
    						  val upperBound: Int =  8, 
    						  val nbPoints:Int = 1024) extends WaveletInterface{
  
   val psiXval =  (lowerBound.toDouble to upperBound.toDouble by ((upperBound - lowerBound) / nbPoints.toDouble)).toArray[Double]
   
   
   def getPsiXval() : Array[Double] = {
     psiXval.map(x => x - psiXval(0)).toArray[Double]
   }
   
  }
  




case class MexicanHat (override val lowerBound:Int = -8, 
    						  override val upperBound: Int =  8, 
    						  override val nbPoints:Int = 1024) extends MotherWavelet{
	
	def values() : Array[Complex]= {
	  
	  val inter = math.abs(lowerBound - upperBound) / nbPoints.toFloat
	  var x = lowerBound.toDouble
	  var a = new Array[Complex](nbPoints + 1)
	  var i = 0 //counter
	  while (x <= upperBound) {
	    a(i) = new Complex( 2 / math.sqrt(3) *  math.pow(math.Pi, -0.25) * (1 - math.pow(x, 2)) * math.exp(- math.pow(x, 2) / 2), 0)
	    psiXval(i) = x
	    i+=1
	    x += inter
	  }
	  a
	}
	
	def isReal() : Boolean = {
	  true
	}
}

case class Morlet(override val lowerBound:Int = -8, 
				  override val upperBound: Int =  8,
				  override val nbPoints:Int = 1024) extends MotherWavelet{
	
  
	def values() : Array[Complex]= {
			val inter = math.abs(lowerBound - upperBound) / nbPoints.toFloat
	  var x = lowerBound.toDouble
	  var a = new Array[Complex](nbPoints + 1)
	  var i = 0 //counter
	  while (x <= upperBound) {
	    a(i) = new Complex(math.sqrt(math.sqrt(2/math.Pi))*math.exp(-2/2*x*x)*math.cos(2*math.Pi*x), math.sqrt(math.sqrt(2/math.Pi))*math.exp(-2/2*x*x)*math.sin(2*math.Pi*x)) 
	    psiXval(i) = x
	    i+=1
	    x += inter
	  }
	  a
	}
	
	def isReal() : Boolean = {
	  false
	}

}


	