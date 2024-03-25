package fr.profi.mzdb.util.math.wavelet

trait IWaveletInterface[T] {
  def values(): Array[T]

  def isReal(): Boolean
}

abstract class MotherWavelet(val lowerBound: Int = -6,
                             val upperBound: Int = 6,
                             val nbPoints: Int = 256) extends IWaveletInterface[Float] {

  val psiXval = (lowerBound.toFloat until upperBound.toFloat by ((upperBound - lowerBound) / nbPoints.toFloat)).toArray[Float]
  //println("psiXval length:"+ psiXval.length)

  def getPsiXval(): Array[Float] = {
    val f = psiXval(0)
    psiXval.map(x => x - f).toArray //[Float]
  }

}

/** Second derivative of the gaussian. Du et Al. Centwave algorithm */
case class MexicanHat(override val lowerBound: Int = -6,
                      override val upperBound: Int = 6,
                      override val nbPoints: Int = 256) extends MotherWavelet {

  def values(): Array[Float] = {

    val inter = math.abs(lowerBound - upperBound) / nbPoints.toFloat
    var x = lowerBound.toFloat
    var a = new Array[Float](nbPoints )//+ 1)
    var i = 0 //counter
    while (x < upperBound) {
      a(i) = ((2.0 / (math.sqrt(3.0) * math.pow(math.Pi, -0.25))) * (1.0 - (x * x)) * math.exp(-(x * x) / 2.0)).toFloat//density normal function ? Du et al 2005
      psiXval(i) = x
      i += 1
      x += inter
    }
    a
  }

  def isReal(): Boolean = true
}

/** Ridger wavelet, equivalent to the first derivative of the gaussian. Wee et al. */
case class Ridger(override val lowerBound: Int = -6,
                  override val upperBound: Int = 6,
                  override val nbPoints: Int = 256) extends MotherWavelet {

  def values(): Array[Float] = {

    val inter = math.abs(lowerBound - upperBound) / nbPoints.toFloat
    var x = lowerBound.toFloat
    var a = new Array[Float](nbPoints + 1)
    var i = 0 //counter
    while (x <= upperBound) {
      a(i) = (-x * math.exp(-(x * x) / 2.0)).toFloat// / math.sqrt(2.0 * math.Pi) )
      psiXval(i) = x
      i += 1
      x += inter
    }
    a
  }

  def isReal(): Boolean = true
  
}
/*
case class Morlet(override val lowerBound: Int = -8,
                  override val upperBound: Int = 8,
                  override val nbPoints: Int = 1024) extends MotherWavelet {

  def values(): Array[Complex] = {
    val inter = math.abs(lowerBound - upperBound) / nbPoints.toFloat
    var x = lowerBound.toDouble
    var a = new Array[Complex](nbPoints + 1)
    var i = 0 //counter
    while (x <= upperBound) {
      a(i) = new Complex(math.sqrt(math.sqrt(2 / math.Pi)) * math.exp(-2 / 2 * x * x) * math.cos(2 * math.Pi * x), math.sqrt(math.sqrt(2 / math.Pi)) * math.exp(-2 / 2 * x * x) * math.sin(2 * math.Pi * x))
      psiXval(i) = x
      i += 1
      x += inter
    }
    a
  }

  def isReal(): Boolean = {
    false
  }

}*/


	