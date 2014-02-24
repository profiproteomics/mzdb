package fr.profi.mzdb.algo.signal.distortion

import scala.util.Random

trait IErrorGenerator {
  
  val stdDevCoeff = 0.0
  protected val _randomGenerator = new Random()
  
  def nextRandomError( value: Double )

}

case class FixedErrorGenerator( override val stdDevCoeff: Double ) extends IErrorGenerator {
  
  def nextRandomError( value: Double ) = {
    _randomGenerator.nextGaussian() * stdDevCoeff * value
  }  

}

case class BinnedErrorGenerator() {

}

case class LinearErrorGenerator() {

}

case class NonLinearErrorGenerator() {

}


