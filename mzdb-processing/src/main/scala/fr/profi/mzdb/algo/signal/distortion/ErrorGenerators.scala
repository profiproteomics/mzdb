package fr.profi.mzdb.algo.signal.distortion

import scala.util.Random

trait IErrorGenerator {
  
  protected val _randomGenerator = new Random()
  
  def nextError( value: Double ): Double

}

class GaussianAbsoluteErrorGenerator( val error: Double, isOscillating: Boolean = false ) extends IErrorGenerator {
  
  var lastSign = 1
  
  def nextError( value: Double ): Double = {
    val finalError = _randomGenerator.nextGaussian() * error
    if( isOscillating == false ) finalError
    else {
      lastSign *= -1
      lastSign * finalError.abs
    }
  }

}

class GaussianRelativeErrorGenerator( val stdDevCoeff: Double, isOscillating: Boolean = false ) extends IErrorGenerator {
  
  var lastSign = 1
  
  def nextError( value: Double ): Double = {
    val finalError = _randomGenerator.nextGaussian() * stdDevCoeff * value
    if( isOscillating == false ) finalError
    else {
      lastSign *= -1
      lastSign * finalError.abs
    }
  }

}

class OscillatingAbsoluteErrorGenerator( val error: Double ) extends IErrorGenerator {

  var lastSign = 1
  
  def nextError( value: Double ): Double = {
    lastSign *= -1
    lastSign * error
  }

}

class OscillatingRelativeErrorGenerator( val stdDevCoeff: Double ) extends IErrorGenerator {
  
  var lastSign = 1
  
  def nextError( value: Double ): Double = {
    lastSign *= -1
    lastSign * stdDevCoeff * value
  }

}



/*
class BinnedErrorGenerator() {

}

class LinearErrorGenerator() {

}

class NonLinearErrorGenerator() {

}
*/

