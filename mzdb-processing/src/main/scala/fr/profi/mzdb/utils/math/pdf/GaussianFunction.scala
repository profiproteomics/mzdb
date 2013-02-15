package fr.profi.mzdb.utils.math.pdf

import math.{exp,log,sqrt}

/**
 * @author David Bouyssie
 *
 */
object GaussianFunction extends IProbabilityFunction {
  
  def apply( xZero: Double, yMax: Double, sigma: Double ) = new GaussianModel(xZero,yMax,sigma)
  
  val sigmaToFwhmFactor = 2 * sqrt( 2 * log(2) )

  def yMax( xZero: Double, sigmaSquared: Double, x: Double, y: Double ): Double = {
    require( sigmaSquared > 0 )
    
    // Compute normalized x value
    val nx = x - xZero
    
    // Compute and return yMax
    y / exp( - ( nx*nx ) /( 2 * sigmaSquared) )
  }  

  def y( xZero: Double, sigmaSquared: Double, yMax: Double, x: Double ): Double = {
    require( sigmaSquared > 0 )
    
    // Compute normalized x value
    val nx = x - xZero
    
    // Compute and return y
    yMax * exp( - ( nx*nx ) /( 2 * sigmaSquared) )
  }
  
  
  def sigma( width: Double, relativeHeight: Double ): Double = {
    require( width > 0 )
    require( relativeHeight > 0 && relativeHeight <= 1 )
    
    width / ( 2* sqrt(- 2 * log(relativeHeight) ) )    
  }

  def width( sigma: Double, relativeHeight: Double): Double = {
    require( sigma > 0 )
    require( relativeHeight <= 1 )
    
    sigma * ( 2 * sqrt(- 2 * log(relativeHeight) ) )
  }
  
  def fwhm( sigma: Double ) = sigma * sigmaToFwhmFactor
  
}

class GaussianModel (
  val xZero: Double,
  var yMax: Double,
  val sigma: Double
) extends IProbabilityFunctionInstance {
  
  def this( xZero: Double, yMax: Double, width: Double, relativeHeight: Double ) = {
    this( xZero: Double, yMax: Double, GaussianFunction.sigma(width,relativeHeight) )
  }
  
  
  lazy val fwhm: Double = GaussianFunction.fwhm( sigma )
  private lazy val _sigmaSquared = sigma * sigma

  
  def optimizeYMax( x: Double, y: Double ): Unit = {
    this.yMax = GaussianFunction.yMax( xZero, _sigmaSquared, x, y )
  }


  def getYValue( x: Double ): Double = {
    GaussianFunction.y( xZero, _sigmaSquared, yMax, x )
  }

  def getWidth( y: Double ): Double = {
    GaussianFunction.width( sigma, y/yMax )
  }

  def getArea() {
  
    ////// TODO
  }
  

  def calcRmsd( dataPoints: Array[Any] ): Unit = {
  
    ////// TODO
  }

  
}