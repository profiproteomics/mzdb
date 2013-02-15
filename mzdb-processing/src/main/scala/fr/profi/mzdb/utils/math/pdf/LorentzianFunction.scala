package fr.profi.mzdb.utils.math.pdf

/**
 * @author David Bouyssie
 *
 */
object LorentzianFunction {

  /*
////// Load essentials here, other modules loaded on demand later
  use Carp
  use Math::Trig ':pi'
  
  ////// Define attributes
  has 'x_zero' = ( isa = 'Num', is = 'ro', required = 1 )
  has 'y_max' = ( isa = 'Num', is = 'ro', required = 1 )
  has 'hwhm' = ( isa = 'Num', is = 'ro', required = 1, )
  
  ////// Define cached attributes which help to speed up computations
  has '_hwhm_square' = ( is = 'ro', required = 0, builder = '_build_hwhm_square', lazy = 1, init_arg = undef )
  has '_theo_y_max' = ( isa = 'Num', is = 'ro', required = 0, builder = '_build_theo_y_max', lazy = 1,  init_arg = undef )
    
  def _buildHwhmSquare (): Unit = {
    val ( this ) = _
    return this.hwhm ** 2
  }
  
  def _buildTheoYMax (): Unit = {
    val ( this ) = _
    return this._getTheoYValue( this.xZero )
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Method: get_y_value()
  //
  def getYValue(  ): Unit = {
    
    val theoY = this._getTheoYValue( x ) 
    val scaledY = theoY * this.yMax / this._theoYMax
    
    return scaledY

  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Method: _get_theo_y_value()
  //
  def _getTheoYValue( x: Double ): Unit = {
    
    ////// Retrieve lorentzian properties
    val hwhm = this.hwhm
    val hwhmSquare = this._hwhmSquare
    val xZero = this.xZero
    
    val theoY = (1/pi) * (hwhm /( (x - xZero)**2 + hwhmSquare) )
    
    return theoY

  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Method: get_half_width()
  //
  def getHalfWidth( relativeHeight: Double ): Unit = {
  
    ////// Retrieve lorentzian properties
    val hwhm = this.hwhm
    val hwhmSquare = this._hwhmSquare
    val theoYMax = this._theoYMax
    val y = relativeHeight * theoYMax
    //die "y value (y) is out of range (0 < y <= yMax )" if y > yMax or y <= 0
    
    val halfWidth = sqrt( hwhm / ( y * pi ) - hwhm**2 )
    val scaledHalfWidth = halfWidth * this.yMax / theoYMax
    
    return scaledHalfWidth
  }
  
  ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  // Method: get_area()
  //
  method get_area() {
  
    ////// TODO
  }
  */
}