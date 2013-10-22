package fr.profi.mzdb.utils.math

import math.{pow,sqrt}

/*
* Object in scala for calculating cosine similarity and Pearson correlation coefficent
* More information:
* http://en.wikipedia.org/wiki/Cosine_similarity
* http://en.wikipedia.org/wiki/Pearson%27s_correlation_coefficient
*/
object VectorSimilarity {
  
  /** The dot function.
  * Returns the dot operation of the 2 arrays
  * => f(a[0],b[0]) + f(a[1],b[1]) + ... + f(a[n-1],b[n-1])
  */
  def dot[T](as: Iterable[T], bs: Iterable[T])(f: (T,T) => Double): Double =
    (for ((a, b) <- as zip bs) yield f(a, b)) sum
  
  /** The dotProduct function.
  * Returns the dot product of the 2 double arrays
  * => (a[0]*b[0]) + (a[1]*b[1])+ ... + (a[n-1]*b[n-1])
  */
  def dotProduct(vector1: Array[Double], vector2: Array[Double]) = dot(vector1, vector2)(_*_)
  def dotProduct(vector1: Array[Float], vector2: Array[Float]) = dot(vector1, vector2)(_*_)
  def dotProduct(vector1: Array[Int], vector2: Array[Int]) = dot(vector1, vector2)(_*_)
  
  /**
  * Return the magnitude of an array
  * We multiply each element, sum it, then square root the result.
  */
  def magnitude(vector: Array[Double]): Double = {
    sqrt( vector.fold(0.0) { (x2Sum,x) => x2Sum + (x * x) } )
  }
  
  /**
  * This method takes 2 equal length arrays of Double
  * It returns a double representing similarity of the 2 arrays
  * 0.9925 would be 99.25% similar
  * (x dot y)/||X|| ||Y||
  */
  def cosineSimilarity(vector1: Array[Double], vector2: Array[Double]): Double = {
    require(vector1.size == vector2.size, "vectors must have same length" )
    dotProduct(vector1, vector2)/(magnitude(vector1) * magnitude(vector2))
  }
  
  /**
   * Calculate uncentered correlation coefficient (cosine similarity)
   * Fastest implementation than the previous cosineSimilarity one (one single loop).
   * 
   * @param values1     first points
   * @param values2     second points
   * @return Uncentered correlation coefficient
   */
  def uncenteredCorrelation(vector1: Array[Double], vector2: Array[Double] ): Double = {
    require(vector1.size == vector2.size, "vectors must have same length" )
    
    val N = vector1.length
    
    var sum_coproduct = 0.0
    var sum_sq_x = 0.0
    var sum_sq_y = 0.0
    
    var i = -1
    while( i < N ) {
      i += 1
      val x = vector1(i)
      val y = vector2(i)
      
      sum_coproduct += x * y
      sum_sq_x += x * x
      sum_sq_y += y * y
    }
    
    sum_coproduct/sqrt(sum_sq_x * sum_sq_y)  
  }
  
  /**
   * Calculate the coefficient of determination
   * @param values1     first points
   * @param values2     second points
   * @return Coefficient of determination
   */
  def squaredPearsonCorrelation(vector1: Array[Float], vector2: Array[Float]): Double = {
    pow( pearsonCorrelation(vector1,vector2), 2 )
  }
  
  def squaredPearsonCorrelation(vector1: Array[Double], vector2: Array[Double]): Double = {
    pow( pearsonCorrelation(vector1,vector2), 2 )
  }
  
  /**
   * Calculate Pearson's product-moment correlation coefficient
   * Code adapted from Wikipedia:
   * http://en.wikipedia.org/wiki/Pearson%27s_correlation_coefficient
   * 
   * @param values1     first points
   * @param values2     second points
   * @return Pearson's correlation coefficient
   */
  def pearsonCorrelation(vector1: Array[Float], vector2: Array[Float] ): Double = {
    pearsonCorrelation( vector1.map(_.toDouble), vector2.map(_.toDouble) )
  }
  
  def pearsonCorrelation(vector1: Array[Double], vector2: Array[Double] ): Double = {
    
    val N = vector1.length
    require( N == vector2.length, "vectors must have same length" )
    require( N > 0, "vectors must contain data" )    
    
    var sum_sq_x = 0.0
    var sum_sq_y = 0.0
    var sum_coproduct = 0.0
    var mean_x = vector1(0)
    var mean_y = vector2(0)
    
    for( i <- 2 to N ) {
      val sweep = (i-1.0)/i
      val delta_x = vector1(i-1)-mean_x;
      val delta_y = vector2(i-1)-mean_y;
      sum_sq_x += delta_x * delta_x * sweep;
      sum_sq_y += delta_y * delta_y * sweep;
      sum_coproduct += delta_x * delta_y * sweep;
      mean_x += delta_x / i;
      mean_y += delta_y / i;
    }
    
    val pop_sd_x = sqrt( sum_sq_x / N )
    val pop_sd_y = sqrt( sum_sq_y / N )
    val cov_x_y = sum_coproduct / N
    
    cov_x_y / (pop_sd_x*pop_sd_y)
  }
  
  /**
  * This method takes 2 equal length arrays of Double
  * It returns a double representing the RSMD between the 2 arrays
  * http://en.wikipedia.org/wiki/Root-mean-square_deviation
  * 
  */
  def rmsd(vector1: Array[Double], vector2: Array[Double]): Double = {
    require(vector1.size == vector2.size, "vectors must have same length" )
    
    val sumSquare = (vector1, vector2).zipped.map( (x,y) =>
      math.pow(y - x, 2)
    ).sum
    
    math.sqrt(sumSquare / vector1.size)
  }

}