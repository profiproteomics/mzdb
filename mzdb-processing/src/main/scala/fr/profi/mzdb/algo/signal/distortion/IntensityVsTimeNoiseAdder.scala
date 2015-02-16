package fr.profi.mzdb.algo.signal.distortion

class IntensityVsTimeNoiseAdder( errorGenerator: IErrorGenerator ) {
  
  def addNoise( rtIntPairs: Array[(Float,Double)] ): Array[(Float,Double)] = {
    
    for( (rt,intensity) <- rtIntPairs ) yield {
      val intensityPlusNoise = intensity + errorGenerator.nextError(intensity)
      rt -> intensityPlusNoise
    }
    
  }

}