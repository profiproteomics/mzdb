package fr.profi.mzdb.algo.signal.detection

import fr.profi.mzdb.model.IPeakelData
import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
trait IPeakelFinder {
  
  def findPeakelsIndices(peaks: Seq[Peak] ): Array[(Int,Int)] = {
    findPeakelsIndices( peaks.map( p => (p.getLcContext.getElutionTime , p.getIntensity.toDouble) ).toArray )
  }
  
  def findPeakelsIndices(peakel: IPeakelData): Array[(Int,Int)] = {
    findPeakelsIndices( peakel.getElutionTimeIntensityPairs.toArray.map( p => (p._1 , p._2.toDouble) ).toArray )
  }
  
  def findPeakelsIndices(rtIntPairs: Array[(Float,Double)] ): Array[(Int,Int)]

}