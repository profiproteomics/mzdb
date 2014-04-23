package fr.profi.mzdb.algo.signal.detection

import fr.profi.mzdb.model.Peak

/**
 * @author David Bouyssie
 *
 */
trait IPeakelFinder {
  
  def findPeakelsIndexes(peaks: Seq[Peak] ): Array[Tuple2[Int,Int]]

}