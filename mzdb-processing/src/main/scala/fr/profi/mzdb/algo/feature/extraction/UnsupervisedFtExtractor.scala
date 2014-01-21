package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.MzDbReader
import scala.beans.BeanProperty

/**
 * @author David Bouyssie
 *
 */


class UnsupervisedFtExtractor(@BeanProperty reader: MzDbReader, 
							  @BeanProperty nbConsecutiveScanMin: Int = 5,
							  @BeanProperty nbConsecutiveScanMax: Int = 100) {
  // allow

}