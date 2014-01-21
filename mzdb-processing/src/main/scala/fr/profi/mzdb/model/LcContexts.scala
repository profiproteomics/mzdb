package fr.profi.mzdb.model

import scala.beans.BeanProperty

/** The Class ElutionTimeContext.
 * @author David Bouyssie
 *
 */
case class ElutionTimeContext( @BeanProperty elutionTime: Float ) extends ILcContext {

    /* (non-Javadoc)
   * @see fr.profi.mzdb.model.ILcContext#getScanId()
   */
  def getScanId = 0

}

/** The Class ScanContext.
 * @author David Bouyssie
 *
 */
case class ScanIdContext( @BeanProperty scanId: Int ) extends ILcContext {

  /* (non-Javadoc)
   * @see fr.profi.mzdb.model.ILcContext#getElutionTime()
   */
  def getElutionTime = 0

}