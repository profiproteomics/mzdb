package fr.profi.mzdb.model

import scala.beans.BeanProperty

/** 
 * The Class ElutionTimeContext.
 * @author David Bouyssie
 *
 */
case class ElutionTimeContext( @BeanProperty elutionTime: Float ) extends ILcContext {

  /**
   * @see fr.profi.mzdb.model.ILcContext#getSpectrumId()
   */
  def getSpectrumId = 0

}

/**
 * The Class SpectrumContext.
 * @author David Bouyssie
 *
 */
case class SpectrumIdContext( @BeanProperty spectrumId: Long ) extends ILcContext {

  /**
   * @see fr.profi.mzdb.model.ILcContext#getElutionTime()
   */
  def getElutionTime = 0

}


case class FullLcContext(
  @BeanProperty spectrumId: Long,
  @BeanProperty elutionTime: Float
) extends ILcContext 

