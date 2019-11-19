package fr.profi.mzdb.model

import scala.beans.BeanProperty

case class LcMsPeak(
  @BeanProperty val spectrumId: Long,
  @BeanProperty val elutionTime: Float,
  @BeanProperty val mz: Double,
  @BeanProperty val intensity: Float
)