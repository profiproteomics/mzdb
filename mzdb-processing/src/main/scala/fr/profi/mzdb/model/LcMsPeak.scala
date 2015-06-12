package fr.profi.mzdb.model

import scala.beans.BeanProperty

// TODO: keep a MessagePack compatible structure ???
@org.msgpack.annotation.Message
case class LcMsPeak(
  // MessagePack requires mutable fields
  @BeanProperty var scanId: Long,
  @BeanProperty var elutionTime: Float,
  @BeanProperty var mz: Double,
  @BeanProperty var intensity: Float
) {
  // Plain constructor needed for MessagePack
  def this() = this(0,Float.NaN,Double.NaN,Float.NaN)
}