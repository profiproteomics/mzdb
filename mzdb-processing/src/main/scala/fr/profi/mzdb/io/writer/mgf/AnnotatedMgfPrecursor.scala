package fr.profi.mzdb.io.writer.mgf

class AnnotatedMgfPrecursor(precMz: Double, charge: Integer, rt: Float) extends MgfPrecursor(precMz, charge.intValue(), rt) {

  private var annotations = Map.empty[String, Any]

  def this(precMz: Double, charge: Integer) = this(precMz, charge.intValue(), 0)

  def this(precMz: Double, rt: Float) = this(precMz, null, rt)

  def addAnnotation(key: String, value: Any): Unit = {
    annotations += (key -> value)
  }

  def getAnnotation(key : String): Any = {
    annotations(key)
  }

  def getAnnotationOrElse(key: String, value: Any): Any = {
    annotations.getOrElse(key, value)
  }
}
