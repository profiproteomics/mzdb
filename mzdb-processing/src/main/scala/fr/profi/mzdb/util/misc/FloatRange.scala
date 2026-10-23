package fr.profi.mzdb.util.misc

/**
 * Replacement of the Float ranges (`1f to 64f by 2f`, `a until b by step`) which have been removed from the Scala 2.13 standard library.
 *
 * It reproduces the Scala 2.12 behavior bit for bit:
 *  - the number of elements is computed like the 2.12 NumericRange does (BigDecimal quotient and remainder of the Float values widened to Double),
 *  - the i-th element is `start + step * i` (computed with Float arithmetic).
 */
object FloatRange {

  private def toBigDecimal(x: Float): BigDecimal = BigDecimal(x.toDouble)

  /** Returns the number of elements of the range (0 if the range is empty) */
  def count(start: Float, end: Float, step: Float, isInclusive: Boolean): Int = {
    if (step == 0f) throw new IllegalArgumentException("step cannot be 0.")
    else if (start == end) { if (isInclusive) 1 else 0 }
    else if ((start < end) != (step > 0f)) 0
    else {
      val diff = end - start
      val jumps = toBigDecimal(diff).quot(toBigDecimal(step)).floatValue.toLong
      val remainder = toBigDecimal(diff).remainder(toBigDecimal(step)).floatValue
      val longCount = jumps + (if (!isInclusive && remainder == 0f) 0 else 1)
      require(longCount <= Int.MaxValue, s"The range $start to $end by $step has too many elements")
      longCount.toInt
    }
  }

  private def build(start: Float, end: Float, step: Float, isInclusive: Boolean): Array[Float] = {
    val n = count(start, end, step, isInclusive)
    val values = new Array[Float](n)
    var i = 0
    while (i < n) {
      values(i) = start + step * i.toFloat
      i += 1
    }
    values
  }

  /** Same as `start to end by step` in Scala 2.12 */
  def inclusive(start: Float, end: Float, step: Float): Array[Float] = build(start, end, step, isInclusive = true)

  /** Same as `start until end by step` in Scala 2.12 */
  def exclusive(start: Float, end: Float, step: Float): Array[Float] = build(start, end, step, isInclusive = false)
}
