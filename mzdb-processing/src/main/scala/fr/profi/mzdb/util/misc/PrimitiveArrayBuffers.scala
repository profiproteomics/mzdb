package fr.profi.mzdb.util.misc

/**
 * Growable buffer of primitive Long values (no boxing).
 *
 * Unlike scala.collection.mutable.ArrayBuilder, it can be read (apply, toArray, slice)
 * any number of times and appended to after a read.
 */
final class LongArrayBuffer(initialCapacity: Int = 16) {
  require(initialCapacity >= 0, "initialCapacity must be >= 0")

  private var elems: Array[Long] = new Array[Long](initialCapacity)
  private var _size: Int = 0

  def length: Int = _size
  def isEmpty: Boolean = _size == 0

  def apply(idx: Int): Long = {
    if (idx < 0 || idx >= _size) throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${_size - 1})")
    elems(idx)
  }

  /** Ensures that at least `size` elements can be stored without reallocation. */
  def sizeHint(size: Int): Unit = {
    if (size > elems.length) elems = java.util.Arrays.copyOf(elems, size)
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    if (minCapacity > elems.length) {
      val newCapacity = java.lang.Math.max(minCapacity, java.lang.Math.max(16, elems.length * 2))
      elems = java.util.Arrays.copyOf(elems, newCapacity)
    }
  }

  def +=(value: Long): this.type = {
    ensureCapacity(_size + 1)
    elems(_size) = value
    _size += 1
    this
  }

  def ++=(values: Array[Long]): this.type = {
    ensureCapacity(_size + values.length)
    System.arraycopy(values, 0, elems, _size, values.length)
    _size += values.length
    this
  }

  def clear(): Unit = { _size = 0 }

  /** Returns a copy of the buffered values. */
  def toArray: Array[Long] = java.util.Arrays.copyOf(elems, _size)

  /** Returns a copy of the values in the index range [from, until). */
  def slice(from: Int, until: Int): Array[Long] = {
    val lo = java.lang.Math.max(from, 0)
    val hi = java.lang.Math.min(until, _size)
    if (hi <= lo) new Array[Long](0) else java.util.Arrays.copyOfRange(elems, lo, hi)
  }

  /** Returns a new buffer holding a copy of the values in the index range [from, until) (single copy). */
  def sliceBuffer(from: Int, until: Int): LongArrayBuffer = {
    val buffer = new LongArrayBuffer(0)
    buffer.elems = this.slice(from, until)
    buffer._size = buffer.elems.length
    buffer
  }
}

object LongArrayBuffer {
  def apply(values: Array[Long]): LongArrayBuffer = new LongArrayBuffer(values.length) ++= values
}

/**
 * Growable buffer of primitive Float values (no boxing).
 *
 * Unlike scala.collection.mutable.ArrayBuilder, it can be read (apply, toArray, slice)
 * any number of times and appended to after a read.
 */
final class FloatArrayBuffer(initialCapacity: Int = 16) {
  require(initialCapacity >= 0, "initialCapacity must be >= 0")

  private var elems: Array[Float] = new Array[Float](initialCapacity)
  private var _size: Int = 0

  def length: Int = _size
  def isEmpty: Boolean = _size == 0

  def apply(idx: Int): Float = {
    if (idx < 0 || idx >= _size) throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${_size - 1})")
    elems(idx)
  }

  /** Ensures that at least `size` elements can be stored without reallocation. */
  def sizeHint(size: Int): Unit = {
    if (size > elems.length) elems = java.util.Arrays.copyOf(elems, size)
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    if (minCapacity > elems.length) {
      val newCapacity = java.lang.Math.max(minCapacity, java.lang.Math.max(16, elems.length * 2))
      elems = java.util.Arrays.copyOf(elems, newCapacity)
    }
  }

  def +=(value: Float): this.type = {
    ensureCapacity(_size + 1)
    elems(_size) = value
    _size += 1
    this
  }

  def ++=(values: Array[Float]): this.type = {
    ensureCapacity(_size + values.length)
    System.arraycopy(values, 0, elems, _size, values.length)
    _size += values.length
    this
  }

  def clear(): Unit = { _size = 0 }

  /** Returns a copy of the buffered values. */
  def toArray: Array[Float] = java.util.Arrays.copyOf(elems, _size)

  /** Returns a copy of the values in the index range [from, until). */
  def slice(from: Int, until: Int): Array[Float] = {
    val lo = java.lang.Math.max(from, 0)
    val hi = java.lang.Math.min(until, _size)
    if (hi <= lo) new Array[Float](0) else java.util.Arrays.copyOfRange(elems, lo, hi)
  }

  /** Returns a new buffer holding a copy of the values in the index range [from, until) (single copy). */
  def sliceBuffer(from: Int, until: Int): FloatArrayBuffer = {
    val buffer = new FloatArrayBuffer(0)
    buffer.elems = this.slice(from, until)
    buffer._size = buffer.elems.length
    buffer
  }
}

object FloatArrayBuffer {
  def apply(values: Array[Float]): FloatArrayBuffer = new FloatArrayBuffer(values.length) ++= values
}

/**
 * Growable buffer of primitive Double values (no boxing).
 *
 * Unlike scala.collection.mutable.ArrayBuilder, it can be read (apply, toArray, slice)
 * any number of times and appended to after a read.
 */
final class DoubleArrayBuffer(initialCapacity: Int = 16) {
  require(initialCapacity >= 0, "initialCapacity must be >= 0")

  private var elems: Array[Double] = new Array[Double](initialCapacity)
  private var _size: Int = 0

  def length: Int = _size
  def isEmpty: Boolean = _size == 0

  def apply(idx: Int): Double = {
    if (idx < 0 || idx >= _size) throw new IndexOutOfBoundsException(s"$idx is out of bounds (min 0, max ${_size - 1})")
    elems(idx)
  }

  /** Ensures that at least `size` elements can be stored without reallocation. */
  def sizeHint(size: Int): Unit = {
    if (size > elems.length) elems = java.util.Arrays.copyOf(elems, size)
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    if (minCapacity > elems.length) {
      val newCapacity = java.lang.Math.max(minCapacity, java.lang.Math.max(16, elems.length * 2))
      elems = java.util.Arrays.copyOf(elems, newCapacity)
    }
  }

  def +=(value: Double): this.type = {
    ensureCapacity(_size + 1)
    elems(_size) = value
    _size += 1
    this
  }

  def ++=(values: Array[Double]): this.type = {
    ensureCapacity(_size + values.length)
    System.arraycopy(values, 0, elems, _size, values.length)
    _size += values.length
    this
  }

  def clear(): Unit = { _size = 0 }

  /** Returns a copy of the buffered values. */
  def toArray: Array[Double] = java.util.Arrays.copyOf(elems, _size)

  /** Returns a copy of the values in the index range [from, until). */
  def slice(from: Int, until: Int): Array[Double] = {
    val lo = java.lang.Math.max(from, 0)
    val hi = java.lang.Math.min(until, _size)
    if (hi <= lo) new Array[Double](0) else java.util.Arrays.copyOfRange(elems, lo, hi)
  }

  /** Returns a new buffer holding a copy of the values in the index range [from, until) (single copy). */
  def sliceBuffer(from: Int, until: Int): DoubleArrayBuffer = {
    val buffer = new DoubleArrayBuffer(0)
    buffer.elems = this.slice(from, until)
    buffer._size = buffer.elems.length
    buffer
  }
}

object DoubleArrayBuffer {
  def apply(values: Array[Double]): DoubleArrayBuffer = new DoubleArrayBuffer(values.length) ++= values
}
