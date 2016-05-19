package fr.profi.mzdb.model

import scala.beans.BeanProperty
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

import com.fasterxml.jackson.annotation.JsonFormat

import org.apache.commons.math3.stat.StatUtils

import fr.profi.mzdb.utils.misc.InMemoryIdGen

trait IPeakelCursorProvider {
  def getNewCursor(): IPeakelDataCursor
}

trait IPeakelDataContainer extends IPeakelCursorProvider {
  
  def getPeaksCount(): Int
  def getElutionTimeIntensityPairs(): Array[(Float,Float)]
  
  // TODO: re-implement me ?
  /*private def _interpolateFirstElutionTimeAtHalfMaximum( intensityTimePairs: Seq[(Float, Float)], halfApexIntensity: Float): Float = {
    
    val firstPeakIndex2 = intensityTimePairs.indexWhere(_._1 >= halfApexIntensity)
    val firstPeakIndex1 = if (firstPeakIndex2 > 0) firstPeakIndex2 - 1 else 0
    
    // If we don't have two distinct peaks, return time value of the first peakel peak
    val firstPeakTime = if (firstPeakIndex2 <= firstPeakIndex1) getElutionTimes.head
    else {
      
      // Linear interpolation
      val interpolatedTime = linearInterpolation(
        halfApexIntensity,
        Seq( intensityTimePairs(firstPeakIndex1), intensityTimePairs(firstPeakIndex2) ),
        fixOutOfRange = false
      )
      
      interpolatedTime
    }
    
    firstPeakTime
  }*/
  
  /** Just check elution peak in terms of duration in nb spectra */
  def hasEnoughPeaks(minPeaksCount:Int): Boolean = {
    
    if (this.getPeaksCount() < minPeaksCount)
      return false
    
    true
  }

  def toPeaks( lcContextBySpectrumId: LongMap[_ <: ILcContext] ): Array[Peak] = {
    val peakelCursor = this.getNewCursor()
    
    val peaks = new Array[Peak](this.getPeaksCount())
    while( peakelCursor.next() ) {
      peaks(peakelCursor.cursorIndex) = peakelCursor.toPeak( lcContextBySpectrumId )
    }
    
    peaks
  }
  
  def toLcMsPeaks(): Array[LcMsPeak] = {
    val peakelCursor = this.getNewCursor()
    
    val lcMspeaks = new Array[LcMsPeak](this.getPeaksCount())
    while( peakelCursor.next() ) {
      lcMspeaks(peakelCursor.cursorIndex) = peakelCursor.toLcMsPeak()
    }
    
    lcMspeaks
  }
  
}

object Peakel extends InMemoryIdGen {
  
  def calcMeanAndCv( values: ArrayBuffer[Float] ): (Float,Float) = {
    if( values == null ) return (0f,0f)
    
    val valuesAsDoubles = values.map(_.toDouble).toArray
    this.calcMeanAndCv(valuesAsDoubles)
  }
  
  def calcMeanAndCv( values: Array[Double] ): (Float,Float) = {
    if( values == null ) return (0f,0f)
    
    val mean = StatUtils.mean(values)
    val variance = StatUtils.variance(values, mean)
    val sd = math.sqrt(variance)
    val cv = if( sd > 0 ) 100 * mean / sd else 0f
   
    (mean.toFloat, cv.toFloat )
  }
  
  def integratePeakel(peakelCursor: IPeakelDataCursor): (Float,Float) = {
    
    //val apexIntensity = this.getIntensityValues.max  
    //val halfApexIntensity = apexIntensity / 2
    //val lastTime = getElutionTimes.last
    
    //val intensitiesAboveHM = intensityList.zipWithIndex.filter(_._1 >= halfApexIntensity )
    //print(apex.getLcContext().getElutionTime()+"\t")
    
    // --- Interpolate the time of peaks at the half maximum peakel intensity ---
    //val intensityElutionTimePairs = getIntensityValues.zip(getElutionTimes)
    //val leftTimeAtHalfApex = _interpolateFirstElutionTimeAtHalfMaximum(intensityElutionTimePairs, halfApexIntensity)
    //val rightTimeAtHalfApex = _interpolateFirstElutionTimeAtHalfMaximum(intensityElutionTimePairs.reverse, halfApexIntensity)
    //print( mz + "\t"+ leftTimeAtHalfApex + "\t" + rightTimeAtHalfApex + "\t" + defPeaksAboveHM.length)
    
    // Search for the apex and integrate IPs
    var computedSum = 0f
    var computedArea = 0f
    //var computedAAHM = 0f
    var prevPeakTime = 0f
    var prevPeakIntensity = 0f
    //var prevPeakIntensityAboveHM = halfApexIntensity
    //var prevPeakTimeAboveHM = leftTimeAtHalfApex
    
    while( peakelCursor.next() ) {
      
      // Compute intensity sum
      val intensity = peakelCursor.getIntensity()
      val curPeakTime = peakelCursor.getElutionTime()

      computedSum += intensity
      
      // Compute intensity area
      if( peakelCursor.cursorIndex > 0 ) {
        val deltaTime = curPeakTime - prevPeakTime
        computedArea += (intensity + prevPeakIntensity ) * deltaTime / 2
      }
      // Compute intensity uahm
      /*if (curPeakTime >= leftTimeAtHalfApex && prevPeakTimeAboveHM < lastTime ) {
        
        if( curPeakTime <= rightTimeAtHalfApex)  {
          val deltaTime = curPeakTime - prevPeakTimeAboveHM
          computedAAHM += (peak.intensity + prevPeakIntensityAboveHM - apexIntensity) * deltaTime / 2
          prevPeakTimeAboveHM = curPeakTime
          prevPeakIntensityAboveHM = peak.intensity
        } else {
          val deltaTime = curPeakTime - prevPeakTimeAboveHM
          computedAAHM += (prevPeakIntensityAboveHM - halfApexIntensity) * deltaTime / 2
          prevPeakIntensityAboveHM = halfApexIntensity
          prevPeakTimeAboveHM = lastTime
        }
      }*/
      prevPeakTime = curPeakTime
      prevPeakIntensity = intensity
    }
    
    //println("\t" + computedArea + "\t" +computedAAHM )
    
    if( computedArea == 0 ) {
      computedArea = computedSum
      //computedAAHM = computedSum
    }
    
    ( computedSum, computedArea ) // fwhm = rightTimeAtHalfApex - leftTimeAtHalfApex
  }
  
}

/** The Class Peakel.
 * @author David Bouyssie
 *
 */
case class Peakel(
  @BeanProperty var id: Int = Peakel.generateNewId(),
  @BeanProperty spectrumIds: Array[Long],
  @BeanProperty elutionTimes: Array[Float],
  @BeanProperty mzValues: Array[Double],
  @BeanProperty intensityValues: Array[Float],
  @BeanProperty var intensitySum: Float = 0f,
  @BeanProperty var area: Float = 0f,
  @BeanProperty leftHwhmMean: Float = 0f,
  @BeanProperty leftHwhmCv: Float = 0f,
  @BeanProperty rightHwhmMean: Float = 0f,
  @BeanProperty rightHwhmCv: Float = 0f
) extends IPeakelDataContainer with ILcContext {
  
  @BeanProperty val peaksCount = spectrumIds.length
  def getNewCursor(): PeakelCursor = new PeakelCursor(this)
  
  // Make some requirements
  require( spectrumIds != null && peaksCount > 0, "some spectrumIds must be provided" )
  require( mzValues != null && mzValues.length > 0, "some mzValues must be provided" )
  require( spectrumIds.length == mzValues.length, "spectrumIds and mzValues must have the same size" )
  require( mzValues.length == intensityValues.length, "mzList and intensityList must have the same size" )

  def this(
    id: Int,
    dataMatrix: PeakelDataMatrix,
    intensitySum: Float,
    area: Float,
    leftHwhmMean: Float,
    leftHwhmCv: Float,
    rightHwhmMean: Float,
    rightHwhmCv: Float
  ) {
    this(
      id,
      dataMatrix.spectrumIds,
      dataMatrix.elutionTimes,
      dataMatrix.mzValues,
      dataMatrix.intensityValues,
      intensitySum,
      area,
      leftHwhmMean,
      leftHwhmCv,
      rightHwhmMean,
      rightHwhmCv
    )
  }
  
  @BeanProperty val apexIndex: Int = {
    
    var maxIntensity = 0f
    var apexIdx = 0
    
    var idx = 0
    while (idx < peaksCount) {
      val intensity = intensityValues(idx)
      if (intensity > maxIntensity) {
        maxIntensity = intensity
        apexIdx = idx
      }
      idx += 1
    }
    
    apexIdx
  }
  
  def getApexSpectrumId() = spectrumIds(apexIndex)
  def getFirstSpectrumId() = spectrumIds(0)
  def getLastSpectrumId() = spectrumIds(peaksCount -1)
  
  def getApexElutionTime() = elutionTimes(apexIndex)
  def getFirstElutionTime() = elutionTimes(0)
  def getLastElutionTime() = elutionTimes(peaksCount -1)
  
  def getApexMz() = mzValues(apexIndex)
  def getApexIntensity() = intensityValues(apexIndex)
  def getMz() = mzValues(apexIndex) // TODO: lazy val ???
  def getElutionTimeIntensityPairs() = elutionTimes.zip(intensityValues)
  
  def getCursorAtApex(): PeakelCursor = PeakelCursor(this, apexIndex)
  
  // ILcContext java interface implementation 
  def getSpectrumId() : Long = getApexSpectrumId()
  def getElutionTime(): Float = getApexElutionTime()
  
  // Access to data of a peak at a given index
  def getPeakElutionTime(pIdx: Int): Float = elutionTimes(pIdx)
  def getPeakMz(pIdx: Int): Double = mzValues(pIdx)
  def getPeakIntensity(pIdx: Int): Float = intensityValues(pIdx)
  
  def calcDuration(): Float = getLastElutionTime - getFirstElutionTime
  
  def calcWeightedAverageTime(): Float = {
    var intensitySum = 0f
    
    val weightedTimeSum = intensityValues.zipWithIndex.foldLeft(0f) { (timeSum,intensityWithIdx) =>
      val(intensity,index) = intensityWithIdx
      intensitySum += intensity
      timeSum + (elutionTimes(index) * intensity)
    }
    
    weightedTimeSum / intensitySum
  }
  
  /*def calcPeakelsIntersection( otherPeakel: Peakel ): Pair[Array[Peak],Array[Peak]] = {
    val thisPeakByTime = this.peakByElutionTime
    val otherPeakByTime = otherPeakel.peakByElutionTime
    val intersectingTimes = (thisPeakByTime.keys ++ otherPeakByTime.keys)
      .groupBy(e=>e)
      .withFilter(_._2.size == 2)
      .map(_._1)
      .toArray.sorted
    
    Pair( intersectingTimes.map( thisPeakBytTime(_) ), intersectingTimes.map( otherPeakByTime(_) ) )    
  }*/
  
  def toPeakelDataMatrix(): PeakelDataMatrix = {
    new PeakelDataMatrix( spectrumIds, elutionTimes, mzValues, intensityValues )
  }
  
}

object PeakelDataMatrix {
  
  def pack(peakelDataMatrix: PeakelDataMatrix): Array[Byte] = {
    
    // Read values from peakelDataMatrix
    val spectrumIds = peakelDataMatrix.spectrumIds
    val elutionTimes = peakelDataMatrix.elutionTimes
    val mzValues = peakelDataMatrix.mzValues
    val intensityValues = peakelDataMatrix.intensityValues
    val len = spectrumIds.length
    
    val packer = org.msgpack.core.MessagePack.newDefaultBufferPacker()
    
    // Create an array header corresponding to the matrix of 4 following arrays
    packer.packArrayHeader(4)
    
    packer.packArrayHeader(len)
    var i = 0; while (i < len) { packer.packLong(spectrumIds(i)); i += 1}
    
    packer.packArrayHeader(len)
    i = 0; while (i < len) { packer.packFloat(elutionTimes(i)); i += 1}
    
    packer.packArrayHeader(len)
    i = 0; while (i < len) { packer.packDouble(mzValues(i)); i += 1}
    
    packer.packArrayHeader(len)
    i = 0; while (i < len) { packer.packFloat(intensityValues(i)); i += 1}
    
    val result = packer.toByteArray()
    
    packer.close()
    
    result
  }
  
  def unpack(peakelMessageAsBytes: Array[Byte]): PeakelDataMatrix = {
    
    val unpacker = org.msgpack.core.MessagePack.newDefaultUnpacker(peakelMessageAsBytes)
    
    // Read array header corresponding to the matrix of 4 following arrays
    unpacker.unpackArrayHeader()
    
    // Read first array length to instantiate the 4 arrays
    val len = unpacker.unpackArrayHeader()
    val spectrumIds = new Array[Long](len)
    val elutionTimes = new Array[Float](len)
    val mzValues = new Array[Double](len)
    val intensityValues = new Array[Float](len)
    
    var i = 0; while (i < len) { spectrumIds(i) = unpacker.unpackLong(); i += 1}
    
    unpacker.unpackArrayHeader()
    i = 0; while (i < len) { elutionTimes(i) = unpacker.unpackFloat(); i += 1}
    
    unpacker.unpackArrayHeader()
    i = 0; while (i < len) { mzValues(i) = unpacker.unpackDouble(); i += 1}
    
    unpacker.unpackArrayHeader()
    i = 0; while (i < len) { intensityValues(i) = unpacker.unpackFloat(); i += 1}
    
    unpacker.close()
    
    PeakelDataMatrix(spectrumIds,elutionTimes,mzValues,intensityValues)
  }
}

/** Class which was used for MessagePack serialization purpose
 *  Important: now data have to be first wrapped and unwrapped into a PeakelDataMatrix.MsgPackType
 **/
@JsonFormat(shape=JsonFormat.Shape.ARRAY)
case class PeakelDataMatrix(
  @BeanProperty val spectrumIds: Array[Long],
  @BeanProperty val elutionTimes: Array[Float],
  @BeanProperty val mzValues: Array[Double],
  @BeanProperty val intensityValues: Array[Float]
) extends IPeakelDataContainer {
  
  @BeanProperty lazy val peaksCount = spectrumIds.length
  def getNewCursor(): PeakelDataMatrixCursor = new PeakelDataMatrixCursor(this)
  
  def getElutionTimeIntensityPairs() = elutionTimes.zip(intensityValues)
  
  /** For backward compatibility, TODO: use Peakel static method instead **/
  def integratePeakel(): (Float,Float) = {
    Peakel.integratePeakel(this.getNewCursor())
  }
}

// TODO: rename into PeakelMapper ?
case class PeakelCoordinates(
  peakLists: Array[PeakList], // this array may contain null values
  peakIndices: Array[Int],
  var cursorIndex: Int = -1 // note: here this is the index of peakLists and peakIndices arrays
) { // extends IPeakelDataContainer with IPeakelDataCursor
  require( peakLists.length == peakIndices.length, "peakLists and peakIndices must have the same length")
  
  def this( size: Int ) = {
    this(
      new Array[PeakList](size),
      new Array[Int](size)
    )
  }
  
  @BeanProperty val peaksCount = peakLists.length
  require( peaksCount > 0, "PeakelCoordinates can't be empty")
  
  // TODO: rename minPeakListIdx && maxPeakListIdx
  var minIdx: Int = 0
  var maxIdx: Int = peaksCount - 1
  
  def removePeak(idx: Int) = {
    peakLists(idx) = null
    peakIndices(idx) = -1
  }
  
  /*def getNewCursor(): IPeakelDataCursor = {
    new PeakelCoordinates( peakLists, peakIndices, minIdx - 1)
  }
  
  private var curPeakList: PeakList = null
  private var curPeakIndex: Int = -1
  
  override def next(): Boolean = {
    if( cursorIndex >= maxIdx ) false
    else {
      cursorIndex += 1
      curPeakList = peakLists(cursorIndex)
      curPeakIndex = peakIndices(cursorIndex)
      true
    }
  }
  override def previous(): Boolean = {
    if( cursorIndex <= minIdx ) false
    else {
      cursorIndex -= 1
      curPeakList = peakLists(cursorIndex)
      curPeakIndex = peakIndices(cursorIndex)
      true
    }
  }
  
  def peakelData: IPeakelDataContainer = this
  def getSpectrumId(): Long = curPeakList.getSpectrumHeader().getId
  def getElutionTime(): Float = curPeakList.getSpectrumHeader().getElutionTime
  def getMz(): Double = curPeakList.getMzList()(cursorIndex)
  def getIntensity(): Float = curPeakList.getIntensityList()(cursorIndex)
  def getLeftHwhm(): Float = curPeakList.getLeftHwhmList()(cursorIndex)
  def getRightHwhm(): Float = curPeakList.getRightHwhmList()(cursorIndex)
  */
  
  private def _getDefinedPeakListsCount(): Int = {
    
    val peakelLength = 1 + maxIdx - minIdx
    
    var defPklCount = 0
    var curIdx = 0
    while (curIdx < peakelLength) {
      val peakListIdx = minIdx + curIdx
      val pkl = peakLists(peakListIdx)
      if (pkl != null) defPklCount += 1
      
      curIdx += 1
    }
    
    defPklCount
  }
  
  def getElutionTimeIntensityPairs(): Array[(Float,Double)] = {
    
    val defPklCount = _getDefinedPeakListsCount()
    val rtIntPairs = new Array[(Float,Double)](defPklCount)
    
    var peakListIdx = minIdx
    var curIdx = 0
    while (curIdx < defPklCount) {
      
      val pkl = peakLists(peakListIdx)
      
      if (pkl != null) {
        val peakIdx = peakIndices(peakListIdx)
        
        val specHeader = pkl.getSpectrumHeader()
        rtIntPairs(curIdx) = (specHeader.getElutionTime(), pkl.getIntensityList()(peakIdx).toDouble )
        
        curIdx += 1
      }
      
      peakListIdx += 1
    }
    
    rtIntPairs
  }
  
  def getDefinedPeakListIndexMapping(): LongMap[Int] = {
    
    val defPklCount = _getDefinedPeakListsCount()
    val indexMapping = new LongMap[Int](defPklCount)
    
    var peakListIdx = minIdx
    var curIdx = 0
    while (curIdx < defPklCount) {
      
      val pkl = peakLists(peakListIdx)
      
      if (pkl != null) {
        indexMapping(curIdx) = peakListIdx
        curIdx += 1
      }
      
      peakListIdx += 1
    }
    
    indexMapping
  }
  
  def toPeakel(id: Int = Peakel.generateNewId()): Peakel = {
    
    val defPklCount = _getDefinedPeakListsCount()
    val spectrumIds = new Array[Long](defPklCount)
    val elutionTimes = new Array[Float](defPklCount)
    val mzValues = new Array[Double](defPklCount)
    val intensityValues = new Array[Float](defPklCount)
    val leftHwhms = new Array[Double](defPklCount)
    val rightHwhms = new Array[Double](defPklCount)
    
    var peakListIdx = minIdx
    var curIdx = 0
    while (curIdx < defPklCount) {
      
      val pkl = peakLists(peakListIdx)
      
      if (pkl != null) {
        val peakIdx = peakIndices(peakListIdx)
        
        val specHeader = pkl.getSpectrumHeader()
        spectrumIds(curIdx) = specHeader.getId()
        elutionTimes(curIdx) = specHeader.getElutionTime()
        mzValues(curIdx) = pkl.getMzList()(peakIdx)
        intensityValues(curIdx) = pkl.getIntensityList()(peakIdx)
        
        val leftHwhmList = pkl.getLeftHwhmList()
        if (leftHwhmList != null) 
          leftHwhms(curIdx) = leftHwhmList(peakIdx)
          
        val rightHwhmList = pkl.getRightHwhmList()
        if (rightHwhmList != null) 
          rightHwhms(curIdx) = rightHwhmList(peakIdx)
          
        curIdx += 1
      }
      
      peakListIdx += 1
    }
    
    val(leftHwhmMean,leftHwhmCv) = Peakel.calcMeanAndCv(leftHwhms)
    val(rightHwhmMean,rightHwhmCv) = Peakel.calcMeanAndCv(rightHwhms)
    
    val peakel = Peakel(
      id = id,
      spectrumIds,
      elutionTimes,
      mzValues,
      intensityValues,
      0f,
      0f,
      leftHwhmMean,
      leftHwhmCv,
      rightHwhmMean,
      rightHwhmCv
    )
    
    val(intensity, area) = Peakel.integratePeakel(peakel.getNewCursor())
    peakel.intensitySum = intensity
    peakel.area = area
    
    peakel
  }
}

class PeakelBuilder(
  @BeanProperty val spectrumIds: ArrayBuffer[Long] = new ArrayBuffer[Long](),
  @BeanProperty val elutionTimes: ArrayBuffer[Float] = new ArrayBuffer[Float](),
  @BeanProperty val mzValues: ArrayBuffer[Double] = new ArrayBuffer[Double](),
  @BeanProperty val intensityValues: ArrayBuffer[Float] = new ArrayBuffer[Float](),
  @BeanProperty val leftHwhms: ArrayBuffer[Float] = new ArrayBuffer[Float](),
  @BeanProperty val rightHwhms: ArrayBuffer[Float] = new ArrayBuffer[Float]()
) extends IPeakelDataContainer {
  
  def getElutionTimeIntensityPairs() = elutionTimes.zip(intensityValues).toArray
  
  def getPeaksCount(): Int = spectrumIds.length
  def getNewCursor(): PeakelBuilderCursor = new PeakelBuilderCursor(this)
  
  def this( bufferSize: Int ) = {
    this(
      new ArrayBuffer[Long](bufferSize),
      new ArrayBuffer[Float](bufferSize),
      new ArrayBuffer[Double](bufferSize),
      new ArrayBuffer[Float](bufferSize),
      new ArrayBuffer[Float](bufferSize),
      new ArrayBuffer[Float](bufferSize)
    )
  }
  
  def this( peaks: Seq[Peak] ) = {
    this(peaks.length)
    
    this += peaks
  }
  
  def this( peakelDataMatrix: PeakelDataMatrix ) = {
    this( peakelDataMatrix.mzValues.length )
    
    this.spectrumIds ++= peakelDataMatrix.spectrumIds
    this.elutionTimes ++= peakelDataMatrix.elutionTimes
    this.mzValues ++= peakelDataMatrix.mzValues
    this.intensityValues ++= peakelDataMatrix.intensityValues
  }
  
  def this( peakelDataMatrix: PeakelDataMatrix, fromIdx: Int, toIdx: Int ) = {
    this( peakelDataMatrix.mzValues.length )
    
    this.spectrumIds ++= peakelDataMatrix.spectrumIds
    this.elutionTimes ++= peakelDataMatrix.elutionTimes
    this.mzValues ++= peakelDataMatrix.mzValues
    this.intensityValues ++= peakelDataMatrix.intensityValues
  }
  
  def +=( peak: Peak ): PeakelBuilder = {
    require( peak != null, "peak is null")
    
    val lcContext = peak.getLcContext()
    
    this.add(lcContext.getSpectrumId, lcContext.getElutionTime, peak.mz, peak.intensity, peak.leftHwhm, peak.rightHwhm)
    
    this
  }
  
  def +=( peaks: Iterable[Peak] ): PeakelBuilder = {
    require( peaks != null, "peaks is null")
    
    for( peak <- peaks ) this += peak    
    this
  }
  
  /*
  def add( spectrumId: Int, elutionTime: Float, mz: Double, intensity: Float ): PeakelBuilder = {
    spectrumIdBuffer += spectrumId
    elutionTimeBuffer += elutionTime
    mzBuffer += mz
    intensityBuffer += intensity
    
    this
  }
  */
  
  def add( spectrumId: Long, elutionTime: Float, mz: Double, intensity: Float, leftHwhm: Float, rightHwhm: Float ): PeakelBuilder = {
    spectrumIds += spectrumId
    elutionTimes += elutionTime
    mzValues += mz
    intensityValues += intensity
    leftHwhms += leftHwhm
    rightHwhms += rightHwhm
    
    this
  }
  
  def result( id: Int = Peakel.generateNewId() ): Peakel = {
    //val leftHwhmValues = if( leftHwhmBuffer.count(_ > 0) == 0 ) null else leftHwhmBuffer.toArray
    //val rightHwhmValues = if( rightHwhmBuffer.count(_ > 0) == 0 ) null else rightHwhmBuffer.toArray
    
    val(intensity, area) = Peakel.integratePeakel(this.getNewCursor())
    
    val(leftHwhmMean,leftHwhmCv) = Peakel.calcMeanAndCv(leftHwhms)
    val(rightHwhmMean,rightHwhmCv) =  Peakel.calcMeanAndCv(rightHwhms)
    
    Peakel(
      id = id,
      spectrumIds.toArray,
      elutionTimes.toArray,
      mzValues.toArray,
      intensityValues.toArray,
      intensity,
      area,
      leftHwhmMean,
      leftHwhmCv,
      rightHwhmMean,
      rightHwhmCv
    )
  }
  
  def calcFwhms(): Array[Float] = {
    val( lh, rh ) = ( this.leftHwhms, this.rightHwhms )
    if( lh == null || rh == null ) return null

    this.spectrumIds.indices.toArray.map(i => lh(i) + rh(i) )
  }
  
  /** Restrict to is inclusive here **/
  def restrictToSpectrumIdRange( firstSpectrumId: Long, lastSpectrumId: Long ): Option[PeakelBuilder] = {
    
    val matchingSpectrumIdsWithIdx = spectrumIds.zipWithIndex.filter { case (spectrumId,idx) =>
      spectrumId >= firstSpectrumId && spectrumId <= lastSpectrumId
    }
    if( matchingSpectrumIdsWithIdx.length < 2 ) return None
    
    val firstIdx = matchingSpectrumIdsWithIdx.head._2
    val lastBoundary = matchingSpectrumIdsWithIdx.last._2 + 1
    
    val spectrumIdCount = spectrumIds.length
    val newPeakelBuilder = new PeakelBuilder(
      spectrumIds.slice(firstIdx, lastBoundary),
      elutionTimes.slice(firstIdx, lastBoundary),
      mzValues.slice(firstIdx, lastBoundary),
      intensityValues.slice(firstIdx, lastBoundary),
      if( leftHwhms.length == spectrumIdCount ) null else leftHwhms.slice(firstIdx, lastBoundary),
      if( rightHwhms.length == spectrumIdCount ) null else rightHwhms.slice(firstIdx, lastBoundary)
    )
    
    Some( newPeakelBuilder )
  }
  
}

trait IPeakelDataCursor {
  
  def peakelData: IPeakelDataContainer
  var cursorIndex: Int
  
  require( cursorIndex >= -1 && cursorIndex <= peakelData.getPeaksCount(), "cursorIndex is out of bounds")
    
  //def isOutOfBounds(): Boolean = peakIndex <= -1 || peakIndex >= peakel.lcContexts.length
  
  def next(): Boolean = {
    if( cursorIndex >= peakelData.getPeaksCount() - 1 ) false
    else {
      cursorIndex += 1
      true
    }
  }
  def previous(): Boolean = {
    if( cursorIndex <= 0 ) false
    else {
      cursorIndex -= 1
      true
    }
  }
  
  //def getLcContext(): ILcContext = peakel.lcContexts(peakIndex)
  def getSpectrumId(): Long
  def getElutionTime(): Float
  def getMz(): Double
  def getIntensity(): Float
  
  def toPeak( lcContextBySpectrumId: LongMap[_ <: ILcContext] ): Peak = {
    new Peak( getMz, getIntensity, 0f, 0f, lcContextBySpectrumId(getSpectrumId) )
  }
  
  def toLcMsPeak(): LcMsPeak = {
    new LcMsPeak( getSpectrumId, getElutionTime, getMz, getIntensity )
  }
}

case class PeakelCursor(
  peakelData: Peakel,
  var cursorIndex: Int = - 1
) extends IPeakelDataCursor {  
  def getSpectrumId(): Long = peakelData.spectrumIds(cursorIndex)
  def getElutionTime(): Float = peakelData.elutionTimes(cursorIndex)
  def getMz(): Double = peakelData.mzValues(cursorIndex)
  def getIntensity(): Float = peakelData.intensityValues(cursorIndex)
}

case class PeakelBuilderCursor(
  peakelData: PeakelBuilder,
  var cursorIndex: Int = - 1
) extends IPeakelDataCursor {  
  def getSpectrumId(): Long = peakelData.spectrumIds(cursorIndex)
  def getElutionTime(): Float = peakelData.elutionTimes(cursorIndex)
  def getMz(): Double = peakelData.mzValues(cursorIndex)
  def getIntensity(): Float = peakelData.intensityValues(cursorIndex)
}

case class PeakelDataMatrixCursor(
  peakelData: PeakelDataMatrix,
  var cursorIndex: Int = - 1
) extends IPeakelDataCursor {
  def getSpectrumId(): Long = peakelData.spectrumIds(cursorIndex)
  def getElutionTime(): Float = peakelData.elutionTimes(cursorIndex)
  def getMz(): Double = peakelData.mzValues(cursorIndex)
  def getIntensity(): Float = peakelData.intensityValues(cursorIndex)
}

