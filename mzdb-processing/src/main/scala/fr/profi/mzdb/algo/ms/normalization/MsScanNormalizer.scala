package fr.profi.mzdb.algo.ms.normalization

import scala.collection.mutable.LongMap
import fr.profi.mzdb.model.SpectrumHeader
import fr.profi.mzdb.MzDbReader
import fr.profi.util.collection._

/**
 * @author David Bouyssie
 *
 */
object MsSpectrumNormalizer {
  
  def computeNfBySpectrumId(mzDbReader: MzDbReader): LongMap[Float] = {
    
    val cyclesCount = mzDbReader.getCyclesCount()
    val medians = new Array[Float]( cyclesCount )
    val spectrumIds = new Array[Long]( cyclesCount )
    
    val msSpectrumIter = mzDbReader.getSpectrumIterator( 1 )
    while( msSpectrumIter.hasNext ) {
      val spectrum = msSpectrumIter.next
      
      val intensities = spectrum.toPeaks().map { _.getIntensity }
      val medInt = median(intensities)
      
      val sh = spectrum.getHeader
      val idx = sh.getCycle - 1
      spectrumIds(idx) = sh.getId
      medians(idx) = medInt.toFloat
    }
    
    val nfs = computeNFs(medians)
    
    (spectrumIds zip nfs) toLongMapWith { case(id,nf) => (id -> nf) }
  }

  def computeNFs( sic: Array[Float] ) = {
    
    val nbSpectra = sic.length
    
    val smoothedValues = smoothValues(sic,1)
    val nfs = new Array[Float](nbSpectra)
    
    for( idx <- 0 until nbSpectra ) {
      val intensity = sic(idx)
      val corIntensity = smoothedValues(idx)
      nfs(idx) = corIntensity/intensity
    }
    
    nfs
  }
  
  protected def median(s: Seq[Float]): Double = {
    val (lower, upper) = s.sortWith(_<_).splitAt(s.size / 2)
    if (s.size % 2 == 0) (lower.last + upper.head) / 2.0 else upper.head
  }
  
  protected def smoothValues(values: Array[Float], times: Int): Array[Float] = {
    
    import mr.go.sgfilter.SGFilterMath3
    
    val(nl,nr,order) = (10,10,1)
    
    val leftPad = Array.fill(nl)( values.head )
    val rightPad = Array.fill(nl)( values.last )
    val coeffs = SGFilterMath3.computeSGCoefficients(nl,nr,order)    
    val sgFilter = new SGFilterMath3(nl,nr)
    
    var smoothedValues = values
    for( i <- 1 to times ) {
      smoothedValues = sgFilter.smooth(values,leftPad,rightPad,coeffs)
    }
    
    smoothedValues
  }

}