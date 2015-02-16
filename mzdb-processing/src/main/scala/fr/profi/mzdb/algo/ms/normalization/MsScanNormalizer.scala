package fr.profi.mzdb.algo.ms.normalization

import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.MzDbReader

/**
 * @author David Bouyssie
 *
 */
object MsScanNormalizer {
  
  def computeNfByScanId(mzDbReader: MzDbReader): Map[Int,Float] = {
    
    val cyclesCount = mzDbReader.getCyclesCount()
    val medians = new Array[Float]( cyclesCount )
    val scanIds = new Array[Int]( cyclesCount )
    
    val msScanIter = mzDbReader.getMsScanIterator( 1 )
    while( msScanIter.hasNext ) {
      val scan = msScanIter.next
      
      val intensities = scan.getPeaks().map { _.getIntensity }
      val medInt = median(intensities)
      
      val sh = scan.getHeader
      val idx = sh.getCycle - 1
      scanIds(idx) = sh.getId
      medians(idx) = medInt.toFloat
    }
    
    val nfs = computeNFs(medians)
    
    Map() ++ (scanIds zip nfs) map { case(id,nf) => (id -> nf) }
  }

  def computeNFs( sic: Array[Float] ) = {
    
    val nbScans = sic.length
    
    val smoothedValues = smoothValues(sic,1)
    val nfs = new Array[Float](nbScans)
    
    for( idx <- 0 until nbScans ) {
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