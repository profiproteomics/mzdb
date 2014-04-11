package fr.profi.mzdb.algo.ms.calibration
import fr.profi.mzdb.MzDbReader
import scala.collection.mutable.ArrayBuffer
import java.util.TreeMap
import fr.profi.mzdb.model.PeakList
import scala.collection.JavaConversions._
import java.io.PrintWriter
import java.io.File


object CalibrationCurveWriter {
	
    
	
	private def compute(mzdb:MzDbReader, 
	                    maxDeltaPpm : Double, 
	                    intensityThresh:Double = 0.9, 
	                    skipBeginningScans: Int = 0) : Pair[TreeMap[Int, Double], TreeMap[Int, Double]] ={
	  
	  
	  var ms1Iter = mzdb.getMsScanIterator(1)
	  for (i <- 0 until skipBeginningScans) {
	     if (ms1Iter.hasNext) ms1Iter.next
	     else throw new Throwable("reach last scan while trying to skip beginning scans")
	  }
	  
	  
	  
	  var m445 = new TreeMap[Int, Double]
	  var result = new TreeMap[Int, Double]
	  
	  var firstScan = if (ms1Iter.hasNext) ms1Iter.next() else throw new Throwable("not enough scans...")
	  
	  while (ms1Iter.hasNext) {
		  
	    var secondScan = ms1Iter.next
	    
	    var mergedSpectrum = firstScan.getPeaks().map{ x=> new ClassedPeakScala(x.getMz, x.getIntensity(), 1) } ++ secondScan.getPeaks().map{ x=> new ClassedPeakScala(x.getMz, x.getIntensity(), 2) }
	    
	    //eliminate noise little peaks using quantile approach, other techniques ?
	    //val sortedIntensities = mergedSpectrum.map (_.getIntensity).toSeq.sortWith(_<_)
	    val thresh = mergedSpectrum.map (_.getIntensity).toSeq.sortWith(_<_)((intensityThresh * mergedSpectrum.length) toInt)
	    
	    mergedSpectrum = mergedSpectrum.filter(_.getIntensity > thresh)
	    
	    var deltaMass = new ArrayBuffer[Double]
	    var i = 0
	    
	    while (i < mergedSpectrum.length - 2 ) {

			if ( mergedSpectrum(i).order != mergedSpectrum(i+1).order ){
				
				var mzFirst = 0.0
				var mzSecond = 0.0
				var pFirst :ClassedPeakScala = null
				var pSecond : ClassedPeakScala = null
				var gap = 1
				
				if ( mergedSpectrum(i+1).order == 1) {
					pFirst = mergedSpectrum(i+1)
					pSecond = mergedSpectrum(i)
				}
				else { 
					pSecond = mergedSpectrum(i+1)
					pFirst = mergedSpectrum(i)
				}
				
				if ( mergedSpectrum(i+2).order == mergedSpectrum(i).order ){
					
					var t = (math.abs(mergedSpectrum(i+2).getMz - mergedSpectrum(i+1).getMz) < math.abs(mergedSpectrum(i).getMz - mergedSpectrum(i+1).getMz));
					var goodPeak : ClassedPeakScala = null
					if (t) {
						goodPeak =  mergedSpectrum(i+2)
						gap +=1
					}
					else
						 goodPeak = mergedSpectrum(i)
					
					if (goodPeak.order == 1) {
						pFirst = goodPeak;
						pSecond = mergedSpectrum(i+1)
					}
					else {
						pSecond = goodPeak;
						pFirst = mergedSpectrum(i+1)
					}
				}
							
				mzFirst = pFirst.getMz; 
				mzSecond = pSecond.getMz;
				var deltaMz = mzSecond - mzFirst;
	
				var mean = (mzFirst + mzSecond)/2f
				var maxDeltaMz = mean * maxDeltaPpm / 1e6;
				
				if ( math.abs(deltaMz) < maxDeltaMz ) {
					deltaMass += daToPpm(mean, deltaMz)
					i += gap
				}
			}
			i+=1
		}//end first while
	    
	    var peak445 = new PeakList(firstScan.getPeaks(), 100.0).getNearestPeak(445.12, 445.12 * maxDeltaPpm / 1e6)
	    if (peak445 != null) m445.put(firstScan.getHeader().getId(), peak445.getMz)
	    
	    result.put(firstScan.getHeader().getId, deltaMass.sum / deltaMass.length)
	  }//end iterator
	  
	  var output = new TreeMap[Int, Double]
	  var sum = 0.0
	  for (entry <- result.entrySet()) {
	    sum += entry.getValue
	    output.put(entry.getKey, sum)
	  }
	  
	  (output, m445)
	}
	
	
	def writeCalibrationFile(mzDbFile:String, maxDeltaPpm:Double) = {
	  var mzdb = new MzDbReader(mzDbFile, false)
	  var result = compute(mzdb, maxDeltaPpm)
	  var o = result._1
	  var m445 = result._2
	  val writer = new PrintWriter(new File(mzDbFile + "_calib.txt" ))
	  
	  for (entry <- o.entrySet()) {
	    writer.write(entry.getKey + "\t" + entry.getValue + "\t" + m445.get(entry.getKey) + "\n")
	  }
	}
	
	def daToPpm( mz: Double,  d: Double) : Double = {d * 1e6 / mz}
		
}