package fr.profi.mzdb.algo.feature.extraction

import fr.profi.mzdb.MzDbReader
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.PeakListTree
import fr.profi.mzdb.model.PutativeFeature
import fr.profi.mzdb.model.ScanHeader
import scala.collection.mutable.ArrayBuffer
import fr.profi.mzdb.model.Peak
import fr.profi.mzdb.utils.ms.MsUtils
import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.utils.math.wavelet.MexicanHat

class PredictedMzFtExtractor(
  //val mzDbReader: MzDbReader,
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val mzTolPPM: Float,
  val maxNbPeaksInIP: Int,
  val minNbOverlappingIPs: Int
) extends AbstractSupervisedFtExtractor {

  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree ): Option[Feature] = {
    
    val theoIP = putativeFt.theoreticalIP
    val moz = putativeFt.getMz // suppose to be the mz of the monoisotopic right ?
    val charge = putativeFt.getCharge // charge magically deduced by the machine
    
    val mzTolDa = MsUtils.ppmToDa(moz, mzTolPPM)
    
    //less memory usage than map
    //need the scanIds because they may not be filled in the LcContext of one peak (can have hole during extraction)
    //which is really bad/sad...
    val xic = new ArrayBuffer[Peak]
    val xicScanIDs = new ArrayBuffer[Int]
    
    //buid the xic with getNearestPeak for each scan
    for (id  <- pklTree.scansIDs) { 
      val p = pklTree.getNearestPeak(id, moz, mzTolDa)
      if ( ! p.isEmpty ) {
    	  xic += pklTree.getNearestPeak(id, moz, mzTolDa).get 
    	  xicScanIDs += id
      }
    }
   
	//build cwt, if no good ...
	val peakelFinder = new WaveletBasedPeakelFinder( xic ) //gaussainfirstder (Coombes) by default
	val peakels = peakelFinder.findCwtPeakels()
	
	//return Option[Feature] if cwt did not found any peaks
	if ( peakels.isEmpty) 
	  return Option.empty[Feature]
	
	//take the highest peakel since we have to return only one feature ?
	//val highestPeakel = peakels.sortBy( x=> xic(x.apex).getIntensity ).last
	//by elutionTime ? Some work TODO here LcContext well filled ?
	val highestPeakel = peakels.sortBy( x => math.abs(xic(x.apex).getLcContext().getElutionTime() - putativeFt.getElutionTime)).first

	val isotopicPatterns = new Array[Option[IsotopicPattern]](highestPeakel.maxIdx - highestPeakel.minIdx + 1)

	var c = 0
	for ( i <- highestPeakel.minIdx to highestPeakel.maxIdx) {
	  val peak = xic(i)
	  val scanID = xicScanIDs(i)
	  val ipOpt = pklTree.extractIsotopicPattern(scanHeaderById(scanID), theoIP, mzTolPPM, 2)
	  if( ipOpt.isDefined ) {
        val ip = ipOpt.get
        val intensity = ip.intensity
        // If we have peaks
        if( ip.peaks.length > 0 ) {
          val olpIPs = this._extractOverlappingIPs( ip, theoIP, pklTree )
          // Set overlapping IPs if at least one has been found
          val nbOlpIPs = olpIPs.length
          if( nbOlpIPs > 0 ) {
            ip.overlappingIps = olpIPs.toArray
          }
        }
	  }
	  isotopicPatterns(c) = ipOpt
	}
	
	// FIXME: check why ip is null => it should not be
	val definedIps = isotopicPatterns.filter(ip => ip != null && ip.isDefined).map(_.get)
	
	// use of the constructor that build peakels
    val f =  new Feature(
      Feature.generateNewId(),
      moz,
      charge,
      definedIps
    )
    
    this.updateFtOverlappingFeatures(
      f,
      definedIps,
      this.minNbOverlappingIPs
    )
    
    Some(f)
    
  }
}
