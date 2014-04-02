package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer

import fr.profi.mzdb.algo.signal.detection.WaveletBasedPeakelFinder
import fr.profi.mzdb.model._
import fr.profi.mzdb.utils.ms.MsUtils

class PredictedMzFtExtractor(
  val scanHeaderById: Map[Int,ScanHeader],
  val nfByScanId: Map[Int,Float],
  val xtractConfig:FeatureExtractorConfig,
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
) extends AbstractSupervisedFtExtractor {

  def extractFeature( putativeFt: PutativeFeature, pklTree: PeakListTree ): Option[Feature] = {
    
    val theoIP = putativeFt.theoreticalIP
    val moz = putativeFt.getMz // suppose to be the mz of the monoisotopic right ?
    val charge = putativeFt.getCharge // charge magically deduced by the machine
    val mzTolPPM = xtractConfig.mzTolPPM 
    
    val mzTolDa = MsUtils.ppmToDa(moz, mzTolPPM)
    
    //less memory usage than map
    //need the scanIds because they may not be filled in the LcContext of one peak (can have hole during extraction)
    //which is really bad/sad...
    val xic = new ArrayBuffer[Peak]
    val xicScanIDs = new ArrayBuffer[Int]
    
    //buid the xic with getNearestPeak for each scan
    for (id  <- pklTree.scanIds) { 
      val p = pklTree.getNearestPeak(id, moz, mzTolDa)
      if ( p != null) {
    	  xic += p 
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
	//by elutionTime ?
	val highestPeakel = peakels.sortBy( x => math.abs(xic(x.index).getLcContext().getElutionTime() - putativeFt.getElutionTime)).head

	val isotopicPatterns = new Array[Option[IsotopicPattern]](highestPeakel.maxIdx - highestPeakel.minIdx + 1)

	var c = 0
	for ( i <- highestPeakel.minIdx to highestPeakel.maxIdx) {
	  val peak = xic(i)
	  val scanID = xicScanIDs(i)
	  val ipOpt = pklTree.extractIsotopicPattern(scanHeaderById(scanID), theoIP, mzTolPPM )
	  isotopicPatterns(c) = ipOpt
	}
	
	// FIXME: check why ip is null => it should not be
	val definedIps = isotopicPatterns.filter(ip => ip != null && ip.isDefined).map(_.get)
	
	// use of the constructor that build peakels
    val f =  new Feature(moz, charge, definedIps)
    
    this.overlappingFeaturesExtractor.extractOverlappingFeatures(f, theoIP, pklTree)
    
    Some(f)
    
  }
}
