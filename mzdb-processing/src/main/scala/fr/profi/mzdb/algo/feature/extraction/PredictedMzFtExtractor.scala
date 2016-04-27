package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LongMap

import fr.profi.mzdb.algo.signal.detection.waveletImpl._
import fr.profi.mzdb.model._
import fr.profi.mzdb.utils.ms.MsUtils

class PredictedMzFtExtractor(
  val spectrumHeaderById: LongMap[SpectrumHeader],
  val nfBySpectrumId: LongMap[Float],
  val xtractConfig: FeatureExtractorConfig,
  val peakelDetectionConfig: PeakelDetectionConfig = PeakelDetectionConfig(DetectionAlgorithm.WAVELET),
  val overlapXtractConfig: OverlappingFeatureExtractorConfig
) extends AbstractSupervisedFtExtractor {

  def extractFeature(putativeFt: PutativeFeature, pklTree: PeakListTree): Option[Feature] = {

    val theoIP = putativeFt.theoreticalIP
    val moz = putativeFt.getMz // suppose to be the mz of the monoisotopic right ?
    val charge = putativeFt.getCharge // charge magically deduced by the machine
    val mzTolPPM = xtractConfig.mzTolPPM

    val mzTolDa = MsUtils.ppmToDa(moz, mzTolPPM)

    //less memory usage than map
    //need the spectrumIds because they may not be filled in the LcContext of one peak (can have hole during extraction)
    //which is really bad/sad...
    val xic = new ArrayBuffer[Peak]
    val xicSpectrumIDs = new ArrayBuffer[Long]

    //buid the xic with getNearestPeak for each spectrum
    for (id <- pklTree.spectrumIds) {
      val p = pklTree.getNearestPeak(id, moz, mzTolDa)
      if (p != null) {
        xic += p
        xicSpectrumIDs += id
      }
    }

    //build cwt, if no good ...
    val peakelFinder = new WaveletPeakelFinderNeumann(xic) //gaussainfirstder (Coombes) by default
    val cwtPeakels = peakelFinder.findCwtPeakels()

    //return Option[Feature] if cwt did not found any peaks
    if (cwtPeakels.isEmpty)
      return Option.empty[Feature]

    //take the highest peakel since we have to return only one feature ?
    //by elutionTime ?
    val highestPeakel = cwtPeakels.sortBy(x => math.abs(xic(x.apexIndex).getLcContext().getElutionTime() - putativeFt.getElutionTime)).head

    val isotopicPatterns = new Array[Option[IsotopicPattern]](highestPeakel.maxIdx - highestPeakel.minIdx + 1)

    var c = 0
    for (i <- highestPeakel.minIdx to highestPeakel.maxIdx) {
      val peak = xic(i)
      val spectrumID = xicSpectrumIDs(i)
      val ipOpt = pklTree.extractIsotopicPattern(spectrumHeaderById(spectrumID), theoIP, mzTolPPM)
      isotopicPatterns(c) = ipOpt
    }

    // FIXME: check why ip is null => it should not be
    val definedIps = isotopicPatterns.filter(ip => ip != null && ip.isDefined).map(_.get)

    // TODO: use of the constructor that take peakels
    val f = new Feature(
      mz = moz,
      charge = charge,
      isotopicPatterns = definedIps,
      isPredicted = true
    )

    this.overlappingFeaturesExtractor.extractOverlappingFeatures(f, theoIP, pklTree)

    Some(f)

  }
}
