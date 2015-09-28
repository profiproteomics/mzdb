package fr.profi.mzdb

import fr.profi.mzdb.model.Peak
import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.Feature
import scala.collection.mutable.ArrayBuffer
import org.apache.commons.lang3.tuple.ImmutablePair
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._
import fr.profi.mzdb.model.ILcContext
import fr.profi.util.stat.EntityHistogramComputer
import fr.profi.mzdb.model.IsolationWindow

case class Spectrum(precursorMz: Double, precursorCharge: Int, elutionTime: Float, peaks: Array[Peak])

/**
 *  May provide additionnal configuration ?
 */
class MzDbMSnDemultiplexer(mzDbReader: MzDbReader) extends LazyLogging { // for each
  
  val spectrumHeaderById = mzDbReader.getMs2SpectrumHeaderById.map { case (k,v) => k.toLong -> v } toMap
  val minNbPeaksInSpectrum = 3

  def demultiplexMSnData(): Array[Spectrum] = {
    /* DIA Maps
   * // 1 - detectFeatures ( MzDbFeatureDetector -> MS Level 1)
   * 
   *  for each (minParentMz, maxParentMz) {
   *  ms2 rs iterator
   * // 2 - detectPeakels ( MzDbFeatureDetector -> MS Level 2)
   * // 3 - _groupMSnCorrelatedPeakels( ms1Features, ms2Peakels )
   */
    println("Unsupervised ms1 feature detection")
    val ftDetectorConf = FeatureDetectorConfig(msLevel = 1, mzTolPPM = 15)
    val ftDetectorMs1 = new MzDbFeatureDetector(mzDbReader, ftDetectorConf)
    
    // ensure data are sorted by mz 
    val ms1Fts = ftDetectorMs1.detectFeatures().filter( ft => (ft.charge == 2 || ft.charge == 3) ).sortBy(_.getMz)
    println(s"# ${ms1Fts.length} detected ms1 features")
    
    // ms2 stuffs
    //Setting a new config to our featureDetector
    val ftDetectorConfMs2 = FeatureDetectorConfig(msLevel = 2, mzTolPPM = 20)

    // get dia ranges
    //convert to scala object, TODO: do that properly
    val isolationWindows = mzDbReader.getDIAIsolationWindows()

    // group ms1Features by isolation window range
    val ms1FtsByIWIdx = this._groupFtsByIsolationWindowIdx(isolationWindows, ms1Fts)

    //java list has not foreach interface ! conversion ?
    val spectra = new ArrayBuffer[Spectrum]()
    for (i <- 0 until isolationWindows.length) {
      val pair = isolationWindows(i)
      val (minParentMz, maxParentMz) = (pair.getMinMz, pair.getMaxMz)
      println(s"Unsupervised ms2 feature detection: range(${minParentMz}, ${maxParentMz})")
      
      val ftDetectorMs2 = new MzDbFeatureDetector(mzDbReader, ftDetectorConfMs2)
      val lcMSnRsIter = mzDbReader.getLcMsnRunSliceIterator(minParentMz, maxParentMz)

      val peakels = ftDetectorMs2.detectPeakels(lcMSnRsIter)

      //val peakelGroupByTime = featureDetector.groupCorrelatedPeakels(peakels)

      // filter peakel on currentParentMzs
      // ArrayBuffer[Feature] should never be empty
      val ms1FtsSub = ms1FtsByIWIdx(i)
      val newSpectra = this._groupMSnCorrelatedPeakels(ms1FtsSub.toArray, peakels)

      spectra ++= newSpectra
    }

    spectra.toArray
  }

  /**
   * Receive ms1Fts filtered on mz dimension
   */
  private def _groupMSnCorrelatedPeakels(ms1Fts: Array[Feature], msnPeakels: Array[Peakel]): Array[Spectrum] = {
    logger.info("combining peakels into features...")
    
    val allLcEntities = ms1Fts ++ msnPeakels
    
    val peakelTimeByLcEntity = allLcEntities.map { lcEntity => lcEntity -> _extractWeightedAverageTime(lcEntity) } toMap
    
    // Clusterize peakels having an apex separated by a given time value (10 secs)    
    val histoComputer = new EntityHistogramComputer[Product with Serializable with ILcContext]( allLcEntities, { entity  =>
      //peakel.apexSpectrumContext.getElutionTime
      //entity.getElutionTime
      peakelTimeByLcEntity(entity)
    })
    
    val peakelTimes = peakelTimeByLcEntity.values
    val timeRange = peakelTimes.max - peakelTimes.min
    val peakelBins = histoComputer.calcHistogram(nbins = (timeRange / 3f).toInt)

    // TODO: keep the information about the sliding to avoid redundant results
    // Starting points should always come from the second bin
    val spectra = new ArrayBuffer[Spectrum](peakelBins.size)
    
    peakelBins.sliding(3).foreach { peakelBinGroup =>

      val peakelGroup = peakelBinGroup.flatMap(_._2)
      if (peakelGroup.isEmpty == false) {
        val meanTime = peakelBinGroup.map(_._1.center).sum / peakelBinGroup.length
        val times = peakelGroup.map { peakelTimeByLcEntity(_) }

        logger.debug(s"min time is ${times.min} and max time is ${times.max}")
        
        val( ms1FtsInGroup, msnPeakels ) = (new ArrayBuffer[Feature],new ArrayBuffer[Peakel])
        
        // Partitions entities regarding their type
        peakelGroup.map { peakelOrFt =>
          peakelOrFt match {
            case f: Feature => ms1FtsInGroup += f
            case p: Peakel  => msnPeakels += p
          }
        }
        
        if( msnPeakels.length >= minNbPeaksInSpectrum ) {
          for( ft <- ms1FtsInGroup ) {
            spectra += Spectrum(
              precursorMz = ft.mz,
              precursorCharge = ft.charge,
              elutionTime = meanTime.toFloat,
              peaks = msnPeakels.toArray.map(_.getCursorAtApex().toPeak(spectrumHeaderById) ).sortBy(_.getMz)
            )
          }
        }
        
      }
    }
    
    spectra.toArray
  }
  
  private def _extractWeightedAverageTime( peakelOrFeature: ILcContext ): Float = {
    peakelOrFeature match {
      case p: Peakel  => p.calcWeightedAverageTime
      case f: Feature => f.weightedAverageTime
    }
  }

  /*private def _toSpectra(peakelGroupByMs1Feature: Array[(Feature, Array[Peakel])]): Array[Spectrum] = {
    peakelGroupByMs1Feature.map {
      case (ms1Ft, peakels) =>
        val peaks = peakels.map(p => p.getApex)
        Spectrum(ms1Ft.getMz, ms1Ft.getCharge, peaks)
    }
  }*/

  def _groupFtsByIsolationWindowIdx(isolationWindowRanges: Array[IsolationWindow], ms1Fts: Array[Feature]): HashMap[Int, ArrayBuffer[Feature]] = {

    val ftsByIWIdx = new HashMap[Int, ArrayBuffer[Feature]]()
    val firstIW = isolationWindowRanges(0)
    val rangesCount = isolationWindowRanges.length
    
    var currIdx = 0
    var currMaxParentMz = firstIW.getMaxMz
    for (ft <- ms1Fts) {
      val mz = ft.getMz
      
      if (mz > currMaxParentMz) {
        
        // Do not increase the index if feature m/z exceeds last isolation window m/z
        if( currIdx + 1 < rangesCount) {
          currIdx += 1
        }
        
        currMaxParentMz = isolationWindowRanges(currIdx).getMaxMz
      }
      
      ftsByIWIdx.getOrElseUpdate(currIdx, new ArrayBuffer[Feature]) += ft
    }
    
    ftsByIWIdx
  }
}