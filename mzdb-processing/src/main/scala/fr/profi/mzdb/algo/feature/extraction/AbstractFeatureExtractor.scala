package fr.profi.mzdb.algo.feature.extraction

import scala.Array.canBuildFrom
import scala.annotation.migration
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.fasterxml.jackson.annotation.JsonInclude
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.mzdb.model.Feature
import fr.profi.mzdb.model.IsotopicPattern
import fr.profi.mzdb.model.IsotopicPatternLike
import fr.profi.mzdb.model.OverlappingIsotopicPattern
import fr.profi.mzdb.model.ScanHeader
import fr.profi.mzdb.model.TheoreticalIsotopePattern
import fr.profi.mzdb.model.PeakListTree

abstract class AbstractFeatureExtractor extends Logging  {

  val scanHeaderById: Map[Int, ScanHeader]
  val nfByScanId: Map[Int, Float]
  val scanHeaders = scanHeaderById.values.toArray.sortBy(_.getId)
  val ms1ScanHeaderByCycleNum = Map() ++ scanHeaders.filter(_.getMsLevel() == 1 ).map(sh => sh.getCycle -> sh)
  

  private val TIME_INDEX_WIDTH = 15

  private val _scanIdsByTimeIndex: HashMap[Int, ArrayBuffer[Int]] = {

    val _tmpScanIdsMap = new HashMap[Int, ArrayBuffer[Int]]()

    for ((scanId, scanH) <- scanHeaderById) {
      val timeIndex = (scanH.getTime / TIME_INDEX_WIDTH).toInt
      _tmpScanIdsMap.getOrElseUpdate(timeIndex, new ArrayBuffer[Int]()) += scanH.getId
    }

    _tmpScanIdsMap
  }
  
  def getScanHeaderForTime(time: Float, msLevel: Int) = {

    val timeIndex = (time / TIME_INDEX_WIDTH).toInt

    var nearestScanHeader: ScanHeader = null
    val scanIdsByTimeIndex = this._scanIdsByTimeIndex

    for (index <- timeIndex - 1 to timeIndex + 1) {
      if (scanIdsByTimeIndex.contains(index)) {
        for (
          tmpScanId <- scanIdsByTimeIndex(index);
          scanH <- scanHeaderById.get(tmpScanId);
          if scanH.getMsLevel() == msLevel
        ) {
          if (nearestScanHeader == null || (scanH.getTime() - time).abs < (nearestScanHeader.getTime() - time).abs) {
            nearestScanHeader = scanH
          }
        }
      }
    }
    nearestScanHeader
  }


}


  