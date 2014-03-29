package fr.profi.mzdb.algo.feature.extraction

import scala.annotation.migration
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import com.typesafe.scalalogging.slf4j.Logging
import fr.profi.mzdb.model._

abstract class AbstractFeatureExtractor extends Logging {

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
  
  def getScanHeaderForTime(time: Float, msLevel: Int): Option[ScanHeader] = {

    val timeIndex = (time / TIME_INDEX_WIDTH).toInt

    var nearestSH: ScanHeader = null
    val scanIdsByTimeIndex = this._scanIdsByTimeIndex

    for (index <- timeIndex - 1 to timeIndex + 1) {
  
      for (
        tmpScanIdOpt <- scanIdsByTimeIndex.get(index);
        tmpScanId <- tmpScanIdOpt;
        scanH <- scanHeaderById.get(tmpScanId)
        if scanH.getMsLevel() == msLevel
      ) {
        if ( nearestSH == null || (scanH.getTime() - time).abs < (nearestSH.getTime() - time).abs) {
          nearestSH = scanH
        }
      }
    }
    
    Option(nearestSH)
  }


}


  