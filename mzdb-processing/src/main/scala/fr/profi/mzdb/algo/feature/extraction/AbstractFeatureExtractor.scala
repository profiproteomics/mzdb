package fr.profi.mzdb.algo.feature.extraction

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

import com.typesafe.scalalogging.LazyLogging

import fr.profi.mzdb.model._

abstract class AbstractFeatureExtractor extends LazyLogging {
  
  /** Required parameters */
  def spectrumHeaderById: Map[Long, SpectrumHeader]
  def nfBySpectrumId: Map[Long, Float]
  
  /** Computed values */
  protected val spectrumHeaders = spectrumHeaderById.values.toArray.sortBy(_.getId)
  protected val ms1SpectrumHeaderByCycleNum = spectrumHeaders.withFilter(_.getMsLevel() == 1 ).map(sh => sh.getCycle -> sh).toMap
  protected val ms1SpectrumIdByCycleNum: Map[Int, Long] = ms1SpectrumHeaderByCycleNum.map { case (cycle,sh) => cycle -> sh.getId }
  
  private val TIME_INDEX_WIDTH = 15
  
  /*private val spectrumIdsByTimeIndex: HashMap[Int, ArrayBuffer[Int]] = {

    val _tmpSpectrumIdsMap = new HashMap[Int, ArrayBuffer[Int]]()

    for ((spectrumId, spectrumH) <- spectrumHeaderById) {
      val timeIndex = (spectrumH.getTime / TIME_INDEX_WIDTH).toInt
      _tmpSpectrumIdsMap.getOrElseUpdate(timeIndex, new ArrayBuffer[Int]()) += spectrumH.getId
    }

    _tmpSpectrumIdsMap
  }*/
  
  private val spectrumHeadersByTimeIndex: HashMap[Int, ArrayBuffer[SpectrumHeader]] = {

    val _tmpSpectrumHeaderMap = new HashMap[Int, ArrayBuffer[SpectrumHeader]]()

    for ( spectrumH <- spectrumHeaders ) {
      val timeIndex = (spectrumH.getTime / TIME_INDEX_WIDTH).toInt
      _tmpSpectrumHeaderMap.getOrElseUpdate(timeIndex, new ArrayBuffer[SpectrumHeader]()) += spectrumH
    }

    _tmpSpectrumHeaderMap
  }
  
  protected def getSpectrumHeaderForTime(time: Float, msLevel: Int): Option[SpectrumHeader] = {

    val timeIndex = (time / TIME_INDEX_WIDTH).toInt

    var nearestSH: SpectrumHeader = null

    val timeIndexes = (timeIndex - 1 to timeIndex + 1).toArray
    val matchingSpectrumHeadersOpts = timeIndexes.map( spectrumHeadersByTimeIndex.get(_) )
    
    for(
      matchingSpectrumHeadersOpt <- matchingSpectrumHeadersOpts;
      matchingSpectrumHeaders <- matchingSpectrumHeadersOpt;
      spectrumH <- matchingSpectrumHeaders
      if spectrumH.getMsLevel() == msLevel
    ) {
      if ( nearestSH == null || (spectrumH.getTime() - time).abs < (nearestSH.getTime() - time).abs) {
        nearestSH = spectrumH
      }
    }
    
    Option(nearestSH)
  }
  
  /*protected def getSpectrumHeaderForTime(time: Float, msLevel: Int): Option[SpectrumHeader] = {

    val timeIndex = (time / TIME_INDEX_WIDTH).toInt

    var nearestSH: SpectrumHeader = null
    val spectrumIdsByTimeIndex = this.spectrumIdsByTimeIndex

    for (index <- timeIndex - 1 to timeIndex + 1) {
  
      for (
        tmpSpectrumIdOpt <- spectrumIdsByTimeIndex.get(index);
        tmpSpectrumId <- tmpSpectrumIdOpt;
        spectrumH <- spectrumHeaderById.get(tmpSpectrumId)
        if spectrumH.getMsLevel() == msLevel
      ) {
        if ( nearestSH == null || (spectrumH.getTime() - time).abs < (nearestSH.getTime() - time).abs) {
          nearestSH = spectrumH
        }
      }
    }
    
    Option(nearestSH)
  }*/

}


  