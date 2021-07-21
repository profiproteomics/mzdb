package fr.profi.mzdb.peakeldb

import java.io.File
import scala.collection.mutable.ArrayBuffer
import com.almworks.sqlite4java.SQLiteConnection
import com.almworks.sqlite4java.SQLiteStatement
import com.github.davidmoten.rtree.RTree
import com.github.davidmoten.rtree.geometry
import fr.profi.mzdb.Settings
import fr.profi.mzdb.peakeldb.io.PeakelDbReader
import fr.profi.mzdb.peakeldb.io.PeakelDbWriter
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.PeakelDataMatrix
import rx.lang.scala.Observable

import java.util.Arrays
import org.apache.commons.math3.stat.correlation.PearsonsCorrelation

object PeakelDbHelper {

  // Important: SQlite R*Tree floating values are 32bits floats, thus we need to expand the search
  // Advice from SQLite developers (https://www.sqlite.org/rtree.html#roundoff_error):
  // Applications should expand their contained-within query boxes slightly (by 0.000012%)
  // by rounding down the lower coordinates and rounding up the top coordinates, in each dimension.
  private val SQLITE_RTREE_UB_CORR = 1.0 + 0.00000012
  private val SQLITE_RTREE_LB_CORR = 1.0 - 0.00000012

  private val MIN_CORRELATION_THRESHOLD = Settings.isotopicPeakelsCorrelationThreshold
  private val PEARSON = new PearsonsCorrelation()
  

  def findPeakelIdsInRange(
    rTree: RTree[java.lang.Integer, geometry.Point],
    minMz: Double,
    maxMz: Double,
    minTime: Float,
    maxTime: Float): Seq[Int] = {

    // Retrieve peakel ids using the in-memory R*Tree
    val peakelIdIter = rTree.search(
      geometry.Geometries.rectangle(
        minMz, minTime, maxMz, maxTime
      )
    ).toBlocking().toIterable().iterator()

    val peakelIds = new ArrayBuffer[Int]()
    while (peakelIdIter.hasNext()) {
      peakelIds += peakelIdIter.next().value()
    }

    peakelIds
  }

  def findPeakelsInRange(
    sqliteConn: SQLiteConnection,
    rTreeOpt: Option[RTree[java.lang.Integer, geometry.Point]],
    minMz: Double,
    maxMz: Double,
    minTime: Float,
    maxTime: Float): Array[Peakel] = {

    // Retrieve peakel ids using the in-memory R*Tree
    val peakelIds = if (rTreeOpt.isDefined) {
      findPeakelIdsInRange(rTreeOpt.get, minMz, maxMz, minTime, maxTime)
    } // Retrieve peakel ids using the peakelDB R*Tree
    else {
      PeakelDbReader.findPeakelIdsInRangeFromPeakelDB(sqliteConn, minMz, maxMz, minTime, maxTime)
    }

    //assert(peakelIdsMem.length == peakelIds.length, "something wrong in Rtrees")

    val peakels = PeakelDbReader._loadPeakelsForIds(sqliteConn, peakelIds).toArray

    peakels
  }
  
    
   def findCorrelatingPeakel(ref: Peakel, peakels: Array[Peakel]): Option[Peakel] = {
    
    val correlations = peakels.map { peakel => 
      computeCorrelation(ref,peakel) -> peakel
    }
    val (correlation, bestPeakel) = correlations.maxBy(_._1)

    if (correlation > MIN_CORRELATION_THRESHOLD) Some(bestPeakel)
    else None
  }

  def computeCorrelation(p1: Peakel, p2: Peakel): Double = {

    val (y1, y2) = zipPeakelIntensities(p1, p2)
    PEARSON.correlation(y1, y2)

  }
 
  private def zipPeakelIntensities(p1: Peakel, p2: Peakel): (Array[Double], Array[Double]) = {

    val p1Intensities = p1.getIntensityValues()
    val p2Intensities = p2.getIntensityValues()
    var indexes = {p1.spectrumIds ++ p2.spectrumIds}.toList.distinct.sortWith((a,b) => a < b)
    
    def _buildIntensityVector(p: Peakel): Array[Double] = {
      val y = Array.fill(indexes.size){0.0} 

      var index = 0;
      var peakelCursor = p.getNewCursor()
      while (peakelCursor.next()) {
        while ((index < indexes.length) && (indexes(index) < peakelCursor.getSpectrumId())) {
          index += 1
        }
        if (indexes(index) == peakelCursor.getSpectrumId()) {
          y(index) = peakelCursor.getIntensity()
        }
      }
      y
    }
    
    (_buildIntensityVector(p1),_buildIntensityVector(p2))
  }

}