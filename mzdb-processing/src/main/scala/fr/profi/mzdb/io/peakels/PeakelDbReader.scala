package fr.profi.mzdb.io.peakels

import java.io.File

import scala.collection.mutable.ArrayBuffer

import com.almworks.sqlite4java.SQLiteConnection
import com.almworks.sqlite4java.SQLiteStatement
import com.github.davidmoten.rtree.RTree
import com.github.davidmoten.rtree.geometry

import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.PeakelDataMatrix
import rx.lang.scala.Observable
import java.util.Arrays
import org.apache.commons.math3.stat.correlation.PearsonsCorrelation

object PeakelDbReader {

  // Important: SQlite R*Tree floating values are 32bits floats, thus we need to expand the search
  // Advice from SQLite developers (https://www.sqlite.org/rtree.html#roundoff_error):
  // Applications should expand their contained-within query boxes slightly (by 0.000012%)
  // by rounding down the lower coordinates and rounding up the top coordinates, in each dimension.
  private val SQLITE_RTREE_UB_CORR = 1.0 + 0.00000012
  private val SQLITE_RTREE_LB_CORR = 1.0 - 0.00000012

  private val PEARSON = new PearsonsCorrelation()
  
  def findPeakelIdsInRangeFromPeakelDB(
    sqliteConn: SQLiteConnection,
    minMz: Double,
    maxMz: Double,
    minTime: Float,
    maxTime: Float): Seq[Int] = {

    // Retrieve peakel ids using the peakelDB R*Tree
    val peakelIdRtreeSqlQuery = "SELECT id, min_mz, max_mz FROM peakel_rtree WHERE min_mz >= ? AND max_mz <= ? AND min_time >= ? AND max_time <= ?"

    val peakelIdStmt = sqliteConn.prepare(peakelIdRtreeSqlQuery, false)
    val peakelIds = new ArrayBuffer[Int]()

    try {
      peakelIdStmt
        .bind(1, minMz * SQLITE_RTREE_LB_CORR)
        .bind(2, maxMz * SQLITE_RTREE_UB_CORR)
        .bind(3, minTime)
        .bind(4, maxTime)
      //println(s"minMz=$minMz maxMz=$maxMz minTime=$minTime maxTime=$maxTime")

      while (peakelIdStmt.step()) {
        val mz1 = peakelIdStmt.columnDouble(1)
        val mz2 = peakelIdStmt.columnDouble(2)
        // Filter the peakel again to compensate for query padding
        if (mz1 >= minMz || mz2 <= maxMz) {
          peakelIds += peakelIdStmt.columnInt(0)
        }
      }

    } finally {
      // Release resources
      peakelIdStmt.dispose()
    }

    peakelIds
  }


  def streamPeakels(
    peakelFile: File
  ): Observable[Array[Peakel]] = {

    Observable[Array[Peakel]] { subscriber =>
            
      // Open TMP SQLite file
      val sqliteConn = new SQLiteConnection(peakelFile)
      sqliteConn.openReadonly()
      
      try {
        
        val peakelsBuffer = loadAllPeakels(sqliteConn)
        
        subscriber.onNext(peakelsBuffer.toArray)
        subscriber.onCompleted()
  
      } finally {
        // Release resources
        sqliteConn.dispose()
      }
    }

  }

  
  def loadAllPeakels(sqliteConn: SQLiteConnection, sizeHint: Int = 50000): ArrayBuffer[Peakel] = {
    loadManyPeakels(sqliteConn, sizeHint)
  }

  def loadManyPeakels(
    sqliteConn: SQLiteConnection,
    sizeHint: Int = 50000,
    idPredicate: Option[Int => Boolean] = None): ArrayBuffer[Peakel] = {

    val dbFile = sqliteConn.getDatabaseFile
    // Read file if not in-memory database
    if (dbFile != null) _readWholeFile(sqliteConn.getDatabaseFile)

    val peakelSqlQuery = "SELECT id, peaks, left_hwhm_mean, left_hwhm_cv, right_hwhm_mean, right_hwhm_cv FROM peakel;"

    _loadPeakelsForQuery(sqliteConn, peakelSqlQuery, sizeHint, idPredicate)
  }

    def loadPeakelDbInMemory(peakelDb: SQLiteConnection): SQLiteConnection = {
    val backup = peakelDb.initializeBackup(null)
    val memPeakelDb = backup.getDestinationConnection()
    while (!backup.isFinished()) {
      backup.backupStep(-1)
    }

    backup.dispose(false) // false means "do not dispose the in-memory database"

    memPeakelDb
  }
  
  private def _readWholeFile(file: File) {

    val fis = new java.io.FileInputStream(file)
    val b = new Array[Byte](1024 * 1024)

    while (fis.available() != 0) {
      fis.read(b)
    }

    fis.close()
  }

  def _loadPeakelsForIds(sqliteConn: SQLiteConnection, peakelIds: Seq[Int]): ArrayBuffer[Peakel] = {

    val peakelPkSqlQuery = "SELECT id, peaks, left_hwhm_mean, left_hwhm_cv, " +
      s"right_hwhm_mean, right_hwhm_cv FROM peakel WHERE id IN (${peakelIds.mkString(",")});"

    val peakels = _loadPeakelsForQuery(sqliteConn, peakelPkSqlQuery, peakelIds.length)

    assert(peakelIds.length == peakels.length, "invalid number of retrieved peakels from peakelDB file")

    peakels
  }

  private def _loadPeakelsForQuery(
    sqliteConn: SQLiteConnection,
    sqlQuery: String,
    sizeHint: Int = 100,
    idPredicate: Option[Int => Boolean] = None): ArrayBuffer[Peakel] = {

    val peakelStmt = sqliteConn.prepare(sqlQuery, false)

    val peakels = new ArrayBuffer[Peakel](sizeHint)

    try {

      if (idPredicate.isDefined) {
        while (peakelStmt.step()) {
          if (idPredicate.get.apply(peakelStmt.columnInt(0))) {
            peakels += _buildPeakel(peakelStmt)
          }
        }
      } else {
        while (peakelStmt.step()) {
          peakels += _buildPeakel(peakelStmt)
        }
      }

    } finally {
      // Release resources
      peakelStmt.dispose()
    }

    peakels
  }

  private def _buildPeakel(peakelStmt: SQLiteStatement): Peakel = {
    val peakelId = peakelStmt.columnInt(0)
    val peakelMessageAsBytes = peakelStmt.columnBlob(1)

    //val peakelMessage = ProfiMsgPack.deserialize[PeakelDataMatrix](peakelMessageAsBytes)
    val peakelMessage = PeakelDataMatrix.unpack(peakelMessageAsBytes)
    val (intensitySum, area) = peakelMessage.integratePeakel()

    new Peakel(
      peakelId,
      peakelMessage,
      intensitySum,
      area,
      leftHwhmMean = peakelStmt.columnDouble(2).toFloat,
      leftHwhmCv = peakelStmt.columnDouble(3).toFloat,
      rightHwhmMean = peakelStmt.columnDouble(4).toFloat,
      rightHwhmCv = peakelStmt.columnDouble(5).toFloat
    )
  }


}