package fr.profi.mzdb.featuredb

import com.almworks.sqlite4java.SQLiteConnection
import com.typesafe.scalalogging.LazyLogging
import fr.profi.util.serialization.ProfiJson

import java.io.File
import scala.collection.mutable.ArrayBuffer

object FeatureDbReader extends LazyLogging {


   def loadAssignedPeakelIds(sqliteConn: SQLiteConnection) : Set[Int] = {
     val sqlQuery = "SELECT peakels FROM feature"
     val stmt = sqliteConn.prepare(sqlQuery, false)

     val peakelIds = ArrayBuffer[Int]()

     while(stmt.step()) {
       val peakelIndexes = stmt.columnBlob(0)
        peakelIds.appendAll(ProfiJson.deserialize[Array[Int]](new String(peakelIndexes)))
     }

     peakelIds.toSet
  }

  def main(args: Array[String]): Unit = {
    val connection = new SQLiteConnection(new File("C:\\Local\\bruley\\Tests\\Proline-Zero-2.2.0-SNAPSHOT\\data\\tmp\\Ft-QEKAC160601_18.raw-8017318006187307595.sqlite"))
    connection.openReadonly

    val peakelIds = loadAssignedPeakelIds(connection)

    System.out.println("peakels : "+peakelIds.size)

  }
}