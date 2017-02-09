package fr.profi.mzdb.io.peakels

import com.almworks.sqlite4java.SQLiteConnection
import fr.profi.mzdb.model.PeakelDataMatrix
import fr.profi.mzdb.model.Peakel
import java.io.File

object PeakelDbWriter {
  
    def initPeakelStore(fileLocation: File): SQLiteConnection = {

    // Open SQLite conenction
    val connection = new SQLiteConnection(fileLocation)
    connection.open(true) // allowCreate = true

    // SQLite optimization
    connection.exec("PRAGMA synchronous=OFF;")
    connection.exec("PRAGMA journal_mode=OFF;")
    connection.exec("PRAGMA temp_store=2;")
    connection.exec("PRAGMA cache_size=100000;")

    // Create the DDL
    // Note that this DDL will be later used directly in the mzDB
    val ddlQuery = """
    CREATE TABLE peakel (
      id INTEGER NOT NULL PRIMARY KEY,
      mz REAL NOT NULL,
      elution_time REAL NOT NULL,
      apex_intensity REAL NOT NULL,
      area REAL NOT NULL,
      duration REAL NOT NULL,
      left_hwhm_mean REAL,
      left_hwhm_cv REAL,
      right_hwhm_mean REAL,
      right_hwhm_cv REAL,
      is_overlapping TEXT NOT NULL,
      features_count INTEGER NOT NULL,
      peaks_count INTEGER NOT NULL,
      peaks BLOB NOT NULL,
      param_tree TEXT,
      first_spectrum_id INTEGER NOT NULL,
      last_spectrum_id INTEGER NOT NULL,
      apex_spectrum_id INTEGER NOT NULL,
      ms_level INTEGER NOT NULL,
      map_id INTEGER
    );

    CREATE VIRTUAL TABLE peakel_rtree USING rtree(
      id INTEGER NOT NULL PRIMARY KEY,
      min_ms_level INTEGER NOT NULL,
      max_ms_level INTEGER NOT NULL, 
      min_parent_mz REAL NOT NULL,
      max_parent_mz REAL NOT NULL,
      min_mz REAL NOT NULL,
      max_mz REAL NOT NULL,
      min_time REAL NOT NULL,
      max_time REAL NOT NULL
    );
"""
    /*
      FOREIGN KEY (first_spectrum_id) REFERENCES spectrum (id),
      FOREIGN KEY (last_spectrum_id) REFERENCES spectrum (id),
      FOREIGN KEY (apex_spectrum_id) REFERENCES spectrum (id),
      FOREIGN KEY (map_id) REFERENCES map (id)
   */

    connection.exec(ddlQuery)

    connection
  }

   def storePeakelsInPeakelDB(sqliteConn: SQLiteConnection, peakels: Array[Peakel]) {

    // BEGIN TRANSACTION
    sqliteConn.exec("BEGIN TRANSACTION;");

    // Prepare the insertion in the peakel table
    val peakelStmt = sqliteConn.prepare(
      s"INSERT INTO peakel VALUES (${Array.fill(20)("?").mkString(",")})"
    )
    // Prepare the insertion in the peakel_rtree table
    val peakelIndexStmt = sqliteConn.prepare(
      s"INSERT INTO peakel_rtree VALUES (${Array.fill(9)("?").mkString(",")})"
    )

    try {
      for (peakel <- peakels) {

        val scanInitialIds = peakel.getSpectrumIds()
        val peakelMessage = peakel.toPeakelDataMatrix()
        val peakelMessageAsBytes = PeakelDataMatrix.pack(peakelMessage)

        val peakelMz = peakel.getMz
        val peakelTime = peakel.getApexElutionTime

        var fieldNumber = 1
        peakelStmt.bind(fieldNumber, peakel.id); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakelMz); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakelTime); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.getApexIntensity); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.area); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.calcDuration); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.leftHwhmMean); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.leftHwhmCv); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.rightHwhmMean); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.rightHwhmCv); fieldNumber += 1
        peakelStmt.bind(fieldNumber, 0); fieldNumber += 1 // is_overlapping (0|1 boolean encoding)
        peakelStmt.bind(fieldNumber, 0); fieldNumber += 1 // features_count
        peakelStmt.bind(fieldNumber, peakel.spectrumIds.length); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakelMessageAsBytes); fieldNumber += 1
        peakelStmt.bindNull(fieldNumber); fieldNumber += 1 // param_tree
        peakelStmt.bind(fieldNumber, scanInitialIds.head); fieldNumber += 1
        peakelStmt.bind(fieldNumber, scanInitialIds.last); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.getApexSpectrumId); fieldNumber += 1
        peakelStmt.bind(fieldNumber, 1); fieldNumber += 1 // ms_level
        peakelStmt.bindNull(fieldNumber); // map_id

        peakelStmt.step()
        peakelStmt.reset()

        fieldNumber = 1
        peakelIndexStmt.bind(fieldNumber, peakel.id); fieldNumber += 1
        peakelIndexStmt.bind(fieldNumber, 1); fieldNumber += 1 // min_ms_level
        peakelIndexStmt.bind(fieldNumber, 1); fieldNumber += 1 // max_ms_level
        peakelIndexStmt.bind(fieldNumber, 0d); fieldNumber += 1 // min_parent_mz
        peakelIndexStmt.bind(fieldNumber, 0d); fieldNumber += 1 // max_parent_mz
        peakelIndexStmt.bind(fieldNumber, peakelMz); fieldNumber += 1 // min_mz
        peakelIndexStmt.bind(fieldNumber, peakelMz); fieldNumber += 1 // max_mz
        peakelIndexStmt.bind(fieldNumber, peakelTime); fieldNumber += 1 // min_time
        peakelIndexStmt.bind(fieldNumber, peakelTime); // max_time

        peakelIndexStmt.step()
        peakelIndexStmt.reset()
      }
    } finally {
      // Release statements
      peakelStmt.dispose()
      peakelIndexStmt.dispose()
    }

    // COMMIT TRANSACTION
    sqliteConn.exec("COMMIT TRANSACTION;");
  }
}