package fr.profi.mzdb.peakeldb.io

import java.io.File

import com.almworks.sqlite4java.SQLiteConnection
import fr.profi.mzdb.model.Peakel
import fr.profi.mzdb.model.PeakelDataMatrix
import fr.profi.mzdb.model.SpectrumHeader
import fr.profi.util.collection._
import java.text.SimpleDateFormat
import java.util.Date

import fr.profi.mzdb.peakeldb.model.PeakelTable

object PeakelDbWriter {

  def initPeaklUpdate(fileLocation: File): SQLiteConnection = {
    // Open SQLite connection
    val connection = new SQLiteConnection(fileLocation)
    connection.open(true) // allowCreate = true

    // SQLite optimization
    connection.exec("PRAGMA synchronous=OFF;")
    connection.exec("PRAGMA journal_mode=OFF;")
    connection.exec("PRAGMA temp_store=2;")
    connection.exec("PRAGMA cache_size=100000;")
    connection
  }

  def initPeakelStore(fileLocation: File): SQLiteConnection = {

    // Open SQLite connection
    val connection = new SQLiteConnection(fileLocation)
    connection.open(true) // allowCreate = true

    // SQLite optimization
    connection.exec("PRAGMA synchronous=OFF;")
    connection.exec("PRAGMA journal_mode=OFF;")
    connection.exec("PRAGMA temp_store=2;")
    connection.exec("PRAGMA cache_size=100000;")

    // Create the DDL
    // Note that this DDL will be later used in the peakelDB format
    val ddlQuery = """
CREATE TABLE peakeldb_file (
                id INTEGER NOT NULL,
                name VARCHAR NOT NULL,
                description VARCHAR,
                raw_file_name VARCHAR NOT NULL,
                is_dia_experiment BOOLEAN NOT NULL,
                creation_timestamp TIMESTAMP NOT NULL,
                modification_timestamp TIMESTAMP NOT NULL,
                serialized_properties CLOB,
                CONSTRAINT peakeldb_file_pk PRIMARY KEY (id)
);

CREATE TABLE lcms_map (
                id INTEGER NOT NULL,
                ms_level INTEGER NOT NULL,
                peakel_count INTEGER NOT NULL,
                serialized_properties CLOB,
                peakeldb_file_id INTEGER NOT NULL,
                CONSTRAINT lcms_map_pk PRIMARY KEY (id),
                CONSTRAINT peakeldb_file_lcms_map_fk FOREIGN KEY (peakeldb_file_id) REFERENCES peakeldb_file (id)
);

CREATE TABLE lcms_map_relation (
                parent_lcms_map_id INTEGER NOT NULL,
                child_lcms_map_id INTEGER NOT NULL,
                min_parent_mz REAL NOT NULL,
                max_parent_mz REAL NOT NULL,
                CONSTRAINT lcms_map_relation_pk PRIMARY KEY (parent_lcms_map_id, child_lcms_map_id)
                CONSTRAINT lcms_map_lcms_map_relation_fk1 FOREIGN KEY (parent_lcms_map_id) REFERENCES lcms_map (id)
                CONSTRAINT lcms_map_lcms_map_relation_fk2 FOREIGN KEY (child_lcms_map_id) REFERENCES lcms_map (id)		
);

CREATE TABLE peakel (
                id INTEGER NOT NULL,
                moz DOUBLE NOT NULL,
                elution_time REAL NOT NULL,
                duration REAL NOT NULL,
                gap_count INTEGER NOT NULL,
                apex_intensity REAL NOT NULL,
                area REAL NOT NULL,
                amplitude REAL NOT NULL,
                intensity_cv REAL NOT NULL,
                left_hwhm_mean REAL,
                left_hwhm_cv REAL,
                right_hwhm_mean REAL,
                right_hwhm_cv REAL,
                is_interfering BOOLEAN NOT NULL,
                peak_count INTEGER NOT NULL,
                peaks BLOB NOT NULL,
                serialized_properties CLOB,
                first_spectrum_id INTEGER NOT NULL,
                apex_spectrum_id INTEGER NOT NULL,
                last_spectrum_id INTEGER NOT NULL,
                map_id INTEGER NOT NULL,
                CONSTRAINT peakel_pk PRIMARY KEY (id)
                CONSTRAINT lcms_map_peakel_fk FOREIGN KEY (map_id) REFERENCES lcms_map (id)
);

CREATE INDEX peakel_map_idx ON peakel ( map_id );

CREATE TABLE peakel_rtree (
                id INTEGER NOT NULL,
                min_lcms_map_id INTEGER NOT NULL,
                max_lcms_map_id INTEGER NOT NULL,
                min_mz REAL NOT NULL,
                max_mz REAL NOT NULL,
                min_time REAL NOT NULL,
                max_time REAL NOT NULL,
                CONSTRAINT peakel_rtree_pk PRIMARY KEY (id)
                CONSTRAINT peakel_peakel_rtree_fk FOREIGN KEY (id) REFERENCES peakel (id)
);

"""
    connection.exec(ddlQuery)

    connection.exec("BEGIN TRANSACTION;")
    val peakelDbFileStmt = connection.prepare(s"INSERT INTO peakeldb_file VALUES (${Array.fill(8)("?").mkString(",")})")
    peakelDbFileStmt.bind(1, 1);           // id
    peakelDbFileStmt.bind(2, "no name");   // name VARCHAR NOT NULL,
    peakelDbFileStmt.bindNull(3);          // description VARCHAR
    peakelDbFileStmt.bind(4, "no name");   // raw_file_name VARCHAR NOT NULL
    peakelDbFileStmt.bind(5, 1);           // is_dia_experiment BOOLEAN NOT NULL (0|1 boolean encoding)
    peakelDbFileStmt.bind(6, new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date()));   // creation_timestamp TIMESTAMP NOT NULL
    peakelDbFileStmt.bind(7, new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date()));   // modification_timestamp TIMESTAMP NOT NULL,
    peakelDbFileStmt.bindNull(8);   // serialized_properties CLOB,
    
    peakelDbFileStmt.step()

    val lcmsMapStmt = connection.prepare(s"INSERT INTO lcms_map VALUES (${Array.fill(5)("?").mkString(",")})")
    lcmsMapStmt.bind(1, 1)  // id INTEGER NOT NULL,
    lcmsMapStmt.bind(2, 1)  // ms_level INTEGER NOT NULL,
    lcmsMapStmt.bind(3, 0)  // peakel_count INTEGER NOT NULL,
    lcmsMapStmt.bindNull(4) // serialized_properties CLOB,
    lcmsMapStmt.bind(5, 1)  // peakeldb_file_id INTEGER NOT NULL,
    
    lcmsMapStmt.step()
    
    connection.exec("COMMIT TRANSACTION;")
    
    connection
  }

   def storePeakelsInPeakelDB(
    sqliteConn: SQLiteConnection,
    peakels: Array[Peakel],
    ms1SpecHeaders: Array[SpectrumHeader]
  ) {
    
    // Retrieve some mappings relative to the mzDB spectra ids
    val initialIdByMzDbSpecId = ms1SpecHeaders.toLongMapWith { sh =>
      sh.getId -> sh.getInitialId
    }
    val cycleByMzDbSpecId = ms1SpecHeaders.toLongMapWith { sh =>
      sh.getId -> sh.getCycle
    }

    // BEGIN TRANSACTION
    sqliteConn.exec("BEGIN TRANSACTION;")

    // Prepare the insertion in the peakel table
    val peakelStmt = sqliteConn.prepare(
      s"INSERT INTO peakel VALUES (${Array.fill(21)("?").mkString(",")})"
    )
    // Prepare the insertion in the peakel_rtree table
    val peakelIndexStmt = sqliteConn.prepare(
      s"INSERT INTO peakel_rtree VALUES (${Array.fill(7)("?").mkString(",")})"
    )
    
    try {
      for (peakel <- peakels) {

        val scanInitialIds = peakel.getSpectrumIds().map(initialIdByMzDbSpecId(_))
        val peakelMessage = peakel.toPeakelDataMatrix()
        val peakelMessageAsBytes = PeakelDataMatrix.pack(peakelMessage)

        val peakelMz = peakel.getMz
        val peakelTime = peakel.getApexElutionTime

        var fieldNumber = 1
        peakelStmt.bind(fieldNumber, peakel.id); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakelMz); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakelTime); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.calcDuration()); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.calcGapCount(cycleByMzDbSpecId)); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.getApexIntensity); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.area); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.calcAmplitude()); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.calcIntensityCv()); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.leftHwhmMean); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.leftHwhmCv); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.rightHwhmMean); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.rightHwhmCv); fieldNumber += 1
        peakelStmt.bind(fieldNumber, 0); fieldNumber += 1 // is_interfering (0|1 boolean encoding)
        peakelStmt.bind(fieldNumber, peakel.spectrumIds.length); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakelMessageAsBytes); fieldNumber += 1
        peakelStmt.bindNull(fieldNumber); fieldNumber += 1 // serialized properties
        peakelStmt.bind(fieldNumber, scanInitialIds.head); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.getApexSpectrumId); fieldNumber += 1
        peakelStmt.bind(fieldNumber, scanInitialIds.last); fieldNumber += 1
        peakelStmt.bind(fieldNumber, 1); // map_id

        peakelStmt.step()
        peakelStmt.reset()

        fieldNumber = 1
        peakelIndexStmt.bind(fieldNumber, peakel.id); fieldNumber += 1
        peakelIndexStmt.bind(fieldNumber, 1); fieldNumber += 1 // min_lcms_map_id
        peakelIndexStmt.bind(fieldNumber, 1); fieldNumber += 1 // max_lcms_map_id
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
    sqliteConn.exec("COMMIT TRANSACTION;")
  }

  def updatePeakelsInPeakelDB(
                              sqliteConn: SQLiteConnection,
                              peakels: Array[Peakel],
                              initialIdBySpectrumId: Map[Long, Long]
                            ) {


    // BEGIN TRANSACTION
    sqliteConn.exec("BEGIN TRANSACTION;")

    // Prepare the insertion in the peakel table
    val peakelStmt = sqliteConn.prepare(
      s"UPDATE ${PeakelTable.tableName} SET ${PeakelTable.DURATION} = ?, ${PeakelTable.PEAK_COUNT} = ?, ${PeakelTable.PEAKS} = ?, ${PeakelTable.FIRST_SPECTRUM_ID} = ?, ${PeakelTable.APEX_INTENSITY} = ?," +
        s"  ${PeakelTable.LAST_SPECTRUM_ID} = ?  WHERE ${PeakelTable.ID} = ?"
    )

    try {
      for (peakel <- peakels) {

        val scanInitialIds = peakel.getSpectrumIds().map(initialIdBySpectrumId(_))
        val peakelMessage = peakel.toPeakelDataMatrix()
        val peakelMessageAsBytes = PeakelDataMatrix.pack(peakelMessage)

        var fieldNumber = 1
        peakelStmt.bind(fieldNumber, peakel.calcDuration()); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.spectrumIds.length); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakelMessageAsBytes); fieldNumber += 1
        peakelStmt.bind(fieldNumber, scanInitialIds.head); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.getApexSpectrumId); fieldNumber += 1
        peakelStmt.bind(fieldNumber, scanInitialIds.last); fieldNumber += 1
        peakelStmt.bind(fieldNumber, peakel.getId()); // map_id

        peakelStmt.step()
        peakelStmt.reset()

      }
    } finally {
      // Release statements
      peakelStmt.dispose()
    }

    // COMMIT TRANSACTION
    sqliteConn.exec("COMMIT TRANSACTION;")
  }
}