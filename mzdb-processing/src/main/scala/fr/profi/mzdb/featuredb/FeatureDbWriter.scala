package fr.profi.mzdb.featuredb

import com.almworks.sqlite4java.SQLiteConnection
import com.typesafe.scalalogging.LazyLogging
import fr.profi.mzdb.model.Feature
import fr.profi.util.serialization.ProfiJson

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.collection.mutable

object FeatureDbWriter extends LazyLogging {

  def initFeatureStore(fileLocation: File): SQLiteConnection = {

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
CREATE TABLE featuredb_file (
                id INTEGER NOT NULL,
                name VARCHAR NOT NULL,
                description VARCHAR,
                raw_file_name VARCHAR NOT NULL,
                creation_timestamp TIMESTAMP NOT NULL,
                modification_timestamp TIMESTAMP NOT NULL,
                serialized_properties CLOB,
                CONSTRAINT featuredb_file_pk PRIMARY KEY (id)
);

CREATE TABLE lcms_map (
                id INTEGER NOT NULL,
                ms_level INTEGER NOT NULL,
                features_count INTEGER NOT NULL,
                serialized_properties CLOB,
                featuredb_file_id INTEGER NOT NULL,
                CONSTRAINT lcms_map_pk PRIMARY KEY (id),
                CONSTRAINT featuredb_file_lcms_map_fk FOREIGN KEY (featuredb_file_id) REFERENCES featuredb_file (id)
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

CREATE TABLE feature (
                id INTEGER NOT NULL,
                moz DOUBLE NOT NULL,
                charge INTEGER NOT NULL,
                elution_time REAL NOT NULL,
                duration REAL NOT NULL,
                intensity REAL NOT NULL,
                peakel_count INTEGER NOT NULL,
                peakels BLOB NOT NULL,
                base_peakel INTEGER NOT NULL,
                serialized_properties CLOB,
                apex_spectrum_id INTEGER NOT NULL,
                map_id INTEGER NOT NULL,
                CONSTRAINT feature_pk PRIMARY KEY (id)
                CONSTRAINT lcms_map_feature_fk FOREIGN KEY (map_id) REFERENCES lcms_map (id)
);

CREATE TABLE feature_ref (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                reference_id INTEGER NOT NULL,
                serialized_properties CLOB,
                feature_id INTEGER NOT NULL,
                CONSTRAINT feature_ref_feature_fk FOREIGN KEY (feature_id) REFERENCES feature (id)
);

CREATE INDEX feature_map_idx ON feature ( map_id );
CREATE INDEX feature_ref_idx ON feature_ref ( reference_id );


"""
    connection.exec(ddlQuery)

    connection.exec("BEGIN TRANSACTION;")
    val featureDbFileStmt = connection.prepare(s"INSERT INTO featuredb_file VALUES (${Array.fill(7)("?").mkString(",")})")
    featureDbFileStmt.bind(1, 1);   // id
    featureDbFileStmt.bind(2, "no name");   // name VARCHAR NOT NULL,
    featureDbFileStmt.bindNull(3);          // description VARCHAR
    featureDbFileStmt.bind(4, "no name");   // raw_file_name VARCHAR NOT NULL
    featureDbFileStmt.bind(5, new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date()));   // creation_timestamp TIMESTAMP NOT NULL
    featureDbFileStmt.bind(6, new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(new Date()));   // modification_timestamp TIMESTAMP NOT NULL,
    featureDbFileStmt.bindNull(7);   // serialized_properties CLOB,

    featureDbFileStmt.step()

    val lcmsMapStmt = connection.prepare(s"INSERT INTO lcms_map VALUES (${Array.fill(5)("?").mkString(",")})")
    lcmsMapStmt.bind(1, 1)  // id INTEGER NOT NULL,
    lcmsMapStmt.bind(2, 1)  // ms_level INTEGER NOT NULL,
    lcmsMapStmt.bind(3, 0)  // features_count INTEGER NOT NULL,
    lcmsMapStmt.bindNull(4) // serialized_properties CLOB,
    lcmsMapStmt.bind(5, 1)  // peakeldb_file_id INTEGER NOT NULL,

    lcmsMapStmt.step()

    connection.exec("COMMIT TRANSACTION;")

    connection
  }


   def storeFeatureReferences(
    sqliteConn: SQLiteConnection,
    featuresIdRefIdTuple: Array[(Int, Long)]
  ) {

    // BEGIN TRANSACTION
    sqliteConn.exec("BEGIN TRANSACTION;")

     val featureReferenceStmt = sqliteConn.prepare(
       s"INSERT INTO feature_ref(reference_id, serialized_properties, feature_id) VALUES (${Array.fill(3)("?").mkString(",")})"
     )

    // Prepare the insertion in the feature_ref table
    try {
      for ((featureId, peptideId) <- featuresIdRefIdTuple) {

        var fieldNumber = 1

        fieldNumber = 1
        featureReferenceStmt.bind(fieldNumber, peptideId); fieldNumber += 1
        featureReferenceStmt.bindNull(fieldNumber); fieldNumber += 1
        featureReferenceStmt.bind(fieldNumber, featureId);

        featureReferenceStmt.step()
        featureReferenceStmt.reset()

      }
    } finally {
      // Release statements
      featureReferenceStmt.dispose()
    }

    // COMMIT TRANSACTION
    sqliteConn.exec("COMMIT TRANSACTION;")
  }


  def storeFeatures(
      sqliteConn: SQLiteConnection,
      features: Array[Feature]
  ) {

    // BEGIN TRANSACTION
    sqliteConn.exec("BEGIN TRANSACTION;")

    // Prepare the insertion in the feature table
    val featureStmt = sqliteConn.prepare(
      s"INSERT INTO feature VALUES (${Array.fill(12)("?").mkString(",")})"
    )

    val alreadyWrittenFeatureIds = mutable.HashSet.empty[Int]

    try {
      for (feature <- features) {

        var fieldNumber = 1

        if (!alreadyWrittenFeatureIds.contains(feature.id)) {

          val peakelIndexes = ProfiJson.serialize(feature.getPeakels().map(p => p.id))

          featureStmt.bind(fieldNumber, feature.id);
          fieldNumber += 1
          featureStmt.bind(fieldNumber, feature.getMz);
          fieldNumber += 1
          featureStmt.bind(fieldNumber, feature.charge);
          fieldNumber += 1
          featureStmt.bind(fieldNumber, feature.getElutionTime);
          fieldNumber += 1
          featureStmt.bind(fieldNumber, feature.calcDuration());
          fieldNumber += 1
          featureStmt.bind(fieldNumber, feature.getBasePeakel().getApexIntensity());
          fieldNumber += 1
          featureStmt.bind(fieldNumber, feature.getPeakelsCount());
          fieldNumber += 1
          featureStmt.bind(fieldNumber, peakelIndexes);
          fieldNumber += 1
          featureStmt.bind(fieldNumber, feature.getBasePeakelIndex());
          fieldNumber += 1
          featureStmt.bindNull(fieldNumber);
          fieldNumber += 1 // serialized properties
          featureStmt.bind(fieldNumber, feature.getApexSpectrumId);
          fieldNumber += 1
          featureStmt.bind(fieldNumber, 1);

          featureStmt.step()
          featureStmt.reset()
          alreadyWrittenFeatureIds += feature.id

        }
      }
    } finally {
      // Release statements
      featureStmt.dispose()
    }

    // COMMIT TRANSACTION
    sqliteConn.exec("COMMIT TRANSACTION;")
  }

}