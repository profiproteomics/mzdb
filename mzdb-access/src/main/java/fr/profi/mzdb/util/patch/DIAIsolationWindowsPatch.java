package fr.profi.mzdb.util.patch;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;
import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.model.params.IsolationWindowParamTree;
import fr.profi.mzdb.db.model.params.Precursor;
import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.table.BoundingBoxMsnRtreeTable;
import fr.profi.mzdb.db.table.BoundingBoxTable;
import fr.profi.mzdb.db.table.RunTable;
import fr.profi.mzdb.model.AcquisitionMode;
import fr.profi.mzdb.model.IsolationWindow;
import fr.profi.mzdb.model.SpectrumHeader;
import fr.profi.mzdb.util.sqlite.SQLiteQuery;
import fr.profi.mzdb.util.sqlite.SQLiteRecord;
import fr.profi.mzdb.util.sqlite.SQLiteRecordIterator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class DIAIsolationWindowsPatch {

  private static final Logger logger = LoggerFactory.getLogger(DIAIsolationWindowsPatch.class);


  private static List<IsolationWindow> toIsolationWindowsList(Map<SpectrumHeader, IsolationWindow> headers) throws SQLiteException {

    List<IsolationWindow> list = headers.values().stream().distinct().collect(Collectors.toList());
    list.sort((o1, o2) -> Double.compare(o1.getMinMz(), o2.getMaxMz()));

    return list;
  }

  private static void updateIsolationWindows(SQLiteConnection connection, Map<SpectrumHeader, IsolationWindow> headers) throws SQLiteException {

    String sqlString = "UPDATE bounding_box_msn_rtree SET min_parent_mz = ?, max_parent_mz = ? WHERE bounding_box_msn_rtree.id IN (SELECT id FROM bounding_box WHERE bounding_box.first_spectrum_id = ?)";
    int count = 0;
    SQLiteStatement stmt = connection.prepare(sqlString, true);
    connection.exec("BEGIN TRANSACTION;");

    for (Map.Entry<SpectrumHeader, IsolationWindow> entry : headers.entrySet()) {
      SpectrumHeader header = entry.getKey();
      IsolationWindow window = entry.getValue();
      long firstSpectrumId = (long) (header.getBBFirstSpectrumId());
      stmt.bind(1, window.getMinMz()).bind(2, window.getMaxMz()).bind(3, firstSpectrumId);
      stmt.step();
      stmt.reset(true);
      count++;
      if (count % 10 == 0) {
        logger.info("updating header {}/{}", count, headers.size());
      }
    }

    connection.exec("COMMIT;");

  }

  private static void updateIsolationWindows2(SQLiteConnection connection, Map<SpectrumHeader, IsolationWindow> headers) throws SQLiteException {

    String queryStr= "SELECT bounding_box.first_spectrum_id, bounding_box_msn_rtree.* FROM bounding_box_msn_rtree, bounding_box WHERE bounding_box_msn_rtree.id = bounding_box.id";

    String createIndex = "CREATE VIRTUAL TABLE bounding_box_msn_rtree USING rtree(\n" +
            "id INTEGER NOT NULL PRIMARY KEY, \n" +
            "min_ms_level REAL NOT NULL, \n" +
            "max_ms_level REAL NOT NULL, \n" +
            "min_parent_mz REAL NOT NULL, \n" +
            "max_parent_mz REAL NOT NULL, \n" +
            "min_mz REAL NOT NULL, \n" +
            "max_mz REAL NOT NULL, \n" +
            "min_time REAL NOT NULL, \n" +
            "max_time REAL NOT NULL);";

    String insertQuery = "INSERT INTO bounding_box_msn_rtree VALUES(?,?,?,?,?,?,?,?,?)";

    SQLiteRecordIterator records = new SQLiteQuery(connection, queryStr).getRecordIterator();
    List<MsnRtreeEntry> entries = new ArrayList<>();

    while (records.hasNext()) {
      SQLiteRecord record = records.next();
      MsnRtreeEntry entry = new MsnRtreeEntry();

      entry.firstSpectrumId = record.columnInt(BoundingBoxTable.FIRST_SPECTRUM_ID);
      entry.id = record.columnInt(BoundingBoxMsnRtreeTable.ID);
      entry.minMsLevel = record.columnDouble(BoundingBoxMsnRtreeTable.MIN_MS_LEVEL);
      entry.maxMsLevel = record.columnDouble(BoundingBoxMsnRtreeTable.MAX_MS_LEVEL);
      entry.minParentMz = record.columnDouble(BoundingBoxMsnRtreeTable.MIN_PARENT_MZ);
      entry.maxParentMz = record.columnDouble(BoundingBoxMsnRtreeTable.MAX_PARENT_MZ);
      entry.minMz = record.columnDouble(BoundingBoxMsnRtreeTable.MIN_MZ);
      entry.maxMz = record.columnDouble(BoundingBoxMsnRtreeTable.MAX_MZ);
      entry.minTime = record.columnDouble(BoundingBoxMsnRtreeTable.MIN_TIME);
      entry.maxTime = record.columnDouble(BoundingBoxMsnRtreeTable.MAX_TIME);

      entries.add(entry);
    }
    logger.info("read {} Msn RTree index entries", entries.size());

    Map<Integer, List<MsnRtreeEntry>> indexEntriesByFirstSpectrumId = entries.stream().collect(Collectors.groupingBy(MsnRtreeEntry::getFirstSpectrumId));

    logger.info("{} mapped index Msn RTree", indexEntriesByFirstSpectrumId.size());

    for (Map.Entry<SpectrumHeader, IsolationWindow> entry : headers.entrySet()) {
      SpectrumHeader header = entry.getKey();
      IsolationWindow window = entry.getValue();
      List<MsnRtreeEntry> indexEntries = indexEntriesByFirstSpectrumId.get(header.getBBFirstSpectrumId());
      for (MsnRtreeEntry indexEntry : indexEntries) {
        indexEntry.minParentMz = window.getMinMz();
        indexEntry.maxParentMz = window.getMaxMz();
      }
    }

    logger.info("index entries updated (in memory)");

    connection.exec("BEGIN TRANSACTION;");
    connection.exec("DROP TABLE bounding_box_msn_rtree;");
    connection.exec("COMMIT;");

    logger.info("index dropped");

    connection.exec("BEGIN TRANSACTION;");
    connection.exec(createIndex);
    connection.exec("COMMIT;");

    logger.info("index created");
    int count = 0;
    connection.exec("BEGIN TRANSACTION;");
    SQLiteStatement stmt = connection.prepare(insertQuery, true);
    for (MsnRtreeEntry entry : entries) {
        stmt.bind(1, entry.id).bind(2, entry.minMsLevel).bind(3, entry.maxMsLevel);
        stmt.bind(4, entry.minParentMz).bind(5, entry.maxParentMz);
        stmt.bind(6, entry.minMz).bind(7, entry.maxMz);
        stmt.bind(8, entry.minTime).bind(9, entry.maxTime);
        stmt.step();
        stmt.reset(true);
      count++;
      if (count % 100000 == 0) {
        logger.info("updating header {}/{}", count, entries.size());
      }

    }
    connection.exec("COMMIT;");
    logger.info("{} index entries inserted",entries.size());

  }



  public static Map<SpectrumHeader, IsolationWindow> retrieveTrueIsolationWindows(MzDbReader reader) throws SQLiteException {
    Map<SpectrumHeader, IsolationWindow> windows = new HashMap<>();
    SpectrumHeader[] headers = reader.getMs2SpectrumHeaders();
    SpectrumHeader.loadPrecursors(headers, reader.getConnection());
    for (SpectrumHeader header : headers) {
      Precursor precursor = header.getPrecursor();

      if (precursor != null) {
        String center = null, upper = null, lower = null;
        IsolationWindowParamTree tree = precursor.getIsolationWindow();
        for (CVParam cvParam : tree.getCVParams()) {
          if (cvParam.getAccession().equals("MS:1000827")) {
            center = cvParam.getValue();
          } else if (cvParam.getAccession().equals("MS:1000828")) {
            lower = cvParam.getValue();
          } else if (cvParam.getAccession().equals("MS:1000829")) {
            upper = cvParam.getValue();
          }
        }
        windows.put(header, new IsolationWindow(Double.valueOf(center) - Double.valueOf(lower), Double.valueOf(center) + Double.valueOf(upper)));
      }
    }
    return windows;
  }

  public static void run(String filePath) {
    updateDIAModeForPRM(filePath);
    patchDIAWindows(filePath);
  }

  public static void patchDIAWindows(String filepath) {
    MzDbReader reader = null;
    SQLiteConnection connection = null;
    try {
      reader = new MzDbReader(new File(filepath), false);
      AcquisitionMode acqMode = null;
      acqMode = reader.getAcquisitionMode();
      if (acqMode != null && acqMode.isDataIndependantAcquisition()) {
        int[] range = reader.getMzRange(1);
        logger.info("MS1 mz Range :: " + range[0] + "-" + range[1]);
        int[] range_ms2 = reader.getMzRange(2);
        logger.info("MS2 mz Range :: " + range_ms2[0] + "-" + range_ms2[1]);
        Map<SpectrumHeader, IsolationWindow> headers = retrieveTrueIsolationWindows(reader);

        List<IsolationWindow> windows = toIsolationWindowsList(headers);

        for (IsolationWindow window : windows) {
          logger.info("window : [{},{}]", window.getMinMz(), window.getMaxMz());
        }

        reader.close();
        reader = null;

        connection = new SQLiteConnection(new File(filepath));
        connection.open();
        logger.info("Updating {} spectrum headers bb indexes", headers.size());
        updateIsolationWindows2(connection, headers);
        connection.dispose();
      }
    } catch (Exception e) {
      logger.error("Error during isolation window fix/retrieval", e);
    } finally {
      if(reader != null)
        reader.close();

      if(connection != null && connection.isOpen())
        connection.dispose();
    }
  }

  public static void updateDIAModeForPRM(String filepath) {
    MzDbReader reader = null;
    SQLiteConnection connection = null;
    try {
      reader = new MzDbReader(new File(filepath), false) {
        public boolean isPRM() {
          try {
            List<String> lines = extractFromInstrumentMethod("PRM");
            lines.addAll(extractFromInstrumentMethod("Scan tMSn"));
            lines.addAll(extractFromInstrumentMethod("Scan tMS2"));
            return !lines.isEmpty();
          } catch (Exception e) {
            logger.error("Cannot infer PRM mode from InstrumentMethods", e);
          }
          return false;
        }

        @Override
        public AcquisitionMode getAcquisitionMode() throws SQLiteException {
          if (isPRM()) {
            return AcquisitionMode.PRM;
          }
          return super.getAcquisitionMode();
        }
      };

      AcquisitionMode acqMode = reader.getAcquisitionMode();
      if (acqMode.equals(AcquisitionMode.PRM)) {
        connection = new SQLiteConnection(new File(filepath));
        connection.open();
        String queryStr = "SELECT * FROM run";
        SQLiteRecordIterator records = new SQLiteQuery(connection, queryStr).getRecordIterator();
        final SQLiteRecord record = records.next();
        String paramTree = record.columnString(RunTable.PARAM_TREE);
        final int id = record.columnInt(RunTable.ID);
        paramTree = paramTree.replace("DDA", "SWATH");

        String sqlString = "UPDATE run SET param_tree = ? WHERE id = ? ";
        SQLiteStatement stmt = connection.prepare(sqlString, false);
        connection.exec("BEGIN TRANSACTION;");
        stmt.bind(1, paramTree);
        stmt.bind(2, id);
        stmt.step();
        connection.exec("COMMIT;");
        connection.dispose();
        logger.info("Acquisition Mode patched to SWATH");
      }
    } catch(Exception e){
      logger.error("Error during isolation window fix/retrieval", e);
    } finally {
      if(reader != null)
        reader.close();

      if(connection != null && connection.isOpen())
        connection.dispose();
    }
  }
}

class MsnRtreeEntry {
  int firstSpectrumId;
  int id;
  double minMsLevel;
  double maxMsLevel;
  double minParentMz;
  double maxParentMz;
  double minMz;
  double maxMz;
  double minTime;
  double maxTime;

  public int getFirstSpectrumId() {
    return firstSpectrumId;
  }
}