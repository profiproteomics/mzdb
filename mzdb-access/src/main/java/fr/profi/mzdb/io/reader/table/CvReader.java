package fr.profi.mzdb.io.reader.table;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.db.model.params.param.CV;
import fr.profi.mzdb.db.table.CvTable;
import fr.profi.mzdb.util.sqlite.ISQLiteRecordExtraction;
import fr.profi.mzdb.util.sqlite.SQLiteQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class CvReader extends AbstractTableModelReader<CV> {

  final Logger logger = LoggerFactory.getLogger(CvReader.class);

  public CvReader(SQLiteConnection connection) {
    super(connection);
  }

  @Override
  protected ISQLiteRecordExtraction<CV> buildRecordExtractor() {
    return record -> {
      String cvId = record.columnString(CvTable.ID);
      String fullName = record.columnString(CvTable.FULL_NAME);
      String version = record.columnString(CvTable.VERSION);
      String uri = record.columnString(CvTable.URI);
      return new CV(cvId, fullName, version, uri);
    };
  }


  public CV getCv(String id) throws SQLiteException {
    return new SQLiteQuery(connection, "SELECT * FROM "+CV.TABLE_NAME+" WHERE id = ?").bind(1, id)
            .extractRecord(recordExtractor);
  }

  public List<CV> getCvList() throws SQLiteException {
    return getRecordList(CV.TABLE_NAME);
  }

}
