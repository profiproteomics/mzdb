package fr.profi.mzdb.io.reader.table;


import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.db.model.params.param.CVUnit;
import fr.profi.mzdb.db.table.CvUnitTable;
import fr.profi.mzdb.util.sqlite.ISQLiteRecordExtraction;
import fr.profi.mzdb.util.sqlite.SQLiteQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class CvUnitReader extends AbstractTableModelReader<CVUnit>{

  final Logger logger = LoggerFactory.getLogger(CvReader.class);

  public CvUnitReader(SQLiteConnection connection) {
    super(connection);
  }


  @Override
  protected ISQLiteRecordExtraction<CVUnit> buildRecordExtractor() {
    return record -> {
      String acc = record.columnString(CvUnitTable.ACCESSION);
      String name = record.columnString(CvUnitTable.NAME);
      String cvId = record.columnString(CvUnitTable.CV_ID);
      return new CVUnit(acc, name, cvId);
    };
  }
  public CVUnit getCvUnit(String accession) throws SQLiteException {
    return new SQLiteQuery(connection, "SELECT * FROM "+CVUnit.TABLE_NAME+" WHERE id = ?").bind(1, accession)
            .extractRecord(recordExtractor);
  }

  public List<CVUnit> getCvUnit() throws SQLiteException {
    return getRecordList(CVUnit.TABLE_NAME);
  }

}
