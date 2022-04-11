package fr.profi.mzdb.io.reader.table;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.db.model.params.param.CVTerm;
import fr.profi.mzdb.db.table.CVTermTable;
import fr.profi.mzdb.util.sqlite.ISQLiteRecordExtraction;
import fr.profi.mzdb.util.sqlite.SQLiteQuery;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class CvTermReader extends AbstractTableModelReader<CVTerm> {


  final Logger logger = LoggerFactory.getLogger(CvTermReader.class);

  public CvTermReader(SQLiteConnection connection) {
    super(connection);
  }

  @Override
  protected ISQLiteRecordExtraction<CVTerm> buildRecordExtractor() {
    return record -> {
      String accession = record.columnString(CVTermTable.ACCESSION);
      String name = record.columnString(CVTermTable.NAME);
      String unitAcc = record.columnString(CVTermTable.UNIT_ACCESSION);
      String cvId = record.columnString(CVTermTable.CV_ID);

      return new CVTerm(accession, name, unitAcc, cvId);
    };
  }

  public CVTerm getCvTerm(String accession) throws SQLiteException {
    return new SQLiteQuery(connection, "SELECT * FROM "+CVTerm.TABLE_NAME+" WHERE id = ?").bind(1, accession)
            .extractRecord(recordExtractor);
  }

  public List<CVTerm> getCvTerms() throws SQLiteException {
    return getRecordList(CVTerm.TABLE_NAME);
  }

}


