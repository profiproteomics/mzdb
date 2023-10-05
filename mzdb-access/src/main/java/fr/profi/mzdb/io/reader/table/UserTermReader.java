package fr.profi.mzdb.io.reader.table;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.db.model.params.param.CVTerm;
import fr.profi.mzdb.db.model.params.param.UserTerm;
import fr.profi.mzdb.db.table.CVTermTable;
import fr.profi.mzdb.db.table.UserTermTable;
import fr.profi.mzdb.util.sqlite.ISQLiteRecordExtraction;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

public class UserTermReader extends AbstractTableModelReader<UserTerm> {
    final Logger logger = LoggerFactory.getLogger(UserTermReader.class);

    public UserTermReader(SQLiteConnection connection) {
        super(connection);
    }

    @Override
    protected ISQLiteRecordExtraction<UserTerm> buildRecordExtractor() {
        return record -> {
            long id = record.columnLong(UserTermTable.ID);
            String name = record.columnString(UserTermTable.NAME);
            String unitAcc = record.columnString(UserTermTable.UNIT_ACCESSION);
            String type = record.columnString(UserTermTable.TYPE);

            return new UserTerm(id, name, unitAcc, type);
        };
    }

    public List<UserTerm> getUserTerms() throws SQLiteException {
        return getRecordList(UserTerm.TABLE_NAME);
    }

}
