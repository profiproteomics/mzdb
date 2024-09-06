package fr.profi.mzdb.io.reader;


import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.db.model.SharedParamTree;
import fr.profi.mzdb.db.model.params.ReferencableParamGroup;
import fr.profi.mzdb.db.table.SharedParamTreeTable;
import fr.profi.mzdb.io.reader.table.AbstractTableModelReader;
import fr.profi.mzdb.io.reader.table.ParamTreeParser;
import fr.profi.mzdb.util.sqlite.ISQLiteRecordExtraction;
import fr.profi.mzdb.util.sqlite.SQLiteRecord;

import java.util.List;

public class SharedParamTreeReader extends AbstractTableModelReader<SharedParamTree> {

  public SharedParamTreeReader(SQLiteConnection connection) {
    super(connection);
  }

  @Override
  protected ISQLiteRecordExtraction<SharedParamTree> buildRecordExtractor() {
    return new ISQLiteRecordExtraction<SharedParamTree>() {

      public SharedParamTree extract(SQLiteRecord r) throws SQLiteException {

        int id = r.columnInt(SharedParamTreeTable.ID);
        String data = r.columnString(SharedParamTreeTable.DATA);
        String schemaName = r.columnString(SharedParamTreeTable.SCHEMA_NAME);


        if (data != null) {
          return new SharedParamTree(id, ParamTreeParser.parseReferencableParamGroup(data), schemaName);
        } else {
          return new SharedParamTree(id, new ReferencableParamGroup(schemaName), schemaName);
        }
      }
    };
  }

  public SharedParamTree getSharedParamTree(int id )throws SQLiteException {
    return getRecord(SharedParamTree.TABLE_NAME, id);
  }

  public List<SharedParamTree> getSharedParamTreeList( )throws SQLiteException {
    return getRecordList(SharedParamTree.TABLE_NAME);
  }


}
