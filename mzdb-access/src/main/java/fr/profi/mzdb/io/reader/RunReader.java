package fr.profi.mzdb.io.reader;

import java.time.Instant;
import java.util.List;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.db.model.Run;
import fr.profi.mzdb.db.table.RunTable;
import fr.profi.mzdb.utils.sqlite.ISQLiteRecordExtraction;
import fr.profi.mzdb.utils.sqlite.SQLiteRecord;

// TODO: Auto-generated Javadoc

/**
 * The Class RunReader.
 * 
 * @author David Bouyssie
 */
public class RunReader extends AbstractTableModelReader<Run> {

	/**
	 * Instantiates a new source file reader.
	 * 
	 * @param connection
	 *            the connection
	 */
	public RunReader(SQLiteConnection connection) throws SQLiteException {
		super(connection);
	}

	protected ISQLiteRecordExtraction<Run> recordExtractor = new ISQLiteRecordExtraction<Run>() {

		public Run extract(SQLiteRecord r) throws SQLiteException {

			int id = r.columnInt(RunTable.ID);
			String name = r.columnString(RunTable.NAME);
			Instant startTimestamp = Instant.parse( r.columnString(RunTable.START_TIMESTAMP));
			String paramTreeAsStr = r.columnString(RunTable.PARAM_TREE);

			return new Run(id, name, startTimestamp, ParamTreeParser.parseParamTree(paramTreeAsStr));
		}
	};

	public Run getRun(int id) throws SQLiteException {
		return getRecord(Run.TABLE_NAME, id);
	}
	
	public List<Run> getRunList() throws SQLiteException {
		return getRecordList(Run.TABLE_NAME);
	}
	
}
