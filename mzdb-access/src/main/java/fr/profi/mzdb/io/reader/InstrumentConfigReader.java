package fr.profi.mzdb.io.reader;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.db.model.InstrumentConfiguration;
import fr.profi.mzdb.db.table.InstrumentConfigurationTable;
import fr.profi.mzdb.utils.sqlite.ISQLiteRecordExtraction;
import fr.profi.mzdb.utils.sqlite.SQLiteQuery;
import fr.profi.mzdb.utils.sqlite.SQLiteRecord;

// TODO: Auto-generated Javadoc
/**
 * The Class InstrumentConfigReader.
 * 
 * @author David Bouyssie
 */
class InstrumentConfigReader {

	/** The connection. */
	protected SQLiteConnection connection = null;

	/**
	 * Instantiates a new instrument config reader.
	 * 
	 * @param connection
	 *            the connection
	 */
	public InstrumentConfigReader(SQLiteConnection connection) {
		super();
		this.connection = connection;
	}

	/**
	 * Gets the instrument config.
	 * 
	 * @param id
	 *            the id
	 * @return the instrument config
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	public InstrumentConfiguration getInstrumentConfig(int id) throws SQLiteException {

		return new SQLiteQuery(connection, "select * from instrument_configuration where id = ?").bind(1, id)
				.extractRecord(new ISQLiteRecordExtraction<InstrumentConfiguration>() {

					public InstrumentConfiguration extract(SQLiteRecord r) throws SQLiteException {

						int id = r.columnInt(InstrumentConfigurationTable.ID);
						String name = r.columnString(InstrumentConfigurationTable.NAME);
						int softwareId = r.columnInt(InstrumentConfigurationTable.SOFTWARE_ID);
						String paramTreeAsStr = r.columnString(InstrumentConfigurationTable.PARAM_TREE);

						return new InstrumentConfiguration(id, name, softwareId, ParamTreeParser
								.parseInstrumentConfigParamTree(paramTreeAsStr));
					}

				});
	}

}
