/**
 * 
 */
package fr.profi.mzdb.utils.sqlite;

import com.almworks.sqlite4java.SQLiteException;

/**
 * @author David Bouyssie
 * 
 */
public interface ISQLiteRecordExtraction<T> {
	T extract(SQLiteRecord record) throws SQLiteException;
}
