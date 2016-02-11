package fr.profi.mzdb.utils.sqlite;

import com.almworks.sqlite4java.SQLiteConnection;

/**
 * @author bouyssie
 *
 * @param <T>
 */
public interface ISQLiteConnectionFunction<T> {

	public abstract T apply(SQLiteConnection connection) throws Exception;

}