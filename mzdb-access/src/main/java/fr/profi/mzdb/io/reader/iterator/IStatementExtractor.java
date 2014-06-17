package fr.profi.mzdb.io.reader.iterator;

import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;

public interface IStatementExtractor<T> {

	public T extractObject(SQLiteStatement stmt) throws SQLiteException;

}
