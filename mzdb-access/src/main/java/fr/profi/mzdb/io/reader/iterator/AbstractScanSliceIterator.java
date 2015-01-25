package fr.profi.mzdb.io.reader.iterator;

import java.io.StreamCorruptedException;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.BoundingBox;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;

public abstract class AbstractScanSliceIterator {

	protected final MzDbReader mzDbReader;
	protected final SQLiteStatement statement;
	protected final BoundingBoxIterator boundingBoxIterator;
	protected BoundingBox firstBB;
	protected int msLevel;

	public AbstractScanSliceIterator(MzDbReader mzdbReader, String sqlQuery, int msLevel)
			throws SQLiteException, StreamCorruptedException {

		this.mzDbReader = mzdbReader;

		SQLiteConnection conn = mzDbReader.getConnection();
		SQLiteStatement stmt = conn.prepare(sqlQuery, true); // false = disable
		// statement cache
		stmt.bind(1, msLevel);

		this.boundingBoxIterator = new BoundingBoxIterator(mzDbReader, stmt, msLevel);

		this.statement = stmt;
		this.msLevel = msLevel;

		initBB();
	}

	public AbstractScanSliceIterator(
		MzDbReader mzDbReader,
		String sqlQuery,
		int msLevel,
		double minMz,
		double maxMz
	) throws SQLiteException, StreamCorruptedException {

		this.mzDbReader = mzDbReader;
		this.msLevel = msLevel;

		SQLiteConnection conn = mzDbReader.getConnection();
		SQLiteStatement stmt = conn.prepare(sqlQuery, true);

		// bind the two arguments
		stmt.bind(1, msLevel);
		stmt.bind(2, minMz);
		stmt.bind(3, maxMz);

		this.boundingBoxIterator = new BoundingBoxIterator(mzDbReader, stmt, this.msLevel);

		this.statement = stmt;

		initBB();
	}

	public SQLiteStatement getStatement() {
		return this.statement;
	}

	protected void initBB() {
		if (boundingBoxIterator.hasNext())
			this.firstBB = boundingBoxIterator.next();
		else {
			this.firstBB = null;
		}
	}

	public void closeStatement() {
		statement.dispose();
	}

	public boolean hasNext() {

		if (this.firstBB != null) { // this.statement.hasRow() ) {//
			return true;
		} else {
			this.closeStatement();
			return false;
		}
	}

	public void remove() {
		throw new UnsupportedOperationException("Unsuported Operation");
	}

}
