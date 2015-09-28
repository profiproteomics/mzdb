package fr.profi.mzdb.io.reader.iterator;

import java.io.StreamCorruptedException;
import java.util.Map;

import fr.profi.mzdb.AbstractMzDbReader;
import fr.profi.mzdb.io.reader.bb.BoundingBoxBuilder;
import fr.profi.mzdb.model.BoundingBox;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.SpectrumHeader;

import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;

public class BoundingBoxIterator extends AbstractStatementIterator<BoundingBox> {

	protected final Map<Long, SpectrumHeader> spectrumHeaderById;
	protected final Map<Long, DataEncoding> dataEncodingBySpectrumId;
	
	public BoundingBoxIterator(
		AbstractMzDbReader mzDbReader,
		SQLiteStatement stmt
	) throws SQLiteException, StreamCorruptedException {
		super(mzDbReader, stmt);
		
		this.spectrumHeaderById = this.mzDbReader.getSpectrumHeaderById();			
		this.dataEncodingBySpectrumId = this.mzDbReader.getDataEncodingBySpectrumId();
	}

	public BoundingBoxIterator(
		AbstractMzDbReader mzDbReader,
		SQLiteStatement stmt,
		int msLevel
	) throws SQLiteException, StreamCorruptedException {
		super(mzDbReader, stmt);
		
		if( msLevel == 1 ) this.spectrumHeaderById = this.mzDbReader.getMs1SpectrumHeaderById();
		else if( msLevel == 2 ) this.spectrumHeaderById = this.mzDbReader.getMs2SpectrumHeaderById();
		else throw new IllegalArgumentException("unsupported MS level: " + msLevel);
			
		this.dataEncodingBySpectrumId = this.mzDbReader.getDataEncodingBySpectrumId();
	}

	public BoundingBox extractObject(SQLiteStatement stmt) throws SQLiteException, StreamCorruptedException {

		int bbId = stmt.columnInt(0);
		byte[] bbBytes = stmt.columnBlob(1);
		int runSliceId = stmt.columnInt(2);
		int firstSpectrumId = stmt.columnInt(3);
		int lastSpectrumId = stmt.columnInt(4);

		BoundingBox bb = BoundingBoxBuilder.buildBB(
			bbId,
			bbBytes,
			firstSpectrumId,
			lastSpectrumId,
			this.spectrumHeaderById,
			this.dataEncodingBySpectrumId
		);		
		bb.setRunSliceId(runSliceId);
		
		return bb;
	}

}
