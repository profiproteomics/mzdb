package fr.profi.mzdb.io.writer.mgf;

import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.SpectrumHeader;

public interface IPrecursorComputation {

	public boolean accept(MzDbReader mzDbReader);

	public MgfPrecursor[] getMgfPrecursors(MzDbReader mzDbReader, SpectrumHeader spectrumHeader) throws SQLiteException;

	public String getMethodName();

	public String getMethodVersion();

}
