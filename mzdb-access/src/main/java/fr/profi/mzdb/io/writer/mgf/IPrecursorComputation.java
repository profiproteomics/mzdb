package fr.profi.mzdb.io.writer.mgf;

import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.SpectrumHeader;

public interface IPrecursorComputation {

	/**
	 * Returns the precursor m/z value of the specified SpectrumHeader.
	 * 
	 * @param spectrumHeader : the MS2 SpectrumHeader
	 * @param mzDbReader : the mzdbReader considered
	 * @return the precursor m/z value of the specified SpectrumHeader
	 */
	
	public double getPrecursorMz(MzDbReader mzDbReader, SpectrumHeader spectrumHeader) throws SQLiteException;
	
	/**
	 * Returns the precursor m/z value of the specified SpectrumHeader.
	 * 
	 * @param spectrumHeader : the MS2 SpectrumHeader
	 * @param mzDbReader : the mzdbReader considered
	 * @return the precursor m/z value of the specified SpectrumHeader
	 */
	
	public int getPrecursorCharge(MzDbReader mzDbReader, SpectrumHeader spectrumHeader) throws SQLiteException;
	
	public String getParamName();
	
	public MgfHeader getMgfHeader(MzDbReader mzDbReader, SpectrumHeader spectrumHeader, String title) throws SQLiteException;

	public MgfPrecursor[] getMgfPrecursors(MzDbReader mzDbReader, SpectrumHeader spectrumHeader) throws SQLiteException;

}
