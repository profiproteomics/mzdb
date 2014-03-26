package fr.profi.mzdb.io.writer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.model.params.param.UserParam;
import fr.profi.mzdb.db.table.SpectrumTable;
import fr.profi.mzdb.io.reader.iterator.MsScanIterator;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.PeakEncoding;
import fr.profi.mzdb.model.Scan;
import fr.profi.mzdb.model.ScanData;
import fr.profi.mzdb.model.ScanHeader;
import fr.profi.mzdb.utils.sqlite.ISQLiteRecordOperation;
import fr.profi.mzdb.utils.sqlite.SQLiteQuery;
import fr.profi.mzdb.utils.sqlite.SQLiteRecord;

/**
 * @author MDB
 */
public class MgfWriter {
	
	private static String LINE_SPERATOR = System.getProperty("line.separator");

	/** */
	private enum MgfField {
		BEGION_IONS("BEGION IONS"),
		END_IONS("END IONS"),
		TITLE("TITLE"),
		PEPMASS("PEPMASS"),
		CHARGE("CHARGE"),
		RTINSECONDS("RTINSECONDS");

		//
		//NEWLINE("\n"), EQUAL("="), PLUS("+");

		private final String fieldString;

		MgfField(String f) {
			this.fieldString = f;
		}

		public String toString() {
			return this.fieldString;
		}
	};

	/** */
	public enum PrecursorMzComputation {
		DEFAULT("default precursor mz"), REFINED_PWIZ("pwiz refined precursor mz"), REFINED_MZDB("mzdb refined precursor mz");

		private final String paramName;

		PrecursorMzComputation(String f) {
			this.paramName = f;
		}

		public String getUserParamName() {
			return this.paramName;
		}

	};

	final Logger logger = LoggerFactory.getLogger(MgfWriter.class);

	private static String titleQuery = "SELECT id, title FROM spectrum WHERE ms_level='2'";

	private final String mzDBFilePath;

	private MzDbReader mzDbReader;

	private Map<Integer, String> titleByScanId = new HashMap<Integer, String>();

	/**
	 * 
	 * @param mzDBFilePath
	 * @throws SQLiteException
	 */
	public MgfWriter(String mzDBFilePath) throws SQLiteException {
		this.mzDBFilePath = mzDBFilePath;

		// Create reader
		try {
			this.mzDbReader = new MzDbReader(this.mzDBFilePath, true);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (SQLiteException e) {
			e.printStackTrace();
		}

		this._fillTitleByScanId();
		
		this.logger.info("Number of loaded spectra titles: " + this.titleByScanId.size());
	}

	private void _fillTitleByScanId() throws SQLiteException {

		/** inner class for treating sql resulting records */
		final class TitleByIdFiller implements ISQLiteRecordOperation {
			private Map<Integer, String> titleById;

			TitleByIdFiller(Map<Integer, String> t) {
				this.titleById = t;
			}

			@Override
			public void execute(SQLiteRecord elem, int idx) throws SQLiteException {
				int id = elem.columnInt(SpectrumTable.ID);
				String title = elem.columnString(SpectrumTable.TITLE);
				titleById.put(id, title);
			}
		} // end inner class

		TitleByIdFiller f = new TitleByIdFiller(this.titleByScanId);
		new SQLiteQuery(this.mzDbReader.getConnection(), titleQuery).forEachRecord(f);

	}

	/**
	 * 
	 * @param mgfFile
	 * @param pm
	 * @throws FileNotFoundException
	 * @throws SQLiteException
	 */
	public void write(
		String mgfFile,
		PrecursorMzComputation precComp,
		float intensityCutoff
	) throws FileNotFoundException, SQLiteException {
		
		// treat path mgfFile ?
		if (mgfFile.isEmpty())
			mgfFile = this.mzDBFilePath + ".mgf";
		
		// iterate over ms2 scan
		Iterator<Scan> scanIterator = new MsScanIterator(this.mzDbReader, 2);
		PrintWriter mgfWriter = new PrintWriter(new File(mgfFile));
		Map<Integer, DataEncoding> dataEncodingByScanId = this.mzDbReader.getDataEncodingByScanId();

		int spectraCount = 0;
		while (scanIterator.hasNext()) {
			Scan s = scanIterator.next();
			DataEncoding dataEnc = dataEncodingByScanId.get(s.getHeader().getId());
			String spectrumAsStr = this.stringifySpectrum(s, dataEnc, precComp, intensityCutoff);
			
			// make a space between to spectrum
			mgfWriter.println(spectrumAsStr);
			mgfWriter.println();
			spectraCount++;
		}
		
		this.logger.info( String.format("MGF file successgully created: %d spectra exported.", spectraCount) );
		
		mgfWriter.flush();
		mgfWriter.close();
	}
	
	protected String stringifySpectrum(
		Scan scan,
		DataEncoding dataEnc,
		PrecursorMzComputation precComp,
		float intensityCutoff
	) throws SQLiteException {
		
		String mzFragFormat = null;
		// FIXME: check if is_high_res parameter is used and is correct
		if( dataEnc.getPeakEncoding() == PeakEncoding.LOW_RES_PEAK ) {
			mzFragFormat = "%1.1f";
		}
		else { // We assume high resolution m/z for fragments
			mzFragFormat = "%1.3f";
		}
		
		// unpack data
		ScanHeader scanHeader = scan.getHeader();
		String title = this.titleByScanId.get(scanHeader.getScanId());
		float time = scanHeader.getElutionTime();
		double precMz = scanHeader.getPrecursorMz();

		if (precComp == PrecursorMzComputation.DEFAULT) {

		} else if (precComp == PrecursorMzComputation.REFINED_PWIZ) {
			try {
				scanHeader.loadScanList(this.mzDbReader);
				UserParam precMzParam = scanHeader
						.getScanList()
						.getScans()
						.get(0)
						.getUserParam("[Thermo Trailer Extra]Monoisotopic M/Z:");
				
				precMz = Double.parseDouble( precMzParam.getValue() );
			} catch (NullPointerException e) {
				this.logger.trace("Refined pwiz user param name not found: fall back to default");
			}
		} else if (precComp == PrecursorMzComputation.REFINED_MZDB) {
			try {
				precMz = Double.parseDouble(
					scanHeader.getUserParam(
						PrecursorMzComputation.REFINED_MZDB.getUserParamName()
					).getValue()
				);
			} catch (NullPointerException e) {
				this.logger.trace("Refined mdb user param name not found: fall back to default");
			}
			// if (precMz == 0.0d) precMz = scanHeader.getPrecursorMz();
		}

		int charge = scanHeader.getPrecursorCharge();
		MgfHeader mgfScanHeader = new MgfHeader(title, precMz, charge, time);
		
		StringBuilder spectrumStringBuilder = new StringBuilder();
		mgfScanHeader.appendToStringBuilder(spectrumStringBuilder);

		// Scan Data
		ScanData data = scan.getData();
		double[] mzs = data.getMzList();
		float[] ints = data.getIntensityList();
		for (int i = 0; i < mzs.length; ++i) {			
			float intensity = ints[i];
			
			if( intensity >= intensityCutoff ) {				
				double mz = mzs[i];
				
				spectrumStringBuilder
					.append( String.format(mzFragFormat, mz) )
					.append( " " )
					.append( String.format("%.0f", intensity) )
					.append( LINE_SPERATOR );
			}
		}
		
		spectrumStringBuilder.append(MgfField.END_IONS);
		
		return spectrumStringBuilder.toString();
	}
	
	/** Class representing a MGF header */
	public class MgfHeader {
		MgfHeaderEntry[] entries;

		public MgfHeader(MgfHeaderEntry[] entries) {
			super();
			this.entries = entries;
		}
		
		/**
		 * 
		 * @param title
		 * @param pepMass
		 * @param charge
		 * @return a new MgfHeader
		 */
		public MgfHeader(String title, double pepMass, int charge) {
			this(
				new MgfHeaderEntry[] { 
					new MgfHeaderEntry(MgfField.TITLE, title),
					new MgfHeaderEntry(MgfField.PEPMASS, pepMass),
					// TODO: use the trailer corresponding to the acquisition polarity (see mzDB meta-data)
					new MgfHeaderEntry(MgfField.CHARGE, charge, "+")
				}
			);
		}
		
		/**
		 * 
		 * @param title
		 * @param pepMass
		 * @param charge
		 * @param rt
		 * @return a new MgfHeader
		 */
		public MgfHeader(String title, double pepMass, int charge, float rt) {
			
			this(
				new MgfHeaderEntry[] { 
					new MgfHeaderEntry(MgfField.TITLE, title),
					new MgfHeaderEntry(MgfField.PEPMASS, pepMass),
					// TODO: use the trailer corresponding to the acquisition polarity (see mzDB meta-data)
					new MgfHeaderEntry(MgfField.CHARGE, charge, "+"),
					new MgfHeaderEntry(MgfField.RTINSECONDS, String.format("%.2f", rt) )
				}
			);
		}
		
		public StringBuilder appendToStringBuilder(StringBuilder sb) {

			sb.append(MgfField.BEGION_IONS).append(LINE_SPERATOR);

			for (MgfHeaderEntry entry : entries) {
				entry.appendToStringBuilder(sb).append(LINE_SPERATOR);
			}

			return sb;
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			return this.appendToStringBuilder(sb).toString();
		}

	}

	/** Class representing a row in the MGF header */
	public class MgfHeaderEntry {

		final MgfField field;
		final Object value;
		final String trailer;

		public MgfHeaderEntry(MgfField field, Object value, String trailer) {
			super();
			this.field = field;
			this.value = value;
			this.trailer = trailer;
		}

		public MgfHeaderEntry(MgfField field, Object value) {
			super();
			this.field = field;
			this.value = value;
			this.trailer = null;
		}
		
		public StringBuilder appendToStringBuilder(StringBuilder sb) {
			sb.append(field).append("=").append(value);
			
			if( this.trailer != null ) {
				sb.append(trailer);
			}

			return sb;
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			return this.appendToStringBuilder(sb).toString();
		}
	}


	/*protected MgfHeader createMgfHeader(String title, double pepMass, int charge) {
		ArrayList<MgfHeaderEntry> entries = new ArrayList<MgfHeaderEntry>();
		entries.add(new MgfHeaderEntry(MgfField.TITLE, title));
		entries.add(new MgfHeaderEntry(MgfField.PEPMASS, pepMass));
		entries.add(new MgfHeaderEntry(MgfField.CHARGE, charge, "+"));
		return new MgfHeader(entries);
	}*/

	/**
	 * 
	 * @param title
	 * @param pepMass
	 * @param charge
	 * @param rt
	 * @return
	 */
	/*protected MgfHeader createMgfHeader(String title, double pepMass, int charge, float rt) {
		
		ArrayList<MgfHeaderEntry> entries = new ArrayList<MgfHeaderEntry>();
		
		entries.add(new MgfHeaderEntry(MgfField.TITLE, title));
		entries.add(new MgfHeaderEntry(MgfField.PEPMASS, String.format("%1.5f", pepMass) ));
		// TODO: use the trailer corresponding to the acquisition polarity (see mzDB meta-data)
		entries.add(new MgfHeaderEntry(MgfField.CHARGE, charge, "+" ));
		entries.add(new MgfHeaderEntry(MgfField.RTINSECONDS, rt));
		
		return new MgfHeader(entries);
	}*/

}
