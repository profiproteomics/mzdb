package fr.profi.mzdb.io.writer;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.table.SpectrumTable;
import fr.profi.mzdb.io.reader.iterator.MsScanIterator;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.DataMode;
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

	/** */
	private enum MgfField {
		BEGION_IONS("BEGION IONS"), END_IONS("END IONS"), TITLE("TITLE"), PEPMASS("PEPMASS"), CHARGE("CHARGE"), RTINSECONDS(
				"RTINSECONDS"),

		//
		NEWLINE("\n"), EQUAL("="), PLUS("+");

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
		DEFAULT(""), REFINED_PWIZ("pwiz refined precursor mz"), REFINED_MZDB("mzdb refined precursor mz");

		private final String paramName;

		PrecursorMzComputation(String f) {
			this.paramName = f;
		}

		public String getUserParamName() {
			return this.paramName;
		}

	};

	final Logger logger = LoggerFactory.getLogger(MgfWriter.class);

	private static String titleQuery = "SELECT id, title FROM spectrum";

	private final String mzDBFilePath;

	private MzDbReader mzDBReader;

	private Map<Integer, String> titleByScanId;

	/**
	 * 
	 * @param mzDBFilePath
	 * @throws SQLiteException
	 */
	public MgfWriter(String mzDBFilePath) throws SQLiteException {
		this.mzDBFilePath = mzDBFilePath;

		// Create reader
		try {
			this.mzDBReader = new MzDbReader(this.mzDBFilePath, true);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (SQLiteException e) {
			e.printStackTrace();
		}

		this.titleByScanId = new HashMap<Integer, String>();
		this._fillTitleByScanId();
		this.logger.debug("length titleByScanId:" + this.titleByScanId.size());
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
		new SQLiteQuery(this.mzDBReader.getConnection(), titleQuery).forEachRecord(f);

	}

	/**
	 * 
	 * @param mgfFile
	 * @param pm
	 * @throws FileNotFoundException
	 * @throws SQLiteException
	 */
	public void write(String mgfFile, PrecursorMzComputation precComp) throws FileNotFoundException, SQLiteException {
		
		// treat path mgfFile ?
		if (mgfFile.isEmpty())
			mgfFile = this.mzDBFilePath + ".mgf";
		
		// iterate over ms2 scan
		Iterator<Scan> scanIterator = new MsScanIterator(this.mzDBReader, 2);
		PrintWriter mgfWriter = new PrintWriter(new File(mgfFile));
		Map<Integer, DataEncoding> dataEncodingByScanId = this.mzDBReader.getDataEncodingByScanId();

		while (scanIterator.hasNext()) {
			Scan s = scanIterator.next();
			DataEncoding dataEnc = dataEncodingByScanId.get(s.getHeader().getId());
			this.writeScan(mgfWriter, s, dataEnc, precComp);
		}

		mgfWriter.close();
	}

	/** class representing a row in the mgfHeader */
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

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder().append(field).append(MgfField.EQUAL).append(value)
					.append(trailer);

			return sb.toString();
		}
	}

	/** class representing a Mgf Header */
	public class MgfHeader {
		ArrayList<MgfHeaderEntry> entries;

		public MgfHeader(ArrayList<MgfHeaderEntry> entries) {
			super();
			this.entries = entries;
		}

		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();

			sb.append(MgfField.BEGION_IONS).append(MgfField.NEWLINE);

			for (MgfHeaderEntry entry : entries) {
				sb.append(entry).append(MgfField.NEWLINE);
			}

			return sb.toString();
		}

	}

	/**
	 * 
	 * @param title
	 * @param pepMass
	 * @param charge
	 * @return
	 */
	protected MgfHeader createMgfHeader(String title, double pepMass, int charge) {
		ArrayList<MgfHeaderEntry> entries = new ArrayList<MgfHeaderEntry>();
		entries.add(new MgfHeaderEntry(MgfField.TITLE, title));
		entries.add(new MgfHeaderEntry(MgfField.PEPMASS, pepMass));
		entries.add(new MgfHeaderEntry(MgfField.CHARGE, charge, MgfField.PLUS.toString()));
		return new MgfHeader(entries);
	}

	/**
	 * 
	 * @param title
	 * @param pepMass
	 * @param charge
	 * @param rt
	 * @return
	 */
	protected MgfHeader createMgfHeader(String title, double pepMass, int charge, float rt) {
		ArrayList<MgfHeaderEntry> entries = new ArrayList<MgfHeaderEntry>();
		entries.add(new MgfHeaderEntry(MgfField.TITLE, title));
		entries.add(new MgfHeaderEntry(MgfField.PEPMASS, String.format("%1.5f", pepMass) ));
		entries.add(new MgfHeaderEntry(MgfField.CHARGE, charge, MgfField.PLUS.toString()));
		entries.add(new MgfHeaderEntry(MgfField.RTINSECONDS, rt));
		return new MgfHeader(entries);
	}

	protected void writeScan(PrintWriter mgfWriter, Scan scan, DataEncoding dataEnc, PrecursorMzComputation precComp) {
		
		String mzFragFormat = null;
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
				precMz = Double.parseDouble(
					scanHeader.getUserParam(
						PrecursorMzComputation.REFINED_PWIZ.getUserParamName()
					).getValue()
				);
			} catch (NullPointerException e) {
				this.logger.info("Refined pwiz user param name not found: fall back to default");
			}
		} else if (precComp == PrecursorMzComputation.REFINED_MZDB) {
			try {
				precMz = Double.parseDouble(
					scanHeader.getUserParam(
						PrecursorMzComputation.REFINED_MZDB.getUserParamName()
					).getValue()
				);
			} catch (NullPointerException e) {
				this.logger.info("Refined mdb user param name not found: fall back to default");
			}
			// if (precMz == 0.0d) precMz = scanHeader.getPrecursorMz();
		}

		int charge = scanHeader.getPrecursorCharge();
		MgfHeader mgfScanHeader = createMgfHeader(title, precMz, charge, time);
		mgfWriter.println(mgfScanHeader.toString());

		// Scan Data
		ScanData data = scan.getData();
		double[] mzs = data.getMzList();
		float[] ints = data.getIntensityList();
		for (int i = 0; i < mzs.length; ++i) {
			double mz = mzs[i];
			float intensity = ints[i];
			mgfWriter.print(String.format(mzFragFormat, mz) + "\\s" + String.format("%f", intensity) + "\\n");
		}
		
		mgfWriter.println(MgfField.END_IONS);
		mgfWriter.println(); // make a space between to spectrum
	}

}
