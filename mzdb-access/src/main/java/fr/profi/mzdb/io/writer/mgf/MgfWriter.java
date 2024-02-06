package fr.profi.mzdb.io.writer.mgf;

import com.almworks.sqlite4java.SQLiteException;
import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.db.table.SpectrumTable;
import fr.profi.mzdb.io.reader.iterator.SpectrumIterator;
import fr.profi.mzdb.model.*;
import fr.profi.mzdb.util.sqlite.ISQLiteRecordOperation;
import fr.profi.mzdb.util.sqlite.SQLiteQuery;
import fr.profi.mzdb.util.sqlite.SQLiteRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.*;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * @author MDB
 */
public class MgfWriter {

	public static String LINE_SPERATOR = System.getProperty("line.separator");
	final Logger logger = LoggerFactory.getLogger(MgfWriter.class);

	private static String titleQuery = "SELECT id, title FROM spectrum WHERE ms_level=?";
	private final String mzDBFilePath;
	private final int msLevel;
	private MzDbReader mzDbReader;
	private Map<Long, String> titleBySpectrumId = new HashMap<>();

	private List<String> headerComments = null;

	private String prolineTitleSeparator = ";";


	/**
	 *
	 * @param mzDBFilePath
	 * @param msLevel
	 * @throws SQLiteException
	 * @throws FileNotFoundException
	 */
	public MgfWriter(String mzDBFilePath, int msLevel, String prolineSpectraTitleSeparator) throws SQLiteException, FileNotFoundException {
		this(mzDBFilePath, msLevel);
		prolineTitleSeparator = prolineSpectraTitleSeparator;
	}

	/**
	 * 
	 * @param mzDBFilePath
	 * @param msLevel
	 * @throws SQLiteException
	 * @throws FileNotFoundException
	 */
	public MgfWriter(String mzDBFilePath, int msLevel) throws SQLiteException, FileNotFoundException {
		if (msLevel < 2 || msLevel > 3) {
			throw new IllegalArgumentException("msLevel must be 2 or 3");
		}
		
		this.mzDBFilePath = mzDBFilePath;
		this.msLevel = msLevel;

		initReader();
	}
	
	public MgfWriter(String mzDBFilePath) throws SQLiteException, FileNotFoundException {
		this(mzDBFilePath, 2);
	}

	private void initReader() throws FileNotFoundException, SQLiteException {
		// Create reader
		this.mzDbReader = new MzDbReader(this.mzDBFilePath, true);
		this.mzDbReader.enablePrecursorListLoading();
		titleBySpectrumId.clear();
		_fillTitleBySpectrumId();
		this.logger.info("Number of loaded spectra titles: " + this.titleBySpectrumId.size());
	}

	public MzDbReader getMzDbReader() {
		if(!mzDbReader.getConnection().isOpen()) {
			try {
				initReader();
			} catch (FileNotFoundException | SQLiteException e) {
				throw new RuntimeException(e);
			}
		}
		return mzDbReader;
	}

	public void setHeaderComments(List<String> headerComments) {
		this.headerComments = headerComments;
	}

	private void _fillTitleBySpectrumId() throws SQLiteException {

		/** inner class for treating sql resulting records */
		final class TitleByIdFiller implements ISQLiteRecordOperation {
			private Map<Long, String> titleById;

			TitleByIdFiller(Map<Long, String> t) {
				this.titleById = t;
			}

			@Override
			public void execute(SQLiteRecord elem, int idx) throws SQLiteException {
				long id = elem.columnLong(SpectrumTable.ID);
				String title = elem.columnString(SpectrumTable.TITLE);
				titleById.put(id, title);
			}
		} // end inner class

		TitleByIdFiller f = new TitleByIdFiller(this.titleBySpectrumId);

		new SQLiteQuery(this.mzDbReader.getConnection(), titleQuery).bind(1, this.msLevel).forEachRecord(f);
	}

	public void write(String mgfFile, PrecursorMzComputationEnum precComp, float mzTolPPM, float intensityCutoff, boolean exportProlineTitle)
		throws SQLiteException, IOException {
		write(mgfFile, new DefaultPrecursorComputer(precComp, mzTolPPM), null, intensityCutoff, exportProlineTitle);
	}

	public void write(String mgfFile, IPrecursorComputation precComp, float intensityCutoff, boolean exportProlineTitle) throws SQLiteException, IOException {
		write(mgfFile, precComp, new DefaultSpectrumProcessor(), intensityCutoff, exportProlineTitle);
	}

	public void write(String mgfFile, IPrecursorComputation precComp, ISpectrumProcessor spectrumProcessor, float intensityCutoff, boolean exportProlineTitle) throws SQLiteException, IOException {
		if(!mzDbReader.getConnection().isOpen()) {
			initReader();
		}

		// treat path mgfFile ?
		if (mgfFile.isEmpty())
			mgfFile = this.mzDBFilePath + ".mgf";
		
		// Configure the mzDbReader in order to load all precursor lists and all scan list when reading spectra headers
		mzDbReader.enablePrecursorListLoading();
		mzDbReader.enableScanListLoading();

		// Iterate MSn spectra
		final Iterator<Spectrum> spectrumIterator = new SpectrumIterator(mzDbReader, mzDbReader.getConnection(), msLevel);
		final PrintWriter mgfWriter = new PrintWriter(new BufferedWriter(new FileWriter(mgfFile)));
		final Map<Long, DataEncoding> dataEncodingBySpectrumId = this.mzDbReader.getDataEncodingBySpectrumId();

		if (headerComments != null && !headerComments.isEmpty()) {
			headerComments.forEach(l -> mgfWriter.println("# " + l));
			mgfWriter.println();
		}
		int spectraCount = 0;
		while (spectrumIterator.hasNext()) {
			
			Spectrum s = spectrumIterator.next();
			long spectrumId = s.getHeader().getId();
			DataEncoding dataEnc = dataEncodingBySpectrumId.get(spectrumId);

			MgfPrecursor[] precursors = precComp.getMgfPrecursors(mzDbReader, s.getHeader());
		  for (int k = 0; k < precursors.length; k++) {
				String spectrumAsStr = this.stringifySpectrum(precursors[k], s, dataEnc, spectrumProcessor, intensityCutoff, exportProlineTitle);
				// Write the spectrum
				mgfWriter.println(spectrumAsStr);
				// Write a blank line between two spectra
				mgfWriter.println();
				spectraCount++;
			}
		}

		this.logger.info(String.format("MGF file successfully created: %d spectra exported.", spectraCount));
		mgfWriter.flush();
		mgfWriter.close();
		mzDbReader.close();
	}

	/**
	 * 
	 * @param spectrum
	 * @param dataEnc
	 * @param intensityCutoff
	 * @return
	 * @throws SQLiteException
	 */
	protected String stringifySpectrum(
	  MgfPrecursor mgfPrecursor,
		Spectrum spectrum,
		DataEncoding dataEnc,
		ISpectrumProcessor spectrumProcessor,
		float intensityCutoff,
		boolean exportProlineTitle
	) throws SQLiteException {

		String mzFragFormat = null;
		// FIXME: check if is_high_res parameter is used and is correct
		if (dataEnc.getPeakEncoding() == PeakEncoding.LOW_RES_PEAK) {
			mzFragFormat = "%.1f";
		} else { // We assume high resolution m/z for fragments
			mzFragFormat = "%.3f";
		}
		// Unpack data
		final SpectrumHeader spectrumHeader = spectrum.getHeader();
		String title;
		if (exportProlineTitle == false)
			title = this.titleBySpectrumId.get(spectrumHeader.getSpectrumId());
		else {
			float timeInMinutes = spectrumHeader.getTime() / 60;
			StringBuilder titleTemplate = new StringBuilder();
			titleTemplate.append("first_cycle:%d").append(prolineTitleSeparator);
			titleTemplate.append("last_cycle:%d").append(prolineTitleSeparator);
			titleTemplate.append("first_scan:%d").append(prolineTitleSeparator);
			titleTemplate.append("last_scan:%d").append(prolineTitleSeparator);
			titleTemplate.append("first_time:%.3f").append(prolineTitleSeparator);
			titleTemplate.append("last_time:%.3f").append(prolineTitleSeparator);

			if (mgfPrecursor.hasAnnotation("source")) {
				titleTemplate.append("source:").append(mgfPrecursor.getAnnotation("source")).append(prolineTitleSeparator);
			}

			if (mgfPrecursor.hasAnnotation("precursor.signal.total.sw")) {
				titleTemplate.append("pif:").append(mgfPrecursor.getAnnotation("precursor.signal.total.sw")).append(prolineTitleSeparator);
			}

			if (mgfPrecursor.hasAnnotation("mgf.id")) {
				titleTemplate.append("mgf_id:").append(mgfPrecursor.getAnnotation("mgf.id")).append(prolineTitleSeparator);
			}

			titleTemplate.append("raw_file_identifier:%s").append(prolineTitleSeparator);

			title = String.format(titleTemplate.toString(),
				spectrumHeader.getCycle(),
				spectrumHeader.getCycle(),
				spectrumHeader.getInitialId(),
				spectrumHeader.getInitialId(),
				timeInMinutes,
				timeInMinutes,
				mzDbReader.getFirstSourceFileName().split("\\.")[0]);
		}

		StringBuilder spectrumStringBuilder = new StringBuilder();


			if (spectrumStringBuilder.length() > 0) spectrumStringBuilder.append(MgfWriter.LINE_SPERATOR).append(MgfWriter.LINE_SPERATOR);

			spectrumStringBuilder.append(MgfField.BEGIN_IONS).append(MgfWriter.LINE_SPERATOR);
			spectrumStringBuilder.append(MgfField.TITLE).append("=").append(title).append(MgfWriter.LINE_SPERATOR);
			spectrumStringBuilder.append(MgfField.SCANS).append("=").append(spectrumHeader.getInitialId()).append(MgfWriter.LINE_SPERATOR);

			mgfPrecursor.appendToStringBuilder(spectrumStringBuilder);

			// Spectrum Data
			final SpectrumData data = spectrumProcessor.processSpectrum(mgfPrecursor, spectrum.getData());
			final double[] mzs = data.getMzList();
			final float[] ints = data.getIntensityList();

			final int intsLength = ints.length;

			for (int i = 0; i < intsLength; ++i) {

				float intensity = ints[i];

				if (intensity >= intensityCutoff) {
					double mz = mzs[i];
					spectrumStringBuilder
									.append(String.format(mzFragFormat, mz))
									.append(" ")
									.append(String.format("%.0f", intensity))
									.append(LINE_SPERATOR);
				}
			}

			spectrumStringBuilder.append(MgfField.END_IONS);
			return spectrumStringBuilder.toString();
	}

}
