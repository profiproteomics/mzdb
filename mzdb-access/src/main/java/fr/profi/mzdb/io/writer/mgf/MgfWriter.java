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
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.util.*;

/**
 * @author MDB
 */
public class MgfWriter {

	public static String LINE_SPERATOR = System.getProperty("line.separator");

	private static final DecimalFormat DEC1 = new DecimalFormat("#.0", new DecimalFormatSymbols(Locale.US));
	private static final DecimalFormat DEC3 = new DecimalFormat("#.000", new DecimalFormatSymbols(Locale.US));
	private static final DecimalFormat NODEC = new  DecimalFormat("#", new DecimalFormatSymbols(Locale.US));

	final Logger logger = LoggerFactory.getLogger(MgfWriter.class);

	private static String titleQuery = "SELECT id, title FROM spectrum WHERE ms_level=?";
	protected final String mzDBFilePath;
	protected final int msLevel;

	protected MzDbReader mzDbReader;
	protected Map<Long, String> titleBySpectrumId = new HashMap<>();
	protected List<String> headerComments = null;
	protected final String prolineTitleSeparator;


	/**
	 *
	 * @param mzDBFilePath
	 * @param msLevel
	 * @throws SQLiteException
	 * @throws FileNotFoundException
	 */
	public MgfWriter(String mzDBFilePath, int msLevel, String prolineSpectraTitleSeparator) throws SQLiteException, FileNotFoundException {
		if (msLevel < 2 || msLevel > 3) {
			throw new IllegalArgumentException("msLevel must be 2 or 3");
		}

		this.mzDBFilePath = mzDBFilePath;
		this.msLevel = msLevel;
		this.prolineTitleSeparator = prolineSpectraTitleSeparator;

		initReader();
	}

	/**
	 * 
	 * @param mzDBFilePath
	 * @param msLevel
	 * @throws SQLiteException
	 * @throws FileNotFoundException
	 */
	public MgfWriter(String mzDBFilePath, int msLevel) throws SQLiteException, FileNotFoundException {
		this(mzDBFilePath, msLevel, ";");
	}
	
	public MgfWriter(String mzDBFilePath) throws SQLiteException, FileNotFoundException {
		this(mzDBFilePath, 2);
	}

	protected void initReader() throws FileNotFoundException, SQLiteException {
		// Create reader
		this.mzDbReader = new MzDbReader(this.mzDBFilePath, true);
		this.mzDbReader.enablePrecursorListLoading();
		this.mzDbReader.enableParamTreeLoading();
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
		final String rawFilename = mzDbReader.getFirstSourceFileName().split("\\.")[0];

		if (headerComments != null && !headerComments.isEmpty()) {
			headerComments.forEach(l -> mgfWriter.println("# " + l));
			mgfWriter.println();
		}
		int spectraCount = 0;
		int precursorsCount = 0;
		int ignoredSpectraCount = 0;

		while (spectrumIterator.hasNext()) {
			
			Spectrum s = spectrumIterator.next();
			long spectrumId = s.getHeader().getId();
			DataEncoding dataEnc = dataEncodingBySpectrumId.get(spectrumId);

			MgfPrecursor[] precursors = precComp.getMgfPrecursors(mzDbReader, s.getHeader());
		  for (int k = 0; k < precursors.length; k++) {
				MgfPrecursor precursor = precursors[k];
				String spectrumTitle = getTitle(precursor, s.getHeader(), rawFilename, exportProlineTitle);
				final SpectrumData processedSpectrumData = spectrumProcessor.processSpectrum(precursor, s.getData());
				String spectrumAsStr = this.stringifySpectrum(precursor, new Spectrum(s.getHeader(), processedSpectrumData), dataEnc, intensityCutoff, spectrumTitle);
				// Write the spectrum
				mgfWriter.println(spectrumAsStr);
				// Write a blank line between two spectra
				mgfWriter.println();
				precursorsCount++;
			}
			if (precursors.length > 0) {
				spectraCount++;
			} else {
				ignoredSpectraCount++;
			}
		}

		this.logger.info("MGF file successfully created: {} precursors exported from {} spectra. {} spectra ignored.", precursorsCount, spectraCount, ignoredSpectraCount);

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
		float intensityCutoff,
		String title)  {

		DecimalFormat mzFragFormat = null;
		// FIXME: check if is_high_res parameter is used and is correct
		if (dataEnc.getPeakEncoding() == PeakEncoding.LOW_RES_PEAK) {
			mzFragFormat = DEC1;
		} else { // We assume high resolution m/z for fragments
			mzFragFormat = DEC3;
		}
		// Unpack data
		final SpectrumHeader spectrumHeader = spectrum.getHeader();

		StringBuilder spectrumStringBuilder = new StringBuilder();

			if (spectrumStringBuilder.length() > 0) spectrumStringBuilder.append(MgfWriter.LINE_SPERATOR).append(MgfWriter.LINE_SPERATOR);

			spectrumStringBuilder.append(MgfField.BEGIN_IONS).append(MgfWriter.LINE_SPERATOR);
			spectrumStringBuilder.append(MgfField.TITLE).append("=").append(title).append(MgfWriter.LINE_SPERATOR);
			spectrumStringBuilder.append(MgfField.SCANS).append("=").append(spectrumHeader.getInitialId()).append(MgfWriter.LINE_SPERATOR);

			mgfPrecursor.appendToStringBuilder(spectrumStringBuilder);

			final double[] mzs = spectrum.getData().getMzList();
			final float[] ints = spectrum.getData().getIntensityList();

			final int intsLength = ints.length;

			for (int i = 0; i < intsLength; ++i) {

				float intensity = ints[i];

				if (intensity >= intensityCutoff) {
					double mz = mzs[i];
					spectrumStringBuilder
									.append(mzFragFormat.format(mz))
									.append(" ")
									.append(Math.round(intensity))
									.append(LINE_SPERATOR);
				}
			}

			spectrumStringBuilder.append(MgfField.END_IONS);
			return spectrumStringBuilder.toString();
	}

	protected String getTitle(MgfPrecursor mgfPrecursor, SpectrumHeader spectrumHeader, String filename, boolean formatAsProlineTitle) {
		String title;
		if (formatAsProlineTitle == false)
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
				filename);
		}
		return title;
	}

}
