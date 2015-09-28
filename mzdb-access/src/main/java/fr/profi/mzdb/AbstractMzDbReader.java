package fr.profi.mzdb;

import java.io.StreamCorruptedException;
import java.util.*;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import fr.profi.mzdb.db.model.*;
import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.db.model.params.param.CVEntry;
import fr.profi.mzdb.db.model.params.param.CVParam;
import fr.profi.mzdb.db.table.BoundingBoxTable;
import fr.profi.mzdb.io.reader.bb.BoundingBoxBuilder;
import fr.profi.mzdb.io.reader.bb.IBlobReader;
import fr.profi.mzdb.io.reader.cache.AbstractDataEncodingReader;
import fr.profi.mzdb.io.reader.cache.AbstractRunSliceHeaderReader;
import fr.profi.mzdb.io.reader.cache.AbstractSpectrumHeaderReader;
import fr.profi.mzdb.io.reader.cache.MzDbEntityCache;
import fr.profi.mzdb.model.*;
import fr.profi.mzdb.utils.ms.MsUtils;
import fr.profi.mzdb.utils.sqlite.SQLiteQuery;
import fr.profi.mzdb.utils.sqlite.SQLiteRecord;
import fr.profi.mzdb.utils.sqlite.SQLiteRecordIterator;

/**
 * Allows to manipulates data contained in the mzDB file.
 *
 * @author David
 */
public abstract class AbstractMzDbReader {
	
	final Logger logger = LoggerFactory.getLogger(AbstractMzDbReader.class);

	final protected BBSizes bbSizes = new BBSizes();

	/** Some fields initialized in the constructor **/
	protected String dbLocation = null;
	protected MzDbEntityCache entityCache = null;
	protected MzDbHeader mzDbHeader = null;
	protected IMzDBParamNameGetter _paramNameGetter = null;

	/** Some readers with internal entity cache **/
	protected AbstractDataEncodingReader _dataEncodingReader = null;
	protected AbstractSpectrumHeaderReader _spectrumHeaderReader = null;
	protected AbstractRunSliceHeaderReader _runSliceHeaderReader = null;

	/**
	 * The is no loss mode. If no loss mode is enabled, all data points will be encoded as highres, i.e. 64
	 * bits mz and 64 bits int. No peak picking and not fitting will be performed on profile data.
	 */
	protected Boolean isNoLossMode;

	/** Define some lazy fields **/
	// TODO: find a CV param representing the information better
	protected AcquisitionMode acquisitionMode = null;
	protected IsolationWindow[] diaIsolationWindows = null;
	protected List<InstrumentConfiguration> instrumentConfigs = null;
	protected List<Run> runs = null;
	protected List<Sample> samples = null;
	protected List<Software> softwareList = null;
	protected List<SourceFile> sourceFiles = null;

	/**
	 * Close the file to avoid memory leaks. Method to be implemented in child classes.
	 */
	public abstract void close();

	/**
	 * Gets the entity cache.
	 *
	 * @return the entity cache
	 */
	public MzDbEntityCache getEntityCache() {
		return this.entityCache;
	}

	public String getDbLocation() {
		return this.dbLocation;
	}

	public MzDbHeader getMzDbHeader() throws SQLiteException {
		return this.mzDbHeader;
	}

	/**
	 *
	 * @return
	 * @throws SQLiteException
	 */
	protected String getModelVersion(SQLiteConnection connection) throws SQLiteException {
		String sqlString = "SELECT version FROM mzdb LIMIT 1";
		return new SQLiteQuery(connection, sqlString).extractSingleString();
	}

	protected String getPwizMzDbVersion(SQLiteConnection connection) throws SQLiteException {
		String sqlString = "SELECT version FROM software WHERE name LIKE '%mzDB'";
		return new SQLiteQuery(connection, sqlString).extractSingleString();
	}

	/**
	 *
	 * @return
	 * @throws SQLiteException
	 */
	public boolean isNoLossMode() throws SQLiteException {

		if (this.isNoLossMode == null) {
			MzDbHeader p = this.getMzDbHeader();

			if (p.getUserParam(this._paramNameGetter.getLossStateParamName()).getValue().equals("false")) {
				this.isNoLossMode = false;
			} else {
				this.isNoLossMode = true;
			}
		}

		return this.isNoLossMode;
	}

	/**
	 *
	 * @return
	 * @throws SQLiteException
	 */
	public BBSizes getBBSizes() throws SQLiteException {
		return this.bbSizes;
	}

	/**
	 * @param bbSizes
	 * @param paramNameGetter
	 * @param header
	 */
	protected void _setBBSizes(IMzDBParamNameGetter paramNameGetter) {
		this.bbSizes.BB_MZ_HEIGHT_MS1 = Double.parseDouble(this.mzDbHeader.getUserParam(paramNameGetter.getMs1BBMzWidthParamName()).getValue());
		this.bbSizes.BB_MZ_HEIGHT_MSn = Double.parseDouble(this.mzDbHeader.getUserParam(paramNameGetter.getMsnBBMzWidthParamName()).getValue());
		this.bbSizes.BB_RT_WIDTH_MS1 = Float.parseFloat(this.mzDbHeader.getUserParam(paramNameGetter.getMs1BBTimeWidthParamName()).getValue());
		this.bbSizes.BB_RT_WIDTH_MSn = Float.parseFloat(this.mzDbHeader.getUserParam(paramNameGetter.getMs1BBTimeWidthParamName()).getValue());
	}

	/**
	 * Gets the last time.
	 *
	 * @return float the rt of the last spectrum
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected float getLastTime(SQLiteConnection connection) throws SQLiteException {
		// Retrieve the number of spectra
		String sqlString = "SELECT time FROM spectrum ORDER BY id DESC LIMIT 1";
		return (float) new SQLiteQuery(connection, sqlString).extractSingleDouble();
	}

	/**
	 * Gets the max ms level.
	 *
	 * @return the max ms level
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getMaxMsLevel(SQLiteConnection connection) throws SQLiteException {
		return new SQLiteQuery(connection, "SELECT max(ms_level) FROM run_slice").extractSingleInt();
	}

	/**
	 * Gets the mz range.
	 *
	 * @param msLevel
	 *            the ms level
	 * @return runSlice min mz and runSlice max mz
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int[] getMzRange(int msLevel, SQLiteConnection connection) throws SQLiteException {

		final SQLiteStatement stmt = connection.prepare("SELECT min(begin_mz), max(end_mz) FROM run_slice WHERE ms_level=?");
		stmt.bind(1, msLevel);
		stmt.step();

		final int minMz = stmt.columnInt(0);
		final int maxMz = stmt.columnInt(1);
		stmt.dispose();

		final int[] mzRange = { minMz, maxMz };
		return mzRange;
	}

	/**
	 * Gets the bounding box count.
	 *
	 * @return int, the number of bounding box
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getBoundingBoxesCount(SQLiteConnection connection) throws SQLiteException {
		return this.getTableRecordsCount("bounding_box", connection);
	}

	/**
	 * Gets the bounding box count.
	 *
	 * @param runSliceId
	 *            the run slice id
	 * @return the number of bounding box contained in the specified runSliceId
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getBoundingBoxesCount(int runSliceId, SQLiteConnection connection) throws SQLiteException {
		String queryStr = "SELECT count(*) FROM bounding_box WHERE bounding_box.run_slice_id = ?";
		return new SQLiteQuery(connection, queryStr).bind(1, runSliceId).extractSingleInt();
	}

	/**
	 * Gets the cycle count.
	 *
	 * @return the cycle count
	 * @throws SQLiteException
	 */
	protected int getCyclesCount(SQLiteConnection connection) throws SQLiteException {
		String queryStr = "SELECT cycle FROM spectrum ORDER BY id DESC LIMIT 1";
		return new SQLiteQuery(connection, queryStr).extractSingleInt();
	}

	/**
	 * Gets the data encoding count.
	 *
	 * @return the data encoding count
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getDataEncodingsCount(SQLiteConnection connection) throws SQLiteException {
		return this.getTableRecordsCount("data_encoding", connection);
	}

	/**
	 * Gets the spectra count.
	 *
	 * @return int the number of spectra
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getSpectraCount(SQLiteConnection connection) throws SQLiteException {
		return this.getTableRecordsCount("spectrum", connection);
	}

	/**
	 * Gets the spectra count for a given MS level.
	 *
	 * @return int the number of spectra
	 * @throws SQLiteException
	 *             the SQLite exception
	 */
	protected int getSpectraCount(int msLevel, SQLiteConnection connection) throws SQLiteException {
		String queryStr = "SELECT count(*) FROM spectrum WHERE ms_level = ?";
		return new SQLiteQuery(connection, queryStr).bind(1, msLevel).extractSingleInt();
	}

	/**
	 * Gets the run slice count.
	 *
	 * @return int the number of runSlice
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getRunSlicesCount(SQLiteConnection connection) throws SQLiteException {
		return this.getTableRecordsCount("run_slice", connection);
	}

	/**
	 * Gets the table records count.
	 *
	 * @param tableName
	 *            the table name
	 * @return the int
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getTableRecordsCount(String tableName, SQLiteConnection connection) throws SQLiteException {
		return new SQLiteQuery(connection, "SELECT seq FROM sqlite_sequence WHERE name = ?").bind(1, tableName).extractSingleInt();
	}

	/**
	 * Gets the data encoding.
	 *
	 * @param id
	 *            the id
	 * @return the data encoding
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	public abstract DataEncoding getDataEncoding(int id) throws SQLiteException;

	/**
	 * Gets the data encoding by spectrum id.
	 *
	 * @return the data encoding by spectrum id
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	public abstract Map<Long, DataEncoding> getDataEncodingBySpectrumId() throws SQLiteException;

	/**
	 * Gets the spectrum data encoding.
	 *
	 * @param spectrumId
	 *            the spectrum id
	 * @return the spectrum data encoding
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	public abstract DataEncoding getSpectrumDataEncoding(long spectrumId) throws SQLiteException;

	/**
	 * Gets the run slices.
	 *
	 * @return array of runSlice instance without data associated
	 * @throws SQLiteException
	 *             the SQLite exception
	 */
	public abstract RunSliceHeader[] getRunSliceHeaders(int msLevel) throws SQLiteException;

	/**
	 * Gets the run slice header by id.
	 *
	 * @param msLevel
	 *            the ms level
	 * @return the run slice header by id
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	public abstract HashMap<Integer, RunSliceHeader> getRunSliceHeaderById(int msLevel) throws SQLiteException;

	/**
	 * Gets the run slice data.
	 *
	 * @param runSliceId
	 *            the run slice id
	 * @return the run slice data
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected RunSliceData getRunSliceData(int runSliceId, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {

		// Retrieve the corresponding run slices
		// TODO: DBO => why a JOIN here ???
		// String queryStr = "SELECT bounding_box.* FROM bounding_box, bounding_box_rtree"
		// + " WHERE bounding_box.id = bounding_box_rtree.id AND bounding_box.run_slice_id = ?"
		// + " ORDER BY first_spectrum_id"; // number
		String queryStr = "SELECT * FROM bounding_box" + " WHERE run_slice_id = ?" + " ORDER BY first_spectrum_id"; // number

		// SQLiteStatement stmt =
		// connection.prepare("SELECT * FROM run_slice WHERE ms_level="+msLevel+" ORDER BY begin_mz ",
		// false);//number ASC", false);
		SQLiteRecordIterator records = new SQLiteQuery(connection, queryStr).bind(1, runSliceId).getRecordIterator();

		List<BoundingBox> bbs = new ArrayList<BoundingBox>();
		// FIXME: getSpectrumHeaderById
		Map<Long, SpectrumHeader> spectrumHeaderById = this.getMs1SpectrumHeaderById();
		Map<Long, DataEncoding> dataEncodingBySpectrumId = this.getDataEncodingBySpectrumId();

		while (records.hasNext()) {
			SQLiteRecord record = records.next();

			int bbId = record.columnInt(BoundingBoxTable.ID);
			byte[] data = record.columnBlob(BoundingBoxTable.DATA);
			int firstSpectrumId = record.columnInt(BoundingBoxTable.FIRST_SPECTRUM_ID);
			int lastSpectrumId = record.columnInt(BoundingBoxTable.LAST_SPECTRUM_ID);
			// float minTime = (float) stmt.columnDouble(3);

			BoundingBox bb = BoundingBoxBuilder.buildBB(bbId, data, firstSpectrumId, lastSpectrumId, spectrumHeaderById, dataEncodingBySpectrumId);
			bb.setRunSliceId(runSliceId);

			bbs.add(bb);
		}

		// TODO: check if faster than order by
		// Collections.sort(bbs); //sort bbs by their rt_min

		List<SpectrumSlice> spectrumSliceList = new ArrayList<SpectrumSlice>();
		for (BoundingBox bb : bbs) {
			SpectrumSlice[] sl = bb.toSpectrumSlices();
			for (SpectrumSlice ss : sl) {
				spectrumSliceList.add(ss);
			}
		}

		// rsd.buildPeakListBySpectrumId();
		return new RunSliceData(runSliceId, spectrumSliceList.toArray(new SpectrumSlice[spectrumSliceList.size()]));
	}

	/**
	 * Gets the bounding box data.
	 *
	 * @param bbId
	 *            the bb id
	 * @return the bounding box data
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected byte[] getBoundingBoxData(int bbId, SQLiteConnection connection) throws SQLiteException {
		String sqlString = "SELECT data FROM bounding_box WHERE bounding_box.id = ?";
		return new SQLiteQuery(connection, sqlString).bind(1, bbId).extractSingleBlob();
	}

	/**
	 * Gets the bounding box first spectrum index.
	 *
	 * @param spectrumId
	 *            the spectrum id
	 * @return the bounding box first spectrum index
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected long getBoundingBoxFirstSpectrumId(long spectrumId, SQLiteConnection connection) throws SQLiteException {
		String sqlString = "SELECT bb_first_spectrum_id FROM spectrum WHERE id = ?";
		return new SQLiteQuery(connection, sqlString).bind(1, spectrumId).extractSingleLong();
	}

	/**
	 * Gets the bounding box min mz.
	 *
	 * @param bbId
	 *            the bb id
	 * @return the bounding box min mz
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected float getBoundingBoxMinMz(int bbId, SQLiteConnection connection) throws SQLiteException {
		String sqlString = "SELECT min_mz FROM bounding_box_rtree WHERE bounding_box_rtree.id = ?";
		return (float) new SQLiteQuery(connection, sqlString).bind(1, bbId).extractSingleDouble();
	}

	/**
	 * Gets the bounding box min time.
	 *
	 * @param bbId
	 *            the bb id
	 * @return the bounding box min time
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected float getBoundingBoxMinTime(int bbId, SQLiteConnection connection) throws SQLiteException {
		String sqlString = "SELECT min_time FROM bounding_box_rtree WHERE bounding_box_rtree.id = ?";
		return (float) new SQLiteQuery(connection, sqlString).bind(1, bbId).extractSingleDouble();
	}

	/**
	 * Gets the bounding box ms level.
	 *
	 * @param bbId
	 *            the bb id
	 * @return the bounding box ms level
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected int getBoundingBoxMsLevel(int bbId, SQLiteConnection connection) throws SQLiteException {

		// FIXME: check that the mzDB file has the bounding_box_msn_rtree table
		String sqlString1 = "SELECT run_slice_id FROM bounding_box WHERE id = ?";
		int runSliceId = new SQLiteQuery(connection, sqlString1).bind(1, bbId).extractSingleInt();

		String sqlString2 = "SELECT ms_level FROM run_slice WHERE run_slice.id = ?";
		return new SQLiteQuery(connection, sqlString2).bind(1, runSliceId).extractSingleInt();

		/*
		 * String sqlString =
		 * "SELECT min_ms_level FROM bounding_box_msn_rtree WHERE bounding_box_msn_rtree.id = ?"; return new
		 * SQLiteQuery(connection, sqlString).bind(1, bbId).extractSingleInt();
		 */
	}

	/**
	 * Gets the MS1 spectrum headers.
	 *
	 * @return the spectrum headers
	 * @throws SQLiteException
	 *             the SQLiteException
	 */
	public abstract SpectrumHeader[] getMs1SpectrumHeaders() throws SQLiteException;

	/**
	 * Gets the MS1 spectrum header by id.
	 *
	 * @return the spectrum header by id
	 * @throws SQLiteException
	 *             the SQLiteException
	 */
	public abstract Map<Long, SpectrumHeader> getMs1SpectrumHeaderById() throws SQLiteException;

	/**
	 * Gets the MS2 spectrum headers.
	 *
	 * @return the spectrum headers
	 * @throws SQLiteException
	 *             the SQLiteException
	 */
	public abstract SpectrumHeader[] getMs2SpectrumHeaders() throws SQLiteException;

	/**
	 * Gets the MS2 spectrum header by id.
	 *
	 * @return the spectrum header by id
	 * @throws SQLiteException
	 *             the SQLiteException
	 */
	public abstract Map<Long, SpectrumHeader> getMs2SpectrumHeaderById() throws SQLiteException;

	/**
	 * Gets all spectrum headers.
	 *
	 * @return the spectrum headers
	 * @throws SQLiteException
	 *             the SQLiteException
	 */
	public abstract SpectrumHeader[] getSpectrumHeaders() throws SQLiteException;

	/**
	 * Gets each spectrum header mapped by its id.
	 *
	 * @return the spectrum header by id
	 * @throws SQLiteException
	 *             the SQLiteException
	 */
	public abstract Map<Long, SpectrumHeader> getSpectrumHeaderById() throws SQLiteException;

	/**
	 * Gets the spectrum header.
	 *
	 * @param id
	 *            the id
	 * @return the spectrum header
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	public abstract SpectrumHeader getSpectrumHeader(long id) throws SQLiteException;

	/**
	 * Gets the spectrum header for time.
	 *
	 * @param time
	 *            the time
	 * @param msLevel
	 *            the ms level
	 * @return spectrumheader the closest to the time input parameter
	 * @throws Exception
	 */
	public abstract SpectrumHeader getSpectrumHeaderForTime(float time, int msLevel) throws Exception;

	/**
	 * Gets the spectrum data.
	 *
	 * @param spectrumId
	 *            the spectrum id
	 * @return the spectrum data
	 * @throws SQLiteException
	 *             the sQ lite exception
	 * @throws StreamCorruptedException
	 */
	protected SpectrumData getSpectrumData(long spectrumId, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {

		Map<Long, SpectrumHeader> spectrumHeaderById = this.getSpectrumHeaderById();
		Map<Long, DataEncoding> dataEncodingBySpectrumId = this.getDataEncodingBySpectrumId();

		long firstSpectrumId = spectrumHeaderById.get(spectrumId).getBBFirstSpectrumId();

		String sqlString = "SELECT * FROM bounding_box WHERE bounding_box.first_spectrum_id = ?";
		SQLiteRecordIterator records = new SQLiteQuery(connection, sqlString).bind(1, firstSpectrumId).getRecordIterator();

		List<BoundingBox> bbS = new ArrayList<BoundingBox>();

		while (records.hasNext()) {
			SQLiteRecord r = records.next();

			int lastSpectrumId = r.columnInt(BoundingBoxTable.LAST_SPECTRUM_ID);

			BoundingBox bb = BoundingBoxBuilder.buildBB(r.columnInt(BoundingBoxTable.ID), r.columnBlob(BoundingBoxTable.DATA), firstSpectrumId, lastSpectrumId,
				spectrumHeaderById, dataEncodingBySpectrumId);
			bb.setRunSliceId(r.columnInt(BoundingBoxTable.RUN_SLICE_ID));

			bbS.add(bb);
		}

		// Construct empty spectrum data
		SpectrumData sd = new SpectrumData(new double[0], new float[0]);

		// int firstSpectrumCycle = getSpectrumHeader(firstSpectrumId).cycle;
		// int cycle = getSpectrumHeader(spectrumId).cycle;
		// int cycleOffset = cycle - firstSpectrumCycle;

		for (BoundingBox bb : bbS) {

			IBlobReader bbReader = bb.getReader();

			/*
			 * System.out.println("searching for " +spectrumId); SpectrumSlice[] ssList =
			 * bbReader.readAllSpectrumSlices(bb.getRunSliceId()); System.out.println("ssList.length: "
			 * +ssList.length); for( SpectrumSlice ss: ssList ) { System.out.println("has spectrum id="
			 * +ss.getSpectrumId()); }
			 */

			// Retrieve only slices corresponding to the provided spectrum id
			int nbSpectra = bb.getSpectraCount();
			for (int spectrumIdx = 0; spectrumIdx < nbSpectra; spectrumIdx++) {
				if (spectrumId == bbReader.getSpectrumIdAt(spectrumIdx)) {
					sd.addSpectrumData(bbReader.readSpectrumSliceDataAt(spectrumIdx));
					break;
				}
			}
		}

		return sd;
	}

	/**
	 * Gets the spectrum.
	 *
	 * @param spectrumId
	 *            the spectrum id
	 * @return the spectrum
	 * @throws SQLiteException
	 *             the SQlite exception
	 * @throws StreamCorruptedException
	 */
	protected Spectrum getSpectrum(long spectrumId, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {
		SpectrumHeader sh = this.getSpectrumHeader(spectrumId);
		SpectrumData sd = this.getSpectrumData(spectrumId, connection);
		return new Spectrum(sh, sd);
	}

	/**
	 * Gets the spectrum peaks.
	 *
	 * @param spectrumId
	 *            the spectrum id
	 * @return the spectrum peaks
	 * @throws SQLiteException
	 *             the sQ lite exception
	 * @throws StreamCorruptedException
	 */
	protected Peak[] getSpectrumPeaks(int spectrumId, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {
		return this.getSpectrum(spectrumId, connection).toPeaks();
	}

	/**
	 * Gets the spectrum slices. Each returned spectrum slice corresponds to a single spectrum.
	 *
	 * @param minmz
	 *            the minMz
	 * @param maxmz
	 *            the maxMz
	 * @param minrt
	 *            the minRt
	 * @param maxrt
	 *            the maxRt
	 * @param msLevel
	 *            the ms level
	 * @return the spectrum slices
	 * @throws SQLiteException
	 *             the sQ lite exception
	 * @throws StreamCorruptedException
	 */
	protected SpectrumSlice[] getMsSpectrumSlices(double minMz, double maxMz, float minRt, float maxRt, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {
		return this._getSpectrumSlicesInRanges(minMz, maxMz, minRt, maxRt, 1, 0.0, connection);
	}

	// TODO: think about msLevel > 2
	protected SpectrumSlice[] getMsnSpectrumSlices(double parentMz, double minFragMz, double maxFragMz, float minRt, float maxRt, SQLiteConnection connection) throws SQLiteException,
			StreamCorruptedException {
		return this._getSpectrumSlicesInRanges(minFragMz, maxFragMz, minRt, maxRt, 2, parentMz, connection);
	}

	private SpectrumSlice[] _getSpectrumSlicesInRanges(double minMz, double maxMz, float minRt, float maxRt, int msLevel, double parentMz, SQLiteConnection connection)
			throws SQLiteException, StreamCorruptedException {

		BBSizes sizes = this.getBBSizes();
		float rtWidth = ((msLevel == 1) ? sizes.BB_RT_WIDTH_MS1 : sizes.BB_RT_WIDTH_MSn);
		double mzHeight = ((msLevel == 1) ? sizes.BB_MZ_HEIGHT_MS1 : sizes.BB_MZ_HEIGHT_MSn);

		double _minMz = minMz - mzHeight;
		double _maxMz = maxMz + mzHeight;
		float _minRt = minRt - rtWidth;
		float _maxRt = maxRt + rtWidth;

		// TODO: query using bounding_box_msn_rtree to use the min_ms_level information even for MS1 data ???
		SQLiteQuery sqliteQuery;
		if (msLevel == 1) {
			String sqlQuery = "SELECT * FROM bounding_box WHERE id IN "
					+ "(SELECT id FROM bounding_box_rtree WHERE min_mz >= ? AND max_mz <= ? AND min_time >= ? AND max_time <= ? )"
					+ " ORDER BY first_spectrum_id";

			sqliteQuery = new SQLiteQuery(connection, sqlQuery, false).bind(1, _minMz).bind(2, _maxMz).bind(3, _minRt).bind(4, _maxRt);

		} else {
			String sqlQuery = "SELECT * FROM bounding_box WHERE id IN " + "(SELECT id FROM bounding_box_msn_rtree" + " WHERE min_ms_level = " + msLevel
					+ " AND max_ms_level = " + msLevel + " AND min_parent_mz <= ? AND max_parent_mz >= ? "
					+ " AND min_mz >= ? AND max_mz <= ? AND min_time >= ? AND max_time <= ? )" + " ORDER BY first_spectrum_id";

			sqliteQuery = new SQLiteQuery(connection, sqlQuery, false).bind(1, parentMz).bind(2, parentMz).bind(3, _minMz).bind(4, _maxMz).bind(5, _minRt)
					.bind(6, _maxRt);
		}

		SQLiteRecordIterator recordIter = sqliteQuery.getRecordIterator();

		Map<Long, SpectrumHeader> spectrumHeaderById = null;
		if (msLevel == 1) {
			spectrumHeaderById = this.getMs1SpectrumHeaderById();
		} else if (msLevel == 2) {
			spectrumHeaderById = this.getMs2SpectrumHeaderById();
		} else {
			throw new IllegalArgumentException("unsupported MS level: " + msLevel);
		}

		Map<Long, DataEncoding> dataEncodingBySpectrumId = this.getDataEncodingBySpectrumId();
		TreeMap<Long, ArrayList<SpectrumData>> spectrumDataListById = new TreeMap<Long, ArrayList<SpectrumData>>();
		HashMap<Long, Integer> peaksCountBySpectrumId = new HashMap<Long, Integer>();

		// Iterate over bounding boxes
		while (recordIter.hasNext()) {

			SQLiteRecord record = recordIter.next();

			int bbId = record.columnInt(BoundingBoxTable.ID);

			// TODO: remove me when the query is performed using msn_rtree
			// if (getBoundingBoxMsLevel(bbId) != msLevel)
			// continue;

			// Retrieve bounding box data
			byte[] data = record.columnBlob(BoundingBoxTable.DATA);
			long firstSpectrumId = record.columnLong(BoundingBoxTable.FIRST_SPECTRUM_ID);
			long lastSpectrumId = record.columnLong(BoundingBoxTable.LAST_SPECTRUM_ID);

			// Build the Bounding Box
			BoundingBox bb = BoundingBoxBuilder.buildBB(bbId, data, firstSpectrumId, lastSpectrumId, spectrumHeaderById, dataEncodingBySpectrumId);
			// bb.setRunSliceId(record.columnInt(BoundingBoxTable.RUN_SLICE_ID));

			IBlobReader bbReader = bb.getReader();
			int bbSpectraCount = bbReader.getSpectraCount();
			long[] bbSpectrumIds = bbReader.getAllSpectrumIds();

			// Iterate over each spectrum
			for (int spectrumIdx = 0; spectrumIdx < bbSpectraCount; spectrumIdx++) {

				long spectrumId = bbSpectrumIds[spectrumIdx];
				SpectrumHeader sh = spectrumHeaderById.get(spectrumId);
				float currentRt = sh.getElutionTime();

				// Filtering on time dimension
				if ((currentRt >= minRt) && (currentRt <= maxRt)) {
					// Filtering on m/z dimension
					SpectrumData spectrumSliceData = bbReader.readFilteredSpectrumSliceDataAt(spectrumIdx, minMz, maxMz);
					if (spectrumSliceData.isEmpty() == false) {
						if (spectrumDataListById.containsKey(spectrumId) == false) {
							spectrumDataListById.put(spectrumId, new ArrayList<SpectrumData>());
							peaksCountBySpectrumId.put(spectrumId, 0);
						}
						spectrumDataListById.get(spectrumId).add(spectrumSliceData);
						peaksCountBySpectrumId.put(spectrumId, peaksCountBySpectrumId.get(spectrumId) + spectrumSliceData.getPeaksCount());
					}
				}
			}
		}

		SpectrumSlice[] finalSpectrumSlices = new SpectrumSlice[spectrumDataListById.size()];

		int spectrumIdx = 0;
		for (Map.Entry<Long, ArrayList<SpectrumData>> entry : spectrumDataListById.entrySet()) {
			Long spectrumId = entry.getKey();
			ArrayList<SpectrumData> spectrumDataList = entry.getValue();
			int peaksCount = peaksCountBySpectrumId.get(spectrumId);
			
			SpectrumData finalSpectrumData = _mergeSpectrumDataList(spectrumDataList, peaksCount);

			finalSpectrumSlices[spectrumIdx] = new SpectrumSlice(spectrumHeaderById.get(spectrumId), finalSpectrumData);

			spectrumIdx++;
		}

		return finalSpectrumSlices;
	}
	
	private SpectrumData _mergeSpectrumDataList( ArrayList<SpectrumData> spectrumDataList, int peaksCount ) {
		
		double[] finalMzList = new double[peaksCount];
		float[] finalIntensityList = new float[peaksCount];
		float[] finalLeftHwhmList = null;
		float[] finalRightHwhmList = null;

		SpectrumData firstSpectrumData = spectrumDataList.get(0);
		if ((firstSpectrumData.getLeftHwhmList() != null) && (firstSpectrumData.getRightHwhmList() != null)) {
			finalLeftHwhmList = new float[peaksCount];
			finalRightHwhmList = new float[peaksCount];
		}

		// TODO: check that spectrumDataList is m/z sorted ???
		int finalPeakIdx = 0;
		for (SpectrumData spectrumData : spectrumDataList) {
			double[] mzList = spectrumData.getMzList();
			float[] intensityList = spectrumData.getIntensityList();
			float[] leftHwhmList = spectrumData.getLeftHwhmList();
			float[] rightHwhmList = spectrumData.getRightHwhmList();

			// Add peaks of this SpectrumData to the final arrays
			int spectrumDataPeaksCount = spectrumData.getPeaksCount();
			for (int i = 0; i < spectrumDataPeaksCount; i++) {
				finalMzList[finalPeakIdx] = mzList[i];
				finalIntensityList[finalPeakIdx] = intensityList[i];

				if ((finalLeftHwhmList != null) && (finalRightHwhmList != null)) {
					finalLeftHwhmList[finalPeakIdx] = leftHwhmList[i];
					finalRightHwhmList[finalPeakIdx] = rightHwhmList[i];
				}

				finalPeakIdx++;
			}
		}

		return new SpectrumData(finalMzList, finalIntensityList, finalLeftHwhmList, finalRightHwhmList);
	}

	/**
	 * Lazy loading of the acquisition mode, parameter
	 *
	 * @return
	 * @throws SQLiteException
	 */
	protected AcquisitionMode getAcquisitionMode(SQLiteConnection connection) throws SQLiteException {

		if (this.acquisitionMode == null) {
			/*
			 * final String sqlString = "SELECT param_tree FROM run"; final String runParamTree = new
			 * SQLiteQuery(connection, sqlString).extractSingleString(); final ParamTree runTree =
			 * ParamTreeParser.parseParamTree(runParamTree);
			 */

			final ParamTree runTree = this.getRuns().get(0).getParamTree(connection);

			try {
				final CVParam cvParam = runTree.getCVParam(CVEntry.ACQUISITION_PARAMETER);
				final String value = cvParam.getValue();
				this.acquisitionMode = AcquisitionMode.valueOf(value);
			} catch (Exception e) {
				this.acquisitionMode = AcquisitionMode.UNKNOWN;
			}
		}

		return this.acquisitionMode;
	}
	
	/**
	 * Get the DIA IsolationWindows
	 * 
	 * @return
	 * @throws SQLiteException
	 */
	protected IsolationWindow[] getDIAIsolationWindows(SQLiteConnection connection) throws SQLiteException {

		// If DIA acquisition, the list will be computed on first use (lazy loading) 
		// Will be always null on non DIA acquisition
		if ((this.diaIsolationWindows == null)) {
			// FIXME: in version 0.9.8 bounding_box_msn_rtree table should be empty.
			// next lines is a workaround
			// if (this.getAcquisitionMode() != AcquisitionMode.SWATH) {
			// return new IsolationWindow[] {};
			// }
			final String sqlQuery = "SELECT DISTINCT min_parent_mz, " + "max_parent_mz FROM bounding_box_msn_rtree ORDER BY min_parent_mz";
			final SQLiteRecordIterator recordIt = new SQLiteQuery(connection, sqlQuery).getRecordIterator();

			ArrayList<IsolationWindow> isolationWindowList = new ArrayList<IsolationWindow>();
			while (recordIt.hasNext()) {
				final SQLiteRecord record = recordIt.next();
				final Double minMz = record.columnDouble("min_parent_mz");
				final Double maxMz = record.columnDouble("max_parent_mz");
				isolationWindowList.add(new IsolationWindow(minMz, maxMz));
			}

			this.diaIsolationWindows = isolationWindowList.toArray(new IsolationWindow[isolationWindowList.size()]);
		}

		return this.diaIsolationWindows;
	}
	
	public abstract List<InstrumentConfiguration> getInstrumentConfigurations() throws SQLiteException;

	public abstract List<Run> getRuns() throws SQLiteException;

	public abstract List<Sample> getSamples() throws SQLiteException;

	public abstract List<Software> getSoftwareList() throws SQLiteException;

	public abstract List<SourceFile> getSourceFiles() throws SQLiteException;

	public String getFirstSourceFileName() throws SQLiteException {
		return this.getSourceFiles().get(0).getName();
		// String sqlString = "SELECT name FROM source_file LIMIT 1";
		// return new SQLiteQuery(connection, sqlString).extractSingleString();
	}

	/**
	 * Gets the xic.
	 *
	 * @param minMz
	 *            the min mz
	 * @param maxMz
	 *            the max mz
	 * @param msLevel
	 *            the ms level
	 * @return the xic
	 * @throws SQLiteException
	 *             the sQ lite exception
	 */
	protected Peak[] getMsXicInMzRange(double minMz, double maxMz, XicMethod method, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {
		return this.getMsXic(minMz, maxMz, -1, -1, method, connection);
	}

	protected Peak[] getMsXicInMzRtRanges(double minMz, double maxMz, float minRt, float maxRt, XicMethod method, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {

		final double mzCenter = (minMz + maxMz) / 2;
		final double mzTolInDa = maxMz - mzCenter;

		return this.getMsXic(mzCenter, mzTolInDa, minRt, maxRt, method, connection);
	}

	protected Peak[] getMsXic(double mz, double mzTolInDa, float minRt, float maxRt, XicMethod method, SQLiteConnection connection) throws SQLiteException,
			StreamCorruptedException {

		final double minMz = mz - mzTolInDa;
		final double maxMz = mz + mzTolInDa;
		final float minRtForRtree = minRt >= 0 ? minRt : 0;
		final float maxRtForRtree = maxRt > 0 ? maxRt : this.getLastTime(connection);

		SpectrumSlice[] spectrumSlices = this.getMsSpectrumSlices(minMz, maxMz, minRtForRtree, maxRtForRtree, connection);

		final double mzTolPPM = MsUtils.DaToPPM(mz, mzTolInDa);
		return this._spectrumSlicesToXIC(spectrumSlices, mz, mzTolPPM, method);
	}

	protected Peak[] getMsnXic(double parentMz, double fragmentMz, double fragmentMzTolInDa, float minRt, float maxRt, XicMethod method, SQLiteConnection connection) throws SQLiteException,
			StreamCorruptedException {

		final double minFragMz = fragmentMz - fragmentMzTolInDa;
		final double maxFragMz = fragmentMz + fragmentMzTolInDa;
		final float minRtForRtree = minRt >= 0 ? minRt : 0;
		final float maxRtForRtree = maxRt > 0 ? maxRt : this.getLastTime(connection);

		SpectrumSlice[] spectrumSlices = this.getMsnSpectrumSlices(parentMz, minFragMz, maxFragMz, minRtForRtree, maxRtForRtree, connection);

		final double fragMzTolPPM = MsUtils.DaToPPM(fragmentMz, fragmentMzTolInDa);
		return this._spectrumSlicesToXIC(spectrumSlices, fragmentMz, fragMzTolPPM, method);
	}

	private Peak[] _spectrumSlicesToXIC(SpectrumSlice[] spectrumSlices, double searchedMz, double mzTolPPM, XicMethod method) throws SQLiteException,
			StreamCorruptedException {

		if (spectrumSlices == null) {
			this.logger.warn("null detected");// throw new
		}

		if (spectrumSlices.length == 0) {
			// logger.warn("Empty spectrumSlices, too narrow request ?");
			return new Peak[0];
		}

		int spectrumSlicesCount = spectrumSlices.length;
		List<Peak> xicPeaks = new ArrayList<Peak>(spectrumSlicesCount);

		switch (method) {
		case MAX: {

			for (int i = 0; i < spectrumSlicesCount; i++) {

				SpectrumSlice sl = spectrumSlices[i];

				Peak[] peaks = sl.toPeaks();
				int peaksCount = peaks.length;

				if (peaksCount == 0) {
					continue;
				}

				Arrays.sort(peaks, Peak.getIntensityComp());

				xicPeaks.add(peaks[peaksCount - 1]);
			}

			return xicPeaks.toArray(new Peak[xicPeaks.size()]);
		}
		case NEAREST: {

			for (int i = 0; i < spectrumSlicesCount; i++) {
				SpectrumSlice sl = spectrumSlices[i];
				SpectrumData slData = sl.getData();

				if (slData.isEmpty()) {
					continue;
				}

				Peak nearestPeak = sl.getNearestPeak(searchedMz, mzTolPPM);

				if (nearestPeak == null) {
					this.logger.error("nearest peak is null but should not be: searchedMz=" + searchedMz + " minMz=" + slData.getMzList()[0] + " tol="
							+ mzTolPPM);
					continue;
				}

				xicPeaks.add(nearestPeak);
			}

			return xicPeaks.toArray(new Peak[xicPeaks.size()]);
		}
		case SUM: {
			for (int i = 0; i < spectrumSlicesCount; i++) {

				SpectrumSlice sl = spectrumSlices[i];

				Peak[] peaks = sl.toPeaks();
				int peaksCount = peaks.length;

				if (peaksCount == 0) {
					continue;
				}

				Arrays.sort(peaks, Peak.getIntensityComp());

				float sum = 0.0f;
				for (Peak p : peaks) {
					sum += p.getIntensity();
				}

				Peak refPeak = peaks[(int) Math.floor(0.5 * peaksCount)];

				xicPeaks.add(new Peak(refPeak.getMz(), sum, refPeak.getLeftHwhm(), refPeak.getRightHwhm(), refPeak.getLcContext()));
			}

			return xicPeaks.toArray(new Peak[xicPeaks.size()]);
		}
		default: {
			this.logger.error("[_spectrumSlicesToXIC]: method must be one of 'MAX', 'NEAREST' or 'SUM', returning null");
			return null;
		}
		}

	}

	/**
	 * Gets the peaks.
	 *
	 * @param minmz
	 *            the minmz
	 * @param maxmz
	 *            the maxmz
	 * @param minrt
	 *            the minrt
	 * @param maxrt
	 *            the maxrt
	 * @param msLevel
	 *            the ms level
	 * @return the peaks
	 * @throws SQLiteException
	 *             the sQ lite exception
	 * @throws StreamCorruptedException
	 */
	protected Peak[] getMsPeaksInMzRtRanges(double minMz, double maxMz, float minRt, float maxRt, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {
		SpectrumSlice[] spectrumSlices = this.getMsSpectrumSlices(minMz, maxMz, minRt, maxRt, connection);
		return this._spectrumSlicesToPeaks(spectrumSlices);
	}
	
	protected Peak[] getMsnPeaksInMzRtRanges(double parentMz, double minFragMz, double maxFragMz, float minRt, float maxRt, SQLiteConnection connection) throws SQLiteException, StreamCorruptedException {
		SpectrumSlice[] spectrumSlices = this.getMsnSpectrumSlices(parentMz, minFragMz, maxFragMz, minRt, maxRt, connection);
		return this._spectrumSlicesToPeaks(spectrumSlices);
	}
	
	// Merge spectrum slices then return a peak array using simply the toPeaks function
    private Peak[] _spectrumSlicesToPeaks(SpectrumSlice[] spectrumSlices) {

		this.logger.debug("SpectrumSlice length : {}", spectrumSlices.length);

		if (spectrumSlices.length == 0) {
			return new Peak[0];
		}
		
		int mergedPeaksCount = 0;
		for( SpectrumSlice spectrumSlice : spectrumSlices ) {
			SpectrumData sd = spectrumSlice.getData();
			mergedPeaksCount += sd.getPeaksCount();
		}
		
		Peak[] peaks = new Peak[mergedPeaksCount];
		
		int peakIdx = 0;
		for( SpectrumSlice spectrumSlice : spectrumSlices ) {
			for( Peak peak : spectrumSlice.toPeaks() ) {
				peaks[peakIdx] = peak;
				peakIdx++;
			}
		}
		
		return peaks;
	}

}