package fr.profi.mzdb.io.reader.cache;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;

import fr.profi.mzdb.AbstractMzDbReader;
import fr.profi.mzdb.io.reader.table.ParamTreeParser;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.PeakEncoding;
import fr.profi.mzdb.model.SpectrumHeader;
import fr.profi.mzdb.utils.sqlite.ISQLiteRecordExtraction;
import fr.profi.mzdb.utils.sqlite.SQLiteQuery;
import fr.profi.mzdb.utils.sqlite.SQLiteRecord;

/**
 * @author David Bouyssie
 * 
 */
public abstract class AbstractSpectrumHeaderReader extends MzDbEntityCacheContainer {
	
	/** The time index width. */
	protected final static int TIME_INDEX_WIDTH = 15;
	
	private int ms1SpectraCount;
	private int ms2SpectraCount;
	private int spectraCount;

	/**
	 * @param mzDbReader
	 */
	public AbstractSpectrumHeaderReader(AbstractMzDbReader mzDbReader, int ms1SpectraCount, int ms2SpectraCount) {
		super(mzDbReader);
		
		this.ms1SpectraCount = ms1SpectraCount;
		this.ms2SpectraCount = ms2SpectraCount;		
		this.spectraCount = ms1SpectraCount + ms2SpectraCount;
	}

	// Define some variable for spectrum header extraction
	private static String _spectrumHeaderQueryStr = 
		"SELECT id, initial_id, cycle, time, ms_level, tic, "+
		"base_peak_mz, base_peak_intensity, main_precursor_mz, main_precursor_charge, " +
		"data_points_count, param_tree, scan_list, precursor_list, data_encoding_id, bb_first_spectrum_id FROM spectrum";
	
	private static String _ms1SpectrumHeaderQueryStr = _spectrumHeaderQueryStr + " WHERE ms_level = 1";	
	private static String _ms2SpectrumHeaderQueryStr = _spectrumHeaderQueryStr + " WHERE ms_level = 2";
	
	private enum SpectrumHeaderCol {

		ID("id"),
		INITIAL_ID("initial_id"),
		CYCLE("cycle"),
		TIME("time"),
		MS_LEVEL("ms_level"),
		TIC("tic"),
		BASE_PEAK_MZ("base_peak_mz"),
		BASE_PEAK_INTENSITY("base_peak_intensity"),
		MAIN_PRECURSOR_MZ("main_precursor_mz"),
		MAIN_PRECURSOR_CHARGE("main_precursor_charge"),
		DATA_POINTS_COUNT("data_points_count"),
		PARAM_TREE("param_tree"),
		SCAN_LIST("spectrum_list"),
		PRECURSOR_LIST("precursor_list"),
		DATA_ENCODING_ID("data_encoding_id"),
		BB_FIRST_SPECTRUM_ID("bb_first_spectrum_id");

		@SuppressWarnings("unused")
		protected final String columnName;

		private SpectrumHeaderCol(String colName) {
			this.columnName = colName;
		}

	}
	
	private static class SpectrumHeaderColIdx {	
		static int id = SpectrumHeaderCol.ID.ordinal();
		static int initialId= SpectrumHeaderCol.INITIAL_ID.ordinal();
		static int cycleCol= SpectrumHeaderCol.CYCLE.ordinal();
		static int time = SpectrumHeaderCol.TIME.ordinal();
		static int msLevel = SpectrumHeaderCol.MS_LEVEL.ordinal();
		static int tic = SpectrumHeaderCol.TIC.ordinal();
		static int basePeakMz = SpectrumHeaderCol.BASE_PEAK_MZ.ordinal();
		static int basePeakIntensity = SpectrumHeaderCol.BASE_PEAK_INTENSITY.ordinal();
		static int mainPrecursorMz = SpectrumHeaderCol.MAIN_PRECURSOR_MZ.ordinal();
		static int mainPrecursorCharge = SpectrumHeaderCol.MAIN_PRECURSOR_CHARGE.ordinal();
		static int dataPointsCount = SpectrumHeaderCol.DATA_POINTS_COUNT.ordinal();
		static int paramTree = SpectrumHeaderCol.PARAM_TREE.ordinal();
		static int scanList = SpectrumHeaderCol.SCAN_LIST.ordinal();
		static int precursorList = SpectrumHeaderCol.PRECURSOR_LIST.ordinal();
		static int dataEncodingId = SpectrumHeaderCol.DATA_ENCODING_ID.ordinal();
		static int bbFirstSpectrumId = SpectrumHeaderCol.BB_FIRST_SPECTRUM_ID.ordinal();
	}

	private ISQLiteRecordExtraction<SpectrumHeader> _spectrumHeaderExtractor = new ISQLiteRecordExtraction<SpectrumHeader>() {

		public SpectrumHeader extract(SQLiteRecord record) throws SQLiteException {

			SQLiteStatement stmt = record.getStatement();

			// long nano = System.nanoTime();
			int msLevel = stmt.columnInt(SpectrumHeaderColIdx.msLevel);

			double precursorMz = 0.0;
			int precursorCharge = 0;
			if (msLevel == 2) {
				precursorMz = stmt.columnDouble(SpectrumHeaderColIdx.mainPrecursorMz);
				precursorCharge = stmt.columnInt(SpectrumHeaderColIdx.mainPrecursorCharge);
			}

			int bbFirstSpectrumId = stmt.columnInt(SpectrumHeaderColIdx.bbFirstSpectrumId);

			DataEncoding dataEnc = mzDbReader.getDataEncoding(stmt.columnInt(SpectrumHeaderColIdx.dataEncodingId));

			boolean isHighRes = dataEnc.getPeakEncoding() == PeakEncoding.LOW_RES_PEAK ? false : true;

			SpectrumHeader sh = new SpectrumHeader(
				stmt.columnLong(SpectrumHeaderColIdx.id),
				stmt.columnInt(SpectrumHeaderColIdx.initialId),
				stmt.columnInt(SpectrumHeaderColIdx.cycleCol),
				(float) stmt.columnDouble(SpectrumHeaderColIdx.time),
				msLevel,
				stmt.columnInt(SpectrumHeaderColIdx.dataPointsCount),
				isHighRes,
				(float) stmt.columnDouble(SpectrumHeaderColIdx.tic),
				stmt.columnDouble(SpectrumHeaderColIdx.basePeakMz),
				(float) stmt.columnDouble(SpectrumHeaderColIdx.basePeakIntensity),
				precursorMz,
				precursorCharge,
				bbFirstSpectrumId
			);
			
			if (SpectrumHeaderReader.loadParamTree) {
				sh.setParamTree( ParamTreeParser.parseParamTree(stmt.columnString(SpectrumHeaderColIdx.paramTree)) );
			}
			if (SpectrumHeaderReader.loadScanList) {
				sh.setScanList(ParamTreeParser.parseScanList(stmt.columnString(SpectrumHeaderColIdx.scanList)));
			}
			if (SpectrumHeaderReader.loadPrecursorList && msLevel > 1) {
				sh.setPrecursor(ParamTreeParser.parsePrecursor(stmt.columnString(SpectrumHeaderColIdx.precursorList)));
			}

			// System.out.println( (double) (System.nanoTime() - nano) / 1e3 );

			// sh.setParamTree(paramTree);

			return sh;
		}

	};
	

	/**
	 * Gets the spectrum headers.
	 *
	 * @param connection
	 *            the connection
	 * @return the spectrum headers
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected SpectrumHeader[] getSpectrumHeaders(SQLiteConnection connection) throws SQLiteException {
		if (this.entityCache != null && this.entityCache.spectrumHeaders != null) {
			return this.entityCache.spectrumHeaders;
		} else {
			SpectrumHeader[] ms1SpectrumHeaders = this.getMs1SpectrumHeaders(connection);
			SpectrumHeader[] ms2SpectrumHeaders = this.getMs2SpectrumHeaders(connection);

			SpectrumHeader[] spectrumHeaders = new SpectrumHeader[ms1SpectrumHeaders.length + ms2SpectrumHeaders.length];

			System.arraycopy(ms1SpectrumHeaders, 0, spectrumHeaders, 0, ms1SpectrumHeaders.length);
			System.arraycopy(ms2SpectrumHeaders, 0, spectrumHeaders, ms1SpectrumHeaders.length, ms2SpectrumHeaders.length);

			if (this.entityCache != null)
				this.entityCache.spectrumHeaders = spectrumHeaders;

			return spectrumHeaders;
		}
	}
	
	/**
	 * Gets the spectrum headers by id.
	 *
	 * @param connection
	 *            the connection
	 * @return the spectrum header by id
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected Map<Long, SpectrumHeader> getSpectrumHeaderById(SQLiteConnection connection) throws SQLiteException {

		if (this.entityCache != null && this.entityCache.spectrumHeaderById != null) {
			return this.entityCache.spectrumHeaderById;
		} else {

			SpectrumHeader[] spectrumHeaders = getSpectrumHeaders(connection);

			int spectraCount = spectrumHeaders.length;
			Map<Long, SpectrumHeader> spectrumHeaderById = new HashMap<Long, SpectrumHeader>(spectraCount);

			for (SpectrumHeader spectrumHeader : spectrumHeaders)
				spectrumHeaderById.put(spectrumHeader.getId(), spectrumHeader);

			if (this.entityCache != null)
				this.entityCache.spectrumHeaderById = spectrumHeaderById;

			return spectrumHeaderById;
		}
	}

	/**
	 * Gets the MS1 spectrum headers.
	 *
	 * @param connection
	 *            the connection
	 * @return the spectrum headers
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected SpectrumHeader[] getMs1SpectrumHeaders(SQLiteConnection connection) throws SQLiteException {

		if (this.entityCache != null && this.entityCache.ms1SpectrumHeaders != null) {
			return this.entityCache.ms1SpectrumHeaders;
		} else {

			// First pass to load the index
			//final SQLiteStatement fakeStmt = connection.prepare(_ms1SpectrumHeaderQueryStr, true);
			//while (fakeStmt.step()) {}
			//fakeStmt.dispose();

			SpectrumHeader[] ms1SpectrumHeaders = new SpectrumHeader[ms1SpectraCount];
			
			new SQLiteQuery(connection, _ms1SpectrumHeaderQueryStr)
				.extractRecords(this._spectrumHeaderExtractor, ms1SpectrumHeaders);

			if (this.entityCache != null)
				this.entityCache.ms1SpectrumHeaders = ms1SpectrumHeaders;

			return ms1SpectrumHeaders;
		}

	}

	/**
	 * Gets the MS1 spectrum header by id.
	 *
	 * @param connection
	 *            the connection
	 * @return the spectrum header by id
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected Map<Long, SpectrumHeader> getMs1SpectrumHeaderById(SQLiteConnection connection) throws SQLiteException {

		if (this.entityCache != null && this.entityCache.ms1SpectrumHeaderById != null) {
			return this.entityCache.ms1SpectrumHeaderById;
		} else {
			SpectrumHeader[] ms1SpectrumHeaders = this.getMs1SpectrumHeaders(connection);

			Map<Long, SpectrumHeader> ms1SpectrumHeaderById = new HashMap<Long, SpectrumHeader>(ms1SpectrumHeaders.length);

			for (SpectrumHeader ms1SpectrumHeader : ms1SpectrumHeaders)
				ms1SpectrumHeaderById.put(ms1SpectrumHeader.getId(), ms1SpectrumHeader);

			if (this.entityCache != null)
				this.entityCache.ms1SpectrumHeaderById = ms1SpectrumHeaderById;

			return ms1SpectrumHeaderById;
		}
	}

	/**
	 * Gets the MS2 spectrum headers.
	 *
	 * @param connection
	 *            the connection
	 * @return the spectrum headers
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected SpectrumHeader[] getMs2SpectrumHeaders(SQLiteConnection connection) throws SQLiteException {

		if (this.entityCache != null && this.entityCache.ms2SpectrumHeaders != null) {
			return this.entityCache.ms2SpectrumHeaders;
		} else {

			SpectrumHeader[] ms2SpectrumHeaders = new SpectrumHeader[ms2SpectraCount];

			new SQLiteQuery(connection, _ms2SpectrumHeaderQueryStr).extractRecords(this._spectrumHeaderExtractor, ms2SpectrumHeaders);

			if (this.entityCache != null)
				this.entityCache.ms2SpectrumHeaders = ms2SpectrumHeaders;

			return ms2SpectrumHeaders;
		}

	}

	/**
	 * Gets the MS2 spectrum header by id.
	 *
	 * @param connection
	 *            the connection
	 * @return the spectrum header by id
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected Map<Long, SpectrumHeader> getMs2SpectrumHeaderById(SQLiteConnection connection) throws SQLiteException {

		if (this.entityCache != null && this.entityCache.ms2SpectrumHeaderById != null) {
			return this.entityCache.ms2SpectrumHeaderById;
		} else {
			SpectrumHeader[] ms2SpectrumHeaders = this.getMs2SpectrumHeaders(connection);

			Map<Long, SpectrumHeader> ms2SpectrumHeaderById = new HashMap<Long, SpectrumHeader>(ms2SpectrumHeaders.length);

			for (SpectrumHeader ms2SpectrumHeader : ms2SpectrumHeaders)
				ms2SpectrumHeaderById.put(ms2SpectrumHeader.getId(), ms2SpectrumHeader);

			if (this.entityCache != null)
				this.entityCache.ms2SpectrumHeaderById = ms2SpectrumHeaderById;

			return ms2SpectrumHeaderById;
		}
	}

	/**
	 * /** Gets the spectrum header.
	 *
	 * @param id
	 *            the id
	 * @param connection
	 *            the connection
	 * @return spectrum header
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected SpectrumHeader getSpectrumHeader(long id, SQLiteConnection connection) throws SQLiteException {
		if (this.entityCache != null) {

			if (this.getMs1SpectrumHeaderById(connection).containsKey(id)) {
				return this.getMs1SpectrumHeaderById(connection).get(id);
			} else if (this.getMs2SpectrumHeaderById(connection).containsKey(id)) {
				return this.getMs2SpectrumHeaderById(connection).get(id);
			} else {
				return null;
			}

		} else {
			String queryStr = _spectrumHeaderQueryStr + " WHERE id = ? ";
			return new SQLiteQuery(connection, queryStr).bind(1, id).extractRecord(this._spectrumHeaderExtractor);
		}
	}

	/**
	 * Gets the spectrum time by id.
	 *
	 * @param connection
	 *            the connection
	 * @return the spectrum time mapped by the spectrum id
	 * @throws SQLiteException
	 *             the SQLite exception
	 */
	protected Map<Long, Float> getSpectrumTimeById(SQLiteConnection connection) throws SQLiteException {

		if (this.entityCache != null && this.entityCache.spectrumTimeById != null) {
			return this.entityCache.spectrumTimeById;
		} else {
			float[] spectrumTimes = new SQLiteQuery(connection, "SELECT time FROM spectrum").extractFloats(spectraCount);
			if( spectraCount != spectrumTimes.length ){
				System.err.println("extractFloats error: spectraCount != spectrumTimes.length");
			}
			Map<Long, Float> spectrumTimeById = new HashMap<Long, Float>(spectraCount);

			// TODO: check this approach is not too dangerous
			// FIXME: load the both values in the SQL query
			long spectrumId = 0;
			for (float spectrumTime : spectrumTimes) {
				spectrumId++;
				spectrumTimeById.put(spectrumId, spectrumTime);
			}

			if (this.entityCache != null)
				this.entityCache.spectrumTimeById = spectrumTimeById;

			return spectrumTimeById;
		}
	}

	/**
	 * Gets the spectrum header for time.
	 *
	 * @param time
	 *            the time
	 * @param msLevel
	 *            the ms level
	 * @param connection
	 *            the connection
	 * @return SpectrumHeader the closest to the time input parameter
	 * @throws Exception
	 *             the exception
	 */
	protected SpectrumHeader getSpectrumHeaderForTime(float time, int msLevel, SQLiteConnection connection) throws Exception {

		if (this.entityCache != null) {
			Map<Integer, ArrayList<Long>> spectrumIdsByTimeIndex = this._getSpectrumIdsByTimeIndex(msLevel, connection);

			int timeIndex = (int) (time / TIME_INDEX_WIDTH);
			SpectrumHeader nearestSpectrumHeader = null;

			for (int index = timeIndex - 1; index <= timeIndex + 1; index++) {

				if (spectrumIdsByTimeIndex.containsKey(index) == false) {
					continue;
				}

				ArrayList<Long> tmpSpectrumIds = spectrumIdsByTimeIndex.get(index);
				for (Long tmpSpectrumId : tmpSpectrumIds) {

					SpectrumHeader spectrumH = this.getSpectrumHeader(tmpSpectrumId, connection);
					if (spectrumH == null) {
						throw new Exception("can' t retrieve spectrum with id =" + tmpSpectrumId);
					}

					if (spectrumH.getMsLevel() != msLevel)
						continue;

					if ( nearestSpectrumHeader == null || 
						 Math.abs(spectrumH.getTime() - time) < Math.abs(nearestSpectrumHeader.getTime() - time) ) {
						nearestSpectrumHeader = spectrumH;
					}
				}
			}

			return nearestSpectrumHeader;
		} else {
			String queryStr = "SELECT id FROM spectrum WHERE ms_level = ? ORDER BY abs(spectrum.time - ?) ASC limit 1";
			int spectrumId = new SQLiteQuery(connection, queryStr)
				.bind(1,msLevel)
				.bind(2,time)
				.extractSingleInt();
			
			return this.getSpectrumHeader(spectrumId, connection);
		}

	}

	/**
	 * Gets the spectrum ids by time index.
	 *
	 * @param msLevel
	 *            the ms level
	 * @param connection
	 *            the connection
	 * @return hashmap of key time index value array of spectrumIds
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	private Map<Integer, ArrayList<Long>> _getSpectrumIdsByTimeIndex(int msLevel, SQLiteConnection connection) throws SQLiteException {

		HashMap<Integer, ArrayList<Long>> spectrumIdsByTimeIndex = null;
		if (this.entityCache != null) {

			if (msLevel == 1 && this.entityCache.ms1SpectrumIdsByTimeIndex != null) {
				spectrumIdsByTimeIndex = (HashMap<Integer, ArrayList<Long>>) this.entityCache.ms1SpectrumIdsByTimeIndex;
			} else if (msLevel == 2 & this.entityCache.ms1SpectrumIdsByTimeIndex != null) {
				spectrumIdsByTimeIndex = (HashMap<Integer, ArrayList<Long>>) this.entityCache.ms2SpectrumIdsByTimeIndex;
			}

		}

		if (spectrumIdsByTimeIndex != null)
			return spectrumIdsByTimeIndex;
		else {
			spectrumIdsByTimeIndex = new HashMap<Integer, ArrayList<Long>>();

			SpectrumHeader[] spectrumHeaders;
			if (msLevel == 1)
				spectrumHeaders = this.getMs1SpectrumHeaders(connection);
			else if (msLevel == 2)
				spectrumHeaders = this.getMs2SpectrumHeaders(connection);
			else
				return null;

			for (SpectrumHeader spectrumH : spectrumHeaders) {
				int timeIndex = (int) (spectrumH.getTime() / TIME_INDEX_WIDTH);

				if (spectrumIdsByTimeIndex.get(timeIndex) == null)
					spectrumIdsByTimeIndex.put(timeIndex, new ArrayList<Long>());

				spectrumIdsByTimeIndex.get(timeIndex).add(spectrumH.getId());
			}

			if (this.entityCache != null) {
				if (msLevel == 1)
					this.entityCache.ms1SpectrumIdsByTimeIndex = spectrumIdsByTimeIndex;
				else if (msLevel == 2)
					this.entityCache.ms2SpectrumIdsByTimeIndex = spectrumIdsByTimeIndex;
			}

			return spectrumIdsByTimeIndex;
		}
	}

	/**
	 * Gets the spectrum ids for time range.
	 *
	 * @param minRT
	 *            the min rt
	 * @param maxRT
	 *            the max rt
	 * @param msLevel
	 *            the ms level
	 * @param connection
	 *            the connection
	 * @return array of integers corresponding to the ids of matching spectrum
	 * @throws SQLiteException
	 *             the SQ lite exception
	 */
	protected long[] getSpectrumIdsForTimeRange(float minRT, float maxRT, int msLevel, SQLiteConnection connection) throws SQLiteException {

		// TODO: use entity cache ?
		SQLiteQuery query = new SQLiteQuery(connection, "SELECT id FROM spectrum WHERE ms_level = ? AND time >= ? AND time <= ?");
		return query.bind(1, msLevel).bind(2, minRT).bind(3, maxRT).extractLongs(1);
	}

}
