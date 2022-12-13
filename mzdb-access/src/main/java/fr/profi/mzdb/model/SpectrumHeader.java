/*
 * Package fr.profi.mzdb.model
 * @author David Bouyssie
 */
package fr.profi.mzdb.model;

import com.almworks.sqlite4java.SQLiteConnection;
import com.almworks.sqlite4java.SQLiteException;
import com.almworks.sqlite4java.SQLiteStatement;
import fr.profi.mzdb.db.model.AbstractTableModel;
import fr.profi.mzdb.db.model.params.Precursor;
import fr.profi.mzdb.db.model.params.PrecursorList;
import fr.profi.mzdb.db.model.params.ScanList;
import fr.profi.mzdb.io.reader.table.ParamTreeParser;
import fr.profi.mzdb.util.sqlite.ISQLiteRecordOperation;
import fr.profi.mzdb.util.sqlite.SQLiteQuery;
import fr.profi.mzdb.util.sqlite.SQLiteRecord;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

// TODO: Auto-generated Javadoc
/**
 * The Class SpectrumHeader.
 * 
 * @author David Bouyssie
 */
public class SpectrumHeader extends AbstractTableModel implements ILcContext {

	/** The id. */
	protected final long id;

	/** The initial id. */
	protected final int initialId;

	/** The cycle. */
	protected final int cycle;

	/** The time. */
	protected final float time;

	/** The ms level. */
	protected final int msLevel;

	protected final String title;

	/** The peaks count. */
	protected final int peaksCount;

	/** Is high resolution boolean. */
	protected final boolean isHighResolution;

	/** total ion chromatogram of the spectrum */
	protected final float tic;

	/** The base peak mz. */
	protected final double basePeakMz;

	/** The base peak intensity. */
	protected final float basePeakIntensity;

	protected final ActivationType activationType;

	protected IsolationWindow isolationWindow;

	/** The precursor mz. */
	protected final Double precursorMz;

	/** The precursor charge. */
	protected final Integer precursorCharge;

	/** The bounding box first spectrum id. */
	protected final int bbFirstSpectrumId;

	/** The spectrum list. */
	protected ScanList scanList = null;
	protected String scanListAsString;

	/** The precursor: contains selected ions list */
	protected Precursor precursor = null;

	protected PrecursorList precursorList = null;

	protected String precursorAsString;

	protected String paramTreeAsString; //String representation of paramTree. Only For Spectrum Header (?)

	/**
	 * Instantiates a new spectrum header.
	 * @param id
	 *            the id
	 * @param initialId
	 *            the initial id
	 * @param cycle
*            the cycle
	 * @param time
*            the time
	 * @param msLevel
*            the ms level
	 * @param title
	 * @param peaksCount
*            the peaks count
	 * @param basePeakMz
*            the base peak mz
	 * @param basePeakIntensity
*            the base peak intensity
	 * @param precursorMz
*            the precursor mz
	 * @param precursorCharge
	 * @param activationType
	 */
	public SpectrumHeader(
					long id,
					int initialId,
					int cycle,
					float time,
					int msLevel,
					String title, int peaksCount,
					boolean isHighResolution,
					float tic,
					double basePeakMz,
					float basePeakIntensity,
					Double precursorMz,
					Integer precursorCharge,
					int firstBBSpectrumId,
					ActivationType activationType) {
		super(id, null);
		this.id = id;
		this.initialId = initialId;
		this.cycle = cycle;
		this.time = time;
		this.msLevel = msLevel;
		this.title = title;
		this.peaksCount = peaksCount;
		this.isHighResolution = isHighResolution;
		this.tic = tic;
		this.basePeakMz = basePeakMz;
		this.basePeakIntensity = basePeakIntensity;
		this.precursorMz = precursorMz;
		this.precursorCharge = precursorCharge;
		this.bbFirstSpectrumId = firstBBSpectrumId;
		this.activationType = activationType;
		isolationWindow = null;
	}

	/**
	 * Gets the id.
	 * 
	 * @return the id
	 */
	public long getId() {
		return id;
	}

	/**
	 * Gets the initial id.
	 * 
	 * @return the initial id
	 */
	public int getInitialId() {
		return initialId;
	}

	/**
	 * Gets the cycle.
	 * 
	 * @return the cycle
	 */
	public int getCycle() {
		return cycle;
	}

	/**
	 * Gets the time.
	 * 
	 * @return the time
	 */
	public float getTime() {
		return time;
	}

	/**
	 * Gets the ms level.
	 * 
	 * @return the ms level
	 */
	public int getMsLevel() {
		return msLevel;
	}

	/**
	 * Gets the peaks count.
	 * 
	 * @return the peaks count
	 */
	public int getPeaksCount() {
		return peaksCount;
	}

	/**
	 * Checks if is high resolution.
	 * 
	 * @return true, if is high resolution
	 */
	public boolean isHighResolution() {
		return isHighResolution;
	}

	/**
	 * Gets the base peak mz.
	 * 
	 * @return the base peak mz
	 */
	public double getBasePeakMz() {
		return basePeakMz;
	}

	/**
	 * Gets the base peak intensity.
	 * 
	 * @return the base peak intensity
	 */
	public float getBasePeakIntensity() {
		return basePeakIntensity;
	}

	/**
	 * Gets the precursor mz.
	 * 
	 * @return the precursor mz
	 */
	public Double getPrecursorMz() {
		return precursorMz;
	}

	/**
	 * Gets the precursor charge.
	 * 
	 * @return the precursor charge
	 */
	public Integer getPrecursorCharge() {
		return precursorCharge;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.model.ILcContext#getSpectrumId()
	 */
	public long getSpectrumId() {
		return this.id;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see fr.profi.mzdb.model.ILcContext#getElutionTime()
	 */
	public float getElutionTime() {
		return this.time;
	}

	public String getTitle() {
		return title;
	}

	public ActivationType getActivationType() {
		return activationType;
	}

	public IsolationWindow getIsolationWindow() {
		return isolationWindow;
	}

	public void setIsolationWindow(IsolationWindow isolationWindow) {
		this.isolationWindow = isolationWindow;
	}

	public int getBBFirstSpectrumId() {
		return this.bbFirstSpectrumId;
	}

	public ScanList getScanList() {
		return this.scanList;
	}
	
	public void setScanList( ScanList scanList ) {
		this.scanList = scanList;
	}

	public void setScanListAsString(String scanListAsString) {
		this.scanListAsString = scanListAsString;
	}

	public Precursor getPrecursor() {
		return this.precursor;
	}

	public void setPrecursor(Precursor precursor) {
		this.precursor = precursor;
	}

	public void setPrecursorAsString(String precursorAsString) {
		this.precursorAsString = precursorAsString;
	}

	public void setParamTreeAsString(String paramTreeAsString) {
		this.paramTreeAsString = paramTreeAsString;
	}


	/** The rt comp. */
	public static Comparator<SpectrumHeader> rtComp = new Comparator<SpectrumHeader>() {
		// @Override
		public int compare(SpectrumHeader o1, SpectrumHeader o2) {
			if (o1.time < o2.time)
				return -1;
			else if (Math.abs(o1.time - o2.time) < 1e-6)
				return 0;
			else
				return 1;
		}
	};

	public float getTIC() {
		return tic;
	}
	
	private static Map<Long, String> _loadXmlFieldBySpectrumId(String sqlString, SQLiteConnection mzDbConnection) {
		Map<Long, String> xmlFieldBySpecId = new HashMap<>();
		
		try {
			SQLiteQuery query = new SQLiteQuery(mzDbConnection, sqlString);
			SQLiteStatement stmt = query.getStatement();
			
			query.forEachRecord(
				new ISQLiteRecordOperation() {
					@Override
					public void execute(SQLiteRecord elem, int idx) throws SQLiteException {
						long id = stmt.columnLong(0);
						String xmlField = stmt.columnString(1);
						xmlFieldBySpecId.put(id, xmlField);
					}
				} // end inner class
			);
		} catch (SQLiteException e) {
			e.printStackTrace();
		}
		
		return xmlFieldBySpecId;
	}

	public static void loadParamTrees(SpectrumHeader[] spectrumHeaders, SQLiteConnection mzDbConnection) {
		SpectrumHeader.loadParamTrees(spectrumHeaders, mzDbConnection, false);
	}

	public static void loadParamTrees(SpectrumHeader[] spectrumHeaders, SQLiteConnection mzDbConnection, boolean cacheStringRepresentation) {
		String sqlString = "SELECT id, param_tree FROM spectrum";
		Map<Long, String> paramTreeBySpecId = _loadXmlFieldBySpectrumId(sqlString, mzDbConnection);
		for (SpectrumHeader header : spectrumHeaders) {
			if (!header.hasParamTree()) {
				String paramTreeAsStr = paramTreeBySpecId.get(header.getId());
				if (paramTreeAsStr != null) {
					header.paramTree = ParamTreeParser.parseParamTree(paramTreeAsStr);
					if(cacheStringRepresentation)
						header.paramTreeAsString = paramTreeAsStr;
				}
			}
		}
	}

	public static void loadScanLists(SpectrumHeader[] spectrumHeaders, SQLiteConnection mzDbConnection) {
		SpectrumHeader.loadScanLists(spectrumHeaders,mzDbConnection,false);
	}


		public static void loadScanLists(SpectrumHeader[] spectrumHeaders, SQLiteConnection mzDbConnection, boolean cacheStringRepresentation) {
		String sqlString = "SELECT id, scan_list FROM spectrum";
		Map<Long, String> scanListBySpecId = _loadXmlFieldBySpectrumId(sqlString, mzDbConnection);
		
		for (SpectrumHeader header : spectrumHeaders) {
			if (header.scanList == null) {
				String scanListAsStr = scanListBySpecId.get(header.getId());
				if (scanListAsStr != null) {
					header.scanList = ParamTreeParser.parseScanList(scanListAsStr);
					if(cacheStringRepresentation)
						header.scanListAsString= scanListAsStr;
				}
			}
		}
	}

	public static void loadPrecursors(SpectrumHeader[] spectrumHeaders, SQLiteConnection mzDbConnection) {
		SpectrumHeader.loadPrecursors(spectrumHeaders,mzDbConnection,false);
	}

	public static void loadPrecursors(SpectrumHeader[] spectrumHeaders, SQLiteConnection mzDbConnection, boolean cacheStringRepresentation) {
		String sqlString = "SELECT id, precursor_list FROM spectrum";
		Map<Long, String> precursorBySpecId = _loadXmlFieldBySpectrumId(sqlString, mzDbConnection);
		
		for (SpectrumHeader header : spectrumHeaders) {
			if (header.precursor == null) {
				String precursorAsStr  = precursorBySpecId.get(header.getId());
				if (precursorAsStr  != null) {
					header.precursor = ParamTreeParser.parsePrecursor(precursorAsStr);
					if(cacheStringRepresentation)
						header.precursorAsString = precursorAsStr;
				}
			}
		}
	}

	@Override
	public void loadParamTree(SQLiteConnection mzDbConnection) throws SQLiteException {
		if (!this.hasParamTree()) {
			// if use load "XML" representation, suppose cache is not necessary
			String paramTreeAsStr = getParamTreeAsString(mzDbConnection, false);
			this.paramTree = ParamTreeParser.parseParamTree(paramTreeAsStr);
		}
	}

	@Override
	public String getParamTreeAsString(SQLiteConnection mzDbConnection) throws SQLiteException {
		return this.getParamTreeAsString(mzDbConnection, true);//If call this method suppose cache may be used
	}

	private String getParamTreeAsString(SQLiteConnection mzDbConnection, boolean cacheStringRepresentation) throws SQLiteException {
		String paramTreeAsSt = paramTreeAsString;
		if(paramTreeAsSt == null){
			String sqlString = "SELECT param_tree FROM spectrum WHERE id = ?";
			paramTreeAsSt = new SQLiteQuery(mzDbConnection, sqlString).bind(1, this.getId()).extractSingleString();
			if(cacheStringRepresentation)
				this.paramTreeAsString = paramTreeAsSt;
		}
		return paramTreeAsSt;
	}

	public void loadScanList(SQLiteConnection mzDbConnection) throws SQLiteException {
		if (scanList == null) {
			String scanListAsStr = getScanListAsString(mzDbConnection, false); // if use load "XML" representation, suppose cache is not necessary
			this.scanList = ParamTreeParser.parseScanList(scanListAsStr);
		}
	}

	public String getScanListAsString(SQLiteConnection mzDbConnection) throws SQLiteException {
		return  this.getScanListAsString(mzDbConnection, true); //If call this method suppose cache may be used
	}

	private String getScanListAsString(SQLiteConnection mzDbConnection, boolean cacheStringRepresentation) throws SQLiteException {
		String scanListAsStr = scanListAsString;
		if(scanListAsStr == null) {
			String sqlString = "SELECT scan_list FROM spectrum WHERE id = ?";
			scanListAsStr = new SQLiteQuery(mzDbConnection, sqlString).bind(1, this.getId()).extractSingleString();
			if(cacheStringRepresentation)
				this.scanListAsString = scanListAsStr;
		}
		return scanListAsStr;
	}

	public void loadPrecursorList(SQLiteConnection mzDbConnection) throws SQLiteException {
		if (precursor == null) {
			// if use load "XML" representation, suppose cache is not necessary
			String precursorAsStr = getPrecursorListAsString(mzDbConnection, false);
			this.precursor = ParamTreeParser.parsePrecursor(precursorAsStr);
		}
	}

	public String getPrecursorListAsString(SQLiteConnection mzDbConnection) throws SQLiteException {
		return this.getPrecursorListAsString(mzDbConnection, true);//If call this method suppose cache may be used
	}

	private String getPrecursorListAsString(SQLiteConnection mzDbConnection, boolean cacheStringRepresentation ) throws SQLiteException {
		String precursorAsStr = precursorAsString;
		if (precursorAsStr == null && msLevel != 1) {
			String sqlString = "SELECT precursor_list FROM spectrum WHERE id = ?";
			precursorAsStr = new SQLiteQuery(mzDbConnection, sqlString).bind(1, this.getId()).extractSingleString();
			if(cacheStringRepresentation)
				this.precursorAsString = precursorAsStr;
		}
		return precursorAsStr;
	}
}
