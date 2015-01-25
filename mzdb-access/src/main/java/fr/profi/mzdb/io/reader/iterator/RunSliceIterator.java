package fr.profi.mzdb.io.reader.iterator;

import java.io.StreamCorruptedException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;

import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.BoundingBox;
import fr.profi.mzdb.model.RunSlice;
import fr.profi.mzdb.model.RunSliceData;
import fr.profi.mzdb.model.RunSliceHeader;
import fr.profi.mzdb.model.ScanSlice;

public class RunSliceIterator extends AbstractScanSliceIterator implements Iterator<RunSlice> {

	private static String sqlQuery = "SELECT bounding_box.* FROM bounding_box, run_slice WHERE run_slice.ms_level = ? AND bounding_box.run_slice_id = run_slice.id  ORDER BY run_slice.begin_mz";

	private static String sqlBoundedQuery = "SELECT bounding_box.* FROM bounding_box, run_slice "
			+ "WHERE run_slice.ms_level = ? "
			+ "AND bounding_box.run_slice_id = run_slice.id  "
			+ "AND run_slice.end_mz >= ? "
			+ "AND run_slice.begin_mz <= ?"
			+ "ORDER BY run_slice.begin_mz";
	
	private static String sqlSwathQuery1 = "SELECT bounding_box.* FROM bounding_box, bounding_box_msn_rtree, run_slice "
			+ "WHERE bounding_box_msn_rtree.id = bounding_box.id "
			+ "AND bounding_box.run_slice_id = run_slice.id "
			+ "AND run_slice.ms_level = ? "
			+ "AND bounding_box_msn_rtree.min_parent_mz >= ? "
			+ "AND bounding_box_msn_rtree.max_parent_mz <= ? " + "ORDER BY run_slice.begin_mz";

	protected ScanSlice[] scanSliceBuffer = null;
	protected boolean bbHasNext = true;
	protected final HashMap<Integer, RunSliceHeader> runSliceHeaderById;

	public RunSliceIterator(MzDbReader inst, int msLevel) throws SQLiteException, StreamCorruptedException {
		super(inst, sqlQuery, msLevel);
		this.runSliceHeaderById = this.mzDbReader.getRunSliceHeaderById(this.msLevel);
	}

	public RunSliceIterator(MzDbReader inst, int msLevel, double minMz, double maxMz)
			throws SQLiteException, StreamCorruptedException {
		super(inst, getBBSelectQuery(msLevel), msLevel, minMz, maxMz);
		this.runSliceHeaderById = this.mzDbReader.getRunSliceHeaderById(msLevel);
	}

	private static String getBBSelectQuery(int msLevel) {
		if (msLevel > 1) 
			return sqlSwathQuery1;
		return sqlBoundedQuery;
	}
	
	protected void initScanSliceBuffer() {
		
		this.scanSliceBuffer = this.firstBB.toScanSlices();
		ArrayList<ScanSlice> sl = new ArrayList<ScanSlice>(Arrays.asList(this.scanSliceBuffer));

		while (bbHasNext = boundingBoxIterator.hasNext()) {
			BoundingBox bb = boundingBoxIterator.next();

			if (bb.getRunSliceId() == this.firstBB.getRunSliceId()) {
				sl.addAll(Arrays.asList(bb.toScanSlices()));
			} else {
				this.firstBB = bb;
				break;
			}
		}

		this.scanSliceBuffer = sl.toArray(new ScanSlice[sl.size()]);

		if (!bbHasNext) {
			this.firstBB = null;
		}
	}

	public RunSlice next() {

		initScanSliceBuffer();

		int runSliceId = this.scanSliceBuffer[0].getRunSliceId();
		RunSliceData rsd = new RunSliceData(runSliceId, this.scanSliceBuffer);
		// rsd.buildPeakListByScanId();

		RunSliceHeader rsh = this.runSliceHeaderById.get(runSliceId);

		return new RunSlice(rsh, rsd);
	}

}
