package fr.profi.mzdb.io.reader.iterator;

import java.util.Iterator;

import com.almworks.sqlite4java.SQLiteException;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.BoundingBox;
import fr.profi.mzdb.model.Scan;
import fr.profi.mzdb.model.ScanSlice;

public class MsScanIterator extends AbstractScanSliceIterator implements Iterator<Scan> {

	private static String sqlQuery = "SELECT bounding_box.* FROM bounding_box, spectrum WHERE spectrum.id = bounding_box.first_spectrum_id AND spectrum.ms_level= ?";
	private int scanSliceIdx;
	
	protected ScanSlice[] scanSliceBuffer = null;
	protected boolean bbHasNext = true;
	
	public MsScanIterator(MzDbReader inst, int msLevel) throws SQLiteException {
		super(inst, sqlQuery, msLevel);
		
		this.initScanSliceBuffer();
	}
	
	protected void initScanSliceBuffer() {
		
		//init stuff
		//if (! bbHasNext) {
		//	scanSliceBuffer = null;
		//	return;
		//}
	
		this.scanSliceBuffer = this.firstBB.asScanSlicesArray();
	
		//for( ScanSlice sSlice: scanSliceBuffer) {
		//	ScanHeader header = mzdb.getScanHeader(sSlice.scanId);
		//	sSlice.resizeDataArrays(header.peaksCount);
		//}
		
		this.scanSliceIdx = 0;

		// Build scan slice buffer
		while (bbHasNext=boundingBoxIterator.hasNext()) {//bbHasNext=
			
				BoundingBox bb = boundingBoxIterator.next();
				ScanSlice[] sSlices = (ScanSlice[]) bb.asScanSlicesArray();
				
				if (sSlices == null)
					continue;
			
		    	if (sSlices[0].getScanId() == scanSliceBuffer[0].getScanId()) {
		    		for (int i=0; i < sSlices.length; i++) {
		    			scanSliceBuffer[i].addScanData(sSlices[i]);//, scanSliceBuffer[i].length);
		    		}
			    }
		    	else {
		    		// Keep this bounding box for next iteration
		    		this.firstBB = bb;	    		
		    		break;
		    	}
		}
	}


	public Scan next() {
	
		//firstScanSlices is not null
		int c = scanSliceIdx;
		scanSliceIdx++;
		
		ScanSlice sSlice = scanSliceBuffer[c];
		
		// If no more scan slices
		
		if (scanSliceIdx == scanSliceBuffer.length) {
			if (bbHasNext)
				initScanSliceBuffer();
			else
				this.firstBB= null;
		}
		
		try {
			return new Scan(mzDbReader.getScanHeader(sSlice.getScanId()), sSlice);
		} catch (SQLiteException e) {
			e.printStackTrace();
			return null;
		}
		
	}
	


}