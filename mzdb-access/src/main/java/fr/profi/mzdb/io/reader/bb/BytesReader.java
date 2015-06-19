package fr.profi.mzdb.io.reader.bb;

import java.io.StreamCorruptedException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Map;

import fr.profi.mzdb.MzDbReader;
import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.ScanData;
import fr.profi.mzdb.model.ScanHeader;
import fr.profi.mzdb.model.ScanSlice;

/**
 * @author marco This implementation is mainly used is mzDbReader
 * <p>
 *   Use a ByteBuffer to store the blob's bytes This class extends AbstractBlobReader.
 * </p>
 */
public class BytesReader extends AbstractBlobReader {

	/** the data */
	protected ByteBuffer _bbByteBuffer;

	/** size of the Blob */
	protected int _blobSize;

	/**
	 * Constructor
	 * 
	 * @param dataEncodings, DataEncoding object for each scan, usually given by a mzDbReaderInstance
	 * @param data, array of byte of the blob
	 * @throws StreamCorruptedException 
	 * @see MzDbReader
	 * @see DataEncoding
	 */
	public BytesReader(
		byte[] bytes,
		long firstScanId,
		long lastScanId,
		Map<Long, ScanHeader> scanHeaderById,
		Map<Long, DataEncoding> dataEncodingByScanId
	) throws StreamCorruptedException {
		super(firstScanId, lastScanId, scanHeaderById, dataEncodingByScanId);
		
		this._bbByteBuffer = ByteBuffer.wrap(bytes);
		this._bbByteBuffer.order(ByteOrder.LITTLE_ENDIAN);
		this._blobSize = bytes.length;
		
		//logger.debug("BytesReader: blobSize="+ _blobSize);
		
		this._indexScanSlices();
	}

	/**
	 * Do a first parse of the blob to determine beginning index of each scan slice
	 * @throws StreamCorruptedException 
	 * 
	 * @see AbstractBlobReader
	 * @see AbstractBlobReader._buildMpaPositions()
	 */
	public void _indexScanSlices() throws StreamCorruptedException {
		
		//int scanSliceIdx = 0;
		int byteIdx = 0;
		
		ArrayList<Integer> scanSliceStartPositions = new ArrayList<Integer>();
		ArrayList<Integer> peaksCounts = new ArrayList<Integer>();
		
		while (byteIdx < _blobSize) {
			
			// Retrieve the scan id
			long scanId = (long) _bbByteBuffer.getInt(byteIdx);	
			//_scanSliceStartPositions[scanSliceIdx] = byteIdx;
			scanSliceStartPositions.add(byteIdx);
			//System.out.println("scan id is: "+scanId);

			// Skip the scan id bytes
			byteIdx += 4;

			// Retrieve the number of peaks
			int peaksCount = _bbByteBuffer.getInt(byteIdx); 
			//_peaksCounts[scanSliceIdx] = peaksCount;
			peaksCounts.add(peaksCount);

			// Skip the peaksCount bytes
			byteIdx += 4;

			// Retrieve the DataEncoding corresponding to this scan
			DataEncoding de = this._dataEncodingByScanId.get(scanId);
			this.checkDataEncodingIsNotNull(de, scanId);
			
			byteIdx += peaksCount * de.getPeakStructSize(); // skip nbPeaks * size of one peak
			
			//scanSliceIdx++;
		} // statement inside a while loop
		
		this._scansCount = scanSliceStartPositions.size();
		this._scanSliceStartPositions = intListToInts(scanSliceStartPositions, _scansCount);
		this._peaksCounts = intListToInts(peaksCounts, _scansCount);
	}

	/**
	 * @see IBlobReader#disposeBlob()
	 */
	public void disposeBlob() {}

	/**
	 * @see IBlobReader#getBlobSize()
	 */
	public int getBlobSize() {
		return _blobSize;
	}

	/**
	 * @see IBlobReader#getScansCount()
	 */
	public int getScansCount() {
		return _scansCount;
	}

	/**
	 * @see IBlobReader#idOfScanAt(int)
	 */
	public long getScanIdAt(int idx) {
		this.checkScanIndexRange(idx);
		return _getScanIdAt(idx);
	}
	
	private long _getScanIdAt(int idx) {
		return (long) _bbByteBuffer.getInt(_scanSliceStartPositions[idx]);
	}

	/**
	 * @see IBlobReader#nbPeaksOfScanAt(int)
	 */
	/*public int nbPeaksOfScanAt(int idx) {
		if (idx < 0 || idx >= _scansCount) {
			throw new IndexOutOfBoundsException("nbPeaksOfScanAt: index out of bounds (i="+idx+"), index counting starts at 0");
		}
		
		return _peaksCounts[idx];
	}*/

	/**
	 * @see IBlobReader#peakAt(int, int)
	 */
	/*public Peak peakAt(int idx, int pos) {
		if (idx > _nbScans || idx < 1) {
			throw new IndexOutOfBoundsException("peakAt: Index out of bound start counting at 1");
		}
		int nbPeaks = this.nbPeaksOfScanAt(idx);
		if (pos > nbPeaks) {
			throw new IndexOutOfBoundsException(
					"peakAt: Index out of bound, peak wanted index superior at scan slice length");
		}
		Peak[] peaks = peaksOfScanAt(idx);
		return peaks[pos];
	}*/

	/**
	 * @see IBlobReader#readScanSliceAt(int)
	 */
	public ScanSlice readScanSliceAt(int idx) {
		long scanId = _getScanIdAt(idx);
		ScanData scanSliceData = this._readScanSliceDataAt(idx, scanId);
		ScanHeader sh = _scanHeaderById.get( scanId );
		
		// Instantiate a new ScanSlice
		return new ScanSlice(sh, scanSliceData);
	}
	
	/**
	 * @see IBlobReader#readScanSliceAt(int)
	 */
	public ScanData readScanSliceDataAt(int idx) {
		return this._readScanSliceDataAt(idx, _getScanIdAt(idx) );		
	}
	
	private ScanData _readScanSliceDataAt(int idx, long scanId) {
		
		// Determine peak size in bytes
		DataEncoding de = this._dataEncodingByScanId.get(scanId);

		// Determine peaks bytes length
		int peaksBytesSize = _peaksCounts[idx] * de.getPeakStructSize();
		
		// Skip scan id and peaks count (two integers)
		int scanSliceStartPos = _scanSliceStartPositions[idx] + 8;

		// Instantiate a new ScanData for the corresponding scan slice
		return this.readScanSliceData(_bbByteBuffer, scanSliceStartPos, peaksBytesSize, de);		
	}

}
