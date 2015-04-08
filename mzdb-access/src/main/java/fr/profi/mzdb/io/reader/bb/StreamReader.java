package fr.profi.mzdb.io.reader.bb;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.Map;

import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.ScanData;
import fr.profi.mzdb.model.ScanHeader;
import fr.profi.mzdb.model.ScanSlice;
import fr.profi.mzdb.utils.primitives.BytesUtils;

/**
 * This class aloow to read a SQLite blob using a stream reader. We process data only in one direction in a
 * sequential way The goal is to request only one time the blob
 * 
 * @author marco
 * 
 */
public class StreamReader extends AbstractBlobReader {

	/** Stream to read */
	private InputStream _stream;

	/**
	 * @param dataEnc
	 *            ScanID the key, dataEncoding the value
	 * @param s
	 *            inputStream
	 * @see AbstractBlobReader
	 * @see AbstractBlobReader#_dataEncodingByScanId
	 */
	public StreamReader(
		InputStream inputStream,
		int firstScanId,
		int lastScanId,
		Map<Integer, ScanHeader> scanHeaderById,
		Map<Integer, DataEncoding> dataEncodingByScanId
	) {
		super(firstScanId, lastScanId, scanHeaderById, dataEncodingByScanId);
		
		this._stream = inputStream;
	}

	/**
	 * @see IBlobReader#disposeBlob()
	 */
	public void disposeBlob() {
		try {
			_stream.close();
		} catch (IOException e) {
			logger.error("IOException has been catched while closing stream", e);
		}
	}

	/**
	 * @see IBlobReader#getScansCount()
	 */
	public int getScansCount() {
		return _scansCount;
	}

	/**
	 * @see IBlobReader#getBlobSize()
	 */
	public int getBlobSize() {
		throw new UnsupportedOperationException("can't compute the size of a stream");
		
		/*int c = 0;
		try {
			while (_stream.read() != 0)
				c++;
		} catch (IOException e) {
			logger.error("IOException catched while calculating the size of the stream", e);
			e.printStackTrace();
		}
		return c;*/
	}

	/**
	 * @see IBlobReader#idOfScanAt(int)
	 */
	public int getScanIdAt(int idx) {
		
		int lastScanId = 0;
		try {
			for (int j = 0; j < idx; j++) {
				
				byte[] scanIdBytes = new byte[4];
				_stream.read(scanIdBytes);
				lastScanId = BytesUtils.bytesToInt(scanIdBytes, 0);

				byte[] peaksCountBytes = new byte[4];
				_stream.read(peaksCountBytes);
				int peaksCount = BytesUtils.bytesToInt(peaksCountBytes, 0);
				
				DataEncoding de = this._dataEncodingByScanId.get(lastScanId);
				this.checkDataEncodingIsNotNull(de, lastScanId);
				
				_stream.skip(peaksCount * de.getPeakStructSize());
			}
			_stream.close();
		} catch (IOException e) {
			logger.error("IOException has been catched while closing stream", e);
		}
		
		return lastScanId;
	}

	/**
	 * @see IBlobReader#nbPeaksOfScanAt(int)
	 */
	/*public int nbPeaksOfScanAt(int i) {
		int lastNbPeaks = 0;
		try {
			for (int j = 1; j <= i; j++) {
				byte[] b = new byte[4];
				_stream.read(b);
				int id = BytesUtils.bytesToInt(b, 0);
				byte[] bytes = new byte[4];
				_stream.read(bytes);
				int nbPeaks = BytesUtils.bytesToInt(bytes, 0);
				lastNbPeaks = nbPeaks;
				DataEncoding de = this._dataEncodingByScanId.get(id);
				int structSize = de.getPeakEncoding().getValue();
				if (de.getMode() == DataMode.FITTED)
					structSize += 8;
				_stream.skip(nbPeaks * structSize);
			}
			_stream.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return lastNbPeaks;
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
	 * @see IBlobReader#scanSliceOfScanAt(int)
	 */
	public ScanSlice readScanSliceAt(int idx) {
		
		byte[] peaksBytes = null;
		int scanId = 0, peaksCount = 0;
		DataEncoding de = null;
		
		try {
			for (int j = 0; j < idx; j++) {
				
				byte[] scanIdBytes = new byte[4];
				_stream.read(scanIdBytes);
				scanId = BytesUtils.bytesToInt(scanIdBytes, 0);

				byte[] peaksCountBytes = new byte[4];
				_stream.read(peaksCountBytes);
				peaksCount = BytesUtils.bytesToInt(peaksCountBytes, 0);

				de = this._dataEncodingByScanId.get(scanId);

				byte[] pb = new byte[peaksCount * de.getPeakStructSize()];
				_stream.read(pb);
				peaksBytes = pb;
			}
			_stream.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (peaksBytes == null) {
			return null;
		}

		ScanData scanSliceData = this.readScanSliceData(ByteBuffer.wrap(peaksBytes), 0, peaksBytes.length, de);
		
		return new ScanSlice(_scanHeaderById.get(scanId), scanSliceData);
	}

	/**
	 * @see IBlobReader#asScanSlicesArray(int, int)
	 */
	/*public ScanSlice[] asScanSlicesArray(int firstScanId, int runSliceId) {
		List<ScanSlice> sl = new ArrayList<ScanSlice>();
		int i = 1;
		while (true) {
			ScanSlice s = this.scanSliceOfScanAt(i);
			if (s == null) {
				break;
			}
			s.setRunSliceId(runSliceId);
			sl.add(s);
			i++;
		}
		try {
			_stream.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return sl.toArray(new ScanSlice[sl.size()]);
	}*/

	/*protected void _indexScanSlices() {

	}*/

}