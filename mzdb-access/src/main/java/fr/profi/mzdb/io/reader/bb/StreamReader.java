package fr.profi.mzdb.io.reader.bb;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.util.Map;

import fr.profi.mzdb.model.DataEncoding;
import fr.profi.mzdb.model.SpectrumData;
import fr.profi.mzdb.model.SpectrumHeader;
import fr.profi.mzdb.model.SpectrumSlice;
import fr.profi.mzdb.util.primitives.BytesUtils;

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
	 * @see AbstractBlobReader
	 * @see AbstractBlobReader#_dataEncodingBySpectrumId
	 */
	public StreamReader(
		final InputStream inputStream,
		final long firstSpectrumId,
		final long lastSpectrumId,
		final Map<Long, SpectrumHeader> spectrumHeaderById,
		final Map<Long, DataEncoding> dataEncodingBySpectrumId
	) {
		super(firstSpectrumId, lastSpectrumId, spectrumHeaderById, dataEncodingBySpectrumId);
		
		this._stream = inputStream;
	}

	/**
	 * @see IBlobReader#disposeBlob()
	 */
	public void disposeBlob() {
		try {
			_stream.close();
		} catch (IOException e) {
			logger.error("IOException has been caught while closing stream", e);
		}
	}

	/**
	 * @see IBlobReader#getSpectraCount()
	 */
	public int getSpectraCount() {
		// FIXME: this information should be added to the BB to optimize performances
		return -1;
	}


	/**
	 * @see IBlobReader#getSpectrumIdAt(int)
	 */
	public long getSpectrumIdAt(final int idx) {
		
		long lastSpectrumId = 0;
		try {
			for (int j = 0; j <= idx; j++) {
				
				final byte[] spectrumIdBytes = new byte[4];
				_stream.read(spectrumIdBytes);
				lastSpectrumId = (long) BytesUtils.bytesToInt(spectrumIdBytes, 0);

				final byte[] peaksCountBytes = new byte[4];
				_stream.read(peaksCountBytes);
				int peaksCount = BytesUtils.bytesToInt(peaksCountBytes, 0);
				
				final DataEncoding de = this._dataEncodingBySpectrumId.get( lastSpectrumId);
				this.checkDataEncodingIsNotNull(de, lastSpectrumId);
				
				_stream.skip(peaksCount * de.getPeakStructSize());
			}
			_stream.close();
		} catch (IOException e) {
			logger.error("IOException has been caught while closing stream", e);
		}
		
		return lastSpectrumId;
	}


	/**
	 * @see IBlobReader#readSpectrumSliceAt(int)
	 */
	public SpectrumSlice readSpectrumSliceAt(final int idx) {
		return this._readSpectrumSliceAt(idx, -1.0, -1.0);
	}

	/**
	 * @see IBlobReader#readSpectrumSliceAt(int)
	 */
	private SpectrumSlice _readSpectrumSliceAt(final int idx, final double minMz, final double maxMz) {
		
		byte[] peaksBytes = null;
		long spectrumId = 0;
		int peaksCount = 0;
		DataEncoding de = null;
		
		try {
			for (int j = 0; j <= idx; j++) {
				
				final byte[] spectrumIdBytes = new byte[4];
				_stream.read(spectrumIdBytes);
				spectrumId = (long) BytesUtils.bytesToInt(spectrumIdBytes, 0);
				de = this._dataEncodingBySpectrumId.get(spectrumId);
				
				final byte[] peaksCountBytes = new byte[4];
				_stream.read(peaksCountBytes);
				peaksCount = BytesUtils.bytesToInt(peaksCountBytes, 0);
				
				final int peaksBytesSize = peaksCount * de.getPeakStructSize();
				
				// If not on specified index
				if( j < idx ) {
					// skip the peaks
					_stream.skip(peaksBytesSize);
				} else {
					// read peaks
					final byte[] pb = new byte[peaksBytesSize];
					_stream.read(pb);
					peaksBytes = pb;
				}

			}
			_stream.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		if (peaksBytes == null) {
			return null;
		}

		final SpectrumData spectrumSliceData = this.readSpectrumSliceData(ByteBuffer.wrap(peaksBytes), 0, peaksBytes.length, de, minMz, maxMz);
		
		return new SpectrumSlice(_spectrumHeaderById.get(spectrumId), spectrumSliceData);
	}
	
	// TODO: call this method from readSpectrumSliceAt instead of calling readSpectrumSliceAt from this methods
	public SpectrumData readSpectrumSliceDataAt(final int idx) {
		return readSpectrumSliceAt(idx).getData();
	}
	
	public SpectrumData readFilteredSpectrumSliceDataAt(final int idx, final double minMz, final double maxMz) {
		return this._readSpectrumSliceAt(idx, minMz, maxMz).getData();
	}

}
