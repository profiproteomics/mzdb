/*
 * Package fr.profi.mzdb.model
 * @author David Bouyssie
 */
package fr.profi.mzdb.model;


import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.nio.ByteOrder;


// TODO: Auto-generated Javadoc
/**
 * The Class DataEncoding. 1 dataEncoding to 1 Spectrum
 * 
 * @author David Bouyssie
 */
public class DataEncoding implements Cloneable, SerializationInterface {

	/** The id. */
	protected long id;

	/** The mode. */
	protected DataMode mode;

	/** The peak encoding. */
	protected PeakEncoding peakEncoding;

	/** The compression. */
	protected String compression;

	/** The byte order. */
	protected ByteOrder byteOrder;
	
	/** The byte order. */
	protected int peakStructSize;

	public DataEncoding(SerializationReader reader) throws IOException {
		read(reader);
	}

	/**
	 * Instantiates a new data encoding.
	 * 
	 * @param id
	 *            the id
	 * @param mode
	 *            the mode
	 * @param peakEncoding
	 *            the peak encoding
	 * @param compression
	 *            the compression
	 * @param byteOrder
	 *            the byte order
	 */
	public DataEncoding(long id, DataMode mode, PeakEncoding peakEncoding, String compression, ByteOrder byteOrder) {
		super();
		this.id = id;
		this.mode = mode;
		this.peakEncoding = peakEncoding;
		this.compression = compression;
		this.byteOrder = byteOrder;
		
		int peakBytesSize = this.getPeakEncoding().getValue();
		if (this.getMode() == DataMode.FITTED)
			peakBytesSize += 8; // add 2 floats (left hwhm and right hwhm)
		
		this.peakStructSize = peakBytesSize;
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
	 * Gets the mode.
	 * 
	 * @return the mode
	 */
	public DataMode getMode() {
		return mode;
	}

	/**
	 * Gets the peak encoding.
	 * 
	 * @return the peak encoding
	 */
	public PeakEncoding getPeakEncoding() {
		return peakEncoding;
	}

	/**
	 * Sets the peak encoding.
	 * 
	 * @param p
	 *            the new peak encoding
	 */
	public void setPeakEncoding(PeakEncoding p) {
		this.peakEncoding = p;
	}

	/**
	 * Gets the compression.
	 * 
	 * @return the compression
	 */
	public String getCompression() {
		return compression;
	}

	/**
	 * Gets the byte order.
	 * 
	 * @return the byte order
	 */
	public ByteOrder getByteOrder() {
		return byteOrder;
	}
	
	public int getPeakStructSize() {
		return peakStructSize;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#clone()
	 */
	public DataEncoding clone() {
		return new DataEncoding(id, mode, peakEncoding, compression, byteOrder);
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {

		writer.writeInt64(id);

		//VDS SQL Not Null
		boolean hasData = mode!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			mode.write(writer);
		}

		hasData = peakEncoding!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			peakEncoding.write(writer);
		}

		hasData = compression!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(compression);
		}

//VDS SQL Not Null
		hasData = byteOrder!=null;
		writer.writeBoolean(hasData);
		if (hasData) {
			writer.writeString(byteOrder.toString());
		}

		writer.writeInt32(peakStructSize);

	}

	@Override
	public void read(SerializationReader reader) throws IOException {

		id = reader.readInt64();

		boolean hasData = reader.readBoolean();
		if (hasData) {
			mode = DataMode.getEnum(reader);
		} else {
			mode = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			peakEncoding = PeakEncoding.getEnum(reader);
		} else {
			peakEncoding = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			compression = reader.readString();
		} else {
			compression = null;
		}

		hasData = reader.readBoolean();
		if (hasData) {
			String byteOrderString = reader.readString();
			if (byteOrderString.equals(ByteOrder.BIG_ENDIAN.toString())) {
				byteOrder = ByteOrder.BIG_ENDIAN;
			} else {
				byteOrder = ByteOrder.LITTLE_ENDIAN;
			}
		} else {
			byteOrder = null;
		}

		peakStructSize = reader.readInt32();

	}

}
