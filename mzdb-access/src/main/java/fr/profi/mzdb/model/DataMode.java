/*
 * Package fr.profi.mzdb.model
 * @author Marc Dubois
 */
package fr.profi.mzdb.model;

// TODO: Auto-generated Javadoc

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.HashMap;

/**
 * The Enum DataMode.
 * 
 * @author Marc Dubois
 */
public enum DataMode implements SerializationInterface {

	/** The profile. */
	PROFILE(-1),

	/** The centroid. */
	CENTROID(12),

	/** The fitted. */
	FITTED(20);

	/** The value. */
	private final int value;

	private static HashMap<Integer, DataMode> map = new HashMap<Integer, DataMode>();

	static {
		for (DataMode legEnum : DataMode.values()) {
			map.put(legEnum.value, legEnum);
		}
	}

	/**
	 * Instantiates a new data mode.
	 * 
	 * @param val
	 *            the val
	 */
	private DataMode(int val) {
		value = val;
	}

	/**
	 * Gets the value.
	 * 
	 * @return the value
	 */
	public int getValue() {
		return value;
	}

	@Override
	public void write(SerializationWriter writer) throws IOException {
		writer.writeInt32(value);
	}

	@Override
	public void read(SerializationReader reader) throws IOException {
		throw new IOException("read is not allowed for Enums");
	}

	public static DataMode getEnum(SerializationReader reader) throws IOException {
		int key = reader.readInt32();
		return map.get(key);
	}
}
