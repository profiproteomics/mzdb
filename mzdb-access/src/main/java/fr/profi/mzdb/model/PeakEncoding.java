package fr.profi.mzdb.model;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.HashMap;

public enum PeakEncoding implements SerializationInterface {
	NO_LOSS_PEAK(16), HIGH_RES_PEAK(12), LOW_RES_PEAK(8);

	private int value;

	private static HashMap<Integer, PeakEncoding> map = new HashMap<Integer, PeakEncoding>();

	static {
		for (PeakEncoding legEnum : PeakEncoding.values()) {
			map.put(legEnum.value, legEnum);
		}
	}

	private PeakEncoding(int value) {
		this.value = value;
	}

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


	public static PeakEncoding getEnum(SerializationReader reader) throws IOException {
		int key = reader.readInt32();
		return map.get(key);
	}
}
