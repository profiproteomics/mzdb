package fr.profi.mzdb.model;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.HashMap;

public enum ActivationType implements SerializationInterface {
  CID(0), ETD(1), EThcD(2), HCD(3), PSD(4), OTHER(5);


  private static HashMap<Integer, ActivationType> map = new HashMap<>();

  static {
    for (ActivationType legEnum : ActivationType.values()) {
      map.put(legEnum.value, legEnum);
    }
  }

  private final int value;

  private ActivationType(int val) {
    value = val;
  }


  @Override
  public void write(SerializationWriter writer) throws IOException {
    writer.writeInt32(value);
  }

  @Override
  public void read(SerializationReader reader) throws IOException {
    throw new IOException("read is not allowed for Enums");
  }

  public static ActivationType getEnum(SerializationReader reader) throws IOException {
    int key = reader.readInt32();
    return map.get(key);
  }
}
