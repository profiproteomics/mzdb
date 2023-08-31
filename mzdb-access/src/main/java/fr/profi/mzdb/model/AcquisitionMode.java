package fr.profi.mzdb.model;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;
import java.util.HashMap;

/**
 * Enumeration representing the acquisition mode. It is stored as a cvParam in the run table.
 * This list is NOT exhaustive:
 *  - DDA voulant dire Data Dependant Acquisition
 *  - DDA=IDA
 *  - and DIA=SWATH
 *  - DDA et DIA are Thermo terms
 *  - IDA et SWATH are AbSciex terms
 */
public enum AcquisitionMode implements SerializationInterface {
	DDA("DDA acquisition","Data Dependant Acquisition (Thermo designation), Warning: in ABI this is called IDA (Information Dependant Acquisition)"),
  DIA("SRM acquisition", "Single reaction monitoring"),
	SWATH("SWATH acquisition","ABI Swath acquisition or Thermo swath acquisition"),
  PRM("PRM acquisition", "Parallel reaction monitoring"),
  MRM("MRM acquisition",		"Multiple reaction monitoring"),
  SRM("SRM acquisition", "Single reaction monitoring"),
  UNKNOWN("UNKNOWN acquisition","unknown acquisition mode");
    // Other one to be added

    private static HashMap<String, AcquisitionMode> map = new HashMap<>();

    static {
        for (AcquisitionMode valEnum : AcquisitionMode.values()) {
            map.put(valEnum.code, valEnum);
        }
    }



    private String description;
    private String code;

    AcquisitionMode(String code, String description) {
        this.code = code;
        this.description = description;
    }

    public String getDescription() {
        return this.description;
    }

    public String toString() {
        return this.code;
    }

    public boolean isDataIndependantAcquisition() {
      return (this == DIA) || (this == SWATH) ||  (this == PRM) || (this == MRM) || (this == SRM) ;
    }

    public static AcquisitionMode getAcquisitionMode(String code){
    	AcquisitionMode val = map.get(code);
        return (val != null) ? val : AcquisitionMode.UNKNOWN;
    }

    @Override
    public void write(SerializationWriter writer) throws IOException {
        writer.writeString(code);
    }

    @Override
    public void read(SerializationReader reader) throws IOException {
        throw new IOException("read is not allowed for Enums");
    }


    public static AcquisitionMode getEnum(SerializationReader reader) throws IOException {
        String key = reader.readString();
        return map.get(key);
    }
}