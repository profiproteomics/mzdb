package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.db.table.CvUnitTable;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

public class CVUnit implements SerializationInterface {

  public static final String TABLE_NAME = CvUnitTable.tableName;

  protected String accession;

  protected String name;

  protected String cvId;

  public CVUnit(SerializationReader reader) throws IOException {
    read(reader);
  }

  public CVUnit(String accession, String name, String cvId) {
    this.accession = accession;
    this.name = name;
    this.cvId = cvId;
  }

  public String getAccession() {
    return accession;
  }

  public void setAccession(String accession) {
    this.accession = accession;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getCvId() {
    return cvId;
  }

  public void setCvId(String cvId) {
    this.cvId = cvId;
  }

  @Override
  public void write(SerializationWriter writer) throws IOException {
    writer.writeString(accession);
    writer.writeString(name);
    writer.writeString(cvId);
  }

  @Override
  public void read(SerializationReader reader) throws IOException {
    accession = reader.readString();
    name = reader.readString();
    cvId = reader.readString();
  }
}
