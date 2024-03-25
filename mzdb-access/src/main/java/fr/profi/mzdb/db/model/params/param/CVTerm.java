package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.db.table.CVTermTable;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

public class CVTerm implements SerializationInterface {

  public static final String TABLE_NAME = CVTermTable.tableName;

  protected String accession;

  protected String name;

  protected String unitAccession;

  protected String cvId;

  public CVTerm(SerializationReader reader) throws IOException {
    read(reader);
  }

  public CVTerm(String accession, String name, String unitAccession, String cvId) {
    this.accession = accession;
    this.name = name;
    this.unitAccession = unitAccession;
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

  public String getUnitAccession() {
    return unitAccession;
  }

  public void setUnitAccession(String unitAccession) {
    this.unitAccession = unitAccession;
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

    boolean hasData = unitAccession!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(unitAccession);
    }

    writer.writeString(cvId);

  }

  @Override
  public void read(SerializationReader reader) throws IOException {
    accession = reader.readString();
    name = reader.readString();

    boolean hasData = reader.readBoolean();
    if (hasData) {
      unitAccession = reader.readString();
    } else {
      unitAccession = null;
    }

    cvId = reader.readString();
  }
}
