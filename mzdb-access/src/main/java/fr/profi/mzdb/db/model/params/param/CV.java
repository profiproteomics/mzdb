package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.db.table.CvTable;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

public class CV implements SerializationInterface {

  public static final String TABLE_NAME = CvTable.tableName;

  protected String cvId;

  protected String fullName;

  protected String cvVersion;

  protected String uri;

  public CV(SerializationReader reader) throws IOException {
    read(reader);
  }

  public CV(String cvId, String fullName, String cvVersion, String uri) {
    this.cvId = cvId;
    this.fullName = fullName;
    this.cvVersion = cvVersion;
    this.uri = uri;
  }

  public String getCvId() {
    return cvId;
  }

  public void setCvId(String cvId) {
    this.cvId = cvId;
  }

  public String getFullName() {
    return fullName;
  }

  public void setFullName(String fullName) {
    this.fullName = fullName;
  }

  public String getCvVersion() {
    return cvVersion;
  }

  public void setCvVersion(String cvVersion) {
    this.cvVersion = cvVersion;
  }

  public String getUri() {
    return uri;
  }

  public void setUri(String uri) {
    this.uri = uri;
  }

  @Override
  public void write(SerializationWriter writer) throws IOException {

    boolean hasData = cvId!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(cvId);
    }

    hasData = fullName!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(fullName);
    }

    hasData = cvVersion!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(cvVersion);
    }

    hasData = uri!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(uri);
    }

  }

  @Override
  public void read(SerializationReader reader) throws IOException {

    boolean hasData = reader.readBoolean();
    if (hasData) {
      cvId = reader.readString();
    } else {
      cvId = null;
    }

    hasData = reader.readBoolean();
    if (hasData) {
      fullName = reader.readString();
    } else {
      fullName = null;
    }

    hasData = reader.readBoolean();
    if (hasData) {
      cvVersion = reader.readString();
    } else {
      cvVersion = null;
    }

    hasData = reader.readBoolean();
    if (hasData) {
      uri = reader.readString();
    } else {
      uri = null;
    }

  }
}
