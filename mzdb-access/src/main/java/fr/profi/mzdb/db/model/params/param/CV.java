package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.db.table.CvTable;

public class CV {

  public static final String TABLE_NAME = CvTable.tableName;

  protected String cvId;

  protected String fullName;

  protected String cvVersion;

  protected String uri;

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
}
