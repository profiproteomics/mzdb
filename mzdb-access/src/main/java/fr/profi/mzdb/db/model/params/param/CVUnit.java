package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.db.table.CvUnitTable;

public class CVUnit {

  public static final String TABLE_NAME = CvUnitTable.tableName;

  protected String accession;

  protected String name;

  protected String cvId;

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

}
