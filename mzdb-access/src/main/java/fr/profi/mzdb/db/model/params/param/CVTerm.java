package fr.profi.mzdb.db.model.params.param;

import fr.profi.mzdb.db.table.CVTermTable;

public class CVTerm {

  public static final String TABLE_NAME = CVTermTable.tableName;

  protected String accession;

  protected String name;

  protected String unitAccession;

  protected String cvId;

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
}
