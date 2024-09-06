package fr.profi.mzdb.db.table;

public enum CvUnitTable {
  ACCESSION("accession"),
  NAME("name"),
  CV_ID("cv_id");

  public static String tableName = "cv_unit";
  private final String columnName;

  CvUnitTable(String colName) {
    this.columnName = colName;
  }

  public String getValue() {
    return columnName;
  }
}
