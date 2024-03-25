package fr.profi.mzdb.db.table;

public enum CVTermTable {
  ACCESSION("accession"),
  NAME("name"),
  UNIT_ACCESSION("unit_accessionb"),
  CV_ID("cv_id");

  public static String tableName = "cv_term";
  private final String columnName;

  CVTermTable(String colName) {
    this.columnName = colName;
  }

  public String getValue() {
    return columnName;
  }
}
