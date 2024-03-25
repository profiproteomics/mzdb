package fr.profi.mzdb.db.table;

public enum UserTermTable {

    ID("id"),
    NAME("name"),
    TYPE("type"),
    UNIT_ACCESSION("unit_accession");

    public static String tableName = "user_term";
    private final String columnName;

    UserTermTable(String colName) {
        this.columnName = colName;
    }

    public String getValue() {
        return columnName;
    }
}
