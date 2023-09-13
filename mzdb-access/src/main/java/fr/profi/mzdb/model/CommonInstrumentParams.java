package fr.profi.mzdb.model;

import fr.profi.mzdb.db.model.AbstractTableModel;
import fr.profi.mzdb.db.model.params.ParamTree;

public class CommonInstrumentParams extends AbstractTableModel {
  protected String schemaName ="CommonInstrumentParams";

  public CommonInstrumentParams(long id, ParamTree paramTree) {
    super(id, paramTree);
  }

  public String getSchemaName() {
    return schemaName;
  }
}
