package fr.profi.mzdb.db.model;

import fr.profi.mzdb.db.model.params.ReferencableParamGroup;
import fr.profi.mzdb.util.misc.AbstractInMemoryIdGen;

public class SharedParamTree extends AbstractInMemoryIdGen {
  public static final String TABLE_NAME = "shared_param_tree";

  /** The id. */
  protected long id;

  /** Associated data. */
  protected ReferencableParamGroup data;

  /** Schema name. */
  protected String schemaName;


  public SharedParamTree(long id, ReferencableParamGroup data, String schemaName) {
    this.id = id;
    this.data = data;
    this.schemaName = schemaName;
  }

  public SharedParamTree() {

  }

  public long getId() {
    return id;
  }

  public void setId(long id) {
    this.id = id;
  }

  public ReferencableParamGroup getData() {
    return data;
  }

  public void setData(ReferencableParamGroup data) {
    this.data = data;
  }

  public String getSchemaName() {
    return schemaName;
  }

  public void setSchemaName(String schemaName) {
    this.schemaName = schemaName;
  }

}
