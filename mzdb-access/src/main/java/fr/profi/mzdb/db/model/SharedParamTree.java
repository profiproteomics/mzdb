package fr.profi.mzdb.db.model;

import fr.profi.mzdb.db.model.params.ReferencableParamGroup;
import fr.profi.mzdb.model.ActivationType;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;
import fr.profi.mzdb.util.misc.AbstractInMemoryIdGen;

import java.io.IOException;

public class SharedParamTree extends AbstractInMemoryIdGen implements SerializationInterface {
  public static final String TABLE_NAME = "shared_param_tree";

  /** The id. */
  protected long id;

  /** Associated data. */
  protected ReferencableParamGroup data;

  /** Schema name. */
  protected String schemaName;

  public SharedParamTree(SerializationReader reader) throws IOException {
    read(reader);
  }

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

  @Override
  public void write(SerializationWriter writer) throws IOException {

    writer.writeInt64(id);

    boolean hasData = data!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      data.write(writer);
    }



    hasData = schemaName!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeString(schemaName);
    }

  }

  @Override
  public void read(SerializationReader reader) throws IOException {

    id = reader.readInt64();


    boolean hasData = reader.readBoolean();
    if (hasData) {
      data = new ReferencableParamGroup(reader);
    } else {
      data = null;
    }

    hasData = reader.readBoolean();
    if (hasData) {
      schemaName =  reader.readString();
    } else {
      schemaName = null;
    }
  }

}
