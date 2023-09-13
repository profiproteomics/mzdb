package fr.profi.mzdb.model;

import fr.profi.mzdb.db.model.AbstractTableModel;
import fr.profi.mzdb.db.model.params.ParamTree;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import java.io.IOException;

public class ProcessingMethod extends AbstractTableModel implements SerializationInterface {

  protected int number;

  protected String dataProcessingName;

  protected Integer softwareId;

  public ProcessingMethod() {

  }

  /**
   * Instantiates a new abstract table model.
   *
   * @param id        the id
   * @param paramTree
   */
  public ProcessingMethod(long id, ParamTree paramTree, int number, String dataProcessingName, Integer softwareId) {
    super(id, paramTree);
    this.number = number;
    this.dataProcessingName = dataProcessingName;
    this.softwareId = softwareId;
  }

  public int getNumber() {
    return number;
  }

  public void setNumber(int number) {
    this.number = number;
  }

  public String getDataProcessingName() {
    return dataProcessingName;
  }

  public void setDataProcessingName(String dataProcessingName) {
    this.dataProcessingName = dataProcessingName;
  }

  public Integer getSoftwareId() {
    return softwareId;
  }

  public void setSoftwareId(Integer softwareId) {
    this.softwareId = softwareId;
  }

  @Override
  public void write(SerializationWriter writer) throws IOException {

    writer.writeInt32(number);
    writer.writeString(dataProcessingName);

    boolean hasData = softwareId!=null;
    writer.writeBoolean(hasData);
    if (hasData) {
      writer.writeInt32(softwareId);
    }


  }

  @Override
  public void read(SerializationReader reader) throws IOException {
    number = reader.readInt32();
    dataProcessingName = reader.readString();

    boolean hasData = reader.readBoolean();
    if (hasData) {
      softwareId = reader.readInt32();
    } else {
      softwareId = null;
    }

  }
}
