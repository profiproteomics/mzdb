package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;
import java.io.IOException;

@XmlRootElement(name = "referenceableParamGroup")
public class ReferencableParamGroup extends AbstractXMLParamTree {

  protected  String id;

  public ReferencableParamGroup() {

  }

  public ReferencableParamGroup(SerializationReader reader) throws IOException {
    read(reader);
  }

  public ReferencableParamGroup(String id) {
    this.id = id;
  }

  @XmlAttribute(required = true)
  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  @Override
  public void write(SerializationWriter writer) throws IOException {
    super.write(writer);

    writer.writeString(id);

  }

  @Override
  public void read(SerializationReader reader) throws IOException {
    super.read(reader);

    id = reader.readString();
  }
}
