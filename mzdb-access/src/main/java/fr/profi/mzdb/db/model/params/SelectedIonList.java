package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class SelectedIonList extends AbstractParamTree {

  @XmlAttribute(required = true)
  @XmlSchemaType(name = "nonNegativeInteger")
  protected int count;


  protected List<SelectedIon> selectedIons;

  public SelectedIonList() {
  }

  public SelectedIonList(SerializationReader reader) throws IOException {
    read(reader);
  }
  
  public SelectedIonList(int c) {
    this.count = c;
  }

  @XmlElement( name="selectedIon" )
  public List<SelectedIon> getSelectedIons() {
    return selectedIons;
  }

  public void setCount(int count) {
    this.count = count;
  }

  public void setSelectedIons(List<SelectedIon> selectedIons) {
    this.selectedIons = selectedIons;
  }

  @Override
  public void write(SerializationWriter writer) throws IOException {
    super.write(writer);

    writer.writeInt32(count);

    writer.writeInt32(selectedIons.size());
    for (SerializationInterface serializableObject : selectedIons) {
      serializableObject.write(writer);
    }


  }

  @Override
  public void read(SerializationReader reader) throws IOException {
    super.read(reader);

    count = reader.readInt32();

    int size = reader.readInt32();
    selectedIons = new ArrayList<>(size);
    for (int i = 0; i < size; i++) {
      SelectedIon element = new SelectedIon(reader);
      selectedIons.add(element);
    }
  }
}