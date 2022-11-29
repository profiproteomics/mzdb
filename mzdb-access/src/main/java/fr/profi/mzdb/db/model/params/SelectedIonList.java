package fr.profi.mzdb.db.model.params;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import java.util.List;

public class SelectedIonList extends AbstractParamTree {

  @XmlAttribute(required = true)
  @XmlSchemaType(name = "nonNegativeInteger")
  protected int count;


  protected List<SelectedIon> selectedIons;

  public SelectedIonList() {
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
}