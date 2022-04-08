package fr.profi.mzdb.db.model.params;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "referenceableParamGroup")
public class ReferencableParamGroup extends AbstractParamTree {

  protected  String id;

  public ReferencableParamGroup() {

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
}
