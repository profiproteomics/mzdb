package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.db.model.params.param.CVEntry;
import fr.profi.mzdb.serialization.SerializationInterface;
import fr.profi.mzdb.serialization.SerializationReader;
import fr.profi.mzdb.serialization.SerializationWriter;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import java.io.IOException;

@XmlRootElement(name = "precursor")
public class Precursor implements SerializationInterface {

  protected String spectrumRef;

  protected IsolationWindowParamTree isolationWindow;

  protected SelectedIonList selectedIonList;

  protected Activation activation;

  public Precursor() {
  }

  public Precursor(SerializationReader reader) throws IOException {
    read(reader);
  }

  public void setSpectrumRef(String spectrumRef) {
    this.spectrumRef = spectrumRef;
  }

  @XmlAttribute(required=true)
  public String getSpectrumRef() {
      return spectrumRef;
  }

  @XmlElement(name="isolationWindow")
  public IsolationWindowParamTree getIsolationWindow() {
      return isolationWindow;
  }

  @XmlElement(name="activation")
  public Activation getActivation() {
      return activation;
  }

  @XmlElement(name="selectedIonList")
  public SelectedIonList getSelectedIonList() {
      return selectedIonList;
  }
  
  public double parseFirstSelectedIonMz() {
	  
	  SelectedIonList sil = this.getSelectedIonList();
	  SelectedIon si = sil.getSelectedIons().get(0);
	  String precMzAsStr = si.getCVParam(CVEntry.SELECTED_ION_MZ).getValue();
	  
      return Double.parseDouble(precMzAsStr);
  }

  public void setIsolationWindow(IsolationWindowParamTree isolationWindow) {
    this.isolationWindow = isolationWindow;
  }

  public void setSelectedIonList(SelectedIonList selectedIonList) {
    this.selectedIonList = selectedIonList;
  }

  public void setActivation(Activation activation) {
    this.activation = activation;
  }

  @Override
  public void write(SerializationWriter writer) throws IOException {

    writer.writeString(spectrumRef);
    isolationWindow.write(writer);
    selectedIonList.write(writer);
    activation.write(writer);
  }

  @Override
  public void read(SerializationReader reader) throws IOException {
    spectrumRef = reader.readString();

    isolationWindow = new IsolationWindowParamTree(reader);

    selectedIonList = new SelectedIonList(reader);

    activation = new Activation(reader);

  }
}
 