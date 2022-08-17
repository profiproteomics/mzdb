package fr.profi.mzdb.db.model.params;

import fr.profi.mzdb.db.model.params.param.CVEntry;

import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement(name = "precursor")
public class Precursor {

  protected String spectrumRef;

  protected IsolationWindowParamTree isolationWindow;

  protected SelectedIonList selectedIonList;

  protected Activation activation;

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
}
 