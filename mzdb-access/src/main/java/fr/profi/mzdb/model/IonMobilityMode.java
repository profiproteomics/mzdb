package fr.profi.mzdb.model;

import java.util.List;

public class IonMobilityMode {

  private IonMobilityType mobilityType;
  private List<String> compensationVoltageValues;

  public IonMobilityMode(IonMobilityType mode, List<String> compensationVoltageValues) {
    this.mobilityType = mode;
    this.compensationVoltageValues = compensationVoltageValues;
  }

  public IonMobilityMode(IonMobilityType mode) {
    this.mobilityType = mode;
  }

  public IonMobilityType getIonMobilityMode() {
    return mobilityType;
  }

  public List<String> getCompensationVoltageValues() {
    return compensationVoltageValues;
  }
}
