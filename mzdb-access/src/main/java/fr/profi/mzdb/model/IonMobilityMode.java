package fr.profi.mzdb.model;

import java.util.List;

public class IonMobilityMode {

  private IonMobilityType mobilityType;
  private List<String> separationValues;

  public IonMobilityMode(IonMobilityType type, List<String> compensationVoltageValues) {
    this.mobilityType = type;
    this.separationValues = compensationVoltageValues;
  }

  public IonMobilityMode(IonMobilityType type) {
    this.mobilityType = type;
  }

  public IonMobilityType getIonMobilityType() {
    return mobilityType;
  }

  public List<String> getSeparationValues() {
    return separationValues;
  }
}
