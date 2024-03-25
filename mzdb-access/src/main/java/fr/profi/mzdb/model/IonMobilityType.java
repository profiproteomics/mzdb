package fr.profi.mzdb.model;

import java.util.Optional;

public enum IonMobilityType {

  TIMS("TIMS", ""),
  FAIMS("FAIMS", "");
  private String description;
  private String code;

  IonMobilityType(String code, String description) {
    this.code = code;
    this.description = description;
  }

  public String getDescription() {
    return this.description;
  }

  public String toString() {
    return this.code;
  }

  public static Optional<IonMobilityType> getIonMobilityMode(String code){
    IonMobilityType[] modes = IonMobilityType.values();
    for (IonMobilityType mode : modes) {
      if (mode.code.equals(code)){
        return Optional.of(mode);
      }
    }
    return Optional.empty();
  }

}
