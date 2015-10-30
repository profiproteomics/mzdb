package fr.profi.mzdb.model;

import java.util.Iterator;

/**
 * Enumeration representing the acquisition mode. It is stored as a cvParam in the run table. This list is
 * NOT exhaustive
 *  - DDA voulant dire Data Dependant Acquisition
 *  - DDA=IDA
 *  - et DIA=SWATH
 *  - DDA et DIA sont des termes du constructeur Thermo
 *  - IDA et SWATH sont des termes du constructeurs AbSciex
 *  - il se peut que d'autres constructeurs utilisent d'autres termes
 *  - mais ce sont les deux constructeurs les plus répandus
 */
public enum AcquisitionMode {
    DDA("DDA acquisition", "Data Dependant Acquisition (Thermo designation), Warning, "
    	+ "in ABI this is called IDA (Information Dependant Acquisition)"), SWATH("SWATH acquisition",
    	"ABI Swath acquisition or Thermo swath acquisition"), MRM("MRM acquisition",
    	"Multiple reaction monitoring"), SRM("SRM acquisition", "Single reaction monitoring"), UNKNOWN(
    	"UNKNOWN acquisition", "unknown acquisition mode");
    // Other one to be added

    private String description;
    private String code;

    AcquisitionMode(String code, String description) {
        this.code = code;
        this.description = description;
    }

    public String getDescription() {
        return this.description;
    }

    public String toString() {
        return this.code;
    }
    
    public static AcquisitionMode getAcquisitionMode(String code){
    	AcquisitionMode[] acqModes = AcquisitionMode.values();
    	for (AcquisitionMode acquisitionMode : acqModes) {
    		if (acquisitionMode.code.equals(code)){
				return acquisitionMode;
			}
		}
    	return AcquisitionMode.UNKNOWN;
    }
    
}