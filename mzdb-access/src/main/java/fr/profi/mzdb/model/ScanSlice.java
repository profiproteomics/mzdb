/*
 * Package fr.profi.mzdb.model
 * @author David Bouyssie
 */
package fr.profi.mzdb.model;

// TODO: Auto-generated Javadoc
/**
 * The Class ScanSlice.
 *
 * @author David Bouyssie
 */
public class ScanSlice extends Scan {

//ScanData {

	/** The scan id. */
  protected int scanId;
	
	/** The run slice id. */
  protected int runSliceId;
	
	/**
	 * Instantiates a new scan slice.
	 *
	 * @param scanId the scan id
	 * @param runSliceId the run slice id
	 * @param mzList the mz list
	 * @param intensityList the intensity list
	 */
  public ScanSlice(ScanHeader header, ScanData scanData) {
    super(header, scanData);
  }
	
  /**
   * Gets the scan id.
   *
   * @return the scan id
   */
  public int getScanId() {
    return scanId;
  }

  /**
   * Sets the scan id.
   *
   * @param scanId the new scan id
   */
  public void setScanId(int scanId) {
    this.scanId = scanId;
  }

  /**
   * Gets the run slice id.
   *
   * @return the run slice id
   */
  public int getRunSliceId() {
    return runSliceId;
  }

  /**
   * Sets the run slice id.
   *
   * @param runSliceId the new run slice id
   */
  public void setRunSliceId(int runSliceId) {
    this.runSliceId = runSliceId;
  }
}
