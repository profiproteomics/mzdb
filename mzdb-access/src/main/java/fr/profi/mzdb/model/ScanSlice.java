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
public class ScanSlice extends ScanData {

	/** The scan id. */
  protected int scanId;
	
	/** The run slice id. */
  protected int runSliceId;
	
	/** The first scan id. */
  protected int firstScanId;
	
	/**
	 * Instantiates a new scan slice.
	 *
	 * @param scanId the scan id
	 * @param runSliceId the run slice id
	 * @param mzList the mz list
	 * @param intensityList the intensity list
	 */
	public ScanSlice(int scanId, int runSliceId, double[] mzList, float[] intensityList) {
		super(mzList, intensityList);
		this.scanId = scanId;
		this.runSliceId = runSliceId;
	}
	
	// TODO: is run slice id mandatory ?
	/**
	 * Instantiates a new scan slice.
	 *
	 * @param mzList the mz list
	 * @param intensityList the intensity list
	 * @param lHwhmList the l hwhm list
	 * @param rHwhmList the r hwhm list
	 */
	public ScanSlice( double[] mzList, float[] intensityList, float[] lHwhmList, float[] rHwhmList ) {
	  super( mzList, intensityList, lHwhmList, rHwhmList );
	  this.scanId = 0;
	  this.runSliceId = 0;
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

  /**
   * Gets the first scan id.
   *
   * @return the first scan id
   */
  public int getFirstScanId() {
    return firstScanId;
  }

  /**
   * Sets the first scan id.
   *
   * @param firstScanId the new first scan id
   */
  public void setFirstScanId(int firstScanId) {
    this.firstScanId = firstScanId;
  }


}
