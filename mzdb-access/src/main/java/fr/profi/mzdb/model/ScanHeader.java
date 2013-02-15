/*
 * Package fr.profi.mzdb.model
 * @author David Bouyssie
 */
package fr.profi.mzdb.model;

import java.util.Comparator;

import fr.profi.mzdb.db.model.AbstractTableModel;

// TODO: Auto-generated Javadoc
/**
 * The Class ScanHeader.
 *
 * @author David Bouyssie
 */
public class ScanHeader extends AbstractTableModel implements ILcContext {
	
  /** The id. */
  protected final int id;
  
  /** The initial id. */
  protected final int initialId;
  
  /** The cycle. */
  protected final int cycle;
  
  /** The time. */
  protected final float time;
  
  /** The ms level. */
  protected final int msLevel;
  
  /** The peaks count. */
  protected final int peaksCount;
  
  /** Is high resolution boolean. */
  protected final boolean isHighResolution;
  
  protected final float tic;
  
  /** The base peak mz. */
  protected final double basePeakMz;
  
  /** The base peak intensity. */
  protected final float basePeakIntensity;
  
  /** The precursor mz. */
  protected final double precursorMz;
  
  /** The precursor charge. */
  protected final int precursorCharge;

  //protected final float tic;

  /**
   * Instantiates a new scan header.
   *
   * @param id the id
   * @param initialId the initial id
   * @param cycle the cycle
   * @param time the time
   * @param msLevel the ms level
   * @param peaksCount the peaks count
   * @param dataMode the data mode
   * @param basePeakMz the base peak mz
   * @param basePeakIntensity the base peak intensity
   * @param precursorMz the precursor mz
   * @param precursorCharge the precursor charge
   */
  public ScanHeader(int id, int initialId, int cycle, float time, int msLevel,
                    int peaksCount, boolean isHighResolution, float tic,
                    double basePeakMz, float basePeakIntensity,
                    double precursorMz, int precursorCharge
                    ) {
    super(id, null);
    this.id = id;
    this.initialId = initialId;
    this.cycle = cycle;
    this.time = time;
    this.msLevel = msLevel;
    this.peaksCount = peaksCount;
    this.isHighResolution = isHighResolution;
    this.tic = tic;
    this.basePeakMz = basePeakMz;
    this.basePeakIntensity = basePeakIntensity;
    this.precursorMz = precursorMz;
    this.precursorCharge = precursorCharge;
  }
  
	/**
	 * Instantiates a new scan header.
	 *
	 * @param id the id
	 * @param initialId the initial id
	 * @param cycle the cycle
	 * @param time the time
	 * @param msLevel the ms level
	 * @param peaksCount the peaks count
	 */
	/*public ScanHeader(int id, int initialId, int cycle, float time, int msLevel, int peaksCount) {
		this( id, initialId, cycle, time, msLevel, peaksCount, false, 0, 0, 0, 0);
	}*/
	
  /**
   * Gets the id.
   *
   * @return the id
   */
  public int getId() {
    return id;
  }

  /**
   * Gets the initial id.
   *
   * @return the initial id
   */
  public int getInitialId() {
    return initialId;
  }

  /**
   * Gets the cycle.
   *
   * @return the cycle
   */
  public int getCycle() {
    return cycle;
  }

  /**
   * Gets the time.
   *
   * @return the time
   */
  public float getTime() {
    return time;
  }

  /**
   * Gets the ms level.
   *
   * @return the ms level
   */
  public int getMsLevel() {
    return msLevel;
  }

  /**
   * Gets the peaks count.
   *
   * @return the peaks count
   */
  public int getPeaksCount() {
    return peaksCount;
  }  
  
  /**
   * Checks if is high resolution.
   *
   * @return true, if is high resolution
   */
  public boolean isHighResolution() {
	  return isHighResolution;
  }
  
  /**
   * Gets the base peak mz.
   *
   * @return the base peak mz
   */
  public double getBasePeakMz() {
    return basePeakMz;
  }

  /**
   * Gets the base peak intensity.
   *
   * @return the base peak intensity
   */
  public float getBasePeakIntensity() {
    return basePeakIntensity;
  }

  /**
   * Gets the precursor mz.
   *
   * @return the precursor mz
   */
  public double getPrecursorMz() {
    return precursorMz;
  }

  /**
   * Gets the precursor charge.
   *
   * @return the precursor charge
   */
  public int getPrecursorCharge() {
    return precursorCharge;
  }
  
  /* (non-Javadoc)
   * @see fr.profi.mzdb.model.ILcContext#getScanId()
   */
  public int getScanId() {
    return this.id;
  }

  /* (non-Javadoc)
   * @see fr.profi.mzdb.model.ILcContext#getElutionTime()
   */
  public float getElutionTime() {
    return this.time;
  }

  /** The rt comp. */
  public static Comparator<ScanHeader> rtComp = new Comparator<ScanHeader>() {
    //@Override
    public int compare(ScanHeader o1, ScanHeader o2) {
      if (o1.time < o2.time)return -1;
      else if (Math.abs(o1.time - o2.time) < 1e-6)return 0;
      else return 1;
    }
  };

  public float getTIC() {
	  return tic;
  }
  
}
